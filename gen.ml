
module E = Expression.Make(Algebra.Field_R)

open E

type env = {
    color : float Array.t;
    back_color : float Array.t option;
    line_color : float Array.t;
    line_width : float;
    specular : float;
    shininess : float;
    precision : float;
    precision_derive : float;
    mindivs : int;
  }

let default_env =
  { color = [|0.75;0.75;0.4;1.0|];
    back_color = Some [|0.75;0.4;0.4;1.0|];
    line_color = [|1.0;1.0;1.0;1.0|];
    line_width = 1.5;
    specular = 0.25;
    shininess = 50.;
    precision = 0.2;
    precision_derive = 0.5;
    mindivs = 0;
  }

type curve =
  { name: string
  ; expr: elem
  ; env : env
  }

type surface =
  { name : string
  ; expr : elem
  ; bound : elem option
  ; env : env
  ; transparent : bool
  ; mutable curves : curve list
  }

type nature =
  Curve of string
| Surface of curve list

let check_simplify e =
  let e = simplify e in
  let bad_vars =
    List.find_all
      (fun v -> v <> "x" && v <> "y" && v <> "z" && v <> "time")
      (E.varlist e)
  in
  if bad_vars <> [] then
    begin
      let bad_vars = List.sort_uniq compare bad_vars in
      let bad_vars = String.concat ", " bad_vars in
      failwith (Printf.sprintf "Illega variable for surfaces: %s" bad_vars);
    end;
  e

let glsl name nature ?(bound=Cst (-1.0)) e =
  let write_defs = E.write_defs ~lang:Glsl in
  let e = check_simplify e in
  (*let _ = E.write_defs Format.std_formatter [("x", e)] in*)
  let bound = check_simplify bound in
  let fname =
    match nature with
     | Curve sname ->
        Format.asprintf "f_%s_%s" sname name
     | Surface _ ->
        Format.asprintf "f_%s" name
  in
  let dxe = derive e "x" in
  let dye = derive e "y" in
  let dze = derive e "z" in
  let color =
    match nature with
    | Surface cs ->
       let colors =
         List.fold_left (fun color ({ name = idc; env; _ }:curve) ->
             Format.asprintf
               "%s\n\
                {vec3 nrm = cross(df_%s_%s(p), dfv);\n\
                vec3 tnrm = NormalMatrix * nrm;\n\
                vec4 m_pos = ModelView * vec4(p,1.0);\n\
                float pxs = %.12f * (distance(eyePos,m_pos.xyz))/screen_size\n\
                            * distance(vec3(0.0), tnrm);\n\
                float vx = abs(f_%s_%s(p) / pxs);\n\
                if (vx < vb) { vb = vx; color = vec4(%.12f,%.12f,%.12f,%.12f); }\n\
                }\n\
                " color name idc env.line_width name idc
                  env.line_color.(0) env.line_color.(1)
                  env.line_color.(2) env.line_color.(3)) "" cs
       in
       Format.asprintf
         "float c%s(vec3 p, out vec4 color)\n\
          {\n\
          vec3 dfv = normalize(d%s(p));\n\
          float vb = 10.0;\n\
          color = vec4(0.0);\n\
          %s\n\
          return vb;\n\
          }\n\
          float b%s(vec3 p) {\n\
            float x = p.x;\n\
            float y = p.y;\n\
            float z = p.z;\n\
            %a
            return res;\n\
          }\n" fname fname colors fname write_defs [("res",  bound)]
  | Curve _ -> ""
  in
  Format.asprintf
    "float %s(vec3 p)\n\
     {\n\
       float x = p.x;\n\
       float y = p.y;\n\
       float z = p.z;\n\
       %a
       return res;\n\
     }\n\
     vec3 d%s(vec3 p)\n\
     {\n\
       float x = p.x;\n\
       float y = p.y;\n\
       float z = p.z;\n\
       %a
       return vec3(dx,dy,dz);\n\
     }\n
     float f_d%s(vec3 p, out vec3 dres)\n\
     {\n\
       float x = p.x;\n\
       float y = p.y;\n\
       float z = p.z;\n\
       %a
       dres = vec3(dx,dy,dz);\n\
       return res;\n\
     }\n%s"
    fname write_defs [("res", e)]
    fname write_defs [("dx", dxe);("dy", dye);("dz", dze)]
    fname write_defs [("res", e);("dx", dxe);("dy", dye);("dz", dze)]
          color

let dispatcher surfaces =
  let res = Buffer.create 1024 in
  let fmt = Format.formatter_of_buffer res in
  Format.fprintf fmt "void init_surfaces() {\n";
  let fn s =
    let env = s.env in
    let back_color = match env.back_color with
      | None -> env.color
      | Some c -> c
    in
    Format.fprintf fmt
      "  surfaces[LASTS] = surface(\n\
           LASTS, %d, %.12f, %.12f,\n\
           vec4(%.12f,%.12f,%.12f,%.12f),\n\
           vec4(%.12f,%.12f,%.12f,%.12f),\n\
           %.12f, %.12f); LASTS++;\n\
       "
      env.mindivs env.precision env.precision_derive
      env.color.(0) env.color.(1) env.color.(2) env.color.(3)
      back_color.(0) back_color.(1) back_color.(2) back_color.(3)
      env.specular env.shininess
  in
  List.iter fn surfaces;
  Format.fprintf fmt "}\n";
  Format.fprintf fmt "float f(int id, vec3 p) {\n\
                        switch (id) {";
  let fn id s =
    Format.fprintf fmt "    case %d: return f_%s(p);" id s.name
  in
  List.iteri fn surfaces;
  Format.fprintf fmt "  default: return 0.0;}\n}\n";
  Format.fprintf fmt "vec3 df(int id, vec3 p) {\n\
                        switch (id) {";
  let fn id s =
    Format.fprintf fmt "    case %d: return df_%s(p);" id s.name
  in
  List.iteri fn surfaces;
  Format.fprintf fmt "  default: return vec3(0.0);}\n}";
  Format.fprintf fmt "float f_df(int id, vec3 p, out vec3 df) {\n\
                        switch (id) {";
  let fn id s =
    Format.fprintf fmt "    case %d: return f_df_%s(p, df);" id s.name
  in
  List.iteri fn surfaces;
  Format.fprintf fmt "  default: df = vec3(0.0); return 0.0;}\n}";
  Format.fprintf fmt "float cf(int id, vec3 p, out vec4 color) {\n\
                        switch (id) {";
  let fn id s =
    Format.fprintf fmt "    case %d: return cf_%s(p, color);" id s.name
  in
  List.iteri fn surfaces;
  Format.fprintf fmt "  default: return 0.0;}\n}";
  Format.fprintf fmt "float bf(int id, vec3 p) {\n\
                        switch (id) {";
  let fn id s =
    Format.fprintf fmt "    case %d: return bf_%s(p);" id s.name
  in
  List.iteri fn surfaces;
  Format.fprintf fmt "  default: return 1.0;}\n}";
  Buffer.contents res

let fsqrt = {
	E.name = "sqrt";
	E.calcul = sqrt;
	E.deriv = E.Div (E.Cst (0.5), E.Tra (0, E.Var "x"));
	E.name_var = "x" }

and fexp = {
	E.name = "exp";
	E.calcul = exp;
	E.deriv = E.Tra (1, E.Var "x");
	E.name_var = "x" }

let fcos = {
	E.name = "cos";
	E.calcul = cos;
	E.deriv = E.Mul (E.Cst (-1.0), E.Tra (4, E.Var "x"));
	E.name_var = "x" }

and fsin = {
	E.name = "sin";
	E.calcul = sin;
	E.deriv = E.Tra (3, E.Var "x");
	E.name_var = "x" }

and ftan = {
	E.name = "tan";
	E.calcul = tan;
	E.deriv = E.Div (E.Cst 1.0, E.Pow( E.Tra (3, E.Var "x"), 2));
	E.name_var = "x" }

let flog = {
	E.name = "log";
	E.calcul = log;
	E.deriv = E.Div (E.Cst 1.0, E.Var "x");
	E.name_var = "x" }

let fatan = {
	E.name = "atan";
	E.calcul = atan;
	E.deriv = E.Div (E.Cst 1.0, E.Add(E.Cst 1.0, E.Pow (E.Var "x", 2)));
	E.name_var = "x" }

let facos = {
	E.name = "acos";
	E.calcul = acos;
	E.deriv = E.Div (E.Cst (-1.0), E.Tra(0, E.Sub(E.Cst 1.0, E.Pow (E.Var "x", 2))));
	E.name_var = "x" }

let fasin = {
	E.name = "asin";
	E.calcul = asin;
	E.deriv = E.Div (E.Cst 1.0, E.Tra(0, E.Sub(E.Cst 1.0, E.Pow (E.Var "x", 2))));
	E.name_var = "x" }

let fsgn = {
	E.name = "sgn";
	E.calcul = (fun x -> if x > 0.0 then 1.0 else if x < 0.0 then -1.0 else 0.0);
	E.deriv = E.Cst 0.0;
	E.name_var = "x" }

let fabs = {
	E.name = "abs";
	E.calcul = abs_float;
	E.deriv = E.Tra(10, E.Var "x");
	E.name_var = "x" }

let fpositive = {
  E.name = "positive";
  E.calcul = (fun x -> if x >= 0.0 then 1.0 else 0.0);
  E.deriv = E.Cst 0.0;
  E.name_var = "x" }

let fnegative = {
  E.name = "negative";
  E.calcul = (fun x -> if x <= 0.0 then 1.0 else 0.0);
  E.deriv = E.Cst 0.0;
  E.name_var = "x" }

let fmax = {
  E.name2 = "max";
  E.calcul2 = max;
  E.deriv1 = E.Tra(5,E.Sub(E.var "x", E.Var "y"));
  E.deriv2 = E.Tra(6,E.Sub(E.var "x", E.Var "y"));
  E.name_var1 = "x";
  E.name_var2 = "y";
}

let fmin = {
  E.name2 = "min";
  E.calcul2 = min;
  E.deriv1 = E.Tra(6,E.Sub(E.var "x", E.Var "y"));
  E.deriv2 = E.Tra(5,E.Sub(E.var "x", E.Var "y"));
  E.name_var1 = "x";
  E.name_var2 = "y";
}

module StringMap = Expression.StringMap

let _ =
  E.trans_array :=
    [| fsqrt; fexp; flog; fcos; fsin; fpositive; fnegative; fatan; facos; fasin; fsgn; fabs; ftan |];

  E.trans_table :=
    List.fold_left (fun acc (name, f) -> StringMap.add name f acc) !E.trans_table
    [ "exp", 1; "sin", 4; "cos", 3; "log", 2; "sqrt", 0;
      "positive", 5; "negative", 6; "atan", 7; "acos", 8; "asin", 9; "sgn", 10; "abs", 11; "tan", 12 ];

  E.trans2_array :=
    [| fmax; fmin |];

  E.trans2_table :=
    List.fold_left (fun acc (name, f) -> StringMap.add name f acc) !E.trans2_table
    [ "max", 0; "min", 1]

(*
let ch = open_out "out.glsl"
let _ = Printf.fprintf ch "%s\n%!" glsl_poly
let _ = close_out ch
 *)

let blank =
  Pacomb.(Blank.line_comments ~cs:(Charset.from_string " \n\r\t") "#")

let semi _ _ _ b i =
  let (c,_,_) = Pacomb.Input.read b i in c = ';'

type cmds =
  { add : env -> ?bound:E.elem -> string -> E.elem -> unit
  ; add_curve : env -> string -> string -> E.elem -> unit
  ; remove : string -> string option -> unit
  ; set_near : float -> unit
  ; set_far : float -> unit
  ; translateX : float -> unit
  ; translateY : float -> unit
  ; translateZ : float -> unit
  ; rotateX : float -> unit
  ; rotateY : float -> unit
  ; rotateZ : float -> unit
  ; background : Gles3.rgba -> unit
  ; text_color : float array -> unit
  ; set_time : float -> unit
  ; set_time_factor : float -> unit
  }

let continue_pause = ref false

let pause time =
  continue_pause := true;
  let t0 = Unix.gettimeofday () in
  while !continue_pause && Unix.gettimeofday () -. t0 < time do
    Unix.sleepf 0.1;
  done

let stop_pause () =
  continue_pause := false

let run (commands:cmds) input_files =
  let env = ref default_env in
  let%parser color =
    '(' (r::FLOAT) ',' (g::FLOAT) ',' (b::FLOAT)
      (a:: ~?[1.0] (',' (a::FLOAT) => a)) ')' =>
      [|r;g;b;a|]
  in
  let%parser rec cmd =
    "let" (id::E.ident) (variables :: ~? [[]]
          ( '(' (l :: ~*[','] E.ident) ')' => l))
          "=" (e::E.parse) =>
      (fun () ->
          E.fun_table :=
            Expression.StringMap.add
              id { variables; value = e; fname = id } !E.fun_table)
    ; "print" (e::E.parse) =>
        (fun () -> Format.printf "%a\n%!" (fun fmt -> write fmt)  e)
    ; "surface" (id::ident) ':' (e::E.parse)
            (bound :: ~? ("bound" (e::E.parse) => e)) =>
        (fun () -> commands.add !env ?bound id e)
    ; "curve" (idc::ident) ':' (e::E.parse) "on" (id::ident) =>
        (fun () -> commands.add_curve !env id idc e)
    ; "remove" (id1::ident) (id2 :: ~? ("on" (i::ident) => i)) =>
        (fun () -> commands.remove id1 id2)
    ; "sleep" (t::FLOAT) => (fun () -> pause t)
    ; "wait" => (fun () -> pause infinity)
    ; "color" "=" (color::color) =>
        (fun () -> env := { !env with color })
    ; "back_color" "=" (back_color::color) =>
        (fun () -> env := { !env with back_color = Some back_color })
    ; "back_color" "=" "none" =>
        (fun () -> env := { !env with back_color = None })
    ; "line_color" "=" (line_color::color) =>
        (fun () -> env := { !env with line_color })
    ; "line_width" "=" (line_width::FLOAT) =>
        (fun () -> env := { !env with line_width })
    ; "specular" "=" (specular::FLOAT) =>
        (fun () -> env := { !env with specular })
    ; "shininess" "=" (shininess::FLOAT) =>
        (fun () -> env := { !env with shininess })
    ; "mindivs" "=" (mindivs::INT) =>
        (fun () -> env := { !env with mindivs })
    ; "precision" "=" (precision::FLOAT) =>
        (fun () -> env := { !env with precision })
    ; "precision_derive" "=" (precision_derive::FLOAT) =>
        (fun () -> env := { !env with precision_derive })
    ; "near" "=" (x::FLOAT) =>
        (fun () -> commands.set_near x)
    ; "far" "=" (x::FLOAT) =>
        (fun () -> commands.set_far x)
    ; "background" "=" (c::color) =>
        (fun () -> commands.background
                     { r = c.(0); g = c.(1); b = c.(2); a = c.(3) })
    ; "text_color" "=" (c::color) =>
        (fun () -> commands.text_color c)
    ; "translateX" (x::FLOAT) =>
        (fun () -> commands.translateX x)
    ; "translateY" (x::FLOAT) =>
        (fun () -> commands.translateY x)
    ; "translateZ" (x::FLOAT) =>
        (fun () -> commands.translateZ x)
    ; "rotateX" (x::FLOAT) =>
        (fun () -> commands.rotateX x)
    ; "rotateY" (x::FLOAT) =>
        (fun () -> commands.rotateY x)
    ; "rotateZ" (x::FLOAT) =>
        (fun () -> commands.rotateZ x)
    ; "time" "=" (x::FLOAT) =>
        (fun () -> commands.set_time x)
    ; "time_factor" "=" (x::FLOAT) =>
        (fun () -> commands.set_time_factor x)

  and cmds = () => ()
    ; cmds (f :: Pacomb.Grammar.test_after semi cmd => f ()) ';' => ()
    ; cmds (env0::('{' => !env)) cmds '}' => (env := env0)
  in

  let%parser file = cmds EOF => true in

  try
    (* no need to stack the buffer of in_channel and those of Pacomb. So
       file desciptor are preferred. *)
    List.iter (fun filename ->
      Printf.printf "reading %S\n%!" filename;
      let _ = Pacomb.Pos.handle_exception ~error:(fun _ -> false)
                (Pacomb.Grammar.parse_file file blank) filename
      in
      ()) input_files;
    Printf.printf "reading standard input\n%!";

    let line_action () = Printf.printf "=> %!" in
    while true do
      let res =
        Pacomb.Pos.handle_exception ~error:(fun _ -> false)
          (Pacomb.Grammar.parse_fd ~line_action file blank) Unix.stdin
      in
      if res then raise End_of_file;
      ()
    done
  with
    e ->
    Printf.printf "%s\n%!" (Printexc.to_string e);
    ()
