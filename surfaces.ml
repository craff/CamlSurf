open Egl
open Gles3
open Gles3.Type
open Shaders
open Buffers
open Matrix
open Formal.Gen

(** simple example, using vertex buffers + one simple texture*)

(** keep the current width,height and ratio in a reference *)
let gwidth = ref 800 and gheight = ref 600
let ratio = ref (float !gwidth /. float !gheight)

let config =
  { red_size = 8 ;
    green_size = 8 ;
    blue_size = 8 ;
    alpha_size = 0 ;
    depth_size = 0 ;
    stencil_size = 0 ;
    samples = 4 }


(** initialization of the main window, and its viewport *)
let ctxt = initialize ~config ~es:false ~width:!gwidth ~height:!gheight "test_gles2"
let _ = viewport ~x:0 ~y:0 ~w:!gwidth ~h:!gheight

(** the modelView matrix of the cube defining the position of the cube, from
    the current time *)
let modelView = ref idt
let translateView = ref (translate 0. 0. 5.)
let center = [|0.;0.;0.|]
let lightPos = [|1.0;3.0;-3.0|]
let eyePos = [|0.;0.;-3.0|]
let eyeUp = [|0.0;1.0;0.0|]
let near = ref 5.0
let far = ref 10.0

let ielements = to_uint_element_buffer gl_static_draw
                    [|0;1;2;   2;1;3; |]

(** the projection matrix: beware, it depends from the screen ratio *)
let projection () =
  (mul (perspective 45.0 !ratio !near !far) (lookat eyePos center eyeUp))

let text_texture msg =
  (Freetype.texture_of_text
     ~line_stretch:1.3
     ~font:"/usr/share/fonts/truetype/dejavu/DejaVuSerif.ttf"
     ~size:32 ~alignment:Right msg)

let _ = Shaders.set_debug false (* disable warnings *)

let text_prg =
  let text_shader =
    ("text_shader",
     of_string gl_vertex_shader Vertex_text.str ::
       of_string gl_fragment_shader Fragment_text.str :: [])
  in
  let prg = compile ~version:"460" text_shader in
  let prg = float_attr prg "in_position"  in
  let prg = float_mat4_uniform prg "Projection" in
  let tex_coordinates =
    to_float_array_buffer gl_static_draw
      [| 0.0; 0.0;
         1.0; 0.0;
         0.0; 1.0;
         1.0; 1.0
      |]
  in
  let prg = buffer_cst_attr prg "in_tex_coordinates" tex_coordinates in
  let prg = texture_2d_uniform prg "texture_text" in
  prg

let add_msg, text_texture, text_size, text_reshape =
  let cfg = query_config ctxt in
  let init_msgs =
    [ Printf.sprintf "GLSL Version: %s" (get_shading_language_version ())
    ; Printf.sprintf "Version: %s" (get_version ())
    ; Printf.sprintf "Renderer: %s" (get_renderer ())
    ; Printf.sprintf "Vendor: %s" (get_vendor ())
    ; Printf.sprintf "R:%d G;%d B:%d A:%d D:%d S:%d"
        cfg.red_size cfg.green_size cfg.blue_size cfg.alpha_size
        cfg.depth_size cfg.samples
    ]
  in
  List.iter (Printf.printf "%s\n%!") init_msgs;
  let messages = ref init_msgs in
  let texture = ref (Textures.gen_gc_texture ()) in

  let w = ref 0.0 and h = ref 0.0 in
  let set_shader () =
    let tex = text_texture (String.concat "\n" !messages) in
    w := float tex.width /. float !gwidth;
    h := float tex.height /. float !gheight;
    texture := tex.texture
  in
  let _ = set_shader () in
  (fun msg ->
    Printf.printf "%s\n%!" msg;
    let rec limit n l =
      match n, l with
      | 0, _ | _, [] -> []
      | n, x::l -> x::(limit (n-1) l)
    in
    messages := msg :: limit 9 !messages ;
    set_shader ()),
  (fun () -> !texture),
  (fun () -> !w, !h),
  (fun () -> set_shader ())

let surfaces = ref []

let mk_prog surfaces =
  let fn () =
    let glsl_surfaces =
      List.map (fun s ->
          let str = glsl s.name (Surface s.curves) ?bound:s.bound s.expr in
          of_string gl_fragment_shader str) surfaces in
    (* TO DO: share code for identical polynomial on difference surface:
       use expr hash has name. *)
    let curves =
      List.flatten
        (List.map (fun s ->
             List.map (fun c -> (s.name, c)) s.curves) surfaces)
    in
    let glsl_curves =
      List.map (fun (sname, (c:curve)) ->
          let str = glsl c.name (Curve sname) c.expr in
          of_string gl_fragment_shader str) curves
    in
    let glsl_dispatcher =
      of_string gl_fragment_shader (dispatcher surfaces)
    in
    let light_implicit_shader =
      ("light_implicit_shader",
       of_string gl_vertex_shader Vertex_implicit.str ::
         of_string gl_fragment_shader Preambule.str ::
           glsl_curves @ glsl_surfaces @ glsl_dispatcher ::
             of_string gl_fragment_shader Solve.str ::
               of_string gl_fragment_shader Fragment_light_implicit.str ::
                 [])
    in
    let iprg = compile ~version:"460" light_implicit_shader in
    let iprg = float_attr iprg "in_position" in

    let iprg = float_mat3_uniform iprg "NormalMatrix" in
    let iprg = float_mat4_uniform iprg "InvModelView" in
    let iprg = float_mat4_uniform iprg "ModelView" in

    let iprg = float_mat4_uniform iprg "Projection" in
    let iprg = float1_uniform iprg "time" in
    let iprg = float1_uniform iprg "far"  in
    let iprg = float1_uniform iprg "near"  in
    let iprg = float1_uniform iprg "screen_size" in

    let iprg = float3v_cst_uniform iprg "lightPos" lightPos in
    let iprg = float4v_cst_uniform iprg "lightDiffuse" [|0.5;0.5;0.5;1.0|] in
    let iprg = float4v_cst_uniform iprg "lightAmbient" [|0.2;0.2;0.2;1.0|] in
    let iprg = float3v_cst_uniform iprg "eyePos" eyePos in
    iprg
  in lazy (fn ())

let main_prg = ref (mk_prog !surfaces)

let remove name name2 =
  (match name2 with
  | None ->
     surfaces := List.filter (fun s -> s.name <> name) !surfaces;
  | Some name2 ->
     let l = List.find_all (fun s -> s.name = name2) !surfaces in
     List.iter (fun s ->
         s.curves <- List.filter
                       (fun (c:curve) -> c.name <> name)
                       s.curves) l);
  main_prg := mk_prog !surfaces

let add env ?bound name expr =
  let open Formal.Gen in
  let surface =
    { name; expr; env; bound
    ; transparent = env.color.(3) < 1.0; curves = [] }
  in
  surfaces := surface ::
                (List.filter (fun s -> s.name <> name) !surfaces);
  surfaces :=
    List.sort (fun s1 s2 -> compare s1.transparent s2.transparent)
      !surfaces;
  main_prg := mk_prog !surfaces

let add_curve env id idc e =
  let s = List.find (fun s ->  s.name = id) !surfaces in
  s.curves <-
    {name = idc; expr = e; env } ::
      (List.filter (fun (c:curve) -> c.name <> idc) s.curves);
  main_prg := mk_prog !surfaces

let do_text = ref true

let dessine_text () =
  if !do_text then
    begin
      enable gl_blend;
      let p = projection () in
      let ip = inverse p in
      let w, h = text_size () in
      let x0 = mulv ip [|1.-.w;1.-.h;0.;1.|] in
      let x1 = mulv ip [|1.;1.-.h;0.;1.|] in
      let x2 = mulv ip [|1.-.w;1.;0.;1.|] in
      let x3 = mulv ip [|1.;1.;0.;1.|] in
      let ivertices =
        to_float_bigarray
          [|x0.(0)/.x0.(3);x0.(1)/.x0.(3);x0.(2)/.x0.(3);
            x1.(0)/.x1.(3);x1.(1)/.x1.(3);x1.(2)/.x1.(3);
            x2.(0)/.x2.(3);x2.(1)/.x2.(3);x2.(2)/.x2.(3);
            x3.(0)/.x3.(3);x3.(1)/.x3.(3);x3.(2)/.x3.(3);
          |]
      in
      draw_buffer_elements text_prg
        gl_triangles ielements (text_texture ()) p ivertices;
      disable gl_blend;
    end

let dessine_implicit t =
  let m = mul !translateView !modelView in
  let im = inverse m in
  let n = normalMatrix m in
  let p = projection () in
  let ip = inverse p in
  let x0 = mulv ip [| -1.;-1.;0.;1.|] in
  let x1 = mulv ip [|1.;-1.;0.;1.|] in
  let x2 = mulv ip [| -1.;1.;0.;1.|] in
  let x3 = mulv ip [|1.;1.;0.;1.|] in
  let ivertices =
    to_float_bigarray
      [|x0.(0)/.x0.(3);x0.(1)/.x0.(3);x0.(2)/.x0.(3);
        x1.(0)/.x1.(3);x1.(1)/.x1.(3);x1.(2)/.x1.(3);
        x2.(0)/.x2.(3);x2.(1)/.x2.(3);x2.(2)/.x2.(3);
        x3.(0)/.x3.(3);x3.(1)/.x3.(3);x3.(2)/.x3.(3);
      |]
  in
  draw_buffer_elements (Lazy.force !main_prg)
    gl_triangles ielements
    (float (min !gwidth !gheight)) !near !far t p m im n ivertices

(** some last initializations of openGL state *)
let _ =
  disable gl_depth_test;
  blend_equation gl_func_add;
  blend_func ~dst:gl_src_alpha
    ~src:gl_dst_alpha;
  clear_color { r = 0.0; g = 0.0; b = 0.1; a = 1.0 }

(** two references to compute the frame rates *)
let firsttime = Unix.gettimeofday ()
let lastfpstime = ref firsttime
let lasttime = ref firsttime
let frames = ref 0
let speedX = ref 0.0
let speedY = ref 0.0
let speedZ = ref 0.0
let speedNear = ref 0.0
let speedFar = ref 0.0
let center = ref 5.0
let translateX a =  translateView := mul (translate a 0.0 0.0) !translateView
let translateY a =  translateView := mul (translate 0.0 a 0.0) !translateView
let translateZ a =  translateView := mul (translate 0.0 0.0 a) !translateView
let rotateX a =  modelView := mul (rotateX a) !modelView
let rotateY a =  modelView := mul (rotateY a) !modelView
let rotateZ a =  modelView := mul (rotateZ a) !modelView


(** the main drawing function, not mush to say, half of it
   if the computation of the frame rates *)
let draw () =
  let t = Unix.gettimeofday () in
  let delta = t -. !lasttime in
  lasttime := t;
  if !speedY <> 0.0 then rotateY (!speedY *. delta);
  if !speedX <> 0.0 then rotateX (!speedX *. delta);
  if !speedZ <> 0.0 then translateZ (!speedZ *. delta);
  if !speedFar <> 0.0 then
    far := max (!far +. !speedFar *. delta) !near;
  if !speedNear <> 0.0 then
    near := min (max (!near +. !speedNear *. delta) 0.1) !far;
  clear [ gl_color_buffer ];
  viewport ~x:0 ~y:0 ~w:!gwidth ~h:!gheight;
  if !surfaces <> [] then dessine_implicit (t -. firsttime);
  dessine_text ();
  swap_buffers ctxt;
  show_errors "after draw";

  incr frames;
  let delta = t -. !lastfpstime in
  if delta > 5.0 then(
    let fps = float !frames /. delta in
    add_msg (Format.asprintf "fps: %.2f" fps);
    frames := 0;
    lastfpstime  := t
  )

let _ = set_key_release_callback ctxt (fun ~key ~state:_ ~x:_ ~y:_ ->
            if key = Key.Escape then exit_loop ctxt
            else if key = Key.Right || key = Key.Left then
              speedY := 0.0
            else if key = Key.Up || key = Key.Down then
              speedX := 0.0
            else if key = Key.PageUp || key = Key.PageDown then
              speedZ := 0.0)

(** call back for key and mouse, just for testing *)
let _ = set_key_press_callback ctxt (fun ~key ~state ~x ~y ->
  try
    if key = Key.Escape then exit_loop ctxt
    else if key = Key.Right then
      speedY := -5e-1
    else if key = Key.Left then
      speedY := 5e-1
    else if key = Key.Up then
      speedX := 5e-1
    else if key = Key.Down then
      speedX := -5e-1
    else if key = Key.PageUp then
      speedZ := 5e-1
    else if key = Key.PageDown then
      speedZ := -5e-1
  (*  else if key = Key.P then
      begin
        if (state :> int) land (Modifier.shift :> int) != 0 then decr prg_N else incr prg_N;
        Printf.printf "N = %d\n%!" !prg_N;
        end*)
    else if key = Key.N then
      begin
        if (state :> int) land (Modifier.shift :> int) != 0 then
          speedNear := -1.0
        else
          speedNear := 1.0
      end
    else if key = Key.F then
      begin
        if (state :> int) land (Modifier.shift :> int) != 0 then
          speedFar := -1.0
        else
          speedFar := 1.0
      end
    else if key = Key.I then
      begin
        do_text := not !do_text
      end
    else if key = Key.Space then
      begin
        stop_pause ()
      end
    else
      Printf.printf "key: %s state: %d x:%d y:%d\n%!"
        (Key.name key) (state :> int) x y
  with e -> Printf.printf "exception: %s" (Printexc.to_string e))

let _ = set_button_press_callback ctxt (fun ~button ~state ~x ~y ->
            Printf.printf "button: %s state: %d x:%d y:%d\n%!"
              (Button.name button) (state :> int) x y)

(** when there is nothing to do, we draw *)
let _ =
  set_idle_callback ctxt
    (fun () ->
      try
        draw ()
      with e ->
        Printf.eprintf "draw raised %s\n%!" (Printexc.to_string e);
        exit_loop ctxt
    )

(** the reshape callback, changing the viewport and ratio
   when the window is resized *)
let _ = set_reshape_callback ctxt (fun ~width ~height ->
  gwidth := width; gheight := height;
  ratio := float width /. float height;
  text_reshape ())

let commands =
  { add; add_curve; remove
  ; set_near = (fun x -> near := x)
  ; set_far = (fun x -> far := x)
  ; translateX; translateY; translateZ
  ; rotateX; rotateY; rotateZ
  }

let _d = Domain.spawn (fun () -> run commands; Egl.exit_loop ctxt)

let _ = draw () (** draw once outside the loop, because all exceptions are caught
                   inside the main loop *)

(** we now start the main loop ! *)
let _ = main_loop ctxt
