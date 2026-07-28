
all:
	dune build @install
	dune promote

install:
	dune install

doc:
	pandoc -f gfm -t html README.md -s -o html/README.html --css style.css --title-prefix="CamlSurf" --mathml
	sed -i '/<img src="Images\/cone3\.png"/c\
		<figure><video width="600" controls autoplay loop muted>\
		<source src="./Images/barth.mp4" type="video/mp4">\
		Your browser does not support the video tag.\
		</video><figcaption>\
		    Animation of the Barth sextic on an integrated GPU.\
	         </figcaption></figure>' html/README.html
