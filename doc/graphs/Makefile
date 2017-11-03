OUTPUTS = basic_ugraph.png \
			  basic_dgraph.png \
              cities.png \
			  prs.png \
			  dfoundation.png \
			  ufoundation.png \
			  dwfoundation.png \
			  uwfoundation.png \
			  cities2.png \
			  upairs.png \
			  dpairs.png \
			  utriples.png \
			  dtriples.png \

phony: $(OUTPUTS)

%.png: %.dot
	fdp -Tpng $< -o $@

cities.png: cities.dot
	dot -Tpng $< -o $@

prs.png: prs.dot
	dot -Tpng $< -o $@

upairs.png: upairs.dot
	dot -Tpng $< -o $@

dpairs.png: dpairs.dot
	dot -Tpng $< -o $@

utriples.png: utriples.dot
	dot -Tpng $< -o $@

dtriples.png: dtriples.dot
	dot -Tpng $< -o $@

clean:
	rm *.png
