OUTPUTS = basic_ugraph.png \
			  basic_dgraph.png \
              cities.png \
			  prs.png \
			  ufoundation.png \
			  dfoundation.png \

phony: $(OUTPUTS)

%.png: %.dot
	fdp -Tpng $< -o $@

cities.png: cities.dot
	dot -Tpng $< -o $@

prs.png: prs.dot
	dot -Tpng $< -o $@

clean:
	rm *.png
