OUTPUTS = basic_ugraph.png \
			  basic_dgraph.png \
              cities.png \
			  prs.png \
			  dfoundation.png \
			  ufoundation.png \
			  dwfoundation.png \
			  uwfoundation.png \
			  cities2.png \

phony: $(OUTPUTS)

%.png: %.dot
	fdp -Tpng $< -o $@

cities.png: cities.dot
	dot -Tpng $< -o $@

prs.png: prs.dot
	dot -Tpng $< -o $@

clean:
	rm *.png
