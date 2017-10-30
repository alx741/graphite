OUTPUTS_FDP = basic_ugraph.png \
			  basic_dgraph.png \

OUTPUTS_DOT = cities.png \
			  prs.png \

phony: $(OUTPUTS_FDP) $(OUTPUTS_DOT)

%.png: %.dot
	fdp -Tpng $< -o $@

cities.png: cities.dot
	dot -Tpng $< -o $@

prs.png: prs.dot
	dot -Tpng $< -o $@

clean:
	rm *.png
