OUTPUTS = basic_ugraph.png \
		  basic_dgraph.png \

phony: $(OUTPUTS)

%.png: %.dot
	fdp -Tpng $< -o $@

clean:
	rm *.png
