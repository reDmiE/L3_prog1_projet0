APINAME=api_triangle

$(APINAME).cma: $(APINAME).ml
	ocamlc -c -o $(APINAME).cmi $(APINAME).mli
	ocamlc -c -o $(APINAME).cmo $(APINAME).ml
	ocamlc -a -o $(APINAME).cma $(APINAME).cmo

$(APINAME).mli: $(APINAME).ml
	ocamlc -i $(APINAME).ml > $(APINAME).mli

clean:
	rm -f $(APINAME).cmo

cleanall:
	rm -f $(APINAME).cmo $(APINAME).cma $(APINAME).cmi

.PHONY: clean cleanall
