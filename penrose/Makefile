
APINAME=api_triangle
$(APINAME).cma:
	ocamlc -c -o $(APINAME).cmi $(APINAME).mli
	ocamlc -c -o $(APINAME).cmo $(APINAME).ml
	ocamlc -a -o $(APINAME).cma $(APINAME).cmo

$(APINAME).mli:
	ocamlc -i $(APINAME).ml > $(APINAME).mli

clean:
	rm $(APINAME).cmo
cleanall:
	rm $(APINAME).cmo $(APINAME).cma $(APINAME).cmi
cleanallall:
	rm $(APINAME).mli $(APINAME).cmo $(APINAME).cma $(APINAME).cmi

.PHONY: clean cleanall cleanallall
