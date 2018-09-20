APINAME=api_rod
$(APINAME).cma:
	ocamlc -c -o $(APINAME).cmi $(APINAME).mli
	ocamlc -c -o $(APINAME).cmo $(APINAME).ml
	ocamlc -a -o $(APINAME).cma $(APINAME).cmo

$(APINAME).mli:
	ocamlc -i $(APINAME).ml > $(APINAME).mli

clean:
	rm -f times.data
	rm -f times.pdf
	rm -f $(APINAME).cma
	rm -f $(APINAME).cmo
	rm -f $(APINAME).cmi

.PHONY: clean
