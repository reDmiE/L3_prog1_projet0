APINAME=api_rod
$(APINAME).cma: $(APINAME).ml
	ocamlc -c -o $(APINAME).cmi $(APINAME).mli
	ocamlc -c -o $(APINAME).cmo $(APINAME).ml
	ocamlc -a -o $(APINAME).cma $(APINAME).cmo

$(APINAME).mli: $(APINAME).ml
	ocamlc -i $(APINAME).ml > $(APINAME).mli

clean:
	rm -f times.data times.pdf $(APINAME).cmo $(APINAME).cmi
cleanall:
	rm -f $(APINAME).cma times.data times.pdf $(APINAME).cmo $(APINAME).cmi

.PHONY: clean cleanall
