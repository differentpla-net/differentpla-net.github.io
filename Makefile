all: tags pngs

tags:
	./scripts/update-tags.sh

DOTS = $(wildcard _posts/*.dot)
PNGS = $(patsubst _posts/%.dot,images/%.png,$(DOTS))
SVGS = $(patsubst _posts/%.dot,images/%.svg,$(DOTS))

pngs: $(PNGS) $(SVGS)

images/%.png: _posts/%.dot
	dot -T png -Gsize=9,15! -Gdpi=50 -o $@ $<

images/%.svg: _posts/%.dot
	dot -T svg -o $@ $<
