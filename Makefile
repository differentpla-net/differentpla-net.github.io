all: tags images

tags:
	./scripts/update-tags.sh

DOTS = $(wildcard _posts/*.dot)
PNGS = $(patsubst _posts/%.dot,images/%.png,$(DOTS))
SVGS = $(patsubst _posts/%.dot,images/%.svg,$(DOTS))

images: $(PNGS) $(SVGS)

images/%.svg: _posts/%.dot
	dot -T svg -o $@ $<

images/%.png: images/%.svg
	convert -resize 600x $< $@
