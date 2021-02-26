all: tags images

tags:
	./scripts/update-tags.sh

DOTS = $(wildcard _posts/*.dot)
PNGS = $(patsubst _posts/%.dot,images/%.png,$(DOTS))
SVGS = $(patsubst _posts/%.dot,images/%.svg,$(DOTS))

images: $(PNGS) $(SVGS)

images/%.svg: _posts/%.dot
	dot -T svg -o $@ $<

images/2021-02-15-ranch-error-closed-ranch-sup.png: images/2021-02-15-ranch-error-closed-ranch-sup.svg
	convert -resize 1140 $< $@

images/2021-02-15-ranch-error-closed-tls-connection-sup.png: images/2021-02-15-ranch-error-closed-tls-connection-sup.svg
	convert -resize x400 $< $@
