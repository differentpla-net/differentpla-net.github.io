all: tags images proof

tags:
	./scripts/update-tags.sh

DOTS = $(wildcard images/*.dot)
PNGS = $(patsubst images/%.dot,images/%.png,$(DOTS))
SVGS = $(patsubst images/%.dot,images/%.svg,$(DOTS))

images: $(PNGS) $(SVGS)

images/%.svg: images/%.dot
	dot -T svg -o $@ $<

images/2021-02-15-ranch-error-closed-ranch-sup.png: images/2021-02-15-ranch-error-closed-ranch-sup.svg
	convert -resize 1140 $< $@

images/2021-02-15-ranch-error-closed-tls-connection-sup.png: images/2021-02-15-ranch-error-closed-tls-connection-sup.svg
	convert -resize x400 $< $@

proof:
	jekyll build
	htmlproofer --disable-external --empty-alt-ignore ./_site/
