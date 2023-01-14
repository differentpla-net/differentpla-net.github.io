all: tags images proof

tags:
	./scripts/update-tags.sh
	./scripts/update-series.sh

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

build:
	jekyll build

proof: build
	htmlproofer --disable-external --empty-alt-ignore --enforce-https=false ./_site/

serve:
	jekyll serve --livereload

# GitHub Pages doesn't use `--future` when generating your site, but neither does it re-generate the site when a post
# from the future becomes a post from the present. So you'll need to trigger a rebuild, somehow.
serve-unpublished:
	jekyll serve --future --unpublished --livereload

build-container:
	podman build -f Dockerfile -t differentpla-net

run-container:
	podman run -p 4000:4000 -p 35729:35729 \
		--volume $(shell pwd):/web \
		--workdir /web \
		differentpla-net \
		jekyll serve --livereload --host 0.0.0.0

proof-container:
	podman run \
		--volume $(shell pwd):/web \
		--workdir /web \
		differentpla-net \
		htmlproofer --disable-external --empty-alt-ignore --enforce-https=false ./_site/
