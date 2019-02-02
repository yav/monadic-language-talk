.PHONY: all

all: talk.html talk.pdf

talk.pdf: talk.md Makefile
	pandoc --to beamer --output talk.pdf talk.md

talk.html: talk.md Makefile talk.css
	pandoc --to revealjs --standalone -S --output talk.html talk.md \
        --css=talk.css \
        -V revealjs-url=./reveal.js \
        -V theme=moon



.PHONY: html
html:
	haddock -html -o api src/ML.hs
