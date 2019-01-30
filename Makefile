talk.pdf: talk.md Makefile
	pandoc --to beamer --output talk.pdf talk.md

talk.html: talk.md Makefile
	pandoc --to revealjs --standalone --output talk.html talk.md \
        -V revealjs-url=./reveal.js



.PHONY: html
html:
	haddock -html -o api src/ML.hs
