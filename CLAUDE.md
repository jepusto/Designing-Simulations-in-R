# CLAUDE.md

This file provides guidance to Claude Code (claude.ai/code) when working with code in this repository.

## What this repo is

This is the source for the book *Designing Monte Carlo Simulations in R* by Luke Miratrix and James Pustejovsky, built with [Quarto](https://quarto.org/) (`project: type: book`). It is not an R package or software project — the `.qmd` files are book chapters, and the R code inside them is pedagogical (simulation examples for readers), not application logic. Preview site: https://jepusto.github.io/Designing-Simulations-in-R/.

## Common commands

Render the whole book (HTML + PDF, per `_quarto.yml`):
```r
quarto::quarto_render()
```
or from the shell:
```
quarto render
```

Render just one chapter while editing (much faster than a full book render):
```
quarto render 015-Case-study-ANOVA.qmd
```

Live preview with auto-reload:
```
quarto preview
```

R package management is via `renv` (R 4.5.3, see `renv.lock`):
```r
renv::restore()   # sync local library to the lockfile
renv::status()     # check for drift
renv::snapshot()   # update the lockfile after adding a dependency
```
The `DESCRIPTION` file (`Package: placeholder`) exists only so `renv`/`.Rbuildignore` tooling has something to key off; this is not a real R package.

There is no test suite or linter for the prose/code in this repo. `renv` and Quarto's own render errors are the correctness check.

## Deployment

`.github/workflows/deploy_bookdown.yml` renders the book (HTML/PDF/EPUB via Quarto + `r-lib/actions/setup-renv`) and pushes `_book/` to the `gh-pages` branch on every push to `main`, unless the commit message contains `[skip build]`. There's no separate CI/test job — a push to `main` is a publish.

## Book structure

`_quarto.yml` is the single source of truth for chapter order and part groupings — it lists which `.qmd` files are in the book and in what sequence. File name numeric prefixes (e.g. `001-`, `010-`, `060-`) are a loose organizational aid, not the actual ordering; always check `_quarto.yml` before assuming a file's position or whether it's even included in the current build.

Roughly, by numeric range:
- `001`–`005`: introductory part (intro, R/tidyverse preliminaries, first t-test simulation)
- `010`–`040`: core mechanics of one simulation study (structure, an ANOVA case study, data-generating processes, estimation procedures, running sims, performance criteria)
- `060`–`080`: systematic/multifactor simulations, presenting and reporting results
- `100`–`120`: computational concerns (file management, debugging/testing, parallel processing)
- `140`–`160`: complex data structure case studies (power analysis, potential outcomes, parametric bootstrap)
- `200`: appendix (coding tidbits)

Other top-level directories:
- `attic/` — retired drafts, older `.Rmd` versions, and scrap content no longer wired into `_quarto.yml`. Not built; treat as reference/history only.
- `case_study_code/`, `code/`, `data/`, `results/` — standalone R scripts, raw datasets, and cached simulation output that back the book's worked examples. These are not sourced automatically at render time; chapter code chunks reproduce or read from them directly. Some of this predates the Quarto port and comes from the authors' own research projects.
- `index_support/` — planning docs and scripts for the book's back-of-book index. `index-planning.md` covers the overall indexing strategy (LaTeX `\index{}` for PDF; no native HTML index in Quarto). `running-examples.md` defines the `example!<name>` index-key convention used for the book's recurring running examples (t-test simulation, heteroskedastic ANOVA/Welch, cluster RCT, bivariate Poisson, etc.) — check it before introducing a new running example or indexing an existing one.
- `latex/`, `html/`, `css/` — format-specific includes: `latex/preamble.tex`/`before_body.tex`/`after_body.tex` for the PDF build (uses the Chapman & Hall `krantz.cls` class, xelatex, natbib/apalike citations), `html/toggle-sidebar.html`/`mathjax-preamble.html` and `css/*.css` for the HTML book.
- `fold-or-hide.lua` — custom Quarto/Pandoc filter providing a `fold-or-hide` attribute on code cells/blocks: in HTML it maps to Quarto's native `code-fold`; in PDF/LaTeX/Typst it hides the code entirely (`echo: false`).

## Cross-referencing and indexing conventions

- Use Quarto crossrefs (`@sec-...`, `@tbl-...`, `@fig-...`) rather than manual links; section anchors follow `{#sec-...}` on headers.
- `\index{}` LaTeX commands are used for the PDF back-of-book index and are silently ignored in HTML output — add them inline at the point a term or running example is substantively introduced, not just mentioned.
- Running examples use the `example!<name>` index key (see `index_support/running-examples.md`); the first full introduction of a primary running example uses `|textbf`.

## Prose and code style (`style_guide.txt`)

- Voice: "we" for the authors (and the collective "we" including the reader); "you" when the reader is meant to do the acting (e.g., exercises); "one" for a more impersonal register.
- R code follows the tidyverse style guide, with one deviation: put spaces just inside parentheses for function definitions/calls and grouped math, e.g. `run_sim <- function( N, J ) { ... }` and `( 2*N + J ) / 2`, not `function(N, J)` / `(2*N+J)/2`.

## Generated files — do not hand-edit or assume tracked

`_book/`, `_bookdown_files/`, `.quarto/`, `*_cache/`, `*_files/`, and per-chapter `.html` are Quarto build output and are gitignored. Top-level `index.aux/.idx/.ilg/.ind/.log/.pdf/.tex/.toc` and `Designing-Simulations-in-R.*` are LaTeX/PDF build byproducts from local `quarto render`/`quarto::quarto_render()` runs — they're normal to see as untracked/dirty after a local PDF build and generally shouldn't be committed.
