# Index Planning Document: *Designing Monte Carlo Simulations in R*

## 1. Book Structure Overview

### Part I: An Introductory Look

**Chapter: Introduction** (`001-introduction.Rmd`)
- Some of simulation's many uses
  - Comparing statistical approaches
  - Assessing performance of complex pipelines
  - Assessing performance under misspecification
  - Assessing finite-sample performance
  - Conducting Power Analyses
  - Simulating processes
- The perils of simulation as evidence
- Simulating to learn
- Why R?
- Organization of the text

**Chapter: Programming Preliminaries** (`003-programming-preliminaries.Rmd`)
- Welcome to the tidyverse
- Functions
  - Rolling your own
  - A dangerous function
  - Using Named Arguments
  - Argument Defaults
  - Function skeletons
- Pipe (`>`) dreams
- Recipes versus Patterns
- Exercises

**Chapter: An Initial Simulation** (`005-initial-t-test-simulation.Rmd`)
- Simulating a single scenario
- A non-normal population distribution
- Simulating across different scenarios
- Extending the simulation design
- Exercises

---

### Part II: Structure and Mechanics of a Simulation Study

**Chapter: Structure of a Simulation Study** (`010-Simulation-structure.Rmd`)
- General structure of a simulation
- Tidy, modular simulations
- Skeleton of a simulation study
  - Data-Generating Process
  - Data Analysis Procedure
  - Repetition
  - Performance summaries
  - Multifactor simulations
- Exercises

**Chapter: Case Study — Heteroskedastic ANOVA and Welch** (`015-Case-study-ANOVA.Rmd`)
- The data-generating model
  - Now make a function (Welch DGP)
  - Cautious coding
- The hypothesis testing procedures
- Running the simulation
- Summarizing test performance
- Exercises (Other alphas, Compare results, Power, Wide or long, Other tests, etc.)

**Chapter: Data-Generating Processes** (`020-Data-generating-models.Rmd`)
- Examples (ANOVA, Bivariate Poisson, Cluster RCT)
- Components of a DGP
- A statistical model is a recipe for data generation
- Plot the artificial data
- Check the data-generating function
- Example: Simulating clustered data
  - Design decisions; model for cluster RCT; equations to code; standardization
- Sometimes a DGP is all you need (3-parameter IRT)
- More to explore / Exercises

**Chapter: Estimation Procedures** (`030-Estimation-procedures.Rmd`)
- Writing estimation functions
- Including Multiple Data Analysis Procedures
- Validating an Estimation Function
  - Checking against existing implementations
  - Checking novel procedures
  - Checking with simulations
- Handling errors, warnings, and other hiccups
  - Capturing errors and warnings
  - Estimator processes that anticipate errors and warnings
  - The `safely()` option
- Exercises

**Chapter: Running the Simulation Process** (`035-running-simulation.Rmd`)
- Repeating oneself
- One run at a time
- Reparameterizing
- Bundling simulations with `simhelpers`
- Seeds and pseudo-random number generators
- Conclusions / Exercises

**Chapter: Performance Measures** (`040-Performance-criteria.Rmd`)
- Measures for Point Estimators
  - Comparing Cluster RCT Estimation Procedures
  - Robust Performance Measures
- Measures for Variance Estimators
  - Calibration
  - Stability
  - Assessing SEs for the Cluster RCT Simulation
- Measures for Confidence Intervals
  - Cluster RCT confidence interval coverage
- Measures for Inferential Procedures (Hypothesis Tests)
  - Validity; Power; Rejection Rates; Cluster RCT inference
- Relative or Absolute Measures?
  - Performance relative to target parameter
  - Performance relative to a benchmark estimator
- Estimands Not Represented By a Parameter
- Uncertainty in Performance Estimates (Monte Carlo Standard Error)
  - Conventional MCSE for point estimators
  - MCSEs for robust measures
  - MCSE for CIs and Hypothesis Tests
  - Jackknife and Bootstrap MCSEs
- Performance Calculations with the `simhelpers` Package
- Concluding thoughts / Exercises

---

### Part III: Systematic Simulations

**Chapter: Multiple Scenarios** (`060-multiple-scenarios.Rmd`)
- Simulating across levels of a single factor
  - Performance summary function
  - Adding performance calculations to the simulation driver
- Simulating across multiple factors
- Using `pmap` to run multifactor simulations
- When to calculate performance metrics
  - Aggregate as you simulate (inside)
  - Keep all simulation runs (outside)
  - Getting raw results ready for analysis
- Summary / Exercises

**Chapter: Designing Multifactor Simulations** (`070-experimental-design.Rmd`)
- Choosing parameter combinations
- Case Study: A multifactor evaluation of cluster RCT estimators
  - Choosing parameters; Redundant factor combinations; Running; Calculating metrics
- Conclusions

**Chapter: Exploring and Presenting Simulation Results** (`072-presentation-of-results.Rmd`)
- Tabulation
  - Example: estimators of treatment variation
- Visualization
  - Examples: RMSE, biserial correlation, variance estimation, heat maps, relative performance
- Modeling
  - Examples: Biserial revisited; cross-classified data
- Reporting

**Chapter: Building Good Visualizations** (`074-building-good-vizualizations.Rmd`)
- Subsetting and Many Small Multiples
- Bundling
- Aggregation
- Comparing true SEs with standardization
- The Bias-SE-RMSE plot
- Assessing the quality of estimated SEs
  - Stability of estimated SEs
- Assessing confidence intervals
- Conclusions / Exercises

**Chapter: Special Topics on Reporting** (`075-special-topics-on-reporting.Rmd`)
- Using regression to analyze simulation results
  - Examples: Biserial; Cluster RCT
- Using regression trees to find important factors
- Analyzing results with few iterations per scenario
- Multilevel Meta Regressions
- What to do with warnings in simulations
- Conclusions

**Chapter: Case Study — Comparing Different Estimators** (`077-case-study-comparing-estimators.Rmd`)
- Bias-variance tradeoffs

**Chapter: Simulations as Evidence** (`080-simulations-as-evidence.Rmd`)
- Making simulations relevant
  - Break symmetries; Multi-factor simulations; DGPs from prior literature; etc.
  - Build a fully calibrated simulation
- Case study: Using copulas to generate calibrated data
  - Steps 1–5 of copula-based calibration
  - Assessing the copula's success; building multifactor simulation
- Simulation Sections in Methods Papers
  - Purpose; Design decisions; Principles of design and presentation
- Concluding Thoughts

---

### Part IV: Computational Considerations

**Chapter: File Management** (`105-file-management.Rmd`)
- Simulation project structure
- Well-structured code files
  - Headers in .R files; the `source` command; storing testing code
- Principled directory structures
- Saving simulation results
  - File formats; Saving simulations as you go

**Chapter: Parallel Processing** (`120-parallel-processing.Rmd`)
- Parallel on your computer
- Parallel on a virtual machine
- Parallel on a cluster
  - CLI and CMD mode; task dispatchers; moving files; checking jobs; running lots of jobs
  - Resources for Harvard's Odyssey

**Chapter: Debugging and Testing** (`130-debugging_and_testing.Rmd`)
- Debugging with `print()`
- Debugging with `browser()`
- Debugging with `debug()`
- Protecting functions with `stopifnot()`
- Testing code (unit testing)

---

### Part V: Complex Data Structures

**Chapter: Simulation for Power Analysis** (`140-simulation-for-power-analysis.Rmd`)
- Getting design parameters from pilot data
- The data generating process
- Running the simulation
- Evaluating power
  - Checking validity; Assessing Precision (SE); Assessing power; MDEs
- Power for Multilevel Data

**Chapter: Simulation under the Potential Outcomes Framework** (`150-potential-outcomes-framework.Rmd`)
- Finite vs. Superpopulation inference
- Data generation processes for potential outcomes
- Finite sample performance measures
- Nested finite simulation procedure
- Calibrated simulations within the potential outcomes framework
  - Matching-based imputation; Model-based imputation; Using imputed data

**Chapter: The Parametric Bootstrap** (`160-parametric-bootstrap.Rmd`)
- Air conditioners: a stolen case study

---

### Appendices

**Appendix: Coding Tidbits** (`200-coding-tidbits.Rmd`)
- How to repeat yourself (`replicate()`, `map()`, other approaches)
- Default arguments for functions
- Profiling Code (`Sys.time()`, `tictoc`, `bench`, `profvis`)
- Optimizing code (and why you often shouldn't)
  - Hand-building functions; computational efficiency; reusing code

**Appendix: Further Readings and Resources** (`210-futher-resources.Rmd`)

---

## 2. Bookdown Index Options

### The short answer

**bookdown does not have built-in support for a traditional back-of-book index in HTML output.** However, it has solid support for PDF/LaTeX output via standard LaTeX indexing. Your options fall into three tiers:

---

### Option A: LaTeX Index for PDF Output (recommended starting point)

This is the most natural and well-supported path. LaTeX has mature indexing infrastructure that works well with bookdown's `pdf_book` output.

**How it works:**

1. Add `\usepackage{imakeidx}` (or `makeidx`) to `preamble.tex`:
   ```latex
   \usepackage{imakeidx}
   \makeindex
   ```

2. Add `\printindex` at the end of the book (e.g., in `210-futher-resources.Rmd` or a new file):
   ```
   \printindex
   ```

3. Sprinkle `\index{}` commands throughout the `.Rmd` files at terms you want indexed:
   ```
   The data-generating process \index{data-generating process} is the first step...
   ```

   For subentries:
   ```
   \index{performance measures!bias}
   \index{performance measures!RMSE}
   ```

   For "see also" cross-references:
   ```
   \index{DGP|see{data-generating process}}
   ```

4. Build the PDF — `pdflatex` + `makeindex` + `pdflatex` again handles the rest. The `bookdown::pdf_book` output already runs these steps.

**Pros:** Polished, standard, automatic page numbers, subentries, cross-references. No extra packages needed.
**Cons:** Index only appears in PDF, not HTML gitbook.

---

### Option B: HTML Index Page (manual or semi-automated)

For the gitbook HTML output, bookdown has no built-in index mechanism (it relies on the search bar instead). But you can create a custom index page:

1. Add a new `Rmd` file (e.g., `999-index-page.Rmd`) that serves as a manually curated "Index" chapter.
2. Use R to partially automate it — scan each chapter for key terms and generate anchor links.
3. Each entry links to a section anchor, e.g.:
   ```markdown
   **data-generating process** — [§ DGP functions](#DGP-functions), [§ DGP examples](#DGP-examples)
   ```

The existing `{#label}` anchors already in your headings (e.g., `{#data-generating-processes}`, `{#performance-measures}`) make this feasible.

**Pros:** Works in HTML, visible to web readers.
**Cons:** Requires significant manual curation; links go to sections, not page numbers.

---

### Option C: Both Together (most complete)

Use LaTeX `\index{}` commands for the PDF index, and also build the HTML index page. The `\index{}` commands are silently ignored in HTML output, so there's no conflict. This is the approach used by several Chapman & Hall/CRC bookdown textbooks.

---

## 3. Recommended Next Steps

Given where the book stands, here is a suggested workflow:

1. **Start with `preamble.tex`** — add `\usepackage{imakeidx}` and `\makeindex` now, so the infrastructure is in place even before you add index entries.
    DONE
    

2. **Decide on key terms to index** — based on the chapter outline above, the natural first-pass index candidates include:
   - Core concepts: data-generating process, estimation procedure, performance measures, Monte Carlo standard error, tidy modular simulation
   - Performance metrics: bias, variance, RMSE, coverage, power, Type I error, rejection rate
   - Methods/packages: `simhelpers`, `purrr`, `furrr`, `future`, `testthat`, `replicate()`, `map()`
   - Simulation design concepts: multifactor simulation, seeds, parallel processing, reparameterization
   - Statistical concepts: confidence interval, hypothesis test, bootstrap, potential outcomes

3. **Add `\index{}` commands chapter by chapter** — start with the most-referenced chapters (the structure chapters 010–040) where core vocabulary is defined.

4. **Decide whether you want an HTML index** — if the book is primarily consumed as HTML (gitbook), this matters more than the PDF index.

5. **Add `\printindex` to the book end** — either as its own `.Rmd` file or appended to the Further Resources chapter.
