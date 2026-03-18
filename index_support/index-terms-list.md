# Pending Index Terms

NOTE: Some terms to possibly add to the index, work in progress

---

## Core Simulation Concepts (15 terms)

7. `simulation skeleton`
8. `single scenario simulation`
9. `multifactor simulation`
10. `simulation replication`
12. `simulation as evidence`
13. `calibrated simulation`
14. `finite-sample performance`
15. `misspecification`

---

## Performance Measures (20 terms)

16. `performance measures`
17. `performance measures!bias`
18. `performance measures!variance`
19. `performance measures!mean squared error`
20. `performance measures!RMSE`
21. `performance measures!relative bias`
22. `performance measures!coverage`
23. `performance measures!rejection rate`
24. `performance measures!Type I error`
25. `performance measures!power`
26. `performance measures!confidence interval width`
27. `performance measures!calibration`
28. `performance measures!stability`
29. `performance measures!robust measures`
30. `Monte Carlo standard error`
31. `MCSE|see{Monte Carlo standard error}`
32. `jackknife (MCSE)`
33. `bootstrap (MCSE)`
34. `size-adjusted power`
35. `estimand`

---

## Data-Generating Processes (10 terms)

36. `data-generating process!components`
37. `data-generating process!checking`
38. `data-generating process!plotting`
39. `data-generating process!standardization`
40. `data-generating process!cluster-randomized trial`
41. `data-generating process!ANOVA`
42. `data-generating process!bivariate Poisson`
43. `data-generating process!item response theory`
44. `data-generating process!meta-regression`

---

## Estimation Procedures (8 terms)

46. `estimation function`
47. `estimation function!validating`
48. `estimation function!multiple procedures`
49. `error handling`
50. `error handling!capturing warnings`
51. `error handling!safely()`
52. `contingent testing`
53. `Welch t-test`

---

## Statistical Methods and Models (10 terms)

54. `analysis of variance (ANOVA)`
55. `heteroskedasticity`
56. `parametric bootstrap`
57. `potential outcomes framework`
58. `finite population inference`
59. `superpopulation inference`
60. `cluster-randomized trial`
61. `multilevel model`
62. `meta-regression`
63. `item response theory`

---

## R Programming Concepts (12 terms)

64. `functions!writing`
65. `functions!default arguments`
66. `functions!named arguments`
67. `functions!skeleton`
68. `pipe operator`
69. `purrr package`
70. `purrr package!map()`
71. `purrr package!pmap()`
72. `replicate()`
73. `tidyverse`
74. `random seed`
75. `pseudo-random number generator`

---

## R Packages (8 terms)

76. `simhelpers package`
77. `furrr package`
78. `future package`
79. `testthat package`
80. `lme4 package`
81. `estimatr package`
82. `clubSandwich package`
83. `kableExtra package`

---

## Computational and File Management (8 terms)

84. `parallel processing`
85. `parallel processing!on a local computer`
86. `parallel processing!on a cluster`
87. `parallel processing!on a virtual machine`
88. `file management!project structure`
89. `file management!saving results`
90. `file management!source command`
91. `profiling code`

---

## Debugging and Testing (4 terms)

92. `debugging!print()`
93. `debugging!browser()`
94. `debugging!stopifnot()`
95. `unit testing`

---

## Visualization and Reporting (5 terms)

96. `visualization!many small multiples`
97. `visualization!Bias-SE-RMSE plot`
98. `visualization!heat maps`
99. `regression tree (simulation analysis)`
100. `simulation section (methods paper)`

---

## Notes on Use

- Terms listed as subentries (e.g., `performance measures!bias`) will appear indented
  under the parent heading in the index.
- Cross-references (`|see{}`) produce a "see ..." note with no page number.
- On the line where a term is *defined*, use `|textbf` to bold the page number:
  `\index{data-generating process|textbf}`
- Add these to `add_index_terms.R` as entries in the `INDEX_TERMS` list,
  using the `term` field for the prose text to find and `index_key` for the
  `\index{}` argument.
