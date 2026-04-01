# Running Examples and Case Studies

Index entries use the form `example!name` (lowercase, to sort correctly under a
single "example" parent heading in the printed index).

---

## Primary Running Examples
*Introduced early and revisited across many chapters.*

| Example | `\index{}` key | Chapters |
|---|---|---|
| Initial t-test simulation | `example!t-test simulation` | 3, 4 |
| Heteroskedastic ANOVA / Welch t-test (Brown & Forsythe 1974) | `example!heteroskedastic ANOVA (Welch)` | 5, 10, 12, exercises throughout |
| Cluster-randomized trial | `example!cluster-randomized trial` | 6, 8, 9, 10, 12, 14, 15, 17 |
| Bivariate Poisson / Pearson correlation CI | `example!bivariate Poisson` | 6, 8, 9, 10, 11, 12 |

---

## Secondary Examples
*Introduced (often in exercises), revisited in a few later chapters.*

| Example | `\index{}` key | Chapters |
|---|---|---|
| 3-parameter IRT model | `example!3-parameter IRT` | 6 (DGP), 8 (estimation) |
| Random effects meta-regression | `example!random effects meta-regression` | 6, 8, 12 (exercises) |
| Vevea-Hedges selection model | `example!Vevea-Hedges selection model` | 6, 8 (exercises, extends meta-regression) |
| Biserial correlation | `example!biserial correlation` | 12, 15 |
| Comparing treatment variation estimators | `example!treatment variation estimators` | 12, 15 |

---

## Self-Contained Chapter Case Studies
*Appear primarily within a single chapter.*

| Example | `\index{}` key | Chapter |
|---|---|---|
| Calibrated simulation using copulas | `example!calibrated simulation (copula)` | 16 |
| Parametric bootstrap (air conditioner data) | `example!parametric bootstrap (air conditioner)` | 20 |
| Power analysis for multilevel data | `example!power analysis (multilevel)` | 18 |
| Potential outcomes / matching imputation | `example!potential outcomes (matching)` | 19 |

---

## Notes on Indexing

- Add `\index{example!cluster-randomized trial}` etc. at the *start* of each
  section or paragraph where a given example is substantively developed —
  not just mentioned in passing.
- For the primary running examples, consider using `|textbf` on the page where
  the example is first fully introduced:
  `\index{example!cluster-randomized trial|textbf}`
- The `add_index_terms.R` script is not well-suited for these entries because
  the example names don't appear as consistent verbatim phrases in the prose.
  These are best added manually, section by section.
