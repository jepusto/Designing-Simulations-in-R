
grep -rn "\\index{DGP|see{data-generating process}}" *.Rmd

for (f in list.files(".", pattern = "\\.Rmd$", full.names = TRUE)) {
  x <- readLines(f, warn = FALSE)
  x2 <- gsub("\\index{DGP|see{data-generating process}}", "", x, fixed = TRUE)
  if (!identical(x, x2)) writeLines(x2, f)
}



grep -rn "\\\\index{[^}]*|see" *.Rmd | grep -v "Designing-Simulations-in-R.Rmd" | grep -v "220-index.Rmd"