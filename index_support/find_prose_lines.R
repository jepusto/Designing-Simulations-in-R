################################################################################
# find_prose_lines.R
#
# Search for phrases across .Rmd files, reporting only prose occurrences
# (skips YAML front matter, HTML comments, fenced code blocks, inline code,
# \@ref() spans, and markdown link URLs — same rules as add_index_terms.R).
#
# Usage:
#   1. Set SEARCH_TERMS to a pipe-separated string of phrases to search for.
#   2. Optionally restrict to specific files with SEARCH_FILES.
#   3. Source or run the script. Results are printed to the console.
#
# Output format (one block per search term):
#   ── "cluster-randomized trial" ──────────────────
#   015-Case-study-ANOVA.Rmd     :  185, 291
#   020-Data-generating-models.Rmd :  73, 313, 359
#
# When multiple terms are given, a combined summary is also printed showing
# every file and line that matched ANY of the terms.
################################################################################

library(stringr)

# ── Configuration ──────────────────────────────────────────────────────────────

# Pipe-separated phrases to search for. All are treated case-insensitively
# unless CASE_SENSITIVE is set to TRUE.
SEARCH_TERMS <- "IRT"

# Restrict search to specific files, or NULL for all .Rmd files
SEARCH_FILES <- NULL   # e.g. c("020-Data-generating-models.Rmd", "030-Estimation-procedures.Rmd")

CASE_SENSITIVE <- FALSE   # usually FALSE is more useful for search

RMD_DIR <- "."


# ── Helper functions (shared with add_index_terms.R) ──────────────────────────

classify_lines <- function(lines) {
  in_code_block   <- FALSE
  in_yaml         <- FALSE
  in_html_comment <- FALSE
  is_prose        <- logical(length(lines))

  for (i in seq_along(lines)) {
    line <- lines[[i]]

    if (i == 1 && str_detect(line, "^---\\s*$")) {
      in_yaml <- TRUE; is_prose[i] <- FALSE; next
    }
    if (in_yaml) {
      is_prose[i] <- FALSE
      if (str_detect(line, "^(---|\\.\\.\\.)\\s*$")) in_yaml <- FALSE
      next
    }
    if (in_html_comment) {
      is_prose[i] <- FALSE
      if (str_detect(line, "-->")) in_html_comment <- FALSE
      next
    }
    if (str_detect(line, "<!--")) {
      is_prose[i] <- FALSE
      if (!str_detect(line, "-->")) in_html_comment <- TRUE
      next
    }
    if (str_detect(line, "^\\s*```")) {
      in_code_block <- !in_code_block
      is_prose[i]   <- FALSE
    } else {
      is_prose[i] <- !in_code_block
    }
  }
  is_prose
}

# Return line numbers where `pattern` matches in prose, excluding protected spans.
prose_match_lines <- function(lines, is_prose, pattern) {
  protected_pattern <- paste0(
    "`[^`]+`",               # inline code
    "|\\\\@ref\\([^)]+\\)",  # \@ref(...)
    "|\\]\\([^)]+\\)"        # markdown link URLs ](...)
  )

  hit_lines <- integer(0)

  for (i in seq_along(lines)) {
    if (!is_prose[i]) next
    line <- lines[[i]]

    # Build safe mask (exclude protected spans)
    protected <- str_locate_all(line, protected_pattern)[[1]]
    n    <- nchar(line)
    safe <- rep(TRUE, n)
    if (nrow(protected) > 0) {
      for (k in seq_len(nrow(protected)))
        safe[protected[k, "start"]:protected[k, "end"]] <- FALSE
    }

    # Find matches of pattern
    m <- str_locate_all(line, pattern)[[1]]
    if (nrow(m) == 0) next

    # Keep only matches entirely within safe regions
    safe_hits <- apply(m, 1, function(r) all(safe[r["start"]:r["end"]]))
    if (any(safe_hits)) hit_lines <- c(hit_lines, i)
  }
  hit_lines
}


# ── Format line numbers into gap-grouped ranges ───────────────────────────────
# Lines within `gap` of each other are collapsed into a single "start-end" range.
# E.g. c(50,53,57,123,145,148) with gap=10 → "50-57, 123, 145-148"
format_lines <- function(lines, gap = 10) {
  if (length(lines) == 0) return("")
  lines <- sort(unique(lines))
  start <- lines[1]; end <- lines[1]
  parts <- character(0)
  for (i in seq_along(lines)[-1]) {
    if (lines[i] - end <= gap) {
      end <- lines[i]
    } else {
      parts <- c(parts, if (start == end) start else paste0(start, "-", end))
      start <- lines[i]; end <- lines[i]
    }
  }
  parts <- c(parts, if (start == end) start else paste0(start, "-", end))
  paste(parts, collapse = ", ")
}


# ── Main search ────────────────────────────────────────────────────────────────

terms <- str_split(SEARCH_TERMS, "\\|")[[1]] |> str_trim()

EXCLUDE  <- c("Designing-Simulations-in-R.Rmd", "220-index.Rmd")
all_rmds <- list.files(RMD_DIR, pattern = "\\.Rmd$", full.names = FALSE)
all_rmds <- sort(all_rmds[!all_rmds %in% EXCLUDE])
if (!is.null(SEARCH_FILES)) all_rmds <- all_rmds[all_rmds %in% SEARCH_FILES]

# results[term][file] = integer vector of line numbers
results <- setNames(vector("list", length(terms)), terms)

for (term in terms) {
  flags   <- if (CASE_SENSITIVE) "" else "(?i)"
  pattern <- paste0(flags, "\\b", str_escape(term), "\\b")
  file_hits <- list()

  for (fname in all_rmds) {
    lines    <- readLines(file.path(RMD_DIR, fname), warn = FALSE)
    is_prose <- classify_lines(lines)
    hits     <- prose_match_lines(lines, is_prose, pattern)
    if (length(hits) > 0) file_hits[[fname]] <- hits
  }
  results[[term]] <- file_hits
}


# ── Output ─────────────────────────────────────────────────────────────────────

pad <- max(nchar(all_rmds)) + 2

for (term in terms) {
  header <- paste0('\u2500\u2500 "', term, '" ', strrep("\u2500", max(0, 60 - nchar(term) - 5)))
  cat(header, "\n")

  file_hits <- results[[term]]
  if (length(file_hits) == 0) {
    cat("  (no prose matches)\n")
  } else {
    for (fname in names(file_hits)) {
      line_str <- format_lines(file_hits[[fname]])
      cat(sprintf("  %-*s  %s\n", pad, fname, line_str))
    }
  }
  cat("\n")
}

# Combined summary when multiple terms given
if (length(terms) > 1) {
  cat(strrep("\u2500", 62), "\n")
  cat("  COMBINED (any term)\n")
  cat(strrep("\u2500", 62), "\n")

  # Merge all hits per file
  combined <- list()
  for (term in terms) {
    for (fname in names(results[[term]])) {
      combined[[fname]] <- sort(unique(c(combined[[fname]], results[[term]][[fname]])))
    }
  }
  if (length(combined) == 0) {
    cat("  (no matches)\n")
  } else {
    for (fname in sort(names(combined))) {
      line_str <- format_lines(combined[[fname]])
      cat(sprintf("  %-*s  %s\n", pad, fname, line_str))
    }
  }
  cat("\n")
}
