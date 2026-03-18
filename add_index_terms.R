################################################################################
# add_index_terms.R
#
# Semi-automated indexing helper for bookdown projects.
#
# For each term in the INDEX_TERMS list below, this script finds occurrences
# in .Rmd files and appends \index{...} immediately after the term — but ONLY
# in prose text, never inside fenced code blocks (```...```) or inline code
# (`...`).
#
# Usage:
#   1. Edit INDEX_TERMS (and optionally TERM_FILES) below.
#   2. Run in DRY_RUN mode first to preview what will change.
#   3. Set DRY_RUN <- FALSE to apply the changes.
#
# See the README section at the bottom for indexing conventions.
################################################################################

library(stringr)

# ── Configuration ──────────────────────────────────────────────────────────────

DRY_RUN <- TRUE          # TRUE = preview only, FALSE = write changes to disk
FIRST_PER_FILE <- TRUE   # TRUE = index only the first match per file (common
                         #        for terms defined/introduced in a chapter)
                         # FALSE = index every match (for important recurring terms)
CASE_SENSITIVE <- TRUE   # TRUE = exact case match, FALSE = case-insensitive

# Directory containing the .Rmd files
RMD_DIR <- "."

# ── Index term definitions ─────────────────────────────────────────────────────
#
# Each entry is a named list:
#   term       - the exact text to search for in prose
#   index_key  - the LaTeX \index{} entry (use ! for subentries)
#   files      - character vector of filenames to search, or NULL for all files
#
# Examples of index_key formats:
#   "data-generating process"          → top-level entry
#   "performance measures!bias"        → subentry: bias under performance measures
#   "DGP|see{data-generating process}" → cross-reference (appears in PDF index)
#
# TIP: Run with DRY_RUN <- TRUE first to preview changes before committing.

INDEX_TERMS <- list(

  # ── Core simulation vocabulary ──
  list(
    term      = "data-generating process",
    index_key = "data-generating process",
    files     = NULL  # all files
  ),
  list(
    term      = "data generating process",
    index_key = "data-generating process",
    files     = NULL
  ),
  list(
    term      = "DGP",
    index_key = "DGP|see{data-generating process}",
    files     = NULL
  ),
  list(
    term      = "Monte Carlo",
    index_key = "Monte Carlo simulation",
    files     = NULL
  ),
  list(
    term      = "simulation study",
    index_key = "simulation study",
    files     = NULL
  ),

  # ── Performance measures ──
  list(
    term      = "bias",
    index_key = "performance measures!bias",
    files     = c("040-Performance-criteria.Rmd")
  ),
  list(
    term      = "root mean squared error",
    index_key = "performance measures!RMSE",
    files     = NULL
  ),
  list(
    term      = "RMSE",
    index_key = "performance measures!RMSE",
    files     = NULL
  ),
  list(
    term      = "coverage",
    index_key = "performance measures!coverage",
    files     = c("040-Performance-criteria.Rmd", "074-building-good-vizualizations.Rmd")
  ),
  list(
    term      = "Type I error",
    index_key = "performance measures!Type I error",
    files     = NULL
  ),
  list(
    term      = "statistical power",
    index_key = "performance measures!power",
    files     = NULL
  ),
  list(
    term      = "Monte Carlo standard error",
    index_key = "Monte Carlo standard error",
    files     = NULL
  ),
  list(
    term      = "MCSE",
    index_key = "MCSE|see{Monte Carlo standard error}",
    files     = NULL
  ),

  # ── Packages ──
  list(
    term      = "simhelpers",
    index_key = "simhelpers package",
    files     = NULL
  ),
  list(
    term      = "furrr",
    index_key = "furrr package",
    files     = NULL
  ),
  list(
    term      = "future",
    index_key = "future package",
    files     = NULL
  ),

  # ── Computational topics ──
  list(
    term      = "parallel processing",
    index_key = "parallel processing",
    files     = NULL
  ),
  list(
    term      = "pseudo-random number",
    index_key = "pseudo-random number generator",
    files     = NULL
  ),
  list(
    term      = "seed",
    index_key = "random seed",
    files     = c("035-running-simulation.Rmd")
  ),
  list(
    term      = "reparameterization",
    index_key = "reparameterization",
    files     = NULL
  ),

  # ── Statistical methods ──
  list(
    term      = "parametric bootstrap",
    index_key = "parametric bootstrap",
    files     = NULL
  ),
  list(
    term      = "potential outcomes",
    index_key = "potential outcomes framework",
    files     = NULL
  ),
  list(
    term      = "cluster-randomized trial",
    index_key = "cluster-randomized trial",
    files     = NULL
  ),
  list(
    term      = "power analysis",
    index_key = "power analysis",
    files     = NULL
  )

  # ── Add more entries here ──
  # list(
  #   term      = "your term",
  #   index_key = "your index entry",
  #   files     = NULL  # or c("specific-file.Rmd")
  # )
)


# ── Helper functions ───────────────────────────────────────────────────────────

# Split an Rmd file's lines into "prose" vs "code" regions.
# Returns a logical vector: TRUE = prose line, FALSE = code line (skip indexing).
classify_lines <- function(lines) {
  in_code_block <- FALSE
  is_prose <- logical(length(lines))

  for (i in seq_along(lines)) {
    line <- lines[[i]]
    # Toggle fenced code block state
    if (str_detect(line, "^\\s*```")) {
      in_code_block <- !in_code_block
      is_prose[i] <- FALSE   # the fence line itself is not prose
    } else {
      is_prose[i] <- !in_code_block
    }
  }
  is_prose
}

# Within a prose line, strip inline code spans before searching,
# then apply replacement only outside those spans.
replace_outside_inline_code <- function(line, pattern, replacement, first_only) {
  # Find inline code positions: `...`
  # We protect those ranges and only substitute outside them.
  inline_ranges <- str_locate_all(line, "`[^`]+`")[[1]]

  # Build a mask of character positions that are "safe" (not in inline code)
  n <- nchar(line)
  safe <- rep(TRUE, n)
  if (nrow(inline_ranges) > 0) {
    for (k in seq_len(nrow(inline_ranges))) {
      safe[inline_ranges[k, "start"]:inline_ranges[k, "end"]] <- FALSE
    }
  }

  # Find all matches of pattern in the full line
  m <- str_locate_all(line, pattern)[[1]]
  if (nrow(m) == 0) return(list(line = line, changed = FALSE))

  # Filter to matches that fall entirely within safe regions
  safe_matches <- apply(m, 1, function(r) all(safe[r["start"]:r["end"]]))
  m <- m[safe_matches, , drop = FALSE]
  if (nrow(m) == 0) return(list(line = line, changed = FALSE))

  if (first_only) m <- m[1, , drop = FALSE]

  # Apply replacements in reverse order so positions stay valid
  for (k in rev(seq_len(nrow(m)))) {
    start <- m[k, "start"]
    end   <- m[k, "end"]
    original_match <- substr(line, start, end)
    line <- paste0(
      substr(line, 1, start - 1),
      replacement(original_match),
      substr(line, end + 1, nchar(line))
    )
  }
  list(line = line, changed = TRUE)
}


# ── Main processing function ───────────────────────────────────────────────────

process_term <- function(entry, rmd_dir, dry_run, first_per_file, case_sensitive) {
  term      <- entry$term
  index_key <- entry$index_key
  files     <- entry$files

  # Select target files
  all_rmds <- list.files(rmd_dir, pattern = "\\.Rmd$", full.names = TRUE)
  if (!is.null(files)) {
    all_rmds <- all_rmds[basename(all_rmds) %in% files]
  }
  if (length(all_rmds) == 0) {
    message("  No matching files for term: ", term)
    return(invisible(NULL))
  }

  # Build regex pattern (word boundary, optional case)
  pattern_flags <- if (case_sensitive) "" else "(?i)"
  pattern <- paste0(pattern_flags, "\\b", str_escape(term), "\\b")

  # Replacement function: append \index{} after the matched text
  make_replacement <- function(matched_text) {
    paste0(matched_text, "\\index{", index_key, "}")
  }

  total_files_changed <- 0

  for (rmd_path in all_rmds) {
    lines    <- readLines(rmd_path, warn = FALSE)
    is_prose <- classify_lines(lines)

    new_lines <- lines
    file_changed <- FALSE
    found_first  <- FALSE

    for (i in seq_along(lines)) {
      if (!is_prose[i]) next
      if (first_per_file && found_first) next

      # Skip if \index{index_key} already follows this term on this line
      already_indexed <- str_detect(lines[i], paste0(
        str_escape(term), "\\\\index\\{", str_escape(index_key), "\\}"
      ))
      if (already_indexed) {
        found_first <- TRUE
        next
      }

      result <- replace_outside_inline_code(
        lines[i], pattern, make_replacement, first_only = (first_per_file && !found_first)
      )

      if (result$changed) {
        new_lines[i] <- result$line
        file_changed  <- TRUE
        found_first   <- TRUE

        if (dry_run) {
          cat(sprintf("\n  [%s] line %d\n", basename(rmd_path), i))
          cat(sprintf("  BEFORE: %s\n", lines[i]))
          cat(sprintf("  AFTER:  %s\n", result$line))
        }
      }
    }

    if (file_changed) {
      total_files_changed <- total_files_changed + 1
      if (!dry_run) {
        writeLines(new_lines, rmd_path)
      }
    }
  }

  if (total_files_changed == 0) {
    message("  (no matches found for: ", term, ")")
  }

  invisible(total_files_changed)
}


# ── Run ────────────────────────────────────────────────────────────────────────

if (DRY_RUN) {
  cat("════════════════════════════════════════════════════════\n")
  cat("  DRY RUN — no files will be modified\n")
  cat("  Set DRY_RUN <- FALSE to apply changes\n")
  cat("════════════════════════════════════════════════════════\n\n")
} else {
  cat("════════════════════════════════════════════════════════\n")
  cat("  APPLYING CHANGES to .Rmd files\n")
  cat("════════════════════════════════════════════════════════\n\n")
}

for (entry in INDEX_TERMS) {
  cat(sprintf("Term: \"%s\"  →  \\index{%s}\n", entry$term, entry$index_key))
  process_term(entry, RMD_DIR, DRY_RUN, FIRST_PER_FILE, CASE_SENSITIVE)
  cat("\n")
}

cat("Done.\n")
if (DRY_RUN) cat("Re-run with DRY_RUN <- FALSE to write changes.\n")


################################################################################
# INDEXING CONVENTIONS (LaTeX \index{} format)
#
# Top-level entry:
#   \index{data-generating process}
#
# Subentry (appears indented under the parent):
#   \index{performance measures!bias}
#   \index{performance measures!RMSE}
#
# Cross-reference ("see" note, no page number):
#   \index{DGP|see{data-generating process}}
#
# Cross-reference with a page number ("see also"):
#   \index{DGP|seealso{data-generating process}}
#
# Bold page number (marks the primary/defining occurrence):
#   \index{data-generating process|textbf}
#
# Page range (wrap the relevant content):
#   \index{long topic|(}   ... content ...   \index{long topic|)}
#
# TIP: Use \index{term|textbf} on the line where the term is DEFINED,
#      and plain \index{term} everywhere else.  This makes the defining
#      page stand out in the printed index.
################################################################################
