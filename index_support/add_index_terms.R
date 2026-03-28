################################################################################
# add_index_terms.R
#
# Semi-automated indexing helper for bookdown projects.
#
# For each term in the INDEX_TERMS list below, this script finds occurrences
# in .Rmd files and appends \index{...} immediately after the term — but ONLY
# in prose text, never inside:
#   - YAML front matter (---...---)
#   - fenced code blocks (```...```)
#   - inline code (`...`)
#   - cross-reference spans (\@ref(...))
#   - markdown hyperlink URLs (...](...))
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

DRY_RUN <- FALSE          # TRUE = preview only, FALSE = write changes to disk
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
  # NOTE: DGP, MCSE, and other abbreviation |see{} cross-references are
  # defined in 220-index.Rmd, not here. Do not add |see{} entries to this
  # list — they belong in that file to avoid duplicates in the printed index.
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

# Split an Rmd file's lines into "prose" vs "code/metadata" regions.
# Returns a logical vector: TRUE = prose line, FALSE = skip (code or YAML).
# Skips:
#   - YAML front matter (the opening ---...--- or ---....... block)
#   - HTML comment blocks (<!--...-->, possibly multi-line)
#   - Fenced code blocks (```...```)
#
# NOTE on HTML comments: Rmd files sometimes contain commented-out code chunks
# like <!--```{r ...} ... ```. The closing ``` on its own line would look like
# a code-block toggle if the opener was missed. We guard against this by
# tracking HTML comment state and ignoring everything inside <!-- ... -->.
classify_lines <- function(lines) {
  in_code_block    <- FALSE
  in_yaml          <- FALSE
  in_html_comment  <- FALSE
  is_prose         <- logical(length(lines))
  
  for (i in seq_along(lines)) {
    line <- lines[[i]]
    
    # ── YAML front matter: only the very first --- opens it ──
    if (i == 1 && str_detect(line, "^---\\s*$")) {
      in_yaml     <- TRUE
      is_prose[i] <- FALSE
      next
    }
    if (in_yaml) {
      is_prose[i] <- FALSE
      # Closed by a second --- or a ... line
      if (str_detect(line, "^(---|\\.\\.\\.)\\s*$")) {
        in_yaml <- FALSE
      }
      next
    }
    
    # ── HTML comment blocks (<!-- ... -->) ──
    # Handle open and close potentially on the same line.
    if (in_html_comment) {
      is_prose[i] <- FALSE
      if (str_detect(line, "-->")) in_html_comment <- FALSE
      next
    }
    if (str_detect(line, "<!--")) {
      is_prose[i]     <- FALSE
      # Only stay in comment mode if the close isn't on the same line
      if (!str_detect(line, "-->")) in_html_comment <- TRUE
      next
    }
    
    # ── Fenced code blocks ──
    if (str_detect(line, "^\\s*```")) {
      in_code_block <- !in_code_block
      is_prose[i]   <- FALSE   # the fence line itself is not prose
    } else {
      is_prose[i] <- !in_code_block
    }
  }
  is_prose
}

# Within a prose line, apply replacement only outside protected spans:
#   - inline code:  `...`
#   - cross-refs:   \@ref(...)
#   - hyperlinks:   [...](...)  — the URL portion
replace_outside_inline_code <- function(line, pattern, replacement, first_only) {
  # Collect all protected ranges
  protected <- rbind(
    str_locate_all(line, "`[^`]+`")[[1]],           # inline code
    str_locate_all(line, "\\\\@ref\\([^)]+\\)")[[1]], # \@ref(...)
    str_locate_all(line, "\\]\\([^)]+\\)")[[1]]       # markdown link URLs ](...)
  )
  
  # Build a mask of character positions that are "safe"
  n <- nchar(line)
  safe <- rep(TRUE, n)
  if (nrow(protected) > 0) {
    for (k in seq_len(nrow(protected))) {
      safe[protected[k, "start"]:protected[k, "end"]] <- FALSE
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

  # |see{} and |seealso{} cross-references must appear exactly once in the
  # whole book — multiple identical "see X" entries look terrible in the index.
  # Auto-detect these and force first-in-book mode regardless of first_per_file.
  first_in_book <- str_detect(index_key, "\\|see")
  found_anywhere <- FALSE   # tracks across files when first_in_book is TRUE

  # Select target files
  EXCLUDE <- c("Designing-Simulations-in-R.Rmd", "220-index.Rmd")
  all_rmds <- list.files(rmd_dir, pattern = "\\.Rmd$", full.names = TRUE)
  all_rmds <- all_rmds[!basename(all_rmds) %in% EXCLUDE]
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
    if (first_in_book && found_anywhere) next   # already placed, skip rest of files

    lines    <- readLines(rmd_path, warn = FALSE)
    is_prose <- classify_lines(lines)

    new_lines <- lines
    file_changed <- FALSE
    found_first  <- FALSE

    for (i in seq_along(lines)) {
      if (!is_prose[i]) next
      if ((first_per_file || first_in_book) && found_first) next
      
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
        new_lines[i]   <- result$line
        file_changed   <- TRUE
        found_first    <- TRUE
        found_anywhere <- TRUE
        
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



if ( FALSE) {
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
}



################################################################################
# CLEANUP HELPER
#
# strip_to_first(index_cmd)
#
# Removes all occurrences of a literal \index{...} command from the .Rmd files
# except the very first one found (in filename order). Useful for fixing
# |see{} entries that were accidentally inserted multiple times.
#
# Example:
#   strip_to_first("\\index{DGP|see{data-generating process}}")
#   strip_to_first("\\index{MCSE|see{Monte Carlo standard error}}")

strip_to_first <- function(index_cmd, rmd_dir = ".") {
  EXCLUDE <- c("Designing-Simulations-in-R.Rmd", "220-index.Rmd")
  files <- sort(list.files(rmd_dir, pattern = "\\.Rmd$", full.names = TRUE))
  files <- files[!basename(files) %in% EXCLUDE]
  kept  <- FALSE
  for (f in files) {
    lines <- readLines(f, warn = FALSE)
    hits  <- which(grepl(index_cmd, lines, fixed = TRUE))
    if (length(hits) == 0) next
    if (!kept) {
      keep_line  <- hits[1]
      remove_pos <- hits[-1]
      kept       <- TRUE
    } else {
      remove_pos <- hits
    }
    if (length(remove_pos) > 0) {
      lines[remove_pos] <- gsub(index_cmd, "", lines[remove_pos], fixed = TRUE)
      writeLines(lines, f)
      cat(sprintf("  Removed %d occurrence(s) from %s\n", length(remove_pos), basename(f)))
    }
  }
  if (!kept) cat("Not found:", index_cmd, "\n") else cat("Done. First occurrence retained.\n")
}
################################################################################


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
