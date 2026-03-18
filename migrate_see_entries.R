################################################################################
# migrate_see_entries.R
#
# One-time migration: finds every \index{A|see{B}} (and |seealso{B}) in the
# main .Rmd files, replaces it with \index{B} (the canonical entry), and
# ensures \index{A|see{B}} is present in 220-index.Rmd.
#
# After running this, prose files contain only canonical \index{term} entries.
# All cross-references live exclusively in 220-index.Rmd.
#
# Usage:
#   Set DRY_RUN <- TRUE to preview, then DRY_RUN <- FALSE to apply.
################################################################################

library(stringr)

DRY_RUN    <- FALSE
RMD_DIR    <- "."
INDEX_FILE <- "220-index.Rmd"
EXCLUDE    <- c("Designing-Simulations-in-R.Rmd", INDEX_FILE)

# ── Scan all prose files for \index{A|see{B}} ─────────────────────────────────

files <- sort(list.files(RMD_DIR, pattern = "\\.Rmd$", full.names = TRUE))
files <- files[!basename(files) %in% EXCLUDE]

# Regex: captures A and B from \index{A|see{B}} or \index{A|seealso{B}}
SEE_PATTERN <- "\\\\index\\{([^|}]+)\\|(see(?:also)?)\\{([^}]+)\\}\\}"

found_pairs <- list()   # named list: "A|see{B}" -> TRUE, for deduplication

for (f in files) {
  lines    <- readLines(f, warn = FALSE)
  new_lines <- lines
  changed  <- FALSE

  for (i in seq_along(lines)) {
    line <- lines[[i]]
    if (!str_detect(line, SEE_PATTERN)) next

    # Collect all matches on this line
    m <- str_match_all(line, SEE_PATTERN)[[1]]
    # m columns: [full_match, A, see_or_seealso, B]

    for (k in seq_len(nrow(m))) {
      full_match  <- m[k, 1]
      A           <- m[k, 2]
      see_type    <- m[k, 3]
      B           <- m[k, 4]

      canonical_entry  <- paste0("\\index{", B, "}")
      see_entry        <- paste0("\\index{", A, "|", see_type, "{", B, "}}")
      found_pairs[[see_entry]] <- TRUE

      if (dry_run_line <- DRY_RUN) {
        cat(sprintf("\n  [%s] line %d\n", basename(f), i))
        cat(sprintf("  REPLACE: %s\n", full_match))
        cat(sprintf("  WITH:    %s\n", canonical_entry))
      }

      new_lines[[i]] <- str_replace(new_lines[[i]], fixed(full_match), canonical_entry)
      changed <- TRUE
    }
  }

  if (changed && !DRY_RUN) {
    writeLines(new_lines, f)
    cat(sprintf("  Updated: %s\n", basename(f)))
  }
}

# ── Update 220-index.Rmd ───────────────────────────────────────────────────────

index_path  <- file.path(RMD_DIR, INDEX_FILE)
index_lines <- readLines(index_path, warn = FALSE)

# Find lines already present in the file
already_present <- sapply(names(found_pairs), function(entry) {
  any(str_detect(index_lines, fixed(entry)))
})
new_entries <- names(found_pairs)[!already_present]

if (length(new_entries) == 0) {
  cat("\n220-index.Rmd: all see-entries already present, nothing to add.\n")
} else {
  cat(sprintf("\n220-index.Rmd: adding %d new see-entries:\n", length(new_entries)))
  for (e in new_entries) cat(sprintf("  %s\n", e))

  if (!DRY_RUN) {
    # Insert new entries just before the \printindex line
    printindex_line <- which(str_detect(index_lines, fixed("\\printindex")))
    if (length(printindex_line) == 0) {
      stop("Could not find \\printindex in ", INDEX_FILE)
    }
    insert_at <- printindex_line[1]
    index_lines <- c(
      index_lines[seq_len(insert_at - 1)],
      new_entries,
      index_lines[insert_at:length(index_lines)]
    )
    writeLines(index_lines, index_path)
    cat(sprintf("  Written to %s\n", INDEX_FILE))
  }
}

if (DRY_RUN) {
  cat("\n── DRY RUN complete. Set DRY_RUN <- FALSE to apply changes. ──\n")
} else {
  cat("\n── Migration complete. ──\n")
}
