#' Package Spell Checking
#'
#' Automatically spell-check package description, documentation, and vignettes.
#'
#' Parses and checks R manual pages, rmd/rnw vignettes, and text fields in the
#' package `DESCRIPTION` file.
#'
#' The preferred spelling language (typically `en-GB` or `en-US`) should be specified
#' in the `Language` field from your package `DESCRIPTION`. To whitelist custom words
#' use the package [WORDLIST][get_wordlist] file which will be added to the dictionary
#' when spell checking. See [update_wordlist] to automatically populate and update this
#' file.
#'
#' The [spell_check_setup] function adds a unit test to your package which automatically
#' runs a spell check on documentation and vignettes during `R CMD check` if the environment
#' variable `NOT_CRAN` is set to `TRUE`. By default this unit test never fails; it merely
#' prints potential spelling errors to the console.
#'
#' Hunspell includes dictionaries for `en_US` and `en_GB` by default. Other languages
#' require installation of a custom dictionary, see [hunspell][hunspell::hunspell] for details.
#'
#' @export
#' @rdname spell_check_package
#' @name spell_check_package
#' @aliases spelling
#' @family spelling
#' @param pkg path to package root directory containing the `DESCRIPTION` file
#' @param vignettes check all `rmd` and `rnw` files in the pkg root directory (e.g.
#' `readme.md`) and package `vignettes` folder.
#' @param use_wordlist ignore words in the package [WORDLIST][get_wordlist] file
#' @param lang set `Language` field in `DESCRIPTION` e.g. `"en-US"` or `"en-GB"`.
#' For supporting other languages, see the [hunspell vignette](https://bit.ly/2EquLKy).
spell_check_package <- function(pkg = ".", vignettes = TRUE, use_wordlist = TRUE){
  # Get package info
  pkg <- as_package(pkg)

  # Get language from DESCRIPTION
  lang <- normalize_lang(pkg$language)

  # Add custom words to the ignore list
  add_words <- if(isTRUE(use_wordlist))
    get_wordlist(pkg$path)
  author <- if(length(pkg[['authors@r']])){
    parse_r_field(pkg[['authors@r']])
  } else {
    strsplit(pkg[['author']], " ", fixed = TRUE)[[1]]
  }
  ignore <- unique(c(pkg$package, author, hunspell::en_stats, add_words))

  # Create the hunspell dictionary object
  dict <- hunspell::dictionary(lang, add_words = sort(ignore))

  # Check Rd manual files
  rd_files <- sort(list.files(file.path(pkg$path, "man"), "\\.rd$", ignore.case = TRUE, full.names = TRUE))
  rd_lines <- lapply(rd_files, spell_check_file_rd, dict = dict)

  # Check 'DESCRIPTION' fields
  pkg_fields <- c("title", "description")
  pkg_lines <- lapply(pkg_fields, function(x){
    spell_check_description_text(textConnection(pkg[[x]]), dict = dict)
  })

  # Combine
  all_sources <- c(rd_files, pkg_fields)
  all_lines <- c(rd_lines, pkg_lines)

  if(isTRUE(vignettes)){
    # Where to check for rmd/md files
    vign_files <- list.files(file.path(pkg$path, "vignettes"), pattern = "\\.r?md$",
                             ignore.case = TRUE, full.names = TRUE, recursive = TRUE)
    root_files <- list.files(pkg$path, pattern = "(readme|news|changes|index).r?md",
                             ignore.case = TRUE, full.names = TRUE)

    # Markdown vignettes
    md_files <- normalizePath(c(root_files, vign_files))
    md_lines <- lapply(sort(md_files), spell_check_file_md, dict = dict)

    # Sweave vignettes
    rnw_files <- list.files(file.path(pkg$path, "vignettes"), pattern = "\\.[rs]nw$", ignore.case = TRUE, full.names = TRUE)
    rnw_lines <- lapply(sort(rnw_files), spell_check_file_knitr, format = "latex", dict = dict)

    # Combine
    all_sources <- c(all_sources, md_files, rnw_files)
    all_lines <- c(all_lines, md_lines, rnw_lines)
  }
  summarize_words(all_sources, all_lines)
}

as_package <- function(pkg){
  if(inherits(pkg, 'package'))
    return(pkg)
  path <- pkg
  description <- if(file.exists(file.path(path, "DESCRIPTION.in"))){
    file.path(path, "DESCRIPTION.in")
  } else {
    normalizePath(file.path(path, "DESCRIPTION"), mustWork = TRUE)
  }
  pkg <- read.dcf(description)[1,]
  Encoding(pkg) = "UTF-8"
  pkg <- as.list(pkg)
  names(pkg) <- tolower(names(pkg))
  pkg$path <- dirname(description)
  structure(pkg, class = 'package')
}

# Find all occurences for each word
summarize_words <- function(file_names, found_line){
  words_by_file <- lapply(found_line, names)
  bad_words <- sort(unique(unlist(words_by_file)))
  out <- data.frame(
    word = bad_words,
    stringsAsFactors = FALSE
  )
  out$found <- lapply(bad_words, function(word) {
    index <- which(vapply(words_by_file, `%in%`, x = word, logical(1)))
    reports <- vapply(index, function(i){
      paste0(basename(file_names[i]), ":", found_line[[i]][word])
    }, character(1))
  })
  structure(out, class = c("summary_spellcheck", "data.frame"))
}

#' @export
print.summary_spellcheck <- function(x, ...){
  if(!nrow(x)){
    cat("No spelling errors found.\n")
    return(invisible())
  }
  words <- x$word
  fmt <- paste0("%-", max(nchar(words), 0) + 3, "s")
  pretty_names <- sprintf(fmt, words)
  cat(sprintf(fmt, "  WORD"), "  FOUND IN\n", sep = "")
  for(i in seq_len(nrow(x))){
    cat(pretty_names[i])
    cat(paste(x$found[[i]], collapse = paste0("\n", sprintf(fmt, ""))))
    cat("\n")
  }
  invisible(x)
}

#' @export
#' @aliases spell_check_test
#' @rdname spell_check_package
#' @param error should `CMD check` fail if spelling errors are found?
#' Default only prints results.
spell_check_setup <- function(pkg = ".", vignettes = TRUE, lang = "en-US", error = FALSE){
  # Get package info
  pkg <- as_package(pkg)
  lang <- normalize_lang(lang)
  pkg$language <- lang
  update_description(pkg, lang = lang)
  update_wordlist(pkg, vignettes = vignettes)
  dir.create(file.path(pkg$path, "tests"), showWarnings = FALSE)
  writeLines(sprintf("if(requireNamespace('spelling', quietly = TRUE))
  spelling::spell_check_test(vignettes = %s, error = %s,
                             skip_on_cran = TRUE)",
    deparse(vignettes), deparse(error)), file.path(pkg$path, "tests/spelling.R"))
  cat(sprintf("Updated %s\n", file.path(pkg$path, "tests/spelling.R")))
}

#' @export
spell_check_test <- function(vignettes = TRUE, error = FALSE, lang = NULL, skip_on_cran = TRUE){
  if(isTRUE(skip_on_cran)){
    not_cran <- Sys.getenv('NOT_CRAN')
    # See logic in tools:::config_val_to_logical
    if(is.na(match(tolower(not_cran), c("1", "yes", "true"))))
      return(NULL)
  }
  out_save <- readLines(system.file("templates/spelling.Rout.save", package = 'spelling'))
  code <- format_syntax(readLines("spelling.R"))
  out_save <- sub("@INPUT@", code, out_save, fixed = TRUE)
  writeLines(out_save, "spelling.Rout.save")

  # Try to find pkg source directory
  pkg_dir <- list.files("../00_pkg_src", full.names = TRUE)
  if(!length(pkg_dir)){
    # This is where it is on e.g. win builder
    check_dir <- dirname(getwd())
    if(grepl("\\.Rcheck$", check_dir)){
      source_dir <- sub("\\.Rcheck$", "", check_dir)
      if(file.exists(source_dir))
        pkg_dir <- source_dir
    }
  }
  if(!length(pkg_dir)){
    warning("Failed to find package source directory")
    return(invisible())
  }
  results <- spell_check_package(pkg_dir, vignettes = vignettes)
  if(nrow(results)){
    if(isTRUE(error)){
      output <- sprintf("Potential spelling errors: %s\n", paste(results$word, collapse = ", "))
      stop(output, call. = FALSE)
    } else {
      cat("Potential spelling errors:\n")
      print(results)
    }
  }
  cat("All Done!\n")
}

update_description <- function(pkg, lang = NULL){
  desc <- normalizePath(file.path(pkg$path, "DESCRIPTION"), mustWork = TRUE)
  lines <- readLines(desc, warn = FALSE)
  if(!any(grepl("spelling", c(pkg$package, pkg$suggests, pkg$imports, pkg$depends)))){
    lines <- if(!any(grepl("^Suggests", lines))){
      c(lines, "Suggests:\n    spelling")
    } else {
      sub("^Suggests:", "Suggests:\n    spelling,", lines)
    }
  }
  is_lang <- grepl("^Language:", lines, ignore.case = TRUE)
  isolang <- gsub("_", "-", lang, fixed = TRUE)
  if(any(is_lang)){
    is_lang <- which(grepl("^Language:", lines))
    lines[is_lang] <- paste("Language:", isolang)
  } else {
    message(sprintf("Adding 'Language: %s' to DESCRIPTION", isolang))
    lines <- c(lines, paste("Language:", isolang))
  }
  writeLines(lines, desc)
}

format_syntax <- function(txt){
  pt <- getOption('prompt')
  ct <- getOption('continue')
  prefix <- c(pt, rep(ct, length(txt) - 1))
  paste(prefix, txt, collapse = "\n", sep = "")
}

parse_r_field <- function(txt){
  tryCatch({
    info <- eval(parse(text = txt))
    unlist(info, recursive = TRUE, use.names = FALSE)
  }, error = function(e){
    NULL
  })
}
