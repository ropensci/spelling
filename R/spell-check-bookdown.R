#' Package Spell Checking
#'
#' Automatically spell-check bookdown source files.
#'
#' Parses and checks bookdown source files, and if these files are present
#' * README.md (when there's no README.Rmd)
#' * NEWS.md
#' *  text fields in the `DESCRIPTION` file if present.
#'
#' The preferred spelling language (typically `en-GB` or `en-US`) should be
#' * specified in the `Language` field from your package `DESCRIPTION`,
#' * or entered as the `lang` argument.
#' To whitelist custom words
#' use the bookdown [WORDLIST][get_wordlist] file which will be added to the dictionary
#' when spell checking. See [update_wordlist] to automatically populate and update this
#' file.
#'
#' Hunspell includes dictionaries for `en_US` and `en_GB` by default. Other languages
#' require installation of a custom dictionary, see [hunspell][hunspell::hunspell] for details.
#'
#' @export
#' @rdname spell_check_bookdown
#' @name spell_check_bookdown
#' @aliases spelling
#' @family spelling
#' @param path path to package root directory containing the `DESCRIPTION` file
#' @param use_wordlist ignore words in the package [WORDLIST][get_wordlist] file
#' @param lang set `Language` field in `DESCRIPTION` e.g. `"en-US"` or `"en-GB"`.
#' For supporting other languages, see the [hunspell vignette](https://bit.ly/2EquLKy).
spell_check_bookdown <- function(path = ".", lang = NULL, use_wordlist = TRUE){

  # Get language
  if (is.null(lang)) {
    if (file.exists(file.path(path, "DESCRIPTION"))){
      pkg <- as_package(path)

      # Get language from DESCRIPTION
      lang <- normalize_lang(pkg$language)
    } else {
      "en-US"
    }
  } else {
    lang <- normalize_lang(lang)
  }

  # Add custom words to the ignore list
  add_words <- if(isTRUE(use_wordlist))
    get_wordlist(path)

  if (file.exists(file.path(path, "DESCRIPTION"))){
    pkg <- as_package(path)
    author <- if(length(pkg[['authors@r']])){
      parse_r_field(pkg[['authors@r']])
    } else {
      if ("author" %in% names(pkg)) {
        strsplit(pkg[['author']], " ", fixed = TRUE)[[1]]
      } else{
        author <- NULL
      }
    }

    meta <- c(pkg$package, author)
  } else {
    meta <- NULL
  }
  ignore <- unique(c(meta, hunspell::en_stats, add_words))

  # Create the hunspell dictionary object
  dict <- hunspell::dictionary(lang, add_words = sort(ignore))

  # Where to check for rmd/md files
  bookdown_files <- list.files(file.path(path), pattern = "\\.rmd$",
                           ignore.case = TRUE, full.names = TRUE, recursive = TRUE)
  root_files <- list.files(file.path(path), pattern = "(readme|news|changes).r?md",
                           ignore.case = TRUE, full.names = TRUE)

  # Markdown files
  md_files <- sort(normalizePath(c(root_files, bookdown_files)))
  md_lines <- lapply(md_files, spell_check_file_md, dict = dict)

  all_sources <- md_files
  all_lines <- md_lines

  # Check 'DESCRIPTION' fields
  if (file.exists(file.path(path, "DESCRIPTION"))){
    pkg_fields <- c("title", "description")
    pkg <- as_package(path)
    pkg_lines <- lapply(pkg_fields, function(x){
      if (x %in% names(pkg)) {
        spell_check_file_text(textConnection(pkg[[x]]), dict = dict)
      } else {
        spell_check_file_text(textConnection(""), dict = dict)
      }
    })

    all_sources <- c(all_sources, pkg_fields)
    all_lines <- c(all_lines, pkg_lines)
  }

  summarize_words(all_sources, all_lines)
}
