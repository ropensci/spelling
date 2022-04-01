#' Spell checking of a project with markdown files
#'
#' Detect all Rmarkdown and markdown files recursively from the project root.
#' It ignores files in the `renv` folder as they most likely contain vignettes
#' from archived packages.
#'
#' @param root path to the project root.
#' @param lang default language to use for spell checking.
#' When omitted it refers to language used during the last time the project was
#' spell checked.
#' The fall-back language is `en-US`.
#'
#' @export
#' @importFrom utils file_test
spell_check_project <- function(root = ".", lang) {
  stopifnot("root is not a string" = is.character(root) && length(root) == 1)
  stopifnot("root is not an existing directory" = file_test("-d", root))
  settings <- parse_settings(root = root, lang = lang)
  files <- list.files(
    root, pattern = "\\.r?md$", ignore.case = TRUE, recursive = TRUE,
    all.files = TRUE
  )
  # ignore vignettes stored by the renv package
  files <- files[!grepl(file.path("renv", "library"), files, ignore.case = TRUE)]
  # ignore md files when an Rmd with the same name exists.
  md <- files[grepl("\\.md$", files, ignore.case = TRUE)]
  rmd <- files[grepl("\\.rmd$", files, ignore.case = TRUE)]
  overlap <- gsub("\\.md$", "", md, ignore.case = TRUE) %in%
    gsub("\\.rmd$", "", rmd, ignore.case = TRUE)
  files <- c(rmd, md[!overlap])

  spell_check_files(path = file.path(root, files), lang = settings$default)
}

parse_settings <- function(root, lang) {
  settings_file <- file.path(root, "spell_check.json")
  if (!missing(lang)) {
    stopifnot("lang is not a string" = is.character(lang) && length(lang) == 1)
    settings <- list(default = lang)
    write_settings(settings = settings, root = root)
    return(settings)
  }
  if (!file_test("-f", settings_file)) {
    settings <- list(default = "en-US")
    write_settings(settings = settings, root = root)
    return(settings)
  }
  raw_settings <- paste(readLines(settings_file), collapse = "")
  settings <- list(
    default = gsub(".*\"default\": \\[\"(.*?)\"\\].*", "\\1", raw_settings)
  )
  return(settings)
}

write_settings <- function(settings, root) {
  content <- sprintf("\"default\": [\"%s\"]", settings$default)
  writeLines(content, file.path(root, "spell_check.json"))
}
