#' Spell checking of a project with markdown files
#'
#' Detect all Rmarkdown and markdown files recursively from the project root.
#' It ignores files in the `renv` folder as they most likely contain vignettes
#' from archived packages.
#'
#' @param root path to the project root.
#' @inheritParams spell_check_files
#' @export
#' @importFrom utils file_test
spell_check_project <- function(root = ".", lang = "en_US") {
  stopifnot("root is not a string" = is.character(root) && length(root) == 1)
  stopifnot("root is not an existing directory" = file_test("-d", root))
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

  spell_check_files(path = file.path(root, files), lang = lang)
}
