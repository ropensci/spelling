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
#' WARNING: setting `lang` will remove any existing changes stored by
#' `change_language()`
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
  files <- normalizePath(files, winslash = "/", mustWork = FALSE)

  # ignore vignettes stored by the renv package
  files <- files[!grepl(file.path("renv", "library"), files, ignore.case = TRUE)]
  # ignore md files when an Rmd with the same name exists.
  md <- files[grepl("\\.md$", files, ignore.case = TRUE)]
  rmd <- files[grepl("\\.rmd$", files, ignore.case = TRUE)]
  overlap <- gsub("\\.md$", "", md, ignore.case = TRUE) %in%
    gsub("\\.rmd$", "", rmd, ignore.case = TRUE)
  files <- c(rmd, md[!overlap])

  # ignore files listed to be ignored
  files <- files[!files %in% settings[["ignore"]]]
  settings <- settings[names(settings) != "ignore"]
  # handle files with a non-default language
  results <- list()
  for (i in tail(seq_along(settings), -1)) {
    relevant <- vapply(
      paste0("^", settings[[i]]), FUN.VALUE = logical(length(files)),
      files = files, FUN = function(x, files) {
        grepl(x, files)
      }
    )
    relevant <- apply(relevant, 1, any)
    to_check <- files[relevant]
    files <- files[!relevant]
    problems <- spell_check_files(
      path = file.path(root, to_check), lang = names(settings[i])
    )
    problems$lang <- rep(names(settings[i]), nrow(problems))
    results[names(settings[i])] <- list(problems)
  }
  # handle remaining files with the default language
  problems <- spell_check_files(
    path = file.path(root, files), lang = settings$default
  )
  problems$lang <- rep(settings[["default"]], nrow(problems))
  results["default"] <- list(problems)
  do.call(rbind, results)
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
  raw_settings <- gsub(".*\"default\": \\[\".*?\"\\]", "", raw_settings)
  while (grepl("\"(.*?)\": \\[(.*?)\\]", raw_settings)) {
    next_lang <- gsub(".*\"(.*?)\": \\[(.*?)\\].*", "\\1", raw_settings)
    next_files <- gsub(".*\"(.*?)\": \\[(.*?)\\].*", "\\2", raw_settings)
    this_file <- character(0)
    while (nchar(next_files) > 0) {
      this_file <- c(this_file, gsub(".*\"(.*?)\".*", "\\1", next_files))
      next_files <- gsub(".*\".*?\"(.*)", "\\1", next_files)
    }
    settings <- c(settings, setNames(list(this_file), next_lang))
    raw_settings <- gsub(".*\".*?\": \\[.*?\\](.*)", "\\1", raw_settings)
  }
  return(settings)
}

format_setting <- function(lang, settings) {
  if (lang == "default") {
    return(sprintf("  \"default\": [\"%s\"]", settings$default))
  }
  if (length(settings[[lang]]) == 0) {
    return("")
  }
  sprintf(
    "  \"%s\": [%s]", lang,
    paste(sprintf("\"%s\"", settings[[lang]]), collapse = ", ")
  )
}

write_settings <- function(settings, root) {
  content <- vapply(
    names(settings), FUN = format_setting, FUN.VALUE = character(1),
    settings = settings
  )
  writeLines(
    c("{", paste(content, collapse = ",\n"), "}"),
    file.path(root, "spell_check.json")
  )
  return(invisible(NULL))
}

#' Change the language for spell checking for a subset of files in a project.
#' @inheritParams spell_check_project
#' @param path the relative path to a file or directory within the project
#' @param lang the language to use when spell checking the file or files with
#' the directory.
#' Use `lang = "ignore"` to exclude `path` from spell checking.
#' @export
#' @importFrom utils file_test
change_language <- function(root, path, lang) {
  stopifnot("lang is not a string" = is.character(lang) && length(lang) == 1)
  stopifnot("lang" = lang != "default")
  stopifnot("root is not a string" = is.character(root) && length(root) == 1)
  stopifnot("root is not a directory" = file_test("-d", root))
  stopifnot("path is not a string" = is.character(path) && length(path) == 1)
  path <- normalizePath(path, winslash = "/", mustWork = FALSE)
  stopifnot(
    "path is neither a file or a directory in root" =
      file_test("-f", file.path(root, path)) ||
      file_test("-d", file.path(root, path))
  )

  # get current settings
  settings <- parse_settings(root = root)

  # remove path from existing settings
  settings <- vapply(
    settings, FUN.VALUE = vector(mode = "list", length = 1), path = path,
    FUN = function(x, path) {
      list(x[!grepl(sprintf("^%s", path), x)])
    }
  )
  # make sure the settings do not contain a reference to a parent folder of path
  # when path is a file
  if (file_test("-f", file.path(root, path))) {
    parent <- dirname(path)
    while (parent != ".") {
      settings <- vapply(
        settings, FUN.VALUE = vector(mode = "list", length = 1), path = parent,
        FUN = function(x, path) {
          list(x[!grepl(sprintf("^%s", path), x)])
        }
      )
      parent <- dirname(parent)
    }
  }

  # add path to the settings
  settings[[lang]] <- sort(c(settings[[lang]], path), method = "radix")
  return(write_settings(settings = settings, root = root))
}
