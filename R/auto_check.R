#' Auto Spell Check
#'
#' Setup automatic spell checking as a unit test for `CMD check`.
#'
#' Use [spell_check_setup()] to add a unit test to your package that that automatically
#' spell checks documentation and vignettes when running `R CMD check`.
#'
#' @export
#' @family spelling
#' @aliases spell_check_test
#' @rdname autotest
#' @name autotest
#' @inheritParams spell_check
spell_check_setup <- function(path = ".", vignettes = TRUE, lang = "en_US"){
  path <- normalizePath(path, mustWork = TRUE)
  update_wordlist(path)
  dir.create(file.path(path, "tests"), showWarnings = FALSE)
  writeLines(sprintf("spelling::spell_check_test(vignettes = %s, lang = %s)",
                     deparse(vignettes), deparse(lang)), file.path(path, "tests/spelling.R"))
  cat(sprintf("Updated %s\n", file.path(path, "tests/spelling.R")))
}

#' @export
spell_check_test <- function(vignettes = TRUE, lang = "en_US"){
  pkg_dir <- list.files("../00_pkg_src", full.names = TRUE)
  if(!length(pkg_dir)){
    warning("Failed to find package source directory")
    return(invisible())
  }
  results <- spell_check_package(pkg_dir, vignettes = vignettes, lang = lang)
  if(length(results))
    stop(sprintf("Potential spelling errors: %s\nIf these are false positives, run spelling::update_wordlist()",
                 paste(names(results),collapse = ", ")))
  cat("All good.\n")
}
