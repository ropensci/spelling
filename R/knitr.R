# Adapted from lintr:::extract_r_source
remove_chunks <- function(path) {
  path <- normalizePath(path, mustWork = TRUE)
  filename <- basename(path)
  lines <- readLines(path)

  pattern <- get_knitr_pattern(filename, lines)
  if (is.null(pattern$chunk.begin) || is.null(pattern$chunk.end)) {
    return(lines)
  }

  starts <- grep(pattern$chunk.begin, lines, perl = TRUE)
  ends <- grep(pattern$chunk.end, lines, perl = TRUE)

  # no chunks found, so just return the lines
  if (length(starts) == 0 || length(ends) == 0) {
    return(lines)
  }

  # Find first ending after a start
  lapply(starts, function(x){
    end <- min(ends[ends > x])
    lines[x:end] <<- ""
  })
  return(lines)
}

get_knitr_pattern <- function(filename, lines) {
  detect_pattern <- getFromNamespace('detect_pattern', 'knitr')
  file_ext <- getFromNamespace('file_ext', 'knitr')
  pattern <- detect_pattern(lines, tolower(file_ext(filename)))
  if (!is.null(pattern)) {
    knitr::all_patterns[[pattern]]
  } else {
    NULL
  }
}
