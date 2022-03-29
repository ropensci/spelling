#' Text Parsers
#'
#' Parse text from various formats and return a data frame with text lines
#' and position in the source document.
#'
#' @noRd
#' @name parse_text
#' @param path markdown file
#' @param yaml_fields character vector indicating which fields of the yaml
#' front matter should be spell checked.
#' @param extensions render markdown extensions? Passed to [commonmark][commonmark::markdown_xml]
#' @importFrom utils head tail
parse_text_md <- function(path, extensions = TRUE, yaml_fields = c("title" ,"subtitle", "description")){
  # Read file and remove yaml font matter
  text <- readLines(path, warn = FALSE, encoding = 'UTF-8')
  parts <- partition_yaml_front_matter(text)
  if(length(parts$front_matter)){
    yaml_fields <- paste(yaml_fields, collapse = "|")
    has_field <- grepl(paste0("^\\s*(",yaml_fields, ")"), parts$front_matter, ignore.case = TRUE)
    text[which(!has_field)] <- ""
  }

  # ignore lines containing <!-- no-spell-check -->
  text <- gsub(".*<!-- spell-check: ignore -->.*", "", text)
  # ignore sections between <!-- no-spell-check-start --> and <!-- no-spell-check-end -->
  start_ignore <- grep("^<!-- spell-check: ignore:start -->$", text)
  end_ignore <- grep("^<!-- spell-check: ignore:end -->$", text)
  if (
    length(start_ignore) != length(end_ignore) || # equal start and end tags
    any(start_ignore >= end_ignore) || # no start tag before and end tag
    any(tail(start_ignore, -1) <= head(end_ignore, -1)) # the next start tag must be after the current end tag
  ) {
    stop("unmatching ^<!-- spell-check: ignore:start --> and ^<!-- spell-check: ignore:end --> detected in ", path)
  }
  for (i in rev(seq_along(start_ignore))) {
    text <- text[-start_ignore[i]:-end_ignore[i]]
  }

  # Get markdown AST as xml doc
  md <- commonmark::markdown_xml(text, sourcepos = TRUE, extensions = extensions)
  doc <- xml2::xml_ns_strip(xml2::read_xml(md))

  # Find text nodes and their location in the markdown source doc
  sourcepos_nodes <-  xml2::xml_find_all(doc, "//*[@sourcepos][text]")
  sourcepos <- xml2::xml_attr(sourcepos_nodes, "sourcepos")
  values <- vapply(sourcepos_nodes, function(x) {
    paste0(collapse = "\n", xml2::xml_text(xml2::xml_find_all(x, "./text")))
  }, character(1))

  # Strip 'heading identifiers', see: https://pandoc.org/MANUAL.html#heading-identifiers
  values <- gsub('\\{#[^\\n]+\\}\\s*($|\\r?\\n)', '\\1', values, perl = TRUE)

  # Strip bookdown text references, see: https://bookdown.org/yihui/bookdown/markdown-extensions-by-bookdown.html#text-references
  values <- gsub("\\(ref:.*?\\)", "", values)
  # Strip stand-alone math formulas
  values <- gsub("\\$\\$.*?\\$\\$", "", values)
  # Strip inline math formulas
  values <- gsub("\\$.*?\\$", "", values)

  data.frame(
    text = values,
    position = sourcepos,
    stringsAsFactors = FALSE
  )
}
