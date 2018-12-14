#' Text Parsers
#'
#' Parse text from various formats and return a data frame with text lines
#' and position in the source document.
#'
#' @rdname parse_text
#' @name parse_text
#' @param path markdown file
#' @param extensions render markdown extensions? Passed to [commonmark][commonmark::markdown_xml]
parse_text_md <- function(path, extensions = TRUE){
  # Get markdown AST as xml doc
  md <- commonmark::markdown_xml(readLines(path, warn = FALSE, encoding = 'UTF-8'),
                                 sourcepos = TRUE, extensions = extensions)
  doc <- xml2::xml_ns_strip(xml2::read_xml(md))

  # Find text nodes and their location in the markdown source doc
  sourcepos_nodes <-  xml2::xml_find_all(doc, "//*[@sourcepos][text]")
  sourcepos <- xml2::xml_attr(sourcepos_nodes, "sourcepos")
  values <- vapply(sourcepos_nodes, function(x) {
    paste0(collapse = "\n", xml2::xml_text(xml2::xml_find_all(x, "./text")))
  }, character(1))

  data.frame(
    text = values,
    position = sourcepos,
    stringsAsFactors = FALSE
  )
}

