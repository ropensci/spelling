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
parse_text_md <- function(path, extensions = TRUE, yaml_fields = c("title" ,"subtitle", "description")){
  # Read file and remove yaml font matter
  text <- readLines(path, warn = FALSE, encoding = 'UTF-8')
  parts <- partition_yaml_front_matter(text)
  if(length(parts$front_matter)){
    yaml_fields <- paste(yaml_fields, collapse = "|")
    has_field <- grepl(paste0("^\\s*(",yaml_fields, ")"), parts$front_matter, ignore.case = TRUE)
    text[which(!has_field)] <- ""
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

  data.frame(
    text = values,
    position = sourcepos,
    stringsAsFactors = FALSE
  )
}
