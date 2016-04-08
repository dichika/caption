#' @export
rakugaki <- function(imageurl){
  require("shiny")
  require("shinygadgets")
  # image
  path_template <- system.file("data/template.html", package = "caption")
  htmltext <- readLines(path_template)
  imageurl <- base64enc::dataURI(file=imageurl)
  tmp <- tempfile()
  writeLines(sprintf(htmltext, imageurl), tmp)
  # shiny
  ui <- htmlTemplate(tmp)
  server <- function(input, output, session) {}
  runGadget(ui, server)
}
