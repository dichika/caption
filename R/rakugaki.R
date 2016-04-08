#' @export
rakugaki <- function(imageurl){
  require("shiny")
  require("shinygadgets")
  # image
  path_template <- system.file("data/template.html", package = "caption")
  tmp <- tempfile()
  if(grepl(c("^http://|^https://"), imageurl)){
    download.file(url = imageurl,
                  destfile = tmp,
                  quiet = TRUE)
  }else{
    tmp <- imageurl
  }
  if(grepl("png$",imageurl)){
    img <- png::readPNG(tmp)
  } else if(grepl("jpeg$|jpg$",imageurl)){
    img <- jpeg::readJPEG(tmp)
  }
  h <- as.character(dim(img)[1])
  w <- as.character(dim(img)[2])

  htmltext <- readLines(path_template)
  htmltext <- paste(htmltext, collapse = "")
  imageurl <- base64enc::dataURI(file=imageurl)
  # get image size
  tmp <- tempfile()
  writeLines(sprintf(htmltext, w, h, imageurl),tmp)
  # shiny
  ui <- htmlTemplate(tmp)
  server <- function(input, output, session) {}
  runGadget(ui, server)
}
