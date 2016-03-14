
#' @export
caption <- function(message, f_input, f_output = NULL, position = 0.1, orientation = "portlait", font_color = "orange", font_size = NULL){
  ext <- tools::file_ext(f_input)
  if(grepl("^http:|^https:", f_input)){
    tmp <- tempfile()
    download.file(f_input, tmp, mode = "wb")
    f_input <- tmp
  }
  f_output <- ifelse(is.null(f_output), paste0("output.", ext), f_output)
  if(ext %in% c("jpg", "jpeg", "JPG", "JPEG")){
    img <- jpeg::readJPEG(f_input)
  }else if(ext %in% c("png","PNG")){
    img <- png::readPNG(f_input)
  } else {
    stop("png or jpeg")
  }
  if(ext %in% c("jpg", "jpeg", "JPG", "JPEG")){
    jpeg(f_output, width=dim(img)[2], height=dim(img)[1])
  }else if(ext %in% c("png","PNG")){
    png(f_output, width=dim(img)[2], height=dim(img)[1])
  } else {
    stop("png or jpeg")
  }
  if(Sys.info()['sysname'] == "Darwin"){
    par(family="HiraMaruProN-W4")
  }
  par(mar = c(0,0,0,0), xpd = NA, mgp = c(0,0,0), oma = c(0,0,0,0), ann = FALSE)
  plot.new()
  plot.window(0:1, 0:1)
  usr <- par("usr")
  rasterImage(img, usr[1], usr[3], usr[2], usr[4])
  font_size <- ifelse(is.null(font_size), trunc(dim(img)[2]/120), font_size)
  text(0.5, position, message, cex = font_size, col = "white")
  dev.off()
}

#' @export
sato <- function(f = NULL){
  f_sato <- ifelse(is.null(f),
              system.file("data/ojisan.png", package = "caption"),
              f
  )
  caption(msg_sato, f_sato)
}
