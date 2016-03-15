#' @export
autocaption <- function(f_input, f_output = NULL, font_family = NULL, score = FALSE, CROWD_VISION_KEY, AZURE_CLIENT_ID, AZURE_CLIENT_SECRET){
  require("translateR")
  require("httr")
  res_json <- getCrowdvisionResult(f_input = f_input,
                                   type = "LABEL_DETECTION",
                                   maxResults = 10,
                                   CROWD_VISION_KEY = CROWD_VISION_KEY)
  res <- jsonlite::fromJSON(content(res_json, as = "text"))$responses$labelAnnotations
  num <- sample(length(res[[1]]$description), 1)
  name_en <- res[[1]]$description[num]
  score <- trunc(res[[1]]$score[num] * 100)
  suppressWarnings(name_ja <- translate(content.vec = name_en,
                                        microsoft.client.id = client_id,
                                        microsoft.client.secret = client_secret,
                                        source.lang = "en",
                                        target.lang = "ja")
  )
  msg <- iconv(Nippon::kakasi(name_ja,"-JH"), from = "UTF-8")
  msg <- ifelse(score, paste(msg, " ", score, "“"), msg)
  caption(message = msg, f_input = f_input, f_output = f_output, font_family = font_family)
  message("DE-KI-TA-YO!!!")
}


#' @export
getCrowdvisionResult <- function(f_input, type = "TEXT_DETECTION", maxResults = 10, CROWD_VISION_KEY){
  u <- paste0("https://vision.googleapis.com/v1/images:annotate?key=", CROWD_VISION_KEY)
  img <- readBin(f_input, "raw", file.info(f_input)[1, "size"])
  base64_encoded <- base64enc::base64encode(img)
  body <- list(requests = list(image = list(content = base64_encoded),
                               features = list(type = type,
                                               maxResults = maxResults))
  )
  res <- POST(url = u,
              encode = "json",
              body = body,
              content_type_json())
  return(res)
}
