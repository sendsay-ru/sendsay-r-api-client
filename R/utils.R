sendsay_send_request <- function(..., .session=NULL, .host=NULL, .redirect=NULL) {
  req <- sendsay_build_request(params = list(...), .session=.session, .host=.host, .redirect=.redirect)

  res <- POST(url = req$url, body = req$body)

  res <- fromJSON(content(res, "text"))

  return (res)
}

sendsay_build_request <- function(params = list(), .session=NULL, .host=NULL, .redirect=NULL) {
  result <- list(params = params, session=.session, host=.host, redirect=.redirect)

  result <- sendsay_set_body(result)
  result <- sendsay_set_url(result)

  result[c("url", "body")]
}

sendsay_set_body <- function(request) {
  if (length(request$params) == 0L) return (request)

  if (request$params$action != "ping" && request$params$action != "login") {
    request$params <- append(request$params, list(session=request$.session))
  }

  request$body <- URLencode(paste("json=1&apiversion=100&request=", toJSON(request$params, auto_unbox = TRUE), sep=""))

  return (request)
}

sendsay_set_url <- function(request) {
  if (is.null(request$redirect)) {
    request$url <- URLencode(request$host)
  } else {
    request$url <- URLencode(paste(request$host, request$redirect, sep=""))
  }

  return (request)
}
