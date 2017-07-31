.MAX_ATTEMTS=5
.SENDSAY_DEFAULT_HOST="https://api.sendsay.ru"

#' Sendsay API
#'
#' Minimal wrapper to access Sendsay's API.
#'
#' @docType package
#' @name sendsay

NULL

#' Query the Sendsay API
#'
#' This is an extremely minimal client. You need to know the API
#' to be able to use this client.
#'
#' to to to
#'
#' @param ... API request parameters
#' @param .session - session
#' @param .host - host
#' @return response for the API request
#'
#' @examples
#' sendsay(action="ping")
#' sendsay(action="login", login="demo", passwd="demo")
#' sendsay(action="login", login="demo", passwd="demo", .host="https://api.sendsay.ru")
#' sendsay(action="ping", .host="https://api.sendsay.ru")
#' sendsay(action="pong", .host="https://api.sendsay.ru", .session="6d47ca72-e09c-42b5-b2bb-2dc88843adb1")
#'
#' @importFrom httr POST content
#' @importFrom jsonlite fromJSON toJSON
#' @importFrom utils URLencode
#'
#' @export
#'

sendsay <- function(..., .session=NULL, .host=.SENDSAY_DEFAULT_HOST) {
  res <- sendsay_send_request(..., session=.session, .host=.host)

  attemts <- 0

  while (!is.null(res$REDIRECT) && attemts < .MAX_ATTEMTS) {
    res <- sendsay_send_request(..., session=.session, .host=.host, .redirect=res$REDIRECT)

    attemts <- attemts + 1
  }

  return (res)
}
