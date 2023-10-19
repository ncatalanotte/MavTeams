.mavteams_env <- new.env(parent = emptyenv())
#' Set the Webhook URL for Sending Messages
#'
#' This function sets the webhook URL that will be used by the `sendMessage` function.
#' It must be called before attempting to send any messages.
#'
#' @param url A character string specifying the webhook URL.
#'
#' @return Invisible NULL. The function is called for its side effect of setting the URL.
#' @export
#' @examples
#' \dontrun{
#' setURL("https://your.webhook.url")
#' }
#' @export
setURL <- function(url) {
  .mavteams_env$.webhook_url <- url
}

#' Send a Message to the Configured Webhook URL
#'
#' This function sends a message to the webhook URL set by `setURL`.
#' The message is formatted as a MessageCard for compatibility with certain platforms.
#'
#' @param title A character string specifying the title of the message.
#' @param subtitle A character string specifying the subtitle of the message.
#' @param body A character string specifying the main body text of the message.
#'
#' @return A message indicating the success or failure of the message sending. 
#' If the message is sent successfully, it prints "Message sent successfully." 
#' Otherwise, it prints an error message with the status code.
#'
#' @export
#' @seealso \code{\link{setURL}} to set the webhook URL before sending messages.
#' @examples
#' \dontrun{
#' setURL("https://your.webhook.url")
#' sendMessage(title = "Test Title", subtitle = "Test Subtitle", body = "Test message body.")
#' }
sendMessage <- function(title, subtitle, body) {
  if (is.null(.mavteams_env$.webhook_url)) {
    stop("Webhook URL not set. Use setURL() first.")
  }
  
  payload <- jsonlite::toJSON(list(
    `@type` = "MessageCard",
    `@context` = "http://schema.org/extensions",
    summary = "Summary",
    sections = list(
      list(
        activityTitle = title,
        activitySubtitle = subtitle,
        text = body
      )
    )
  ), auto_unbox = TRUE)
  
  response <- httr::POST(
    url = .mavteams_env$.webhook_url,
    body = payload,
    encode = "json",
    httr::add_headers("Content-Type" = "application/json")
  )
  
  if (httr::status_code(response) == 200) {
    print("Message sent successfully.")
  } else {
    print(paste("Failed to send message. Status code:", httr::status_code(response)))
  }
}
