library(cah)
library(tidyverse)
library(lubridate)
library(rvest)
library(RSelenium)

scrape_dt <- lubridate::now(tzone = "Australia/Sydney")
board <- cah::pin_azbo()

## Setup the driver and connection ----
os <- cah::get_cah_env()

#----------------------Detects if you are on pipeline or VM
local_testing <- ifelse(os == "vm", TRUE, FALSE)


# If on VM run local selenium driver using firefox
if(local_testing){

  driver <- rsDriver(browser = c("firefox"),
                     port = 4569L, chromever = "108.0.5359.71")

  remote_driver <- driver[["client"]]

  remote_driver$open()


}

# If on Pipeline run run the following to engage selenium
if(local_testing == FALSE){

  server_address = "selenium"

  server_port = 4444L

  remote_driver <- RSelenium::remoteDriver(remoteServerAddr = server_address,
                                           port = server_port,
                                           browserName = "chrome")
  remote_driver$open()

}


#------------------------------------------------------


#' Scrape PM Gov function
#'
#' @param xpath_x (character) Xpath for each box that contains the media information
#'
#' @return (tibble) Tibble of relevant information from each box contining a
#' media release from pm.gov
#' @export
#'
#' @examples
scrape_pm_gov <- function(xpath_x = xpath_seq$xpaths[1] %>% as.character()) {

  html_in <- remote_driver$getPageSource() %>%
    pluck(1) %>%
    xml2::read_html()

  Sys.sleep(1.5)

  classes <- html_in %>%
    rvest::html_element(xpath = xpath_x) %>%
    rvest::html_elements('div') %>%
    rvest::html_attr("class")

  media_item <-   html_in %>%
    rvest::html_element(xpath = xpath_x) %>%
    rvest::html_elements('div') %>%
    rvest::html_element(css = ".media-title" ) %>%
    html_text() %>%
    pluck(1) %>%
    as.character()

  media_blurb <-   html_in %>%
    rvest::html_element(xpath = xpath_x) %>%
    rvest::html_elements('div') %>%
    rvest::html_element(css = ".media-blurb" ) %>%
    html_text() %>%
    pluck(1) %>%
    as.character()

  media_date <-   html_in %>%
    rvest::html_element(xpath = xpath_x) %>%
    rvest::html_elements('div') %>%
    rvest::html_element(css = ".media-date" ) %>%
    html_text() %>%
    pluck(1) %>%
    as.character()

  media_type <-   html_in %>%
    rvest::html_element(xpath = xpath_x) %>%
    rvest::html_elements('div') %>%
    rvest::html_element(css = ".media-type" ) %>%
    html_text() %>%
    pluck(1) %>%
    as.character()

  media_body <-   html_in %>%
    rvest::html_element(xpath = xpath_x) %>%
    rvest::html_elements('div') %>%
    rvest::html_element(css = ".media-body" ) %>%
    html_text() %>%
    pluck(1) %>%
    as.character()

  media_link <- html_in %>%
    rvest::html_element(xpath = xpath_x) %>%
    rvest::html_elements('div') %>%
    rvest::html_element(css = ".media-title" ) %>%
    rvest::html_element("a") %>%
    rvest::html_attr('href') %>%
    pluck(1) %>%
    as.character()

  media_link <- glue::glue("https://www.pm.gov.au/{media_link}")

  returned_tibble <-
    tibble(
      media_title = media_item,
      media_body = media_body,
      media_date = media_date,
      media_type = media_type,
      media_link = media_link
    )

  return(returned_tibble)

}

# Navigate to Website
remote_driver$navigate("https://www.pm.gov.au/media")

# Create Xpath list. The Xpaths for this website are pretty simple. The media
# releases are all inside of a div structures that numerically increase based on
# their position on the webpage


xpath_seq <-
  dplyr::tibble(
    xpaths = rep("/html/body/div[3]/section/div[1]/div/div/div/div[5]/div/div/div/div[1]/div[xxxxxx]", 9),
    nums = seq(1,9)
  ) %>%
  mutate(
    xpaths = str_replace("/html/body/div[3]/section/div[1]/div/div/div/div[5]/div/div/div/div[1]/div[xxxxxx]",pattern = "xxxxxx", replacement =  as.character(nums))
  )

safely_scrape <- safely(scrape_pm_gov, otherwise = NULL)

data_list <- xpath_seq %>%
  pull(xpaths) %>%
  map(
    ~
      safely_scrape(xpath_x = .x %>% as.character())
  )

data_list_filtered <- data_list %>%
  map(
    ~ .x %>% pluck('result')
  ) %>%
  keep(~ !is.null(.x)) %>%
  reduce(bind_rows)


try(remote_driver$quit())
