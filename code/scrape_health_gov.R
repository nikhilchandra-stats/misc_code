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

#' Scrape health Gov function
#'
#' @param xpath_x (character) Xpath for each box that contains the media information
#'
#' @return (tibble) Tibble of relevant information from each box contining a
#' media release from pm.gov
#' @export
#'
#' @examples
scrape_health_gov <- function(xpath_x = xpath_seq$xpaths[1] %>% as.character()) {

  Sys.sleep(1.5)

  html_in <- remote_driver$getPageSource() %>%
    pluck(1) %>%
    xml2::read_html()

  main_class <- html_in %>%
    rvest::html_element(xpath = xpath_x) %>%
    rvest::html_elements('div') %>%
    rvest::html_elements(".col-xs-10")

  div_items <- main_class %>%
    rvest::html_elements('div') %>%
    rvest::html_elements('div') %>%
    html_text2() %>%
    unique()

  media_date <- div_items[1] %>%
    as.character()
  media_type <- div_items[2] %>%
    as.character()
  media_body <- div_items[3] %>%
    as.character()

  media_link <- main_class %>%
    rvest::html_elements('a') %>%
    rvest::html_attr("href") %>%
    as.character()

  media_title <- main_class %>%
    rvest::html_elements('a') %>%
    rvest::html_text2() %>%
    pluck(1) %>%
    as.character()

  media_link <- glue::glue("https://www.health.gov.au{media_link}")

  returned_tibble <-
    tibble(
      media_title = media_title,
      media_body = media_body,
      media_date = media_date,
      media_type = media_type,
      media_link = media_link
    )

  return(returned_tibble)

}

# Navigate to Website
remote_driver$navigate("https://www.health.gov.au/ministers/media-centre")

# Create Xpath list. The Xpaths for this website are pretty simple. The media
# releases are all inside of a div structures that numerically increase based on
# their position on the webpage

xpath_seq <-
  dplyr::tibble(
    xpaths = rep("/html/body/div[1]/div/div[4]/div/div/main/div[1]/div/article/div[1]/div/div[2]/div[2]/div/ul/li[xxxxxx]", 25),
    nums = seq(1,25)
  ) %>%
  mutate(
    xpaths = str_replace("/html/body/div[1]/div/div[4]/div/div/main/div[1]/div/article/div[1]/div/div[2]/div[2]/div/ul/li[xxxxxx]",pattern = "xxxxxx", replacement =  as.character(nums))
  )

safely_scrape <- safely(scrape_health_gov, otherwise = NULL)

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
