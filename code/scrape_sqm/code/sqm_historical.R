library(tidyverse)
library(lubridate)
library(rvest)
library(RSelenium)

helpeR::load_custom_functions()

scrape_dt <- lubridate::now(tzone = "Australia/Sydney")

#----------------------Detects if you are on pipeline or VM
local_testing <- TRUE


# If on VM run local selenium driver using firefox
if(local_testing){

  driver <- RSelenium::rsDriver(browser = "chrome", port = 2000L)

  remote_driver <- driver[["client"]]

  remote_driver$open()

  driver$server$output()


}

#Xpaths
all_houses <- "/html/body/div/div/div/div/div/div[1]/div/svg/g[5]/g[1]/path[1]"
three_bed_house <- "/html/body/div/div/div/div/div/div[1]/div/svg/g[5]/g[3]/path[1]"
all_units <- "/html/body/div/div/div/div/div/div[1]/div/svg/g[5]/g[5]/path[1]"
combined <- "/html/body/div/div/div/div/div/div[1]/div/svg/g[5]/g[9]/path[1]"

all_post_codes <- readxl::read_excel("C:/GitHub/maps data/POA/POA_2021_AUST.xlsx") %>%
  distinct(POA_CODE_2021) %>%
  pull(POA_CODE_2021)

exisiting_postcodes <- fs::dir_info("C:/GitHub/Rent Data") %>%
  select(path) %>%
  mutate(poa = str_extract(path,"ts_[0-9][0-9][0-9][0-9]") %>%
           str_remove("ts_")) %>%
  distinct(poa) %>%
  pull(poa)

all_post_codes <- setdiff(all_post_codes, exisiting_postcodes) %>%
  as_tibble() %>%
  filter(str_detect(value, "0[0-9][0-9][0-9]|2[0-9][0-9][0-9]|6[0-9][0-9][0-9]")) %>%
  pull(value)

for (i in 1:length(all_post_codes)) {

  postcode = all_post_codes[i]

  random_wait_time <- runif(n = 1, min = 6, max = 20) %>% round()

  Sys.sleep(random_wait_time)

  url <- glue::glue("https://sqmresearch.com.au/weekly-rents.php?postcode={postcode}&t=1") %>% as.character()

  remote_driver$navigate(url)

  safely_extract <- safely(extract_chart_data_sqm, otherwise = NULL)

  dat <-
    safely_extract(
      page_source = remote_driver$getPageSource(),
      xpath_chart = combined,
      start_date = "2009-08-01"
    ) %>%
    pluck(1)

  if(any(class(dat) == "tbl_df") & !is.null(dat)){

    dat <- dat %>%
      rename(
        median_weekly_rent = translated_y,
        approximate_date = translated_x
      ) %>%
      mutate(
        poa_code = postcode
      )

    write.csv(dat,glue::glue("C:/GitHub/Rent Data/rent_ts_{postcode}.csv"))

  }


  random_wait_time <- runif(n = 1, min = 3, max = 20) %>% round()

  Sys.sleep(random_wait_time)

}

try(remote_driver$quit())




