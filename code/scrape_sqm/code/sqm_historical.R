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

  driver <- rsDriver(browser = c("firefox"),
                     port = 4570L, chromever = "108.0.5359.71")

  remote_driver <- driver[["client"]]

  remote_driver$open()

  driver$server$output()


}

#CHANGE THIS TO "vacancy" TO MAKE IT VACANCY AND SET IT TO "rent" FOR RENT
vacancy_or_rent <- "vacancy"

#START DATE OF DATA. FOR RENT SET TO "2009-08-01" or "2005-01-01" FOR VACANCY
start_date_of_data <- "2005-08-01"

#Xpaths
all_houses <- "/html/body/div/div/div/div/div/div[1]/div/svg/g[5]/g[1]/path[1]"
three_bed_house <- "/html/body/div/div/div/div/div/div[1]/div/svg/g[5]/g[3]/path[1]"
all_units <- "/html/body/div/div/div/div/div/div[1]/div/svg/g[5]/g[5]/path[1]"
combined <- "/html/body/div/div/div/div/div/div[1]/div/svg/g[5]/g[9]/path[1]"
vacancy_rates <- "/html/body/div/div/div/div/div/div[1]/div/svg/g[7]/g[3]"

#SET THIS TO ONE OF THE XPATH VALUE ABOVE
chosen_metric <-
  ifelse(vacancy_or_rent == "rent", combined, vacancy_rates)

variable_name <-
  ifelse(chosen_metric != vacancy_rates,"median_weekly_rent", "vacancy_rate")

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
  filter(str_detect(value, "[0-9][0-9][0-9][0-9]|2[0-9][0-9][0-9]|6[0-9][0-9][0-9]")) %>%
  pull(value)

for (i in 1:length(all_post_codes)) {

  postcode = all_post_codes[i]

  random_wait_time <- runif(n = 1, min = 6, max = 20) %>% round()

  Sys.sleep(random_wait_time)

  if(vacancy_or_rent == "rent") {

    url <- glue::glue("https://sqmresearch.com.au/weekly-rents.php?postcode={postcode}&t=1") %>% as.character()

  }

  if(vacancy_or_rent == "vacancy") {

    url <- glue::glue("https://sqmresearch.com.au/graph_vacancy.php?postcode={postcode}&t=1") %>% as.character()

  }

  remote_driver$navigate(url)

  safely_extract <- safely(extract_chart_data_sqm, otherwise = NULL)

  xpath_y_trans <-
    ifelse(
      vacancy_or_rent == "vacancy",
      "/html/body/div/div/div/div/div/div[1]/div/svg/g[11]",
      "/html/body/div/div/div/div/div/div[1]/div/svg/g[8]"
    )

  dat <-
    safely_extract(
      page_source = remote_driver$getPageSource(),
      xpath_chart = chosen_metric,
      start_date = start_date_of_data,
      xpath_y = xpath_y_trans
    ) %>%
    pluck(1)

  if(any(class(dat) == "tbl_df") & !is.null(dat)){

    dat <- dat %>%
      rename(
        !!as.name(variable_name) := translated_y,
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




