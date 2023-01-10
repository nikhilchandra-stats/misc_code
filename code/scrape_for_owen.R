library(tidyverse)
library(readr)
library(purrr)

all_name_string_options <- c(
  "[A-Z][a-z]+ [A-Z][a-z]+: [0-9]+-[0-9]+",
  "[A-Z][a-z]+ [A-Z][a-z]+; [0-9]+-[0-9]+",
  "[A-Z][a-z]+ [A-Z][a-z]+. [0-9]+-[0-9]+",
  "[A-Z][a-z][A-Z][a-z]+ [A-Z][a-z][A-Z][a-z]+: [0-9]+-[0-9]+",
  "[A-Z][a-z][A-Z][a-z]+ [A-Z][a-z][A-Z][a-z]+; [0-9]+-[0-9]+",
  "[A-Z][a-z][A-Z][a-z]+ [A-Z][a-z][A-Z][a-z]+. [0-9]+-[0-9]+"
)

all_name_string_options <- all_name_string_options %>%
  paste(collapse = "|")

link_treasury <- "https://trove.nla.gov.au/newspaper/rendition/nla.news-article244669978.txt"
link_finance <- "https://trove.nla.gov.au/newspaper/rendition/nla.news-article244669896.txt"

dat <- xml2::read_html("https://trove.nla.gov.au/newspaper/rendition/nla.news-article244669896.txt") %>%
  rvest::html_text2() %>%
  as.character() %>%
  str_split(pattern = "\n\nN.N", n = Inf) %>%
  pluck(1) %>%
  as_tibble() %>%
  mutate(
    value = str_remove_all(value, "\\!")
  ) %>%
  mutate(applicant_name =
           str_extract(value, "[A-Z][a-zA-Z]+ [A-Z][a-zA-Z]+: [0-9]+-[0-9]+|[A-Z][a-zA-Z]+ [A-Z][a-zA-Z]+; [0-9]+-[0-9]+|[A-Z][a-zA-Z]+ [A-Z][a-zA-Z]+. [0-9]+-[0-9]+")) %>%
  mutate(applicant_name =
           case_when(
             is.na(applicant_name) ~ str_extract(value, "[A-Z][a-zA-Z]+ [A-Z][a-zA-Z]+: [0-9]+-[0-9]+|[A-Z][a-zA-Z]+ [A-Z][a-zA-Z]+; [0-9]+-[0-9]+|[A-Z][a-zA-Z]+ [A-Z][a-zA-Z]+. [0-9]+-[0-9]+"),
             TRUE ~ applicant_name
           )) %>%
  mutate(
    ags_number =
      str_extract(applicant_name, ": [0-9]+-[0-9]+|; [0-9]+-[0-9]+|. [0-9]+-[0-9]+"),
    applicant_name = str_remove_all(applicant_name, ": [0-9]+-[0-9]+|; [0-9]+-[0-9]+|. [0-9]+-[0-9]+"),
  ) %>%
  mutate(across(c(ags_number,applicant_name), .fns = ~ str_remove_all(., ":|;"))) %>%
  mutate(salary = str_extract(value, "\\$[0-9]+-[0-9]+,|5[0-9]+-[0-9]+,|S[0-9]+-[0-9]+,") %>%
           str_remove_all("[a-zA-Z]|$|,")) %>%
  mutate(
    salary =
      case_when(
        str_detect(salary, "5[0-9]+-[0-9]+") & (str_detect(salary, "[a-zA-Z]+ 5")|str_detect(salary, "[0-9] 5")) ~ str_remove(salary,"5"),
        TRUE ~ salary
      )
  ) %>%
  mutate(
    level = str_extract(value, "APS[0-9]|APS Level [0-9]|APS level [0-9]")
  ) %>%
  mutate(value =
           case_when(
             str_detect(value, "[a-zA-Z]\\^") ~ str_replace(value, pattern = "\\^", replacement = "e"),
             TRUE ~ value
           )) %>%
  mutate(
    level =
      case_when(
        is.na(level) & str_detect(value, "EL2|Director|director|Executive (?i)Level II|Executive (?i)Level 2") ~ "EL2",
        is.na(level) & str_detect(value, "EL1|Assistant Director|Assistant director|assistant director|Executive (?i)Level I|Executive (?i)Level 1|Executive level 1") ~ "EL1",
        TRUE ~ level
      )
  ) %>%
  mutate(department =
           case_when(
             str_detect(value, "Finance") & !str_detect(value, "ELECTORAL|Electoral|electoral") ~ "Department of Finance",
             str_detect(value, "ELECTORAL|Electoral|electoral") ~ "AEC",
             str_detect(value, "Treasury") ~ "Treasury",
             str_detect(value, "Prime Minister|PMC|PM&C") ~ "PMC",
           )) %>%
  fill(department, .direction = "down") %>%
  rename(raw_text_row = value)

write.csv(dat, file = "scraped_data.csv", row.names = FALSE)

