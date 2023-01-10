dates_x <-
  c(
    "jan_08_2005",
    "feb_03_2005",
    "mar_03_2005",
    "apr_03_2005",
    "may_05_2005",
    "jun_02_2005",
    "jul_07_2005",
    "aug_05_2005",
    "sep_01_2005",
    "oct_06_2005",
    "nov_03_2005",
    "dec_01_2005"

  )

article_dates <-
  c(
    244669748,
    244670132,
    244670500,
    248292377,
    248292786,
    244713492,
    244713976,
    244708351,
    244708783,
    244709335,
    244714758,
    244715184

  ) %>%
  unique()

pages <- c(
  158,
  832 - 679,
  1564 - 1369,
  2384 - 2215,
  4070 - 3873,
  4792 - 4629,
  5660 - 5477,
  6414 - 6217,
  7236 - 7049,
  8166 - 8007,
  8982 - 8779,
  9822 - 9599
)


x_tibble <-
  tibble(
    dates_x = dates_x,
    url_replace = article_dates,
    pages = pages
  )

url_base <- "https://trove.nla.gov.au/newspaper/rendition/nla.news-articlexxxxxxxxx.txt"
replace_val <- "xxxxxxxxx"

extract_func <- function(url_to_use) {


  dat <- xml2::read_html(url_to_use) %>%
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

  return(dat)


}

produce_article_ids <-
  x_tibble %>%
  split(.$dates_x) %>%
  map_dfr(
    ~ tibble(
      dates_x = rep(.x[1,1] %>% as.character(), .x[1,3] %>% as.numeric()),
      url_replace  = rep(.x[1,2] %>% as.numeric(), .x[1,3] %>% as.numeric()),
      pages = seq(.x[1,2] %>% as.numeric(), .x[1,2] %>% as.numeric() + .x[1,3] %>% as.numeric() - 1, 1)
    )
  )

dat_comp_list <- list()

extract_safely <- safely(extract_func, otherwise = NULL)

c = 0

for (j in 1:dim(x_tibble)[1]) {

  pages_x <- 1:x_tibble$pages[j]
  url_num <- x_tibble$url_replace[j]
  dat_current <- x_tibble$dates_x[j]

  for (i in pages_x) {

    c = c + 1
    Sys.sleep(1)

    article_num <- (url_num + i) %>% as.character()
    url_replacement <- url_base %>%
      str_replace(pattern = "xxxxxxxxx", replacement = article_num)

    dat_x <- extract_safely(url_to_use = url_replacement) %>%
      pluck("result")

    if(any(class(dat_x) == "tbl_df")) {

      dat_x <- dat_x %>%
        mutate(date = dat_current)

    }

    dat_comp_list[[c]] <- dat_x

  }

}

complete_dat <- dat_comp_list %>%
  keep(~ !is.null(.x)) %>%
  map_dfr(bind_rows) %>%
  filter(!is.na(ags_number))

