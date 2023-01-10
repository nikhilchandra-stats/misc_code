library(tidyverse)
library(readr)
library(purrr)

working_age_unemployment <- readabs::read_abs("6202.0", tables = 18, check_local = FALSE)

employment <- working_age_unemployment %>%
  filter(date == ymd("2022-06-01"),
         series %in% c("Employed total ;  Persons ;",
                       "Unemployed total ;  Persons ;",
                       "Not in the labour force (NILF) ;  Persons ;"),
         series_type == "Original") %>%
  transmute(stat = case_when(series == "Employed total ;  Persons ;" ~ "Employed",
                             series == "Unemployed total ;  Persons ;" ~ "Unemployed",
                             series == "Not in the labour force (NILF) ;  Persons ;" ~ "Not in Labour Force"),
            value,
            pop = "General Population") %>%
  mutate(pct = value / sum(value)) %>%
  bind_rows(ndis_emp_summary) %>%
  mutate(employment = factor(stat,
                             levels = c("Employed", "Unemployed", "Not in Labour Force (but would like a job)", "Not in Labour Force"),
                             ordered = TRUE))

dat_uk <- readr::read_csv(fileshare("data/raw/ndis/international_disability_stats/uk_disability_employment.csv")) %>%
  select(-1) %>%
  rename(date = 5 ) %>%
  pivot_longer(-c(date,age), names_to = "cohort", values_to = "pct") %>%
  mutate(
    date = glue::glue("{date}-07-01") %>% lubridate::as_date()
  ) %>%
  mutate(pop2 =
           case_when(
             str_detect(cohort, "Disabled") ~ "People with \na disability (UK)",
             str_detect(cohort, "Non-disabled") ~ "People without \na disability (UK)",
           )
  ) %>%
  mutate(
    sex =
      case_when(
        str_detect(cohort, "men") ~ "male",
        str_detect(cohort, "women") ~ "female",
      )
  ) %>%
  mutate(pop =
           case_when(
             str_detect(cohort, "Disabled") ~ "People with \na disability",
             str_detect(cohort, "Non-disabled") ~ "People without \na disability",
           )
  ) %>%
  select(-cohort) %>%
  mutate(stat = "Employed", employment = stat) %>%
  mutate(
    pct = pct/100,
    country = "UK"
  )

dat_uk_summarised <- dat_uk %>%
  group_by(date, age, pop2, pop, stat, employment, country) %>%
  summarise(pct = mean(pct, na.rm = T), sex = "total") %>%
  ungroup()

dat_nz <- read.csv(fileshare("data/raw/ndis/international_disability_stats/nz_disability_employment.csv")) %>%
  rename(
    pop = disability_status,
    sex = X,
    age = age.group
  ) %>%
  pivot_longer(-c(date, pop, age, sex), names_to = "stat", values_to = "pct") %>%
  mutate(pct = as.numeric(pct)) %>%
  mutate(
    stat = str_replace_all(stat,pattern =  "\\.", " ")
  ) %>%
  mutate(
    stat = case_when(
      stat == "Employment rate" ~ "Employed",
      stat == "Unemployment rate" ~ "Unemployed",
      TRUE ~ stat
    )
  ) %>%
  mutate(
    pop2 =
      case_when(
        pop == "disabled" ~ "People with \na disability (NZ)",
        pop == "non-disabled" ~ "People without \na disability (NZ)",
      )
  ) %>%
  mutate(
    pop = case_when(
      pop == "disabled" ~ "People with \na disability",
      pop == "non-disabled" ~ "People without \na disability",
    )
  ) %>%
  mutate(
    country = "New Zealand"
  ) %>%
  mutate(
    pct = pct/100
  ) %>%
  mutate(employment = stat) %>%
  mutate(date = lubridate::as_date(date))

aus_data1 <- employment %>%
  filter(!str_detect(stat, "Not in Labour Force")) %>%
  group_by(pop) %>%
  summarise(pct = sum(pct, na.rm = T),
            stat = "Labour force participation rate",
            employment = "Labour force participation rate"
  ) %>%
  bind_rows(employment) %>%
  mutate(country = "Australia") %>%
  mutate(pop2 = pop) %>%
  mutate(
    pop =
      case_when(
        pop2 == "NDIS Participants" ~ "People with \na disability",
        pop2 == "General Population" ~ "People without \na disability"
      )
  ) %>%
  mutate(sex = "total")

au_nz_combined <- dat_nz %>%
  bind_rows(aus_data1) %>%
  bind_rows(dat_uk_summarised) %>%
  fill(date, .direction = "down")
