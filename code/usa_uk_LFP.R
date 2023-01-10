dat <- readxl::read_excel("data/a05sanov2022.xls", range = "A9:I374")

names(dat) <- c("date_long", "employment_level", "employment_rate",
                "unemployment_level", "unemployment_rate", "activity_level",
                "activity_rate", "inactivity_level", "inactivity_rate")

transform_ons_participation_rate <- function(.data, age = "16 and over") {

  dat2 <- .data %>%
    mutate(
      date = paste("01-", str_remove(str_extract(date_long, "-[A-Z][a-z]+"), pattern = "-"), "-" , str_extract(date_long, "[0-9]+"), sep = "" )
    ) %>%
    mutate(
      date2 = as.Date(.data$date, format = "%d-%b-%Y")
    ) %>%
    mutate(age_group = )

}


dat_usa <- read_csv( file = "usa_participation_rate.csv") %>%
  mutate(
    age_group = c(rep("25-54", 480), rep("25-54", 480))
  )
