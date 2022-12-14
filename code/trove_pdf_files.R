library(tidyverse)
library(purrr)

pdf_urls_local <- read_csv("data/pdf_links_trove.csv", trim_ws = T) %>%
  pull(pdf_links_trove)


dat <- list()

safely_extract_all_pdf <- safely(.f = extract_details_from_tove_pdf,
                                 otherwise = NULL)

for (i in 1:length(pdf_file_link_read)) {

  rand_wait <- runif(n = 1, min = 1, max = 2) %>%
    round(digits = 2)

  Sys.sleep(rand_wait)

  dat[[i]] <- safely_extract_all_pdf(pdf_urls_local[i]) %>%
    pluck('result')



}


