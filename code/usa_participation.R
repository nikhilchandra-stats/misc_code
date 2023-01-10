
available_indicators <- get_oecd_avlble_indctors_DPLIVE()

LFP_based_indicators <- available_indicators %>%
  filter(str_detect(indicator_desc, "Labour force participation rate")) %>%
  pull(INDICATOR)

dat <- get_oecd_data_DP_LIVE(
  indicators = LFP_based_indicators,
  countries = c("USA"),
  start_time = 2000,
  end_time = 2021
)

usa_25_54 <- niksmacrohelpers::get_bls_2000_to_now(series = "LNS11300060Q")
usa_55_over <- niksmacrohelpers::get_bls_2000_to_now(series = "LNS11324230Q")
usa_16_24 <- niksmacrohelpers::get_bls_2000_to_now(series = "LNS11324887Q")

complete_dat <- usa_25_54 %>%
  mutate(age = "25-54")
  bind_rows(usa_55_over %>%
              mutate(age = "55+")) %>%
  bind_rows(usa_16_24 %>%
              mutate(age = "16-24"))

write.csv(complete_dat %>%
            select(-footnotes, -latest), file = "usa_participation_rate.csv", row.names = F)
