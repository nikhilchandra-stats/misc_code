cah::load_custom_functions()
dat <- readxl::read_excel("data/GM1.xlsx", sheet = 4, skip = 3)

states <- dat %>% distinct(`State and territory (STT): ASGS (2011)`)
age <- dat %>% distinct(Age)
sex <- dat %>% distinct(Sex)
flows <- dat %>% distinct(`Labour force status - previous month`)


dat_national <- dat %>%
  group_by(Month,
           `Labour force status - current month`,
           `Labour force status - previous month`) %>%
  summarise(`Persons - current month ('000)` = sum(`Persons - current month ('000)`, na.rm = T)) %>%
  ungroup() %>%
  mutate(
    `Labour force status - current month` = ifelse(`Labour force status - current month` %in%
      c("Employed full-time", "Employed part-time"), "Employed", `Labour force status - current month`),
    `Labour force status - previous month` = ifelse(`Labour force status - previous month` %in%
                                                      c("Employed full-time", "Employed part-time"), "Employed", `Labour force status - previous month`)
  ) %>%
  group_by(Month,
           `Labour force status - current month`,
           `Labour force status - previous month`) %>%
  summarise(
    `Persons - current month ('000)` = sum(`Persons - current month ('000)`, na.rm = T)
  ) %>%
  mutate(
    flow_group =
      case_when(
        `Labour force status - previous month` %in%
          c("Employed", "Employed") &
          `Labour force status - current month` == "Not in the labour force (NILF)" ~ "Employment to NILF",
        `Labour force status - previous month` %in%
          c("Employed", "Employed") &
          `Labour force status - current month` == "Unemployed" ~ "Employed to Unemployed",

        `Labour force status - previous month` %in%
          c("Not in the labour force (NILF)") &
          `Labour force status - current month` %in%
          c("Employed", "Employed") ~ "NILF to Employed",

        `Labour force status - previous month` == "Unemployed" &
          `Labour force status - current month` %in%
          c("Employed", "Employed") ~ "Unemployed to Employed",


      )
  ) %>%
  group_by(flow_group) %>%
  arrange(Month, .by_group = TRUE) %>%
  mutate(ma = slider::slide_dbl(.before = 12, .f = ~ mean(., na.rm = T), .x = `Persons - current month ('000)` )) %>%
  filter(!is.na(flow_group))

write.csv(dat_national, file = "raw_flow_data.csv", row.names = F)

p1 <- dat_national %>%
  filter(!is.na(flow_group) & flow_group == "NILF to Employed") %>%
  filter(Month > "2005-12-01") %>%
  cah::cah_plot(aes(x = Month, y = ma)) +
  geom_line() +
  geom_smooth(se = FALSE, color = "red", linetype = "dashed") +
  theme_minimal() +
  theme(legend.position = "bottom" , legend.title = element_blank(), axis.title.x = element_blank())

p1 <- p1 %>%
  cah::ch_format_treasury(ylab_1 = "12 month rolling average of persons\nmoving from NILF to employment (000's)", ylab_2 = "12 month rolling average of persons\nmoving from NILF to employment (000's)")


p1

p2 <- dat_national %>%
  filter(!is.na(flow_group) & flow_group == "Unemployed to Employed")%>%
  filter(Month > "2005-12-01") %>%
  cah::cah_plot(aes(x = Month, y = ma)) +
  geom_line() +
  geom_smooth(se = FALSE, color = "red",  linetype = "dashed") +
  theme_minimal() +
  theme(legend.position = "bottom" , legend.title = element_blank() , axis.title.x = element_blank())
p2 <- p2 %>%
  cah::ch_format_treasury(ylab_1 = "12 month rolling average of persons\nmoving from unemployment to employment (000's)", ylab_2 = "12 month rolling average of persons\nmoving from unemployment to employment (000's)")

p2

p3 <- dat_national %>%
  filter(!is.na(flow_group) & flow_group == "Employment to NILF")%>%
  filter(Month > "2005-12-01") %>%
  cah::cah_plot(aes(x = Month, y = ma)) +
  geom_line() +
  geom_smooth(se = FALSE,  color = "red" , linetype = "dashed") +
  theme_minimal() +
  theme(legend.position = "bottom",legend.title = element_blank() , axis.title.x = element_blank())
p3 <- p3 %>%
  cah::ch_format_treasury(ylab_1 = "12 month rolling average of persons\nmoving from employment to NILF (000's)", ylab_2 = "12 month rolling average of persons\nmoving from employment to NILF (000's)")

p3

p4 <- dat_national %>%
  filter(!is.na(flow_group) & flow_group == "Employed to Unemployed")%>%
  filter(Month > "2005-12-01") %>%
  cah::cah_plot(aes(x = Month, y = ma)) +
  geom_line() +
  geom_smooth(se = FALSE, color = "red", linetype = "dashed") +
  theme_minimal() +
  theme(legend.position = "bottom" , legend.title = element_blank() , axis.title.x = element_blank())
p4 <- p4 %>%
  cah::ch_format_treasury(ylab_1 = "12 month rolling average of persons\nmoving from employed to unemployed (000's)", ylab_2 = "12 month rolling average of persons\nmoving from employed to unemployed (000's)")

p4

cah::cah_save(p1, filename = "NILF to employment_12month.png", height = 6.5, width = 14)
cah::cah_save(p2, filename = "unemployment to employment_12month.png", height = 6.5, width = 14)

cah::cah_save(p3, filename = "employment to NILF_12month.png", height = 6.5, width = 14)
cah::cah_save(p4, filename = "employed to unemployed_12month.png", height = 6.5, width = 14)


#------------------------------------------------By Sex


states <- dat %>% distinct(`State and territory (STT): ASGS (2011)`)
age <- dat %>% distinct(Age)
sex <- dat %>% distinct(Sex)
flows <- dat %>% distinct(`Labour force status - previous month`)


dat_national <- dat %>%
  group_by(Month,
           Sex,
           `Labour force status - current month`,
           `Labour force status - previous month`) %>%
  summarise(`Persons - current month ('000)` = sum(`Persons - current month ('000)`, na.rm = T)) %>%
  ungroup() %>%
  mutate(
    flow_group =
      case_when(
        `Labour force status - previous month` %in%
          c("Employed full-time", "Employed part-time") &
          `Labour force status - current month` == "Not in the labour force (NILF)" ~ "Employment to NILF",
        `Labour force status - previous month` %in%
          c("Employed full-time", "Employed part-time") &
          `Labour force status - current month` == "Unemployed" ~ "Employed to Unemployed",

        `Labour force status - previous month` %in%
          c("Not in the labour force (NILF)") &
          `Labour force status - current month` %in%
          c("Employed full-time", "Employed part-time") ~ "NILF to Employed",

        `Labour force status - previous month` == "Unemployed" &
          `Labour force status - current month` %in%
          c("Employed full-time", "Employed part-time") ~ "Unemployed to Employed",


      )
  ) %>%
  group_by(flow_group, Sex) %>%
  arrange(Month, .by_group = TRUE) %>%
  mutate(ma = slider::slide_dbl(.before = 12, .f = ~ mean(., na.rm = T), .x = `Persons - current month ('000)` ))

p1 <- dat_national %>%
  filter(!is.na(flow_group) & flow_group == "NILF to Employed") %>%
  filter(Month > "2005-12-01") %>%
  cah::cah_plot(aes(x = Month, y = ma, color = Sex)) +
  geom_line() +
  geom_smooth(se = FALSE, linetype = "dashed") +
  theme_minimal() +
  theme(legend.position = "bottom" , legend.title = element_blank(), axis.title.x = element_blank())

p1 <- p1 %>%
  cah::ch_format_treasury(ylab_1 = "12 month rolling average of persons\nmoving from NILF to employment (000's)", ylab_2 = "12 month rolling average of persons\nmoving from NILF to employment (000's)")


p1

p2 <- dat_national %>%
  filter(!is.na(flow_group) & flow_group == "Unemployed to Employed")%>%
  filter(Month > "2005-12-01") %>%
  cah::cah_plot(aes(x = Month, y = ma , color = Sex)) +
  geom_line() +
  geom_smooth(se = FALSE, linetype = "dashed") +
  theme_minimal() +
  theme(legend.position = "bottom" , legend.title = element_blank() , axis.title.x = element_blank())
p2 <- p2 %>%
  cah::ch_format_treasury(ylab_1 = "12 month rolling average of persons\nmoving from unemployment to employment (000's)", ylab_2 = "12 month rolling average of persons\nmoving from unemployment to employment (000's)")

p2

p3 <- dat_national %>%
  filter(!is.na(flow_group) & flow_group == "Employment to NILF")%>%
  filter(Month > "2005-12-01") %>%
  cah::cah_plot(aes(x = Month, y = ma , color = Sex)) +
  geom_line() +
  geom_smooth(se = FALSE, linetype = "dashed") +
  theme_minimal() +
  theme(legend.position = "bottom", legend.title = element_blank() , axis.title.x = element_blank())
p3 <- p3 %>%
  cah::ch_format_treasury(ylab_1 = "12 month rolling average of persons\nmoving from employment to NILF (000's)", ylab_2 = "12 month rolling average of persons\nmoving from employment to NILF (000's)")

p3

p4 <- dat_national %>%
  filter(!is.na(flow_group) & flow_group == "Employed to Unemployed")%>%
  filter(Month > "2005-12-01") %>%
  cah::cah_plot(aes(x = Month, y = ma , color = Sex)) +
  geom_line() +
  geom_smooth(se = FALSE, linetype = "dashed") +
  theme_minimal() +
  theme(legend.position = "bottom" , legend.title = element_blank() , axis.title.x = element_blank())
p4 <- p4 %>%
  cah::ch_format_treasury(ylab_1 = "12 month rolling average of persons\nmoving from employed to unemployed (000's)", ylab_2 = "12 month rolling average of persons\nmoving from employed to unemployed (000's)")

p4

cah::cah_save(p1, filename = "NILF to employment_12month_sex.png", height = 6.5, width = 12)
cah::cah_save(p2, filename = "unemployment to employment_12month_sex.png", height = 6.5, width = 12)

cah::cah_save(p3, filename = "employment to NILF_12month_sex.png", height = 6.5, width = 12)
cah::cah_save(p4, filename = "employed to unemployed_12month_sex.png", height = 6.5, width = 12)
