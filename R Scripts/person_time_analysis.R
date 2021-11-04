#Script to replicate CDPHE's person-time vaccine efficacy, code primarily written by CDPHE
#First authored 7/26/2021 by Nicole Harty
#Last update: 7/26/2021

library(dplyr)
library(stringr)
library(lubridate)
library(tidyr)
library(dbplyr)
library(ggplot2)
library(plotly)

source("point_in_time_ve.R")


iz2 <- IZlist %>% 
  left_join(Cases %>%
              select(profileid, eventid, first_name, last_name, date_of_birth),
            by = c("patient_first_name" = "first_name", "patient_last_name" = "last_name", "patient_dob" = "date_of_birth")) %>% 
  mutate(utd = ymd(str_sub(UTD_on + days(14), 1, 10)), 
         first_vacc_date = ymd(str_sub(first_vacc_date, 1, 10)),
         pid = ifelse(is.na(profileid), patient_id, profileid), 
         eid = ifelse(is.na(eventid), patient_id, eventid)) %>%
  group_by(pid, eid) %>% 
  filter(first_vacc_date == min(first_vacc_date)) %>% 
  ungroup() %>% 
  select(-patient_id) %>% 
  distinct()

iz3 <- iz2 %>% 
  mutate(izid = 1:n()) %>% 
  select(-pid, -eid)

c1 <- Cases %>%
  select(profileid, eventid, reporteddate, breakthrough, collectiondate) %>%
  distinct() %>% 
  mutate(profileid = round(profileid)) %>%
  filter(profileid==1251102)

c2 <- Cases %>% 
  select(profileid, eventid, date_of_birth, reporteddate, collectiondate, onsetdate_proxy_dist, patient_id, first_vax, vax_utd, DaysVaxMinusTest, breakthrough) %>%
  distinct() %>%
#  left_join(ids %>% distinct()) %>% 
  left_join(IZlist %>%
              select(-patient_first_name, -patient_last_name, patient_dob), by = "patient_id") %>% 
  group_by(profileid, eventid) %>% 
  filter(all(is.na(first_vacc_date)) | first_vacc_date == min(first_vacc_date)) %>%
  ungroup() %>%
  mutate(days = interval(first_vacc_date, collectiondate) / days(1)) 

#this next part doesn't appear to be working correctly--need to fill in UTD_on column for reinfection cases (I THINK???)
# ugh <- c2 %>% group_by(profileid, eventid) %>% mutate(n = n()) %>% ungroup()
# 
# c3 <- ugh %>% 
#   filter(n > 1) %>%
#   select(profileid, eventid, first_vacc_date, UTD_on, n) %>%
#   group_by(profileid, eventid) %>%
#   fill(utd_on, .direction = "downup") %>% 
#   filter(patient_id == min(patient_id)) %>% 
#   distinct() %>% 
#   ungroup() %>% 
#   select(-n) %>% 
#   bind_rows(ugh %>% filter(n == 1) %>% select(-n))

#I don't think I need lines 53-64 now that I added distinct() in line 43 for df c2
#Also don't think c3 is needed at all

# c3 <- c2 %>% 
#   select(profileid, eventid, first_vacc_date, UTD_on, patient_id) %>%
#   group_by(profileid, eventid) %>%
# #  fill(utd_on, .direction = "downup") %>% 
#   filter(patient_id == min(patient_id)) %>% 
#   distinct() %>% 
#   ungroup() %>% 
#   select(-n) %>% 
#   bind_rows(ugh %>% filter(n == 1) %>% select(-n))


c4 <- c2 %>% 
  mutate(remove = collectiondate > Sys.Date() - days(1)) %>% 
  mutate_at(vars(collectiondate, reporteddate), as.character) %>%
  mutate(collectiondate = ifelse(is.na(collectiondate) | remove, 
                                          reporteddate, 
                                          collectiondate),
         casestatus = case_when(breakthrough==1 ~ "fully vaccinated",
                                !breakthrough & is.na(first_vacc_date) ~ "unvaccinated", 
                                !breakthrough & !is.na(first_vacc_date) & first_vacc_date >= collectiondate ~ "unvaccinated", 
                                !breakthrough & !is.na(first_vacc_date) & first_vacc_date < collectiondate ~ "partially vaccinated")) %>% 
  select(profileid, eventid, collectiondate, casestatus, first_vacc_date, UTD_on)

vaccinated_then_some_cases <- iz3 %>% 
  left_join(c4 %>% filter(casestatus != "unvaccinated")) %>% 
  select(izid, everything()) %>% 
  mutate(utd_on = UTD_on + days(14))

unvaccinated_cases <- c4 %>% 
  anti_join(vaccinated_then_some_cases %>% 
              filter(!is.na(casestatus)) %>% 
              select(profileid, eventid))

# unvaccated cases / unvaccinated person-days 
# partially vaccinated cases / partially vaccinated person-days 
# fully vaccinated cases / fully vaccinated person-days 

# people who don't have a dose in CIIS contribute the full study period 
unvaccinated_no_cases_pop <- RouttPop - nrow(vaccinated_then_some_cases) - nrow(unvaccinated_cases)

vtsc <- vaccinated_then_some_cases %>% 
  mutate(start = ymd("2020-12-14")) %>% 
  select(izid, start, first_vacc_date, utd_on, collectiondate, casestatus) 

vtsc2 <- vtsc %>% 
  mutate(end = ifelse(is.na(collectiondate),
                      as.character(Sys.Date() - days(1)), 
                      collectiondate), 
         end = ymd(end)) %>% 
  mutate(unvax_days = interval(start, first_vacc_date) / days(1))

vtsc3 <- vtsc2 %>% 
  filter(is.na(utd_on)) %>% 
  mutate(partialvax_days = interval(first_vacc_date, end) / days(1), 
         fullvax_days = 0) %>% 
  bind_rows(vtsc2 %>% 
              filter(!is.na(utd_on)) %>% 
              mutate(partialvax_days = interval(first_vacc_date, utd_on) / days(1), 
                     fullvax_days = interval(utd_on, end) / days(1)))

vtsc4 <- vtsc3 %>% 
  select(unvax_days, partialvax_days, fullvax_days) %>% 
  mutate(id = 1:n()) 

uc2 <- unvaccinated_cases %>% 
  mutate(start = ymd("2020-12-14")) %>% 
  mutate(unvax_days = interval(start, collectiondate) / days(1)) %>% 
  mutate(partialvax_days = 0, 
         fullvax_days = 0) %>% 
  mutate(id = 1:n()) %>% 
  select(unvax_days, partialvax_days, fullvax_days, id) %>% 
  mutate(id = id + max(vtsc4$id))

unvax_days_co <- length(seq.Date(ymd("2020-12-14"), Sys.Date() - days(1), by = "days")) * unvaccinated_no_cases_pop

alldays <- vtsc4 %>% 
  bind_rows(uc2) %>% 
  summarise(unvax_days = sum(unvax_days), 
            partialvax_days = sum(partialvax_days), 
            fullvax_days = sum(fullvax_days)) %>% 
  mutate(unvax_days = unvax_days + unvax_days_co)

allcases <- c4 %>% 
  count(casestatus) %>% 
  spread(casestatus, n) %>% 
  janitor::clean_names()

unvaxrate <- 14*100000 * allcases$unvaccinated / alldays$unvax_days
partialvaxrate <- 14*100000 * allcases$partially_vaccinated / alldays$partialvax_days
fullvaxrate <- 14*100000 * allcases$fully_vaccinated / alldays$fullvax_days

col = covid_categorical("muted blue")

results <- tibble(Unvaccinated = unvaxrate, 
                  `Partially\nvaccinated` = partialvaxrate, 
                  `Fully\nvaccinated` = fullvaxrate) %>% 
  gather("x", "y") %>% 
  mutate(x = factor(x, levels = c("Unvaccinated", 
                                  "Partially\nvaccinated", 
                                  "Fully\nvaccinated"))) %>% 
  mutate(label = round(y, 1))

col <- covid_categorical("muted blue")

results %>%   
  ggplot(aes(x, y)) + 
  geom_segment(aes(x = x, xend = x, 
                   y = 0, yend = y), 
               size = 2) + 
  geom_point(size = 15) + 
  geom_text(aes(label = label), color = "white", fontface = "bold") + 
  scale_y_continuous("", limits = c(0, max(results$y) + 40)) + 
  theme_covid() + 
  theme(axis.text.y = element_blank(), 
        axis.line.y = element_blank(), 
        axis.text.x = element_text(size = 12)) + 
  labs(x = "")

ggsave("~/../Downloads/cumulativerisk.png", dpi = 600, height = 4.5, width = 4.5)

(unvaxrate - fullvaxrate) / unvaxrate # protection of full vaccination 

(unvaxrate - partialvaxrate) / unvaxrate # protection of partial vaccination 
