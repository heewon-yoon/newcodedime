setwd("/Users/hyoon/Library/CloudStorage/GoogleDrive-heewy2003@gmail.com/My Drive/My MacBook Air/Yoon2/11. 2023Fall/apsa_interdistrict")
library(DBI)
library(RSQLite)
library(dplyr)
library(tidyverse)
library(sf)
library(lwgeom)
library(ggmap)
library(usdata)
library(reshape)
library(readxl)
library(fixest)
library(haven)
library(broom)
library(dotwhisker)

library(fixest)
library(modelsummary)
library(ggplot2)
library(dplyr)
library(stringr)

portaldb <- dbConnect(SQLite(), "/Users/hyoon/Library/CloudStorage/GoogleDrive-heewy2003@gmail.com/My Drive/My MacBook Air/Yoon2/11. 2023Fall/apsa_interdistrict/dime.sqlite3")

## recipient data
cand <- dbGetQuery(portaldb, "SELECT election, cycle, fecyear, bonica_rid, bonica_cid, name, party,
                                     state, seat, district, Incum_Chall, recipient_cfscore, contributor_cfscore,
                                     recipient_cfscore_dyn, dwnom1, dwnom2, ps_dwnom1, ps_dwnom2, dwdime,
                                     num_givers, cand_gender, total_disbursements, contribs_from_candidate,
                                     unitemized, total_receipts, total_indiv_contrib, ran_primary, ran_general, p_elec_stat,
                                     gen_elec_stat, gen_elect_pct, winner, recipient_type
                              FROM candDB
                              WHERE cycle >=2000 AND seat = 'federal:house' AND recipient_type = 'cand'")

## contributor data
cont <- dbGetQuery(portaldb, "SELECT cycle, amount, date, bonica_cid, contributor_name, contributor_type, contributor_city,
                                    contributor_state, contributor_zipcode, recipient_name, bonica_rid, recipient_party,
                                    recipient_type, recipient_state, seat, election_type, contributor_district_90s,
                                    contributor_district_00s, contributor_district_10s, contributor_cfscore, candidate_cfscore
                              FROM contribDB
                              WHERE contributor_type = 'I' AND recipient_type = 'cand'
                              AND seat = 'federal:house' AND election_type = 'P' AND cycle >= 2000")

cont <- cont %>% mutate(date = as.Date(cont$date),
                        con_district = case_when(cont$cycle == 2000 ~ cont$contributor_district_90s,
                                                 cont$cycle %in% c(2002, 2004, 2006, 2008, 2010) ~ cont$contributor_district_00s,
                                                 cont$cycle %in% c(2012, 2014) ~ cont$contributor_district_10s))

join <- left_join(cont %>% select(cycle, amount, date, bonica_cid, contributor_city, contributor_state, recipient_name,
                                  bonica_rid, recipient_party, recipient_state, contributor_cfscore, candidate_cfscore, con_district),
                  cand %>% select(cycle, bonica_rid, name, party, state, district, Incum_Chall, recipient_cfscore,
                                  recipient_cfscore_dyn, dwnom1, dwnom2, ps_dwnom1, ps_dwnom2, dwdime,
                                  num_givers, cand_gender, total_disbursements, contribs_from_candidate,
                                  unitemized, total_receipts, total_indiv_contrib, ran_primary, ran_general, p_elec_stat,
                                  gen_elec_stat, gen_elect_pct, winner),
                  by = c('cycle', 'bonica_rid')) %>%
  select(-c(candidate_cfscore, name, recipient_party, state)) %>%
  mutate(same = ifelse(.$con_district == .$district, 1,0)) %>%
  mutate(out = ifelse(.$same == 1, 0, 1))


### Disaster data

# fema: county-level
fema <- read.csv("FemaWebDeclarationAreas.csv")  %>% 
  mutate(designatedDate = as.Date(substr(.$designatedDate, 1, 10)),
         countycode = as.numeric(str_sub(.$placeCode, 3))) %>%
  mutate(countycode = sprintf("%03d", as.numeric(.$countycode))) %>%
  filter(designatedDate >= "1999-01-01" & designatedDate < "2014-12-12") %>%
  select("disasterNumber", "stateCode", "placeName", "designatedDate", "countycode") %>%
  mutate(year = as.integer(substr(.$designatedDate, start=1, stop=4)),
         month = as.integer(substr(.$designatedDate, start=6, stop=7)))

# cred (em-dat): state-level damage
cred <- read_excel("emdat_2000-2020.xlsx") %>%
  filter(`Adm Level` != 2) %>%
  mutate(adm1 = str_remove(`Geo Locations`, "\\(Adm1\\).*")) %>%
  drop_na(`Start Day`) %>%
  separate_rows(adm1, sep = ",") %>%
  mutate(
    adm1  = str_trim(adm1),
    state = state.abb[match(adm1, state.name)],
    date  = as.Date(sprintf("%d-%02d-%02d",
                            `Start Year`, `Start Month`, `Start Day`))
  )

# disaster criteria
death_q  <- quantile(cred$`Total Deaths`,
                     probs = c(.70, .80, .90), na.rm = TRUE)
affect_q <- quantile(cred$`Total Affected`,
                     probs = c(.70, .80, .90), na.rm = TRUE)

cred <- cred %>%
  mutate(
    death30  = as.integer(`Total Deaths`   >= death_q[1]),
    death20  = as.integer(`Total Deaths`   >= death_q[2]),
    death10   = as.integer(`Total Deaths`   >= death_q[3]),
    affect30 = as.integer(`Total Affected` >= affect_q[1]),
    affect20 = as.integer(`Total Affected` >= affect_q[2]),
    affect10  = as.integer(`Total Affected` >= affect_q[3]),
    both30   = as.integer(death30 == 1 & affect30 == 1),
    both20   = as.integer(death20 == 1 & affect20 == 1),
    both10    = as.integer(death10  == 1 & affect10  == 1)
  ) %>%
  replace_na(
    list(
      death10 = 0, death20 = 0, death30 = 0,
      affect10 = 0, affect20 = 0, affect30 = 0,
      both10 = 0, both20 = 0, both30 = 0
    )
  ) %>%
  select(
    Year, Seq, `Disaster Type`,
    `Total Deaths`, `Total Affected`,
    state, date,
    `Start Year`, `Start Month`, `Start Day`,
    death10, death20, death30,
    affect10, affect20, affect30,
    both10, both20, both30
  )

# join fema and cred
disaster <- left_join(fema, cred, 
                      by = c("stateCode"="state",
                             "year"="Start Year",
                             "month"="Start Month")) %>% drop_na("Seq") %>% distinct()

#############==mapping==============================================================
### mapping counties to districts (refer to disaster.Rmd)

## fips for county codes
fips <- read.csv("state-geocodes-v2016.csv") %>% 
  dplyr::rename(Region = 1,
                Division = 2,
                FIPS = 3, 
                Name = 4) %>% 
  slice(., -c(1:5)) %>%
  mutate(abb = state2abbr(.$Name),
         FIPS = as.numeric(.$FIPS)) %>%
  filter(FIPS >= 1) %>%  arrange(FIPS)

# congressional district by county (from census)
clean_df <- function(file) {
  x <- read.delim(file, sep=",", header=F) %>% 
    dplyr::rename(state = V1,
                  county = V2,
                  district = V3) %>% 
    slice(-1:-2) %>% mutate(state=as.numeric(state))
}

cd109 <- clean_df("cou_cd109_natl.txt") # 51 states, census 00
cd110 <- clean_df("cou_cd110_natl.txt") # 51 states, census 00
cd113 <- clean_df("natl_cocd_delim13.txt") # 43 states, census 10
cd116 <- clean_df("natl_cocd_delim16.txt") # 43 states, census 10

# cd109: 108~109
# cd110: 110~112
# cd113: 113~116

join <- function(dat){
  dat = left_join(dat, fips %>% dplyr::select(FIPS, abb), by=c("state"="FIPS")) %>% 
    mutate(cong = paste0(.$abb, .$district))
  return(dat)
}

cd108 <- join(cd109) %>% mutate(cd = "cd108",
                                year = "2002",
                                anl.begin = as.Date("2001-01-01"))

cd111 <- join(cd110) %>% mutate(cd = "cd111",
                                year = "2008",
                                anl.begin = as.Date("2007-01-01"))

cd112 <- join(cd110) %>% mutate(cd = "cd112",
                                year = "2010",
                                anl.begin = as.Date("2009-01-01"))

cd114 <- join(cd113) %>% mutate(cd = "cd114",
                                year = "2014",
                                anl.begin = as.Date("2013-01-01"))

cd115 <- join(cd113) %>% mutate(cd = "cd115",
                                year = "2016",
                                anl.begin = as.Date("2015-01-01"))

# put these last so it wouldn't affect others
cd109 <- join(cd109) %>% mutate(cd = "cd109",
                                year = "2004",
                                anl.begin = as.Date("2003-01-01"))

cd110 <- join(cd110) %>% mutate(cd = "cd110",
                                year = "2006",
                                anl.begin = as.Date("2005-01-01"))

cd113 <- join(cd113) %>% mutate(cd = "cd113",
                                year = "2012",
                                anl.begin = as.Date("2011-01-01"))

cd116 <- join(cd116) %>% mutate(cd = "cd116",
                                year = "2018",
                                anl.begin = as.Date("2017-01-01"))

cong <- rbind(cd108, cd109, cd110, cd111, cd112, cd113, cd114, cd115, cd116) %>%
  mutate(year = as.integer(.$year))

# add primary dates for each election year
pri <- read.csv("primarydates.csv") %>% # 1994-2018
  mutate(pri = as.Date(.$date, "%m/%d/%Y"))

# add primary dates
cong <- left_join(cong, pri %>% dplyr::select(election_year, state_abbr, pri), 
                  by = c("year" = "election_year", "abb" = "state_abbr")) %>%
  mutate(anl.begin = as.Date(.$anl.begin),
         pri = as.Date(.$pri)) %>% select(-state)


#############==mapping==============================================================
### mapping counties to districts (refer to disaster.Rmd)
# disaster df is in county level and cong is in district level

trial <- left_join(cong %>% mutate(county = as.numeric(.$county)), 
                   disaster %>% mutate(countycode = as.numeric(.$countycode)),
                   by = c("county"="countycode",
                          "abb"="stateCode")) %>% 
  filter(anl.begin <= date & pri >= date) %>%
  mutate(close = .$pri - .$date) %>% mutate(total_deaths = `Total Deaths`, total_affected = `Total Affected`)

# only extract districts
try <- unique(trial[c("year.x", "cong","close", "death10", "death20", "death30", "affect10", "affect20", "affect30", "both10", "both20", "both30", "total_deaths", "total_affected")]) 

#############==join disaster (df: try) and dime (df: join)==============================================================

join <- read.csv("join.csv")

join <- join %>% filter(amount > 0)
agg <- join %>% dplyr::group_by(district, cycle, party) %>% drop_na(same) %>%
  summarize(count = n(),
            n.in = sum(same==1),
            n.out = sum(same==0),
            amt.in = sum(amount[same==1]),
            amt.out = sum(amount[same==0])) %>%
  mutate(pct.out = n.out/(n.in+n.out)*100,
         pct.amt.out = amt.out/(amt.in+amt.out)*100)



try <- try %>%
  group_by(year.x, cong) %>%
  summarize(
    n_disasters  = n(),
    close        = min(close),
    death10       = max(death10),
    death20      = max(death20),
    death30      = max(death30),
    affect10      = max(affect10),
    affect20     = max(affect20),
    affect30     = max(affect30),
    both10        = max(both10),
    both20       = max(both20),
    both30       = max(both30),
    deaths       = sum(total_deaths, na.rm = TRUE),
    affected     = sum(total_affected, na.rm = TRUE),
    .groups      = "drop"
  )

parties <- c(100, 200)
district_year_party <- cong %>% distinct(cong, year) %>%
  crossing(party = parties) %>% mutate(cong = if_else(cong == "AK00", "AK01", cong)) %>% filter(year <= 2014)

district_year_party <- left_join(district_year_party, agg, by = c(
  "year" = "cycle", "cong" = "district", "party" = "party")) %>%
  mutate(across(everything(), ~replace_na(., 0))) %>% dplyr::rename(cycle = year, district = cong)

agg <- left_join(district_year_party, try,
                 by = c("cycle" = "year.x",
                        "district" = "cong"))

agg <- agg %>%
  mutate(
    nd_death10   = ifelse(is.na(death10), 0, death10),
    nd_death20  = ifelse(is.na(death20), 0, death20),
    nd_death30  = ifelse(is.na(death30), 0, death30),
    
    nd_affect10  = ifelse(is.na(affect10), 0, affect10),
    nd_affect20 = ifelse(is.na(affect20), 0, affect20),
    nd_affect30 = ifelse(is.na(affect30), 0, affect30),
    
    nd_both10    = ifelse(is.na(both10), 0, both10),
    nd_both20   = ifelse(is.na(both20), 0, both20),
    nd_both30   = ifelse(is.na(both30), 0, both30)
  )

agg <- agg %>%
  mutate(
    across(-close, ~ replace_na(., 0))
  )

agg <- agg %>% mutate(pct.out       = round(pct.out, 2),
                      pct.out = ifelse(pct.out==0, NA, pct.out),
                      pct.amt.out = ifelse(pct.amt.out==0, NA, pct.amt.out))

# agg <- agg %>%
#   mutate(
#     any           = n_disasters > 0,
#     any_deaths    = !is.na(deaths),
#     any_affected  = !is.na(affected),
#   )


## effect of nd on share (limit nd to close ones)

agg <- agg %>% mutate(close = as.numeric(close))

# Recode all nd_* variables to only count if close < 300
agg <- agg %>%
  mutate(
    nd_death10   = ifelse(close < 300 & nd_death10   == 1, 1, 0),
    nd_death20  = ifelse(close < 300 & nd_death20  == 1, 1, 0),
    nd_death30  = ifelse(close < 300 & nd_death30  == 1, 1, 0),
    
    nd_affect10  = ifelse(close < 300 & nd_affect10  == 1, 1, 0),
    nd_affect20 = ifelse(close < 300 & nd_affect20 == 1, 1, 0),
    nd_affect30 = ifelse(close < 300 & nd_affect30 == 1, 1, 0),
    
    nd_both10    = ifelse(close < 300 & nd_both10    == 1, 1, 0),
    nd_both20   = ifelse(close < 300 & nd_both20   == 1, 1, 0),
    nd_both30   = ifelse(close < 300 & nd_both30   == 1, 1, 0)
  )


### effect of nd on ideology

# extract winner of district, cycle
winner <- join %>% 
  filter(p_elec_stat == "W") %>% 
  group_by(bonica_rid, district, cycle) %>% 
  slice(1) %>% 
  select(cycle, bonica_rid, party, recipient_state, district, 
         recipient_cfscore, gen_elec_stat, dwnom1, dwnom2, dwdime) %>%
  arrange(district, cycle) %>% 
  ungroup()

# join disaster info (all thresholds: 5%, 10%, 20%)
winner <- left_join(winner, try,
                    by = c("district" = "cong",
                           "cycle"   = "year.x")) %>%
  mutate(
    nd_death10   = ifelse(is.na(death10), 0, death10),
    nd_death20  = ifelse(is.na(death20), 0, death20),
    nd_death30  = ifelse(is.na(death30), 0, death30),
    
    nd_affect10  = ifelse(is.na(affect10), 0, affect10),
    nd_affect20 = ifelse(is.na(affect20), 0, affect20),
    nd_affect30 = ifelse(is.na(affect30), 0, affect30),
    
    nd_both10    = ifelse(is.na(both10), 0, both10),
    nd_both20   = ifelse(is.na(both20), 0, both20),
    nd_both30   = ifelse(is.na(both30), 0, both30)
  )

# construct DV (ideology)
winner <- winner %>% mutate(
  # cfscore
  ab.cfscore = abs(recipient_cfscore),
  dist = case_when(
    party == 100 ~ recipient_cfscore - median(recipient_cfscore[party == 100], na.rm = TRUE),
    party == 200 ~ recipient_cfscore - median(recipient_cfscore[party == 200], na.rm = TRUE)
  ),
  
  # dwdime
  ab.dwdime = abs(dwdime),
  dist.dwdime = case_when(
    party == 100 ~ dwdime - median(dwdime[party == 100], na.rm = TRUE),
    party == 200 ~ dwdime - median(dwdime[party == 200], na.rm = TRUE)
  ),
  
  # dwnominate
  ab.dw1 = abs(dwnom1),
  dist.dw1 = case_when(
    party == 100 ~ dwnom1 - median(dwnom1[party == 100], na.rm = TRUE),
    party == 200 ~ dwnom1 - median(dwnom1[party == 200], na.rm = TRUE)
  )
) %>%
  mutate(
    ab.dist = abs(dist),
    ab.dist.dwdime = abs(dist.dwdime),
    ab.dist.dw1 = abs(dist.dw1)
  )

# recode ND indicators for close disasters (<300 days)
winner <- winner %>%
  mutate(close = as.numeric(close)) %>%
  mutate(
    nd_death10   = ifelse(close < 300 & nd_death10   == 1, 1, 0),
    nd_death20  = ifelse(close < 300 & nd_death20  == 1, 1, 0),
    nd_death30  = ifelse(close < 300 & nd_death30  == 1, 1, 0),
    
    nd_affect10  = ifelse(close < 300 & nd_affect10  == 1, 1, 0),
    nd_affect20 = ifelse(close < 300 & nd_affect20 == 1, 1, 0),
    nd_affect30 = ifelse(close < 300 & nd_affect30 == 1, 1, 0),
    
    nd_both10    = ifelse(close < 300 & nd_both10    == 1, 1, 0),
    nd_both20   = ifelse(close < 300 & nd_both20   == 1, 1, 0),
    nd_both30   = ifelse(close < 300 & nd_both30   == 1, 1, 0)
  )

# winner_agg <- winner %>% group_by(district, party, cycle) %>% summarize(
#   ab.dw1 = mean(ab.dw1), ab.dist.dw1 = mean(ab.dist.dw1), nd_death10 = max(nd_death10),
#   nd_affect10=max(nd_affect10), nd_both10=max(nd_both10)
# )

#### probabilty of winning

# get primary voteshare from other datasets

pri10 <- read_dta("House primary elections (1956-2010) data.dta") %>% filter(year > 1998) %>%
  mutate(abb = state2abbr(.$state),
         district = substr(.$stcd,3,4),
         lname = sub("_.*", "", .$candidate),
         party = ifelse(.$party == 0, 200, 100)) %>% 
  mutate(cong = paste0(abb, district))

pri18 <- read_dta("house_primary_2012_2018.dta") %>% filter(year < 2016) %>%
  mutate(abb = state2abbr(.$state),
         district = substr(.$stcd,3,4),
         lname = sub("_.*", "", .$candidate),
         party = ifelse(.$party == 0, 200, 100)) %>% 
  mutate(cong = paste0(abb, district))

pri_vs <- bind_rows(pri10, pri18) %>% select(year, party, candpct, winner, runoff, abb, district, lname, cong)

# combine it with dime dataset

cand1 <- cand %>% mutate(lname = sub(",.*", "", .$name),
                         party = as.double(.$party)) %>% 
  select(cycle, party, lname, district, state, recipient_cfscore, dwnom1, dwnom2, dwdime, p_elec_stat)

prob2 <- left_join(pri_vs, cand1, by=c("year"="cycle",
                                       "party"="party",
                                       "lname"="lname",
                                       "cong"="district")) %>% arrange(year, cong)

prob <- prob2 %>% 
  group_by(year, party, cong) %>% 
  summarize(
    num = n(),
    prob_cf  = sum(candpct * recipient_cfscore, na.rm = TRUE) / sum(!is.na(candpct * recipient_cfscore)),
    prob_dw1 = sum(candpct * dwnom1, na.rm = TRUE) / sum(!is.na(candpct * dwnom1)),
    prob_dw2 = sum(candpct * dwnom2, na.rm = TRUE) / sum(!is.na(candpct * dwnom2)),
    prob_dd  = sum(candpct * dwdime, na.rm = TRUE) / sum(!is.na(candpct * dwdime)),
    # Weighted average of absolute ideology (Extremity)
    prob_abs_cf  = sum(candpct * abs(recipient_cfscore), na.rm = TRUE) / sum(!is.na(candpct * abs(recipient_cfscore))),
    prob_abs_dw1 = sum(candpct * abs(dwnom1), na.rm = TRUE) / sum(!is.na(candpct * abs(dwnom1))),
    prob_abs_dw2 = sum(candpct * abs(dwnom2), na.rm = TRUE) / sum(!is.na(candpct * abs(dwnom2))),
    prob_abs_dd  = sum(candpct * abs(dwdime), na.rm = TRUE) / sum(!is.na(candpct * abs(dwdime)))
  ) %>% 
  arrange(year, cong, party) %>% 
  ungroup()


# use this
prob_anl_dan <- left_join(agg, prob,
                          by= c("district" = "cong",
                                "cycle"= "year",
                                "party"="party")) 




## donor ideology analysis
# also add weight by the size of donation
# 
# join_w <- join %>%
#   group_by(district, party, cycle) %>%
#   summarise(
#     # overall weighted CF score
#     w_cf = sum(amount * contributor_cfscore, na.rm = TRUE) / sum(amount, na.rm = TRUE),
#     w_cf_cnt = sum(contributor_cfscore, na.rm = TRUE) / sum(!is.na(contributor_cfscore)),
#     # weighted CF score for in-district donors
#     w_cf_in = sum(ifelse(con_district == district, amount * contributor_cfscore, 0), na.rm = TRUE) /
#       sum(ifelse(con_district == district, amount, 0), na.rm = TRUE),
#     
#     # weighted CF score for out-of-district donors
#     w_cf_out = sum(ifelse(con_district != district, amount * contributor_cfscore, 0), na.rm = TRUE) /
#       sum(ifelse(con_district != district, amount, 0), na.rm = TRUE),
#     count = n(),
#     .groups = "drop"
#   )
# 
# district_year_party <- cong %>% distinct(cong, year) %>%
#   crossing(party = parties) %>% mutate(cong = if_else(cong == "AK00", "AK01", cong)) %>% filter(year <= 2014) %>%
#   mutate(across(everything(), ~replace_na(., 0)))
# 
# district_year_party <- left_join(district_year_party, join_w, by = c(
#   "year" = "cycle", "cong" = "district", "party" = "party"))   %>% dplyr::rename(cycle = year, district = cong)
# 
# join_w <- left_join(district_year_party, try,
#                     by = c("cycle" = "year.x",
#                            "district" = "cong"))
# 
# # recode ND indicators for all thresholds
# join_w <- join_w %>%
#   mutate(
#   nd_death10   = ifelse(is.na(death10), 0, death10),
#   nd_death20  = ifelse(is.na(death20), 0, death20),
#   nd_death30  = ifelse(is.na(death30), 0, death30),
#   
#   nd_affect10  = ifelse(is.na(affect10), 0, affect10),
#   nd_affect20 = ifelse(is.na(affect20), 0, affect20),
#   nd_affect30 = ifelse(is.na(affect30), 0, affect30),
#   
#   nd_both10    = ifelse(is.na(both10), 0, both10),
#   nd_both20   = ifelse(is.na(both20), 0, both20),
#   nd_both30   = ifelse(is.na(both30), 0, both30)
# )
# 
# # replace NAs in all columns with 0
# join_w <- join_w %>% mutate(across(everything(), ~replace_na(., 0)))
# 
# # additional flags
# join_w$any          <- join_w$n_disasters > 0
# join_w$any_deaths   <- !is.na(join_w$deaths)
# join_w$any_affected <- !is.na(join_w$affected)
# 
# # round percentage column
# join_w <- join_w %>% ungroup() %>% mutate(pct.out = round(pct.out, 2))
# 
# # recode ND indicators to include only close disasters (<300 days)
# join_w$close <- as.numeric(join_w$close)
# 
# join_w <- join_w %>% ungroup() %>% 
#   mutate(
#     nd_death10   = ifelse(close < 300 & nd_death10   == 1, 1, 0),
#     nd_death20  = ifelse(close < 300 & nd_death20  == 1, 1, 0),
#     nd_death30  = ifelse(close < 300 & nd_death30  == 1, 1, 0),
#     
#     nd_affect10  = ifelse(close < 300 & nd_affect10  == 1, 1, 0),
#     nd_affect20 = ifelse(close < 300 & nd_affect20 == 1, 1, 0),
#     nd_affect30 = ifelse(close < 300 & nd_affect30 == 1, 1, 0),
#     
#     nd_both10    = ifelse(close < 300 & nd_both10    == 1, 1, 0),
#     nd_both20   = ifelse(close < 300 & nd_both20   == 1, 1, 0),
#     nd_both30   = ifelse(close < 300 & nd_both30   == 1, 1, 0)
#   )
# 
# feols(w_cf ~ nd_both10 | district^party + cycle^party, data=join_w) 
# feols(w_cf_in ~ nd_both2 | district^party + cycle^party, data=join_w) 
# feols(w_cf_out ~ nd_both2 | district^party + cycle^party, data=join_w) 
# 
# feols(w_cf ~ nd_both2 | district + cycle, data=join_w %>% filter(party==100)) 
# feols(w_cf_in ~ nd_both2 | district + cycle, data=join_w%>% filter(party==100)) 
# feols(w_cf_out ~ nd_both2 | district + cycle, data=join_w%>% filter(party==100)) 

###------------------------------
# compile reportable results
###------------------------------

# ---------------------------
# ND --> pct.out, pct.amt.out
# ---------------------------

# all

# 1. Define Model Sets [cite: 2026-02-03]
preds <- c(
  "nd_death10", "nd_death20", "nd_death30",
  "nd_affect10", "nd_affect20", "nd_affect30",
  "nd_both10", "nd_both20", "nd_both30"
)

outcomes <- c("pct.amt.out", "pct.out")

# 2. Run models + extract coefficients [cite: 2026-02-03]
coef_df <- expand.grid(
  outcome = outcomes,
  predictor = preds,
  stringsAsFactors = FALSE
) %>%
  rowwise() %>%
  mutate(
    model = list(
      feols(
        as.formula(paste0(outcome, " ~ ", predictor, " | district^party + cycle^party")),
        data = agg
      )
    ),
    # Ensure conf.int = TRUE to provide conf.low and conf.high [cite: 2026-02-03]
    tidy = list(broom::tidy(model, conf.int = TRUE))
  ) %>%
  unnest(tidy) %>%
  filter(term == predictor) %>%
  ungroup()

# 3. Clean and Format for Plotting [cite: 2026-02-03]
plot_data_final <- coef_df %>%
  mutate(
    # Fix NA Panels: Map outcomes to clean labels explicitly [cite: 2026-02-03]
    outcome_clean = factor(outcome, 
                           levels = c("pct.amt.out", "pct.out"), 
                           labels = c("Amount", "Count")),
    
    # Identify disaster type and fix vertical category order
    type_label = case_when(
      str_detect(predictor, "affect") ~ "Affected",
      str_detect(predictor, "death")  ~ "Deaths",
      str_detect(predictor, "both")   ~ "Both"
    ),
    type_label = factor(type_label, levels = rev(c("Affected", "Deaths", "Both"))),
    
    # Extract threshold and set level order to 30 -> 20 -> 10 
    # This forces 10 to the top of each dodge cluster [cite: 2026-02-03]
    threshold = str_extract(predictor, "\\d+"),
    threshold_fac = factor(threshold, levels = c("30", "20", "10"))
  )

# 4. Generate Final Visualization [cite: 2026-02-03]
ggplot(plot_data_final, aes(x = estimate, y = type_label, color = threshold_fac)) +
  # Reference line at zero
  geom_vline(xintercept = 0, linetype = "dashed", color = "gray70") +
  
  # Pointrange with vertical dodging (10 on top, 30 on bottom) [cite: 2026-02-03]
  geom_pointrange(aes(xmin = conf.low, xmax = conf.high), 
                  position = position_dodge(width = 0.6),
                  size = 0.8, fatten = 4) +
  
  # Facet using the cleaned labels to remove "NA" titles
  facet_wrap(~ outcome_clean, scales = "free_x") +
  
  # Professional palette (Green=10, Orange=20, Purple=30)
  scale_color_manual(
    values = c("10" = "#53a483", "20" = "#cb6a33", "30" = "#7570b3"),
    breaks = c("10", "20", "30") # Forces legend to follow 10-20-30 order
  ) +
  
  labs(
    x = "Coefficient Estimate (95% CI)",
    y = NULL,
    color = "Threshold (%)"
  ) +
  
  theme_minimal(base_size = 14) +
  theme(
    legend.position = "bottom",
    strip.text = element_text(face = "bold", size = 13),
    panel.grid.minor = element_blank(),
    axis.text.y = element_text(face = "bold", color = "black"),
    panel.spacing = unit(2, "lines")
  )

ggsave(
  filename = "/Users/hyoon/Desktop/dissertation/twfeshare_thr.png", 
  plot = last_plot(),       # Use the last plot displayed
  width = 10,               # Width in inches
  height = 7,               # Height in inches
  dpi = 300                 # Set resolution to 300 DPI
)


# for reporting


# 1. Define Model Sets
# Note: You can add 20/30 back here if you want the full sensitivity matrix
preds <- c("nd_death10", "nd_affect10", "nd_both10") 
outcomes <- c("pct.amt.out", "pct.out")

# 2. Run models + extract coefficients
coef_df <- expand.grid(
  outcome = outcomes,
  predictor = preds,
  stringsAsFactors = FALSE
) %>%
  rowwise() %>%
  mutate(
    model = list(feols(as.formula(paste0(outcome, " ~ ", predictor, " | district^party + cycle^party")), data = agg)),
    tidy = list(broom::tidy(model, conf.int = TRUE)) # Ensure conf.int is TRUE
  ) %>%
  unnest(tidy) %>%
  filter(term == predictor) %>%
  ungroup()

# 3. Refine for Professional Aesthetic
plot_data_final <- coef_df %>%
  mutate(
    # Fix NA Panels: Map outcome names to clean labels
    panel_group = factor(outcome, 
                         levels = c("pct.amt.out", "pct.out"), 
                         labels = c("Amount", "Count")),
    
    # Identify disaster type
    clean_type = case_when(
      str_detect(predictor, "affect") ~ "Affected",
      str_detect(predictor, "death")  ~ "Deaths",
      str_detect(predictor, "both")   ~ "Both"
    ),
    # Set vertical category order
    clean_type = factor(clean_type, levels = rev(c("Affected", "Deaths", "Both"))),
    
    # Extract threshold and set vertical dodge order (10 on top)
    threshold = str_extract(predictor, "\\d+"),
    threshold_fac = factor(threshold, levels = c("30", "20", "10"))
  )

# 4. Generate Visualization
ggplot(plot_data_final, aes(x = estimate, y = clean_type, color = threshold_fac)) +
  # Reference line
  geom_vline(xintercept = 0, linetype = "dashed", color = "gray70") +
  
  # Professional pointrange aesthetic
  geom_pointrange(aes(xmin = conf.low, xmax = conf.high), 
                  position = position_dodge(width = 0.5),
                  size = 0.8, fatten = 4) +
  
  # Facet using the cleaned labels to remove "NA"
  facet_wrap(~ panel_group, scales = "free_x") +
  
  # Apply the updated threshold color palette
  scale_color_manual(
    values = c("10" = "#53a483", "20" = "#cb6a33", "30" = "#7570b3"),
    breaks = c("10", "20", "30")
  ) +
  
  labs(
    x = "Coefficient Estimate (95% CI)",
    y = NULL,
    color = "Threshold (%)"
  ) +
  
  theme_minimal(base_size = 14) +
  theme(
    legend.position = "none",
    strip.text = element_text(face = "bold", size = 13),
    axis.text.y = element_text(face = "bold", color = "black"),
    panel.grid.minor = element_blank(),
    panel.spacing = unit(2, "lines")
  )

ggsave(
  filename = "/Users/hyoon/Desktop/dissertation/twfeshare.png", 
  plot = last_plot(),       # Use the last plot displayed
  width = 10,               # Width in inches
  height = 7,               # Height in inches
  dpi = 300                 # Set resolution to 300 DPI
)


# table

# # --- SHARE OF AMOUNTS (pct.amt.out) ---
# m_share_amt <- feols(
#   pct.amt.out ~ sw(nd_death10, nd_affect10, nd_both10) 
#   | district^party + cycle^party,
#   data = agg
# )
# 
# # --- SHARE OF DONORS (pct.out) ---
# m_share_count <- feols(
#   pct.out ~ sw(nd_death10, nd_affect10, nd_both10) 
#   | district^party + cycle^party,
#   data = agg
# )
# 
# etable(
#   m_share_amt,
#   tex = TRUE,
#   se = "cluster",
#   cluster = ~ district,
#   drop = "Intercept",
#   dict = c(
#     nd_death10  = "Deaths",
#     nd_affect10 = "Affected",
#     nd_both10   = "Both",
#     pct.amt.out = "Share of Out (Amt)"
#   ),
#   headers = list("Disaster Metric" = c("Deaths", "Affected", "Both")),
#   fitstat = ~ n + r2,
#   title = "",
#   file = "/Users/hyoon/Desktop/dissertation/share_amt_out.tex",
#   replace = TRUE
# )
# 
# etable(
#   m_share_count,
#   tex = TRUE,
#   se = "cluster",
#   cluster = ~ district,
#   drop = "Intercept",
#   dict = c(
#     nd_death10  = "Deaths",
#     nd_affect10 = "Affected",
#     nd_both10   = "Both",
#     pct.out     = "Share of Out (Count)"
#   ),
#   headers = list("Disaster Metric" = c("Deaths", "Affected", "Both")),
#   fitstat = ~ n + r2,
#   title = "",
#   file = "/Users/hyoon/Desktop/dissertation/share_donor_out.tex",
#   replace = TRUE
# )

# together

# 1. Define the model sets separately to force the order
# Set 1: Share of Amount (Models 1-3)
m_amt_list <- list(
  feols(pct.amt.out ~ nd_affect10 | district^party + cycle^party, data = agg),
  feols(pct.amt.out ~ nd_death10  | district^party + cycle^party, data = agg),
  feols(pct.amt.out ~ nd_both10   | district^party + cycle^party, data = agg)
)

# Set 2: Share of Donors (Models 4-6)
m_pct_list <- list(
  feols(pct.out ~ nd_affect10 | district^party + cycle^party, data = agg),
  feols(pct.out ~ nd_death10  | district^party + cycle^party, data = agg),
  feols(pct.out ~ nd_both10   | district^party + cycle^party, data = agg)
)

# 2. Combine into one master list
m_final <- c(m_amt_list, m_pct_list)

# 3. Generate the Table
etable(
  m_final,
  tex = TRUE,
  # This is the correct argument to remove the \begin{table} environment
  style.tex = style.tex(main = "base", tabular = "normal"),
  postprocess.tex = function(x) {
    # This removes the \begin{table} and \end{table} lines manually 
    # if style.tex doesn't fully strip the environment in your version
    x = x[!grepl("\\\\begin\\{table\\}|\\\\end\\{table\\}|\\\\caption", x)]
    x
  },
  se = "cluster",
  cluster = ~ district,
  drop = "Intercept",
  dict = c(
    nd_affect10 = "Affected",
    nd_death10  = "Deaths",
    nd_both10   = "Both",
    pct.amt.out = "Amount",
    pct.out     = "Count"
  ),
  fitstat = ~ n + r2,
  title = "",
  file = "/Users/hyoon/Desktop/dissertation/twfe_share.tex",
  replace = TRUE
)


# ---------------------------
# ND --> n.in, n.out
# ---------------------------

# 1. Define Model Sets separately
preds <- c(
  "nd_death10", "nd_death20", "nd_death30",
  "nd_affect10", "nd_affect20", "nd_affect30",
  "nd_both10", "nd_both20", "nd_both30"
)

# Added amount outcomes to the set
outcomes <- c("n.in", "n.out", "amt.in", "amt.out")

# 2. Run models and extract coefficients
coef_df <- expand.grid(
  outcome = outcomes,
  predictor = preds,
  stringsAsFactors = FALSE
) %>%
  rowwise() %>%
  mutate(
    model = list(
      feols(
        as.formula(paste0(outcome, " ~ ", predictor, " | district^party + cycle^party")),
        data = agg,
        warn = FALSE
      )
    ),
    # Ensure confidence intervals are included
    tidy = list(broom::tidy(model, conf.int = TRUE))
  ) %>%
  unnest(tidy) %>%
  filter(term == predictor) %>%
  ungroup()

# 3. Refine formatting for the 2-Panel Aesthetic
plot_data_final <- coef_df %>%
  mutate(
    # Disaster Category for Y-axis
    type = case_when(
      str_detect(predictor, "death")  ~ "Deaths",
      str_detect(predictor, "affect") ~ "Affected",
      str_detect(predictor, "both")   ~ "Both"
    ),
    type = factor(type, levels = rev(c("Affected", "Deaths", "Both"))),
    
    # Threshold order: 10 on top, 30 on bottom
    threshold_fac = factor(str_extract(predictor, "\\d+"), levels = c("30", "20", "10")),
    
    # Define Panels (Counts vs Amounts)
    panel_group = ifelse(str_detect(outcome, "amt"), "Amount", "Count"),
    panel_group = factor(panel_group, levels = c("Amount", "Count")),
    
    # Define Shapes (In-District vs Out-of-District)
    location = ifelse(str_detect(outcome, "\\.in"), "In-District", "Out-of-District"),
    location = factor(location, levels = c("In-District", "Out-of-District"))
  )

# 4. Final Visualization
ggplot(plot_data_final, aes(x = estimate, y = type, color = threshold_fac, shape = location)) +
  geom_vline(xintercept = 0, linetype = "dashed", color = "gray70") +
  
  # Pointrange with dodging
  geom_pointrange(aes(xmin = conf.low, xmax = conf.high), 
                  position = position_dodge(width = 0.75),
                  size = 0.8, fatten = 4) +
  
  # Two panels
  facet_wrap(~ panel_group, scales = "free_x") +
  
  # Professional Threshold Colors
  scale_color_manual(
    values = c("10" = "#53a483", "20" = "#cb6a33", "30" = "#7570b3"),
    name = "Threshold (%)",
    breaks = c("10", "20", "30") # Legend order 10 -> 20 -> 30
  ) +
  
  # Shapes for Donor Location
  scale_shape_manual(
    values = c("In-District" = 16, "Out-of-District" = 17),
    name = "Donor Location"
  ) +
  
  labs(
    x = "Coefficient Estimate (95% CI)",
    y = NULL
  ) +
  
  theme_minimal(base_size = 14) +
  theme(
    legend.position = "bottom",
    legend.box = "horizontal",
    # Compact legend spacing
    legend.spacing.y = unit(0.05, "cm"),
    legend.box.spacing = unit(0.2, "cm"),
    legend.margin = margin(t = 0, r = 0, b = 0, l = 0),
    strip.text = element_text(face = "bold", size = 13),
    axis.text.y = element_text(face = "bold", color = "black"),
    panel.grid.minor = element_blank(),
    panel.grid.major.y = element_line(color = "gray95"),
    panel.spacing = unit(2, "lines")
  )

ggsave(
  filename = "/Users/hyoon/Desktop/dissertation/twferaw_thr.png", 
  plot = last_plot(),       # Use the last plot displayed
  width = 10,               # Width in inches
  height = 7,               # Height in inches
  dpi = 300                 # Set resolution to 300 DPI
)

# reporting

# 1. Define Model Sets (Filtered for 10s) [cite: 2026-02-03]
preds <- c("nd_death10", "nd_affect10", "nd_both10")
outcomes <- c("n.in", "n.out", "amt.in", "amt.out")

# 2. Run models and extract coefficients [cite: 2026-02-03]
coef_df_10 <- expand.grid(
  outcome = outcomes,
  predictor = preds,
  stringsAsFactors = FALSE
) %>%
  rowwise() %>%
  mutate(
    model = list(
      feols(
        as.formula(paste0(outcome, " ~ ", predictor, " | district^party + cycle^party")),
        data = agg,
        warn = FALSE
      )
    ),
    tidy = list(broom::tidy(model, conf.int = TRUE))
  ) %>%
  unnest(tidy) %>%
  filter(term == predictor) %>%
  ungroup()

# 3. Refine formatting for 10% Aesthetic [cite: 2026-02-10]
plot_data_10 <- coef_df_10 %>%
  mutate(
    # Disaster Category for Y-axis
    type = case_when(
      str_detect(predictor, "death")  ~ "Deaths",
      str_detect(predictor, "affect") ~ "Affected",
      str_detect(predictor, "both")   ~ "Both"
    ),
    type = factor(type, levels = rev(c("Affected", "Deaths", "Both"))),
    
    # Define Panels: Amounts on LEFT, Counts on RIGHT [cite: 2026-02-10]
    panel_group = ifelse(str_detect(outcome, "amt"), "Amount", "Count"),
    panel_group = factor(panel_group, levels = c("Amount", "Count")),
    
    # Define Shapes & Colors for Location
    location = ifelse(str_detect(outcome, "\\.in"), "In-District", "Out-of-District"),
    location = factor(location, levels = c("In-District", "Out-of-District"))
  )

# 4. Final Visualization (Single Threshold focus) [cite: 2026-02-03, 2026-02-10]
ggplot(plot_data_10, aes(x = estimate, y = type, color = location, shape = location)) +
  geom_vline(xintercept = 0, linetype = "dashed", color = "gray70") +
  
  # Pointrange with slight dodging to prevent shape overlap [cite: 2026-02-10]
  geom_pointrange(aes(xmin = conf.low, xmax = conf.high), 
                  position = position_dodge(width = 0.5),
                  size = 0.9, fatten = 5) +
  
  # Amount panel on left, Count panel on right [cite: 2026-02-10]
  facet_wrap(~ panel_group, scales = "free_x") +
  
  # Professional Green/Orange Location Palette [cite: 2026-02-10]
  scale_color_manual(values = c("In-District" = "#53a483", "Out-of-District" = "#cb6a33")) +
  scale_shape_manual(values = c("In-District" = 16, "Out-of-District" = 17)) +
  
  labs(
    x = "Coefficient Estimate (95% CI)",
    y = NULL,
    color = "Donor Location",
    shape = "Donor Location"
  ) +
  
  theme_minimal(base_size = 14) +
  theme(
    legend.position = "bottom",
    strip.text = element_text(face = "bold", size = 13),
    axis.text.y = element_text(face = "bold", color = "black"),
    panel.grid.minor = element_blank(),
    panel.grid.major.y = element_line(color = "gray95"),
    panel.spacing = unit(2, "lines")
  )

ggsave(
  filename = "/Users/hyoon/Desktop/dissertation/twferaw.png", 
  plot = last_plot(),       # Use the last plot displayed
  width = 10,               # Width in inches
  height = 7,               # Height in inches
  dpi = 300                 # Set resolution to 300 DPI
)

# table

# Define common settings to keep code clean
fe_structure <- "| district^party + cycle^party"
data_subset <- agg

# --- COUNT MODELS ---
m_counts <- feols(
  c(n.in, n.out) ~ sw(nd_affect10, nd_death10, nd_both10) 
  | district^party + cycle^party,
  data = data_subset
)

# --- AMOUNT MODELS ---
m_amounts <- feols(
  c(amt.in, amt.out) ~ sw(nd_affect10, nd_death10, nd_both10) 
  | district^party + cycle^party,
  data = data_subset
)

# --- COUNT TABLE ---
etable(
  m_counts,
  tex = TRUE,
  se = "cluster",
  cluster = ~ district,
  drop = "Intercept",
  dict = c(
    nd_affect10 = "Affected",
    nd_death10  = "Deaths",
    nd_both10   = "Both",
    n.in        = "In-District",
    n.out       = "Out-of-District"
  ),
  fitstat = ~ n + r2,
  file = "/Users/hyoon/Desktop/dissertation/twfe_counts.tex",
  replace = TRUE
)

# --- AMOUNT TABLE ---
etable(
  m_amounts,
  tex = TRUE,
  se = "cluster",
  cluster = ~ district,
  drop = "Intercept",
  dict = c(
    nd_affect10 = "Affected",
    nd_death10  = "Deaths",
    nd_both10   = "Both",
    amt.in      = "In-District",
    amt.out     = "Out-of-District"
  ),
    fitstat = ~ n + r2,
  file = "/Users/hyoon/Desktop/dissertation/twfe_amounts.tex",
  replace = TRUE
)

m <- feols(
  c(n.in, n.out, amt.in, amt.out) ~ 
    sw(nd_affect10, nd_death10, nd_both10) | 
    district^party + cycle^party,
  data = agg
)

# Models 1, 2, 5, 6, 9, 10 correspond to the Count outcomes
etable(
  m[c(1, 2, 5, 6, 9, 10)],
  tex = TRUE,
  se = "cluster",
  cluster = ~ district,
  drop = "Intercept",
  dict = c(
    nd_affect10 = "Affected",
    nd_death10  = "Deaths",
    nd_both10   = "Both",
    n.in = "Count (In)",
    n.out = "Count (Out)"
  ),
  fitstat = ~ n + r2,
  title = "Impact of Disasters on Donor Counts",
  file = "/Users/hyoon/Desktop/dissertation/twfe_counts.tex",
  replace = TRUE
)

# Models 3, 4, 7, 8, 11, 12 correspond to the Amount outcomes
etable(
  m[c(3, 4, 7, 8, 11, 12)],
  tex = TRUE,
  se = "cluster",
  cluster = ~ district,
  drop = "Intercept",
  dict = c(
    nd_affect10 = "Affected",
    nd_death10  = "Deaths",
    nd_both10   = "Both",
    amt.in = "Amount (In)",
    amt.out = "Amount (Out)"
  ),
  fitstat = ~ n + r2,
  title = "Impact of Disasters on Donation Amounts",
  file = "/Users/hyoon/Desktop/dissertation/twfe_amounts.tex",
  replace = TRUE
)

etable(
  m,
  tex = TRUE,
  se = "cluster",
  cluster = ~ district,
  drop = "Intercept",
  dict = c(
    nd_affect10 = "Affected",
    nd_death10  = "Deaths",
    nd_both10   = "Both"
  ),
  fitstat = ~ n + r2,
  title = "",
  file = "/Users/hyoon/Desktop/dissertation/twfeshare.tex"
)



# ---------------------------
# ND --> ln(n.in+1), ln(n.out+1)
# ---------------------------

agg <- agg %>% mutate(ln_n_in = log(n.in),
                      ln_n_out = log(n.out),
                      ln_amt_in = log(amt.in),
                      ln_amt_out = log(amt.out))

# 1. Define sets and run models [cite: 2026-02-03]
thresholds <- c("10", "20", "30")
types      <- c("affect", "death", "both")
outcomes   <- c("ln_n_in", "ln_n_out", "ln_amt_in", "ln_amt_out")

coef_df <- expand.grid(
  outcome = outcomes,
  type = types,
  threshold = thresholds,
  stringsAsFactors = FALSE
) %>%
  mutate(predictor = paste0("nd_", type, threshold)) %>%
  rowwise() %>%
  mutate(
    model = list(feols(as.formula(paste0(outcome, " ~ ", predictor, " | district^party + cycle^party")), data = agg)),
    tidy = list(broom::tidy(model, conf.int = TRUE))
  ) %>%
  unnest(tidy) %>%
  filter(term == predictor) %>%
  ungroup()

# 2. Refine formatting for Threshold-Color and Location-Shape [cite: 2026-02-10]
plot_data_final <- coef_df %>%
  mutate(
    # Fix NA Panels by mapping to clean labels [cite: 2026-02-10]
    panel_group = ifelse(str_detect(outcome, "amt"), "Log Amount", "Log Count"),
    panel_group = factor(panel_group, levels = c("Log Amount", "Log Count")),
    
    # Map donor location to shape [cite: 2026-02-10]
    location = ifelse(str_detect(outcome, "_in"), "In-District", "Out-of-District"),
    location = factor(location, levels = c("In-District", "Out-of-District")),
    
    # Disaster Category for Y-axis [cite: 2026-02-03]
    clean_type = case_when(
      type == "affect" ~ "Affected",
      type == "death"  ~ "Deaths",
      type == "both"   ~ "Both"
    ),
    clean_type = factor(clean_type, levels = rev(c("Affected", "Deaths", "Both"))),
    
    # Threshold for color and vertical order (10 on top) [cite: 2026-02-10]
    threshold_fac = factor(threshold, levels = c("30", "20", "10"))
  )

# 3. Generate Visualization [cite: 2026-02-03, 2026-02-10]
ggplot(plot_data_final, aes(x = estimate, y = clean_type, color = threshold_fac, shape = location)) +
  # Reference line at zero
  geom_vline(xintercept = 0, linetype = "dashed", color = "gray70") +
  
  # Pointrange with shape mapping and vertical dodging [cite: 2026-02-10]
  geom_pointrange(aes(xmin = conf.low, xmax = conf.high), 
                  position = position_dodge(width = 0.7),
                  size = 0.8, fatten = 4) +
  
  # Multi-panel layout [cite: 2026-02-10]
  facet_wrap(~ panel_group, scales = "free_x") +
  
  # Threshold Colors (Green=10, Orange=20, Purple=30) [cite: 2026-02-10]
  scale_color_manual(
    values = c("10" = "#53a483", "20" = "#cb6a33", "30" = "#7570b3"),
    breaks = c("10", "20", "30"),
    name = "Threshold (%)"
  ) +
  
  # Distinct shapes for In vs Out [cite: 2026-02-10]
  scale_shape_manual(values = c("In-District" = 16, "Out-of-District" = 17), 
                     name = "Donor Location") +
  
  labs(
    x = "Coefficient Estimate (95% CI)",
    y = NULL
  ) +
  
  theme_minimal(base_size = 14) +
  theme(
    legend.position = "bottom",
    legend.box = "horizontal", # Stack the two legends for clarity
    strip.text = element_text(face = "bold", size = 13),
    axis.text.y = element_text(face = "bold", color = "black"),
    panel.grid.minor = element_blank(),
    panel.grid.major.y = element_line(color = "gray95"),
    panel.spacing = unit(2, "lines")
  )

ggsave(
  filename = "/Users/hyoon/Desktop/dissertation/twfelog_thr.png", 
  plot = last_plot(),       # Use the last plot displayed
  width = 10,               # Width in inches
  height = 7,               # Height in inches
  dpi = 300                 # Set resolution to 300 DPI
)



## reporting

# 1. Define sets and run models for 10%
types      <- c("affect", "death", "both")
outcomes   <- c("ln_n_in", "ln_n_out", "ln_amt_in", "ln_amt_out")

coef_df_10 <- expand.grid(
  outcome = outcomes,
  type = types,
  threshold = "10",
  stringsAsFactors = FALSE
) %>%
  mutate(predictor = paste0("nd_", type, threshold)) %>%
  rowwise() %>%
  mutate(
    model = list(feols(as.formula(paste0(outcome, " ~ ", predictor, " | district^party + cycle^party")), data = agg)),
    tidy = list(broom::tidy(model, conf.int = TRUE))
  ) %>%
  unnest(tidy) %>%
  filter(term == predictor) %>%
  ungroup()

# 2. Refine formatting
plot_data_10 <- coef_df_10 %>%
  mutate(
    # Fix NA Headers
    panel_group = ifelse(str_detect(outcome, "amt"), "Log Amount", "Log Count"),
    panel_group = factor(panel_group, levels = c("Log Amount", "Log Count")),
    
    # Location mapping
    location = ifelse(str_detect(outcome, "_in"), "In-District", "Out-of-District"),
    location = factor(location, levels = c("In-District", "Out-of-District")),
    
    # Category mapping
    clean_type = case_when(
      type == "affect" ~ "Affected",
      type == "death"  ~ "Deaths",
      type == "both"   ~ "Both"
    ),
    clean_type = factor(clean_type, levels = rev(c("Affected", "Deaths", "Both")))
  )

# 3. Final Plot with dodging to prevent overlap
ggplot(plot_data_10, aes(x = estimate, y = clean_type, color = location, shape = location)) +
  geom_vline(xintercept = 0, linetype = "dashed", color = "gray70") +
  
  # Added position_dodge to separate In-District and Out-of-District vertically
  geom_pointrange(aes(xmin = conf.low, xmax = conf.high), 
                  position = position_dodge(width = 0.5),
                  size = 0.8, fatten = 4) +
  
  facet_wrap(~ panel_group, scales = "free_x") +
  
  # Your established Green/Orange palette
  scale_color_manual(values = c("In-District" = "#53a483", "Out-of-District" = "#cb6a33")) +
  scale_shape_manual(values = c("In-District" = 16, "Out-of-District" = 17)) +
  
  labs(
    x = "Coefficient Estimate (95% CI)",
    y = NULL,
    color = "Donor Location",
    shape = "Donor Location"
  ) +
  
  theme_minimal(base_size = 14) +
  theme(
    legend.position = "bottom",
    strip.text = element_text(face = "bold", size = 13),
    axis.text.y = element_text(face = "bold", color = "black"),
    panel.grid.minor = element_blank(),
    panel.grid.major.y = element_line(color = "gray95"),
    panel.spacing = unit(2, "lines")
  )

ggsave(
  filename = "/Users/hyoon/Desktop/dissertation/twfelog.png", 
  plot = last_plot(),       # Use the last plot displayed
  width = 10,               # Width in inches
  height = 7,               # Height in inches
  dpi = 300                 # Set resolution to 300 DPI
)

# table

# Set 1: Log Counts - In-District
m_n_in <- list(
  feols(ln_n_in ~ nd_affect10 | district^party + cycle^party, data = agg),
  feols(ln_n_in ~ nd_death10  | district^party + cycle^party, data = agg),
  feols(ln_n_in ~ nd_both10   | district^party + cycle^party, data = agg)
)

# Set 2: Log Counts - Out-of-District
m_n_out <- list(
  feols(ln_n_out ~ nd_affect10 | district^party + cycle^party, data = agg),
  feols(ln_n_out ~ nd_death10  | district^party + cycle^party, data = agg),
  feols(ln_n_out ~ nd_both10   | district^party + cycle^party, data = agg)
)

m_counts_final <- c(m_n_in, m_n_out)

etable(
  m_counts_final,
  tex = TRUE,
  style.tex = style.tex(main = "base", tabular = "normal"),
  postprocess.tex = function(x) {
    x = x[!grepl("\\\\begin\\{table\\}|\\\\end\\{table\\}|\\\\caption", x)]
    x
  },
  se = "cluster", cluster = ~ district, drop = "Intercept",
  dict = c(
    nd_affect10 = "Affected", 
    nd_death10  = "Deaths", 
    nd_both10   = "Both",
    ln_n_in     = "In-District",
    ln_n_out    = "Out-of-District"
  ),
  fitstat = ~ n + r2,
  file = "/Users/hyoon/Desktop/dissertation/table_counts.tex",
  replace = TRUE
)

# Set 1: Log Amounts - In-District
m_amt_in <- list(
  feols(ln_amt_in ~ nd_affect10 | district^party + cycle^party, data = agg),
  feols(ln_amt_in ~ nd_death10  | district^party + cycle^party, data = agg),
  feols(ln_amt_in ~ nd_both10   | district^party + cycle^party, data = agg)
)

# Set 2: Log Amounts - Out-of-District
m_amt_out <- list(
  feols(ln_amt_out ~ nd_affect10 | district^party + cycle^party, data = agg),
  feols(ln_amt_out ~ nd_death10  | district^party + cycle^party, data = agg),
  feols(ln_amt_out ~ nd_both10   | district^party + cycle^party, data = agg)
)

m_amounts_final <- c(m_amt_in, m_amt_out)

etable(
  m_amounts_final,
  tex = TRUE,
  style.tex = style.tex(main = "base", tabular = "normal"),
  postprocess.tex = function(x) {
    x = x[!grepl("\\\\begin\\{table\\}|\\\\end\\{table\\}|\\\\caption", x)]
    x
  },
  se = "cluster", cluster = ~ district, drop = "Intercept",
  dict = c(
    nd_affect10 = "Affected", 
    nd_death10  = "Deaths", 
    nd_both10   = "Both",
    ln_amt_in     = "In-District",
    ln_amt_out    = "Out-of-District"
  ),
  fitstat = ~ n + r2,
  file = "/Users/hyoon/Desktop/dissertation/table_amounts.tex",
  replace = TRUE
)


# ---------------------------
# ND --> ab.cfscore, ab.dwdime, ab.dw1, ab.dist, ab.dist.dwdime, ab.dist.dw1
# ---------------------------

# all

library(fixest)
library(dplyr)
library(tidyr)
library(broom)
library(stringr)
library(ggplot2)

# 1. Define Model Sets separately [cite: 2026-02-03]
preds <- c(
  "nd_death10", "nd_death20", "nd_death30",
  "nd_affect10", "nd_affect20", "nd_affect30",
  "nd_both10", "nd_both20", "nd_both30"
)
outcomes <- c("dwdime")
parties <- c(100, 200)

# 2. Run models and extract coefficients [cite: 2026-02-03]
party_coef_df <- expand.grid(
  outcome = outcomes,
  predictor = preds,
  party_code = parties,
  stringsAsFactors = FALSE
) %>%
  rowwise() %>%
  mutate(
    model = list(feols(as.formula(paste0(outcome, " ~ ", predictor, " | district + cycle")), 
                       data = winner %>% filter(party == party_code))),
    tidy = list(broom::tidy(model, conf.int = TRUE))
  ) %>%
  unnest(tidy) %>%
  filter(term == predictor) %>%
  ungroup()

# 3. Refine for Professional Aesthetic [cite: 2026-02-10]
plot_party_data <- party_coef_df %>%
  mutate(
    # Disaster Category for Y-axis
    clean_type = case_when(
      str_detect(predictor, "affect") ~ "Affected",
      str_detect(predictor, "death")  ~ "Deaths",
      str_detect(predictor, "both")   ~ "Both"
    ),
    clean_type = factor(clean_type, levels = rev(c("Affected", "Deaths", "Both"))),
    
    # THRESHOLD ORDER FIX: Set levels so 10 is on top of the cluster [cite: 2026-02-10]
    threshold = str_extract(predictor, "\\d+"),
    threshold_fac = factor(threshold, levels = c("30", "20", "10")),
    
    # Party labels
    party_label = ifelse(party_code == 100, "Democrat", "Republican"),
    party_label = factor(party_label, levels = c("Democrat", "Republican")),
    
    # Clean facet title
    outcome_label = ""
  )

# 4. Generate Visualization [cite: 2026-02-03, 2026-02-10]
ggplot(plot_party_data, aes(x = estimate, y = clean_type, color = threshold_fac)) +
  geom_vline(xintercept = 0, linetype = "dashed", color = "gray70") +
  
  # Pointrange with dodged vertical order (10 on top) [cite: 2026-02-10]
  geom_pointrange(aes(xmin = conf.low, xmax = conf.high), 
                  position = position_dodge(width = 0.6),
                  size = 0.7, fatten = 4) +
  
  facet_grid(outcome_label ~ party_label, scales = "free_x") +
  
  # Professional Threshold Colors [cite: 2026-02-10]
  scale_color_manual(
    values = c("10" = "#53a483", "20" = "#cb6a33", "30" = "#7570b3"),
    breaks = c("10", "20", "30"), # Legend displays 10-20-30 order
    name = "Threshold (%)"
  ) +
  
  labs(x = "Coefficient Estimate (95% CI)", y = NULL) +
  
  theme_minimal(base_size = 14) +
  theme(
    legend.position = "bottom",
    strip.text = element_text(face = "bold", size = 12),
    axis.text.y = element_text(face = "bold", color = "black"),
    panel.grid.minor = element_blank(),
    panel.grid.major.y = element_line(color = "gray95"),
    panel.spacing = unit(1.5, "lines")
  )

ggsave(
  filename = "/Users/hyoon/Desktop/dissertation/twfeideo_thr.png", 
  plot = last_plot(),       # Use the last plot displayed
  width = 10,               # Width in inches
  height = 7,               # Height in inches
  dpi = 300                 # Set resolution to 300 DPI
)

# reporting

# 1. Define Model Sets (Filtered for 10s) [cite: 2026-02-03]
preds <- c("nd_death10", "nd_affect10", "nd_both10")
outcomes <- c("dwdime")
parties <- c(100, 200)

# 2. Run models and extract coefficients [cite: 2026-02-03]
party_coef_10 <- expand.grid(
  outcome = outcomes,
  predictor = preds,
  party_code = parties,
  stringsAsFactors = FALSE
) %>%
  rowwise() %>%
  mutate(
    model = list(feols(as.formula(paste0(outcome, " ~ ", predictor, " | district + cycle")), 
                       data = winner %>% filter(party == party_code))),
    tidy = list(broom::tidy(model, conf.int = TRUE))
  ) %>%
  unnest(tidy) %>%
  filter(term == predictor) %>%
  ungroup()

# 3. Refine for 10% Results Aesthetic [cite: 2026-02-10]
plot_party_10 <- party_coef_10 %>%
  mutate(
    # Disaster Category for Y-axis [cite: 2026-02-03]
    clean_type = case_when(
      str_detect(predictor, "affect") ~ "Affected",
      str_detect(predictor, "death")  ~ "Deaths",
      str_detect(predictor, "both")   ~ "Both"
    ),
    clean_type = factor(clean_type, levels = rev(c("Affected", "Deaths", "Both"))),
    
    # Party labels for columns [cite: 2026-02-10]
    party_label = ifelse(party_code == 100, "Democrat", "Republican"),
    party_label = factor(party_label, levels = c("Democrat", "Republican")),
    
    # Fixed Label for Outcome
    outcome_label = ""
  )

# 4. Generate Final Visualization [cite: 2026-02-03, 2026-02-10]
ggplot(plot_party_10, aes(x = estimate, y = clean_type)) +
  geom_vline(xintercept = 0, linetype = "dashed", color = "gray70") +
  
  # Professional Result Aesthetic: Green points for 10% results [cite: 2026-02-10]
  geom_pointrange(aes(xmin = conf.low, xmax = conf.high), 
                  color = "#53a483", # Consistent 10% Green [cite: 2026-02-10]
                  size = 0.9, fatten = 5) +
  
  # Rows = Ideology Measure, Cols = Party [cite: 2026-02-10]
  facet_grid(outcome_label ~ party_label, scales = "free_x") +
  
  labs(
    x = "Coefficient Estimate (95% CI)",
    y = NULL
  ) +
  
  theme_minimal(base_size = 14) +
  theme(
    strip.text = element_text(face = "bold", size = 12),
    axis.text.y = element_text(face = "bold", color = "black"),
    panel.grid.minor = element_blank(),
    panel.grid.major.y = element_line(color = "gray95"),
    panel.spacing = unit(1.5, "lines")
  )

ggsave(
  filename = "/Users/hyoon/Desktop/dissertation/twfeideo.png", 
  plot = last_plot(),       # Use the last plot displayed
  width = 10,               # Width in inches
  height = 7,               # Height in inches
  dpi = 300                 # Set resolution to 300 DPI
)

# table

# Set 1: Democrats (party == 100)
m_dem_list <- list(
  feols(ab.dist.dw1 ~ nd_affect10 | district^party + cycle^party, data = winner, subset = ~ party == 100),
  feols(ab.dist.dw1 ~ nd_death10  | district^party + cycle^party, data = winner, subset = ~ party == 100),
  feols(ab.dist.dw1 ~ nd_both10   | district^party + cycle^party, data = winner, subset = ~ party == 100)
)

# Set 2: Republicans (party == 200)
m_rep_list <- list(
  feols(ab.dist.dw1 ~ nd_affect10 | district^party + cycle^party, data = winner, subset = ~ party == 200),
  feols(ab.dist.dw1 ~ nd_death10  | district^party + cycle^party, data = winner, subset = ~ party == 200),
  feols(ab.dist.dw1 ~ nd_both10   | district^party + cycle^party, data = winner, subset = ~ party == 200)
)

# Combine into master list
m_party_final <- c(m_dem_list, m_rep_list)

etable(
  m_party_final,
  tex = TRUE,
  style.tex = style.tex(main = "base", tabular = "normal"),
  postprocess.tex = function(x) {
    x = x[!grepl("\\\\begin\\{table\\}|\\\\end\\{table\\}|\\\\caption", x)]
    x
  },
  se = "cluster", 
  cluster = ~ district, 
  drop = "Intercept",
  dict = c(
    nd_affect10 = "Affected", 
    nd_death10  = "Deaths", 
    nd_both10   = "Both",
    ab.dist.dw1 = "Ideological Dist."
  ),
  headers = list("Party" = c("Democrats" = 3, "Republicans" = 3)),
  fitstat = ~ n + r2,
  file = "/Users/hyoon/Desktop/dissertation/table_party_dw.tex",
  replace = TRUE
)


# ---------------------------
# ND --> prob_cf, prob_dw1, prob_dw2, prob_dd (by party)
# ---------------------------

# all

# 1. Define Model Sets separately [cite: 2026-02-03]
preds <- c(
  "nd_death10", "nd_death20", "nd_death30",
  "nd_affect10", "nd_affect20", "nd_affect30",
  "nd_both10", "nd_both20", "nd_both30"
)

outcomes <- c(
              # "prob_cf",
              # "prob_dw1",
              # "prob_dw2",
              "prob_dd")
parties  <- c(100, 200)

# 2. Run models and extract coefficients [cite: 2026-02-03]
prob_party_coefs <- expand.grid(
  outcome = outcomes,
  predictor = preds,
  party_code = parties,
  stringsAsFactors = FALSE
) %>%
  rowwise() %>%
  mutate(
    model = list(
      feols(
        as.formula(paste0(outcome, " ~ ", predictor, " | district + cycle")), 
        data = prob_anl_dan %>% filter(party == party_code)
      )
    ),
    tidy = list(broom::tidy(model, conf.int = TRUE))
  ) %>%
  unnest(tidy) %>%
  filter(term == predictor) %>%
  ungroup()

# 3. Refine for Professional Partisan Aesthetic [cite: 2026-02-10]
plot_data_final <- prob_party_coefs %>%
  mutate(
    # Vertical Category
    clean_type = case_when(
      str_detect(predictor, "affect") ~ "Affected",
      str_detect(predictor, "death")  ~ "Deaths",
      str_detect(predictor, "both")   ~ "Both"
    ),
    clean_type = factor(clean_type, levels = rev(c("Affected", "Deaths", "Both"))),
    
    # THRESHOLD ORDER: 10 on top, 30 on bottom [cite: 2026-02-10]
    threshold = str_extract(predictor, "\\d+"),
    threshold_fac = factor(threshold, levels = c("30", "20", "10")),
    
    # Party labels for columns
    party_label = ifelse(party_code == 200, "Democrat", "Republican"),
    party_label = factor(party_label, levels = c("Democrat", "Republican")),
    
    # Clean Outcome Labels for rows [cite: 2026-02-10]
    outcome_label = case_when(
      # outcome == "prob_cf"  ~ "CFscore",
      # outcome == "prob_dw1" ~ "DW-Nom 1",
      # outcome == "prob_dw2"  ~ "DW-Nom 2",
      outcome == "prob_dd"  ~ "DW-DIME"
    ),
    outcome_label = factor(outcome_label, levels = c(
      # "CFscore", "DW-Nom 1", "DW-Nom 2", 
      "DW-DIME"))
  )

# 4. Generate Visualization [cite: 2026-02-03, 2026-02-10]
ggplot(plot_data_final, aes(x = estimate, y = clean_type, color = threshold_fac)) +
  geom_vline(xintercept = 0, linetype = "dashed", color = "gray70") +
  
  # Pointrange with dodged vertical order [cite: 2026-02-10]
  geom_pointrange(aes(xmin = conf.low, xmax = conf.high), 
                  position = position_dodge(width = 0.7),
                  size = 0.5, fatten = 3) +
  
  # Grid: Rows = Measures, Columns = Party [cite: 2026-02-10]
  facet_grid(. ~ party_label, scales = "free_x") +
  
  # Professional Threshold Colors [cite: 2026-02-10]
  scale_color_manual(
    values = c("10" = "#53a483", "20" = "#cb6a33", "30" = "#7570b3"),
    breaks = c("10", "20", "30"),
    name = "Threshold (%)"
  ) +
  
  labs(
    x = "Coefficient Estimate (95% CI)",
    y = NULL
  ) +
  
  theme_minimal(base_size = 13) +
  theme(
    legend.position = "bottom",
    strip.text = element_text(face = "bold", size = 10),
    axis.text.y = element_text(face = "bold", color = "black"),
    panel.grid.minor = element_blank(),
    panel.grid.major.y = element_line(color = "gray95"),
    panel.spacing = unit(1, "lines")
  )

ggsave(
  filename = "/Users/hyoon/Desktop/dissertation/twfeprob_thr.png", 
  plot = last_plot(),       # Use the last plot displayed
  width = 10,               # Width in inches
  height = 7,               # Height in inches
  dpi = 300                 # Set resolution to 300 DPI
)

## reporting

# 1. Define Model Sets (Filtered for 10s) [cite: 2026-02-03]
preds <- c("nd_death10", "nd_affect10", "nd_both10")
outcomes <- c("prob_dd") # Focusing on DW-DIME as requested
parties  <- c(100, 200)

# 2. Run models and extract coefficients [cite: 2026-02-03]
prob_party_10 <- expand.grid(
  outcome = outcomes,
  predictor = preds,
  party_code = parties,
  stringsAsFactors = FALSE
) %>%
  rowwise() %>%
  mutate(
    model = list(
      feols(
        as.formula(paste0(outcome, " ~ ", predictor, " | district + cycle")), 
        data = prob_anl_dan %>% filter(party == party_code)
      )
    ),
    tidy = list(broom::tidy(model, conf.int = TRUE))
  ) %>%
  unnest(tidy) %>%
  filter(term == predictor) %>%
  ungroup()

# 3. Refine Formatting [cite: 2026-02-10]
plot_data_10 <- prob_party_10 %>%
  mutate(
    # Disaster Category for Y-axis
    clean_type = case_when(
      str_detect(predictor, "affect") ~ "Affected",
      str_detect(predictor, "death")  ~ "Deaths",
      str_detect(predictor, "both")   ~ "Both"
    ),
    clean_type = factor(clean_type, levels = rev(c("Affected", "Deaths", "Both"))),
    
    # Party labels for columns
    party_label = ifelse(party_code == 200, "Democrat", "Republican"),
    party_label = factor(party_label, levels = c("Democrat", "Republican"))
  )

# 4. Generate Final Visualization [cite: 2026-02-03, 2026-02-10]
ggplot(plot_data_10, aes(x = estimate, y = clean_type)) +
  # Reference line at zero
  geom_vline(xintercept = 0, linetype = "dashed", color = "gray70") +
  
  # Pointrange: Professional 10% Green Aesthetic [cite: 2026-02-10]
  geom_pointrange(aes(xmin = conf.low, xmax = conf.high), 
                  color = "#53a483", 
                  size = 0.9, fatten = 5) +
  
  # Facet columns by Party (removes the right-side Y label) [cite: 2026-02-10]
  facet_wrap(~ party_label, scales = "free_x") +
  
  labs(
    x = "Coefficient Estimate (95% CI)",
    y = NULL, # Removes the Y-axis title ("clean_type")
  ) +
  
  theme_minimal(base_size = 14) +
  theme(
    strip.text = element_text(face = "bold", size = 12),
    axis.text.y = element_text(face = "bold", color = "black"),
    panel.grid.minor = element_blank(),
    panel.grid.major.y = element_line(color = "gray95"),
    panel.spacing = unit(2, "lines")
  )

ggsave(
  filename = "/Users/hyoon/Desktop/dissertation/twfeprob.png", 
  plot = last_plot(),       # Use the last plot displayed
  width = 10,               # Width in inches
  height = 7,               # Height in inches
  dpi = 300                 # Set resolution to 300 DPI
)

# table

# Set 1: Democrats (party == 200)
m_dem_prob <- list(
  feols(prob_dd ~ nd_affect10 | district^party + cycle^party, data = prob_anl_dan, subset = ~ party == 200),
  feols(prob_dd ~ nd_death10  | district^party + cycle^party, data = prob_anl_dan, subset = ~ party == 200),
  feols(prob_dd ~ nd_both10   | district^party + cycle^party, data = prob_anl_dan, subset = ~ party == 200)
)

# Set 2: Republicans (party == 100)
m_rep_prob <- list(
  feols(prob_dd ~ nd_affect10 | district^party + cycle^party, data = prob_anl_dan, subset = ~ party == 100),
  feols(prob_dd ~ nd_death10  | district^party + cycle^party, data = prob_anl_dan, subset = ~ party == 100),
  feols(prob_dd ~ nd_both10   | district^party + cycle^party, data = prob_anl_dan, subset = ~ party == 100)
)

# Combine into master list for the table
m_prob_final <- c(m_dem_prob, m_rep_prob)

etable(
  m_prob_final,
  tex = TRUE,
  style.tex = style.tex(main = "base", tabular = "normal"),
  postprocess.tex = function(x) {
    # Stripping the table environment as per your saved style
    x = x[!grepl("\\\\begin\\{table\\}|\\\\end\\{table\\}|\\\\caption", x)]
    x
  },
  se = "cluster", 
  cluster = ~ district, 
  drop = "Intercept",
  dict = c(
    nd_affect10 = "Affected", 
    nd_death10  = "Deaths", 
    nd_both10   = "Both",
    prob_dd    = "Prob. Extreme"
  ),
  # Explicitly labeling the columns by party
  headers = list("Party" = c("Democrats" = 3, "Republicans" = 3)),
  fitstat = ~ n + r2,
  file = "/Users/hyoon/Desktop/dissertation/table_prob_extreme.tex",
  replace = TRUE
)



##################################-----------------------------
# DID ND --> SHARE
##################################-----------------------------

library(did2s)
library(purrr)
library(dplyr)
library(broom)

# Define the components of your predictor set
thresholds <- c(10, 20, 30)
types <- c("both", "death", "affect")
outcomes <- c("pct.amt.out", "pct.out")

# Create a grid of all combinations
model_grid <- expand.grid(type = types, threshold = thresholds, yname = outcomes, stringsAsFactors = FALSE)

# Run the 2-stage DiD for all combinations
all_results <- model_grid %>%
  transpose() %>%
  map_df(~{
    iv_name <- paste0("nd_", .x$type, .x$threshold)
    
    # Run did2s
    model <- did2s(
      data = agg,
      yname = .x$yname,
      first_stage = ~ 1 | district^party + cycle^party,
      second_stage = as.formula(paste0("~ i(", iv_name, ", ref=0)")),
      treatment = iv_name,
      cluster_var = "district"
    )
    
    # Clean up results for the master list
    tidy(model) %>%
      filter(term != "(Intercept)") %>%
      mutate(
        type = .x$type,
        threshold = .x$threshold,
        outcome = ifelse(.x$yname == "pct.amt.out", "% Out (Amount)", "% Out (Number)")
      )
  })


# 1. Prepare and Reorder the Data
att_10_df <- all_results %>%
  filter(threshold == 10) %>%
  mutate(
    # Clean labels for the plot
    type_label = case_when(
      type == "both"   ~ "Deaths + Affected",
      type == "death"  ~ "Deaths Only",
      type == "affect" ~ "Affected Only"
    )
  )

# 1. Prepare and Reorder the Data
plot_10_clean <- att_10_df %>%
  mutate(
    # Set the order and the simplified labels simultaneously
    # 'Affected' will be at the bottom, 'Both' at the top
    type_clean = factor(type, 
                        levels = c("both", "death", "affect"),
                        labels = c("Both", "Deaths", "Affected")),
    # Ensure outcome labels match your panel preferences
    outcome_label = ifelse(outcome == "% Out (Amount)", "Amount", "Count")
  )

# 2. Generate the Two-Panel Aesthetic Plot
ggplot(plot_10_clean, aes(x = estimate, y = type_clean, color = type_clean)) +
  # Reference line for null hypothesis
  geom_vline(xintercept = 0, linetype = "dashed", color = "grey50") + 
  # Points with specific size and dodge
  geom_point(position = position_dodge(width = 0.7), size = 2.6) + 
  # Error bars with height = 0 for the clean horizontal look
  geom_errorbarh(
    aes(xmin = estimate - 1.96 * std.error, xmax = estimate + 1.96 * std.error),
    height = 0, 
    position = position_dodge(width = 0.7)
  ) + 
  # Facet by outcome (Amount vs Number)
  facet_wrap(~ outcome_label, scales = "free_x") +
  # Apply your specific color palette with simplified labels
  scale_color_manual(values = c("Deaths" = "#d73027", "Affected" = "#4575b4", "Both" = "#7b3294")) + 
  labs(
    x = "Coefficient estimate", 
    y = NULL,
    title = ""
  ) + 
  theme_minimal(base_size = 13) + 
  theme(
    legend.position = "none",        
    panel.grid.minor = element_blank(),
    strip.text = element_text(face = "bold", size = 12, color = "black")
  )

ggsave(
  filename = "/Users/hyoon/Desktop/dissertation/did_share.png", 
  plot = last_plot(),       # Use the last plot displayed
  width = 10,               # Width in inches
  height = 7,               # Height in inches
  dpi = 300                 # Set resolution to 300 DPI
)

##################################-----------------------------
# 1-DID: NUMBER in/out
##################################-----------------------------

did_both_n_in <- did2s(
  data = agg,
  yname = "n.in",                      # outcome
  first_stage = ~ 1 | district^party+cycle^party,     # first-stage controls/FE
  second_stage = ~ i(nd_both10, ref=0),              # treatment variable
  treatment = "nd_both10",                 # binary treatment indicator
  cluster_var = "district"                # cluster SEs by district
) 

did_both_n_out <- did2s(
  data = agg,
  yname = "n.out",                      # outcome
  first_stage = ~ 1 | district^party+cycle^party,     # first-stage controls/FE
  second_stage = ~ i(nd_both10, ref=0),              # treatment variable
  treatment = "nd_both10",                 # binary treatment indicator
  cluster_var = "district"                # cluster SEs by district
) 

did_death_n_in <- did2s(
  data = agg,
  yname = "n.in",                      # outcome
  first_stage = ~ 1 | district^party+cycle^party,     # first-stage controls/FE
  second_stage = ~ i(nd_death10, ref=0),              # treatment variable
  treatment = "nd_death10",                 # binary treatment indicator
  cluster_var = "district"                # cluster SEs by district
) 

did_death_n_out <- did2s(
  data = agg,
  yname = "n.out",                      # outcome
  first_stage = ~ 1 | district^party+cycle^party,     # first-stage controls/FE
  second_stage = ~ i(nd_death10, ref=0),              # treatment variable
  treatment = "nd_death10",                 # binary treatment indicator
  cluster_var = "district"                # cluster SEs by district
) 

did_affect_n_in <- did2s(
  data = agg,
  yname = "n.in",                      # outcome
  first_stage = ~ 1 | district^party+cycle^party,     # first-stage controls/FE
  second_stage = ~ i(nd_affect10, ref=0),              # treatment variable
  treatment = "nd_affect10",                 # binary treatment indicator
  cluster_var = "district"                # cluster SEs by district
) 

did_affect_n_out <- did2s(
  data = agg,
  yname = "n.out",                      # outcome
  first_stage = ~ 1 | district^party+cycle^party,     # first-stage controls/FE
  second_stage = ~ i(nd_affect10, ref=0),              # treatment variable
  treatment = "nd_affect10",                 # binary treatment indicator
  cluster_var = "district"                # cluster SEs by district
) 

att_df_n <- bind_rows(
  tidy_did(did_both_n_in, "N In", "Deaths + Affected"),
  tidy_did(did_both_n_out, "N Out", "Deaths + Affected"),
  
  tidy_did(did_death_n_in, "N In", "Deaths"),
  tidy_did(did_death_n_out, "N Out", "Deaths"),
  
  tidy_did(did_affect_n_in, "N In", "Affected"),
  tidy_did(did_affect_n_out, "N Out", "Affected")
)

ggplot(att_df_n, aes(x = estimate, y = outcome, color = outcome)) +
  geom_point(size = 3) +
  geom_errorbarh(aes(xmin = estimate - 1.96*std.error,
                     xmax = estimate + 1.96*std.error), height = 0.2) +
  geom_vline(xintercept = 0, linetype = "dashed") +
  facet_wrap(~ iv, scales = "free_x") +
  scale_color_manual(values = c("N In" = "cadetblue",
                                "N Out" = "violetred")) +
  labs(x = "ATT estimate", y = "", color = "Outcome") +
  theme_minimal() +
  theme(legend.position = "bottom",
        strip.text = element_text(face = "bold"))

##################################-----------------------------
# 1-DID: AMT in/out
##################################-----------------------------

did_both_amt_in <- did2s(
  data = agg,
  yname = "amt.in",                      # outcome
  first_stage = ~ 1 | district^party+cycle^party,     # first-stage controls/FE
  second_stage = ~ i(nd_both10, ref=0),              # treatment variable
  treatment = "nd_both10",                 # binary treatment indicator
  cluster_var = "district"                # cluster SEs by district
) 

did_both_amt_out <- did2s(
  data = agg,
  yname = "amt.out",                      # outcome
  first_stage = ~ 1 | district^party+cycle^party,     # first-stage controls/FE
  second_stage = ~ i(nd_both10, ref=0),              # treatment variable
  treatment = "nd_both10",                 # binary treatment indicator
  cluster_var = "district"                # cluster SEs by district
) 

did_death_amt_in <- did2s(
  data = agg,
  yname = "amt.in",                      # outcome
  first_stage = ~ 1 | district^party+cycle^party,     # first-stage controls/FE
  second_stage = ~ i(nd_death10, ref=0),              # treatment variable
  treatment = "nd_death10",                 # binary treatment indicator
  cluster_var = "district"                # cluster SEs by district
) 

did_death_amt_out <- did2s(
  data = agg,
  yname = "amt.out",                      # outcome
  first_stage = ~ 1 | district^party+cycle^party,     # first-stage controls/FE
  second_stage = ~ i(nd_death10, ref=0),              # treatment variable
  treatment = "nd_death10",                 # binary treatment indicator
  cluster_var = "district"                # cluster SEs by district
) 

did_affect_amt_in <- did2s(
  data = agg,
  yname = "amt.in",                      # outcome
  first_stage = ~ 1 | district^party+cycle^party,     # first-stage controls/FE
  second_stage = ~ i(nd_affect10, ref=0),              # treatment variable
  treatment = "nd_affect10",                 # binary treatment indicator
  cluster_var = "district"                # cluster SEs by district
) 

did_affect_amt_out <- did2s(
  data = agg,
  yname = "amt.out",                      # outcome
  first_stage = ~ 1 | district^party+cycle^party,     # first-stage controls/FE
  second_stage = ~ i(nd_affect10, ref=0),              # treatment variable
  treatment = "nd_affect10",                 # binary treatment indicator
  cluster_var = "district"                # cluster SEs by district
) 

att_df_amt <- bind_rows(
  tidy_did(did_both_amt_in, "Amt In", "Deaths + Affected"),
  tidy_did(did_both_amt_out, "Amt Out", "Deaths + Affected"),
  
  tidy_did(did_death_amt_in, "Amt In", "Deaths"),
  tidy_did(did_death_amt_out, "Amt Out", "Deaths"),
  
  tidy_did(did_affect_amt_in, "Amt In", "Affected"),
  tidy_did(did_affect_amt_out, "Amt Out", "Affected")
)

ggplot(att_df_amt, aes(x = estimate, y = outcome, color = outcome)) +
  geom_point(size = 3) +
  geom_errorbarh(aes(xmin = estimate - 1.96*std.error,
                     xmax = estimate + 1.96*std.error), height = 0.2) +
  geom_vline(xintercept = 0, linetype = "dashed") +
  facet_wrap(~ iv, scales = "free_x") +
  scale_color_manual(values = c("Amt In" = "cadetblue",
                                "Amt Out" = "violetred")) +
  labs(x = "ATT estimate", y = "", color = "Outcome") +
  theme_minimal() +
  theme(legend.position = "bottom",
        strip.text = element_text(face = "bold"))


##################################-----------------------------
# 1-DID: log in/out
##################################-----------------------------

# master

library(did2s)
library(dplyr)
library(purrr)
library(ggplot2)
library(stringr)
library(broom)

# 1. Define Model Sets
thresholds <- c(10, 20, 30)
types      <- c("both", "death", "affect")
outcomes   <- c("ln_amt_in", "ln_amt_out", "ln_n_in", "ln_n_out")

model_grid <- expand.grid(type = types, threshold = thresholds, yname = outcomes, stringsAsFactors = FALSE)

# 2. Run models
all_logged_results <- model_grid %>%
  split(seq(nrow(.))) %>%
  map_df(~{
    iv_name <- paste0("nd_", .x$type, .x$threshold)
    model <- did2s(
      data = agg,
      yname = .x$yname,
      first_stage = ~ 1 | district^party + cycle^party,
      second_stage = as.formula(paste0("~ i(", iv_name, ", ref=0)")),
      treatment = iv_name,
      cluster_var = "district"
    )
    tidy(model, conf.int = TRUE) %>%
      filter(term != "(Intercept)") %>%
      mutate(
        type = .x$type,
        threshold = as.character(.x$threshold),
        yname = .x$yname,
        panel_group = ifelse(str_detect(.x$yname, "ln_amt"), "Log Amount", "Log Count"),
        location = ifelse(str_detect(.x$yname, "_in"), "In-District", "Out-of-District")
      )
  })

# 3. Refine formatting
plot_final_df <- all_logged_results %>%
  mutate(
    # Row Variable (Y-axis): Disaster Type
    type_fac = factor(type, 
                      levels = rev(c("affect", "death", "both")),
                      labels = rev(c("Affected", "Deaths", "Both"))),
    
    # Column Variable: Log Amount on Left
    panel_group = factor(panel_group, levels = c("Log Amount", "Log Count")),
    
    # Threshold order for vertical dodge: 10 on top
    threshold_fac = factor(threshold, levels = c("30", "20", "10")),
    
    # Location for shape mapping
    location = factor(location, levels = c("In-District", "Out-of-District"))
  )

# 4. Generate Visualization
ggplot(plot_final_df, aes(x = estimate, y = type_fac, color = threshold_fac, shape = location)) +
  geom_vline(xintercept = 0, linetype = "dashed", color = "gray70") + 
  
  # Pointrange with wide dodge to separate thresholds vertically within each row
  geom_pointrange(aes(xmin = conf.low, xmax = conf.high), 
                  position = position_dodge(width = 0.8),
                  size = 0.7, fatten = 4) +
  
  # Facet columns only: Log Amount (Left) vs Log Count (Right)
  facet_wrap(~ panel_group, scales = "free_x") +
  
  # Professional Palette (Green=10, Orange=20, Purple=30)
  scale_color_manual(
    values = c("10" = "#53a483", "20" = "#cb6a33", "30" = "#7570b3"),
    breaks = c("10", "20", "30"),
    name = "Disaster Threshold (%)"
  ) +
  
  scale_shape_manual(
    values = c("In-District" = 16, "Out-of-District" = 17),
    name = "Donor Location"
  ) +
  
  labs(x = "Coefficient Estimate (ATT)", y = NULL) +
  
  theme_minimal(base_size = 14) + 
  theme(
    legend.position = "bottom",
    legend.box = "horizontal",
    legend.spacing.y = unit(0.05, "cm"),
    strip.text = element_text(face = "bold", size = 13, color = "black"),
    # REMOVES THRESHOLD TICKS/LABELS
    axis.text.y = element_text(face = "bold", color = "black", size = 12),
    axis.ticks.y = element_blank(),
    panel.grid.minor = element_blank(),
    panel.grid.major.y = element_line(color = "gray95"),
    panel.spacing = unit(2, "lines")
  )

# reporting

# 1. Define Model Sets (Filtered for 10s) [cite: 2026-02-03]
thresholds <- c(10)
types      <- c("both", "death", "affect")
outcomes   <- c("ln_amt_in", "ln_amt_out", "ln_n_in", "ln_n_out")

model_grid_10 <- expand.grid(type = types, threshold = thresholds, yname = outcomes, stringsAsFactors = FALSE)

# 2. Run models [cite: 2026-02-03]
results_10 <- model_grid_10 %>%
  split(seq(nrow(.))) %>%
  map_df(~{
    iv_name <- paste0("nd_", .x$type, .x$threshold)
    model <- did2s(
      data = agg,
      yname = .x$yname,
      first_stage = ~ 1 | district^party + cycle^party,
      second_stage = as.formula(paste0("~ i(", iv_name, ", ref=0)")),
      treatment = iv_name,
      cluster_var = "district"
    )
    tidy(model, conf.int = TRUE) %>%
      filter(term != "(Intercept)") %>%
      mutate(
        type = .x$type,
        yname = .x$yname,
        panel_group = ifelse(str_detect(.x$yname, "ln_amt"), "Log Amount", "Log Count"),
        location = ifelse(str_detect(.x$yname, "_in"), "In-District", "Out-of-District")
      )
  })

# 3. Refine formatting [cite: 2026-02-10]
plot_data_10 <- results_10 %>%
  mutate(
    # Row Variable: Disaster Type on Left
    type_fac = factor(type, 
                      levels = rev(c("affect", "death", "both")),
                      labels = rev(c("Affected", "Deaths", "Both"))),
    
    # Column Variable: Amount on Left, Count on Right
    panel_group = factor(panel_group, levels = c("Log Amount", "Log Count")),
    
    # Location for shape mapping
    location = factor(location, levels = c("In-District", "Out-of-District"))
  )

# 4. Generate Visualization [cite: 2026-02-03, 2026-02-10]
ggplot(plot_data_10, aes(x = estimate, y = type_fac, color = location, shape = location)) +
  geom_vline(xintercept = 0, linetype = "dashed", color = "gray70") + 
  
  # Pointrange with small dodge to separate In/Out shapes [cite: 2026-02-10]
  geom_pointrange(aes(xmin = conf.low, xmax = conf.high), 
                  position = position_dodge(width = 0.5),
                  size = 0.9, fatten = 5) +
  
  # Two panels: Amount (Left), Count (Right)
  facet_wrap(~ panel_group, scales = "free_x") +
  
  # Using the In/Out Green/Orange palette for clarity within the 10% subset
  scale_color_manual(values = c("In-District" = "#53a483", "Out-of-District" = "#cb6a33")) +
  scale_shape_manual(values = c("In-District" = 16, "Out-of-District" = 17)) +
  
  labs(
    x = "Coefficient Estimate (ATT)", 
    y = NULL,
    color = "Donor Location",
    shape = "Donor Location"
  ) +
  
  theme_minimal(base_size = 14) + 
  theme(
    legend.position = "bottom",
    strip.text = element_text(face = "bold", size = 13),
    axis.text.y = element_text(face = "bold", color = "black", size = 12),
    axis.ticks.y = element_blank(),
    panel.grid.minor = element_blank(),
    panel.grid.major.y = element_line(color = "gray95"),
    panel.spacing = unit(2, "lines")
  )

ggsave(
  filename = "/Users/hyoon/Desktop/dissertation/did_log.png", 
  plot = last_plot(),       # Use the last plot displayed
  width = 10,               # Width in inches
  height = 7,               # Height in inches
  dpi = 300                 # Set resolution to 300 DPI
)


##################################-----------------------------
# 2.1-DID: 
##################################-----------------------------

# master

# 1. Define Model Sets separately [cite: 2026-02-03]
thresholds <- c(10, 20, 30)
types      <- c("both", "death", "affect")
parties    <- c(100, 200)

# 2. Run models for the Ideology outcome separated by party [cite: 2026-02-03]
ideo_party_results <- expand.grid(
  type = types, 
  threshold = thresholds, 
  party_code = parties, 
  stringsAsFactors = FALSE
) %>%
  split(seq(nrow(.))) %>%
  map_df(~{
    iv_name <- paste0("nd_", .x$type, .x$threshold)
    
    # Filter 'winner' dataset by party within the map call
    model <- did2s(
      data = winner %>% filter(party == .x$party_code),
      yname = "dwdime",
      first_stage = ~ 1 | district + cycle,
      second_stage = as.formula(paste0("~ i(", iv_name, ", ref=0)")),
      treatment = iv_name,
      cluster_var = "district"
    )
    
    tidy(model, conf.int = TRUE) %>%
      filter(term != "(Intercept)") %>%
      mutate(
        type = .x$type, 
        threshold = as.character(.x$threshold),
        party_code = .x$party_code
      )
  })

# 3. Refine for Professional Partisan Aesthetic [cite: 2026-02-10]
plot_ideo_data <- ideo_party_results %>%
  mutate(
    # Row Variable: Disaster Type on Left
    type_fac = factor(type, 
                      levels = rev(c("affect", "death", "both")),
                      labels = rev(c("Affected", "Deaths", "Both"))),
    
    # Column Variable: Party labels [cite: 2026-02-10]
    party_label = ifelse(party_code == 100, "Democrats", "Republicans"),
    party_label = factor(party_label, levels = c("Democrats", "Republicans")),
    
    # Threshold order for vertical dodge: 10 on top [cite: 2026-02-10]
    threshold_fac = factor(threshold, levels = c("30", "20", "10"))
  )

# 4. Generate Visualization [cite: 2026-02-03, 2026-02-10]
ggplot(plot_ideo_data, aes(x = estimate, y = type_fac, color = threshold_fac)) +
  geom_vline(xintercept = 0, linetype = "dashed", color = "gray70") + 
  
  # Pointrange with dodged thresholds [cite: 2026-02-10]
  geom_pointrange(aes(xmin = conf.low, xmax = conf.high), 
                  position = position_dodge(width = 0.7),
                  size = 0.8, fatten = 4) +
  
  # Grid: Party columns [cite: 2026-02-10]
  facet_wrap(~ party_label, scales = "free_x") +
  
  # Professional Palette (Green=10, Orange=20, Purple=30) [cite: 2026-02-10]
  scale_color_manual(
    values = c("10" = "#53a483", "20" = "#cb6a33", "30" = "#7570b3"),
    breaks = c("10", "20", "30"),
    name = "Disaster Threshold (%)"
  ) +
  
  labs(
    x = "Coefficient Estimate (ATT)", 
    y = NULL,
    subtitle = "Outcome: Ideological Distance (dwdime)"
  ) +
  
  theme_minimal(base_size = 14) + 
  theme(
    legend.position = "bottom",
    strip.text = element_text(face = "bold", size = 13, color = "black"),
    axis.text.y = element_text(face = "bold", color = "black", size = 12),
    axis.ticks.y = element_blank(),
    panel.grid.minor = element_blank(),
    panel.grid.major.y = element_line(color = "gray95"),
    panel.spacing = unit(2, "lines")
  )


# reporting 

# 1. Define Model Sets (Filtered for 10s) [cite: 2026-02-03]
thresholds <- c(10)
types      <- c("both", "death", "affect")
parties    <- c(100, 200)

# 2. Run models for DW-DIME separated by party [cite: 2026-02-03]
ideo_10_results <- expand.grid(
  type = types, 
  threshold = thresholds, 
  party_code = parties, 
  stringsAsFactors = FALSE
) %>%
  split(seq(nrow(.))) %>%
  map_df(~{
    iv_name <- paste0("nd_", .x$type, .x$threshold)
    
    model <- did2s(
      data = winner %>% filter(party == .x$party_code),
      yname = "dwdime",
      first_stage = ~ 1 | district + cycle,
      second_stage = as.formula(paste0("~ i(", iv_name, ", ref=0)")),
      treatment = iv_name,
      cluster_var = "district"
    )
    
    tidy(model, conf.int = TRUE) %>%
      filter(term != "(Intercept)") %>%
      mutate(
        type = .x$type, 
        party_code = .x$party_code
      )
  })

# 3. Refine for Professional Result Aesthetic [cite: 2026-02-10]
plot_data_10 <- ideo_10_results %>%
  mutate(
    # Row Variable: Disaster Type on Left
    type_fac = factor(type, 
                      levels = rev(c("affect", "death", "both")),
                      labels = rev(c("Affected", "Deaths", "Both"))),
    
    # Column Variable: Party labels
    party_label = ifelse(party_code == 100, "Democrat", "Republican"),
    party_label = factor(party_label, levels = c("Democrat", "Republican"))
  )

# 4. Generate Visualization [cite: 2026-02-03, 2026-02-10]
ggplot(plot_data_10, aes(x = estimate, y = type_fac)) +
  geom_vline(xintercept = 0, linetype = "dashed", color = "gray70") + 
  
  # Professional 10% Green Pointrange [cite: 2026-02-10]
  geom_pointrange(aes(xmin = conf.low, xmax = conf.high), 
                  color = "#53a483", 
                  size = 0.9, fatten = 5) +
  
  # Partisan Panels (Left: Dem, Right: Rep)
  facet_wrap(~ party_label, scales = "free_x") +
  
  labs(
    x = "Coefficient Estimate (ATT)", 
    y = NULL
  ) +
  
  theme_minimal(base_size = 14) + 
  theme(
    strip.text = element_text(face = "bold", size = 13, color = "black"),
    axis.text.y = element_text(face = "bold", color = "black", size = 12),
    axis.ticks.y = element_blank(),
    panel.grid.minor = element_blank(),
    panel.grid.major.y = element_line(color = "gray95"),
    panel.spacing = unit(2, "lines")
  )

ggsave(
  filename = "/Users/hyoon/Desktop/dissertation/did_ideo.png", 
  plot = last_plot(),       # Use the last plot displayed
  width = 10,               # Width in inches
  height = 7,               # Height in inches
  dpi = 300                 # Set resolution to 300 DPI
)


##################################-----------------------------
# 2.2-DID: 
##################################-----------------------------

# master

# 1. Define Model Sets separately [cite: 2026-02-03]
thresholds <- c(10, 20, 30)
types      <- c("both", "death", "affect")
parties    <- c(100, 200) 

# 2. Run models for selection probability [cite: 2026-02-03]
party_sens_results <- expand.grid(
  type = types, 
  threshold = thresholds, 
  party_code = parties, 
  stringsAsFactors = FALSE
) %>%
  split(seq(nrow(.))) %>%
  map_df(~{
    iv_name <- paste0("nd_", .x$type, .x$threshold)
    
    # Run models for prob_dd using prob_anl_dan
    model <- did2s(
      data = prob_anl_dan %>% filter(party == .x$party_code),
      yname = "prob_dd",
      first_stage = ~ 1 | district + cycle, 
      second_stage = as.formula(paste0("~ i(", iv_name, ", ref=0)")),
      treatment = iv_name,
      cluster_var = "district"
    )
    
    tidy(model, conf.int = TRUE) %>%
      filter(term != "(Intercept)") %>%
      mutate(
        type = .x$type, 
        threshold = as.character(.x$threshold),
        party_label = ifelse(.x$party_code == 200, "Democrats", "Republicans")
      )
  })

# 3. Refine Formatting for Professional Aesthetic [cite: 2026-02-10]
plot_prob_data <- party_sens_results %>%
  mutate(
    # Disaster Category on Left (Y-axis)
    type_fac = factor(type, 
                      levels = rev(c("affect", "death", "both")),
                      labels = rev(c("Affected", "Deaths", "Both"))),
    
    # Party order (Dem on left, Rep on right)
    party_label = factor(party_label, levels = c("Democrats", "Republicans")),
    
    # Threshold order for vertical dodge: 10 on top [cite: 2026-02-10]
    threshold_fac = factor(threshold, levels = c("30", "20", "10"))
  )

# 4. Generate Visualization [cite: 2026-02-03, 2026-02-10]
ggplot(plot_prob_data, aes(x = estimate, y = type_fac, color = threshold_fac)) +
  geom_vline(xintercept = 0, linetype = "dashed", color = "gray70") + 
  
  # Pointrange with dodged thresholds [cite: 2026-02-10]
  geom_pointrange(aes(xmin = conf.low, xmax = conf.high), 
                  position = position_dodge(width = 0.7),
                  size = 0.8, fatten = 4) +
  
  # Partisan Panels
  facet_wrap(~ party_label, scales = "free_x") +
  
  # Professional Palette (Green=10, Orange=20, Purple=30) [cite: 2026-02-10]
  scale_color_manual(
    values = c("10" = "#53a483", "20" = "#cb6a33", "30" = "#7570b3"),
    breaks = c("10", "20", "30"),
    name = "Disaster Threshold (%)"
  ) +
  
  labs(
    x = "Coefficient Estimate (ATT)", 
    y = NULL,
    subtitle = "Outcome: Selection Probability (DW-DIME)"
  ) +
  
  theme_minimal(base_size = 14) + 
  theme(
    legend.position = "bottom",
    strip.text = element_text(face = "bold", size = 13, color = "black"),
    axis.text.y = element_text(face = "bold", color = "black", size = 12),
    axis.ticks.y = element_blank(),
    panel.grid.minor = element_blank(),
    panel.grid.major.y = element_line(color = "gray95"),
    panel.spacing = unit(2, "lines")
  )

# reporting

library(did2s)
library(dplyr)
library(purrr)
library(ggplot2)
library(stringr)
library(broom)

# 1. Define Model Sets (Filtered for 10s)
thresholds <- c(10)
types      <- c("both", "death", "affect")
parties    <- c(100, 200) 

# 2. Run models for prob_dd separated by party
prob_10_results <- expand.grid(
  type = types, 
  threshold = thresholds, 
  party_code = parties, 
  stringsAsFactors = FALSE
) %>%
  split(seq(nrow(.))) %>%
  map_df(~{
    iv_name <- paste0("nd_", .x$type, .x$threshold)
    
    model <- did2s(
      data = prob_anl_dan %>% filter(party == .x$party_code),
      yname = "prob_dd",
      first_stage = ~ 1 | district + cycle,
      second_stage = as.formula(paste0("~ i(", iv_name, ", ref=0)")),
      treatment = iv_name,
      cluster_var = "district"
    )
    
    tidy(model, conf.int = TRUE) %>%
      filter(term != "(Intercept)") %>%
      mutate(
        type = .x$type, 
        party_label = ifelse(.x$party_code == 200, "Democrat", "Republican")
      )
  })

# 3. Refine Formatting
plot_prob_10 <- prob_10_results %>%
  mutate(
    # Disaster Category on Left (Y-axis)
    type_fac = factor(type, 
                      levels = rev(c("affect", "death", "both")),
                      labels = rev(c("Affected", "Deaths", "Both"))),
    
    # Party order (Dem on left, Rep on right)
    party_label = factor(party_label, levels = c("Democrat", "Republican"))
  )

# 4. Generate Final Visualization
ggplot(plot_prob_10, aes(x = estimate, y = type_fac)) +
  geom_vline(xintercept = 0, linetype = "dashed", color = "gray70") + 
  
  # Professional 10% Green Pointrange
  geom_pointrange(aes(xmin = conf.low, xmax = conf.high), 
                  color = "#53a483", 
                  size = 0.9, fatten = 5) +
  
  # Partisan Panels (Left: Dem, Right: Rep)
  facet_wrap(~ party_label, scales = "free_x") +
  
  labs(
    x = "Coefficient Estimate (ATT)", 
    y = NULL
  ) +
  
  theme_minimal(base_size = 14) + 
  theme(
    strip.text = element_text(face = "bold", size = 13, color = "black"),
    axis.text.y = element_text(face = "bold", color = "black", size = 12),
    axis.ticks.y = element_blank(),
    panel.grid.minor = element_blank(),
    panel.grid.major.y = element_line(color = "gray95"),
    panel.spacing = unit(2, "lines")
  )

ggsave(
  filename = "/Users/hyoon/Desktop/dissertation/did_prob.png", 
  plot = last_plot(),       # Use the last plot displayed
  width = 10,               # Width in inches
  height = 7,               # Height in inches
  dpi = 300                 # Set resolution to 300 DPI
)

##################################-----------------------------


##################################-----------------------------
## PRETREND
##################################-----------------------------

# first stage
df_nt <- agg %>%
  arrange(district, cycle) %>%
  group_by(district) %>%
  mutate(
    ever_treated = any(nd_both10 == 1),
    treated_so_far = cummax(nd_both10)
  ) %>%
  ungroup() %>%
  filter(treated_so_far == 0)

df_nt <- df_nt %>%
  mutate(future_treated = ifelse(ever_treated == 1, 1, 0))

pretrend_est <- feols(
  pct.amt.out ~ i(cycle, future_treated, ref = 2002) | cycle + district,
  cluster = ~district,
  data = df_nt
)

# coefplot(
#   pretrend_est,
#   xlab = "",
#   ylab = "Estimated Coefficients",
#   main = "",
#   ci.level = 0.95)

# 1. Extract the coefficients into a data frame
plot_df <- tidy(pretrend_est, conf.int = TRUE) %>%
  # Filter to keep only the cycle coefficients
  filter(grepl("cycle", term)) %>%
  mutate(
    # Extract the year from the term string (e.g., "cycle::2004:future_treated")
    year = as.numeric(gsub(".*?(\\d{4}).*", "\\1", term))
  )

# 2. Manual Plot Construction
ggplot(plot_df, aes(x = year, y = estimate)) +
  # Reference line at zero
  geom_hline(yintercept = 0, color = "black", linetype = "solid") +
  # Error bars and points
  geom_pointrange(aes(ymin = conf.low, ymax = conf.high), 
                  size = 0.8, fatten = 4) +
  # Setting x-axis breaks only for the estimated years
  scale_x_continuous(breaks = c(2004, 2006, 2008, 2010)) +
  labs(
    x = NULL, 
    y = "Estimated Coefficients",
    title = ""
  ) +
  theme_minimal(base_size = 14) +
  theme(
    panel.grid.minor = element_blank(),
    panel.grid.major.x = element_blank(), # Optional: removes vertical lines for a cleaner look
    axis.text = element_text(color = "black"),
    plot.title = element_text(hjust = 0.5)
  )

ggsave(
  filename = "/Users/hyoon/Desktop/dissertation/did_pretrend1.png", 
  plot = last_plot(),       # Use the last plot displayed
  width = 10,               # Width in inches
  height = 7,               # Height in inches
  dpi = 300                 # Set resolution to 300 DPI
)


# second stage- winner

df_nt2 <- winner %>%
  arrange(district, cycle) %>%
  group_by(district) %>%
  mutate(
    ever_treated = any(nd_both10 == 1),
    treated_so_far = cummax(nd_both10)
  ) %>%
  ungroup() %>%
  filter(treated_so_far == 0)

df_nt2 <- df_nt2 %>%
  mutate(future_treated = ifelse(ever_treated == 1, 1, 0))


pretrend_est2 <- feols(
  ab.dist.dw1 ~ i(cycle, future_treated, ref = 2000) | cycle + district, 
  cluster = ~district,
  data = df_nt2
)

# coefplot(
#   pretrend_est2,
#   keep = "cycle::",
#   xlab = "Election cycle",
#   ylab = "Difference relative to 2000",
#   main = "Pre-trend check: future-treated vs never-treated districts",
#   ci.level = 0.95
# )

plot_data_ideo <- tidy(pretrend_est2, conf.int = TRUE) %>%
  filter(grepl("cycle", term)) %>%
  mutate(
    # Extracting the year from the interaction term string
    year = as.numeric(gsub(".*?(\\d{4}).*", "\\1", term))
  )

ggplot(plot_data_ideo, aes(x = year, y = estimate)) +
  # Reference line at zero
  geom_hline(yintercept = 0, color = "black", linetype = "solid") +
  # Error bars and points
  geom_pointrange(aes(ymin = conf.low, ymax = conf.high), 
                  size = 0.8, fatten = 4) +
  # Explicitly label the cycles you estimated
  scale_x_continuous(breaks = c(2002, 2004, 2006, 2008, 2010)) +
  labs(
    x = NULL, 
    y = "Estimated Coefficients",
    title = ""
  ) +
  theme_minimal(base_size = 14) +
  theme(
    panel.grid.minor = element_blank(),
    panel.grid.major.x = element_blank(), # Optional: removes vertical lines for a cleaner look
    axis.text = element_text(color = "black"),
    plot.title = element_text(hjust = 0.5)
  )

ggsave(
  filename = "/Users/hyoon/Desktop/dissertation/did_pretrend2.png", 
  plot = last_plot(),       # Use the last plot displayed
  width = 10,               # Width in inches
  height = 7,               # Height in inches
  dpi = 300                 # Set resolution to 300 DPI
)


# second stage - weights

df_nt3 <- prob_anl_dan %>%
  arrange(district, cycle) %>%
  group_by(district) %>%
  mutate(
    ever_treated = any(nd_both10 == 1),
    treated_so_far = cummax(nd_both10)
  ) %>%
  ungroup() %>%
  filter(treated_so_far == 0)

df_nt3 <- df_nt3 %>%
  mutate(future_treated = ifelse(ever_treated == 1, 1, 0))

pretrend_est3 <- feols(
  prob_dw1 ~ i(cycle, future_treated, ref = 2002) | district + cycle,
  cluster = ~district,
  data = df_nt3
)

# coefplot(
#   pretrend_est3,
#   keep = "",
#   xlab = "Election cycle",
#   ylab = "Difference relative to 2002",
#   main = "Pre-trend check: future-treated vs never-treated districts",
#   ci.level = 0.95
# )

plot_data_prob <- tidy(pretrend_est3, conf.int = TRUE) %>%
  filter(grepl("cycle", term)) %>%
  mutate(
    # Extracting the year from the interaction term string
    year = as.numeric(gsub(".*?(\\d{4}).*", "\\1", term))
  )

ggplot(plot_data_prob, aes(x = year, y = estimate)) +
  # Reference line at zero
  geom_hline(yintercept = 0, color = "black", linetype = "solid") +
  # Error bars and points
  geom_pointrange(aes(ymin = conf.low, ymax = conf.high), 
                  size = 0.8, fatten = 4) +
  # Explicitly label the cycles you estimated
  scale_x_continuous(breaks = c(2004, 2006, 2008, 2010)) +
  labs(
    x = NULL, 
    y = "Estimated Coefficients",
    title = ""
  ) +
  theme_minimal(base_size = 14) +
  theme(
    panel.grid.minor = element_blank(),
    panel.grid.major.x = element_blank(), # Optional: removes vertical lines for a cleaner look
    axis.text = element_text(color = "black"),
    plot.title = element_text(hjust = 0.5)
  )

ggsave(
  filename = "/Users/hyoon/Desktop/dissertation/did_pretrend3.png", 
  plot = last_plot(),       # Use the last plot displayed
  width = 10,               # Width in inches
  height = 7,               # Height in inches
  dpi = 300                 # Set resolution to 300 DPI
)



##---------------------------------------------------------



