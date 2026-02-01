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
  filter(`Adm Level` != 2) %>% # 371 obs 2000-2022
  mutate(adm1 = sub("\\(Adm1).*", "", .$`Geo Locations`)) %>%
  drop_na(`Start Day`)

cred <- separate_rows(cred, adm1, sep=",") 
cred$adm1 <- str_trim(cred$adm1, "left")
cred$adm1 <- str_trim(cred$adm1, "right")
cred$state <- state.abb[match(cred$adm1,state.name)]

cred$date <- as.Date(paste(cred$`Start Year`, cred$`Start Month`, cred$`Start Day`, sep="-"))

cred <- cred %>% select("Year", "Seq", "Disaster Type", "Total Deaths", "Total Affected", "state", "date",
                        "Start Year", "Start Month", "Start Day")


# disaster criteria
cred <- cred %>% mutate(death10 = ifelse(.$`Total Deaths` >= 41, 1, 0),
                        affect10 = ifelse(.$`Total Affected` >= 102000, 1, 0)) %>%
  mutate(death10 = ifelse(is.na(.$death10 == T), 0, .$death10),
         affect10 = ifelse(is.na(.$affect10 == T), 0, .$affect10)) %>%
  mutate(both = ifelse(.$death10 == 1 & affect10 ==1, 1, 0))


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
try <- unique(trial[c("year.x", "cong","close", "death10", "affect10", "both", "total_deaths", "total_affected")]) 

parties <- c(100, 200)
district_year_party <- cong %>% distinct(cong, year) %>%
  crossing(party = parties) %>% mutate(cong = if_else(cong == "AK00", "AK01", cong)) %>% filter(year <= 2014)
#############==join disaster (df: try) and dime (df: join)==============================================================

join <- read.csv("join.csv")

agg <- join %>% dplyr::group_by(district, cycle, party) %>% drop_na(same) %>%
  summarize(count = n(),
            n.in = sum(same==1),
            n.out = sum(same==0),
            amt.in = sum(amount[same==1]),
            amt.out = sum(amount[same==0])) %>%
  mutate(pct.out = n.out/(n.in+n.out)*100,
         pct.amt.out = amt.out/(amt.in+amt.out)*100)

try <- try %>% group_by(year.x, cong) %>% summarize(n_disasters = n(), close = min(close),
                                                    death10 = max(death10),
                                                    affect10 = max(affect10),
                                                    both=max(both),
                                                    deaths = sum(total_deaths),
                                                    affected = sum(total_affected))

district_year_party <- left_join(district_year_party, agg, by = c(
  "year" = "cycle", "cong" = "district", "party" = "party")) %>%
  mutate(across(everything(), ~replace_na(., 0))) %>% dplyr::rename(cycle = year, district = cong)

agg <- left_join(district_year_party, try,
                 by = c("cycle" = "year.x",
                        "district" = "cong"))

agg$nd_death <- ifelse(is.na(agg$death10)==T, 0, agg$death10)
agg$nd_affect <- ifelse(is.na(agg$affect10)==T, 0, agg$affect10)
agg$nd_both <- ifelse(is.na(agg$both)==T, 0, agg$both)

agg <- agg %>% mutate(across(everything(), ~replace_na(., 0)))
agg$any <- agg$n_disasters>0
agg$any_deaths <- !is.na(agg$deaths)
agg$any_affected <- !is.na(agg$affected)

agg <- agg %>% ungroup(.) %>% mutate(pct.out = round(agg$pct.out, 2))

## effect of nd on share (limit nd to close ones)

agg$close <- as.numeric(agg$close)
agg <- agg %>% ungroup(.) %>% mutate(nd_death2 = ifelse(.$close < 300 & .$nd_death == 1, 1, 0),
                                     nd_affect2 = ifelse(.$close < 300 & .$nd_affect == 1, 1, 0),
                                     nd_both2 = ifelse(.$close < 300 & .$nd_both == 1, 1, 0))


### effect of nd on ideology

# extract winner of district, cycle
winner <- join %>% filter(p_elec_stat == "W") %>% 
  group_by(bonica_rid, district, cycle) %>% slice(1) %>% 
  select(cycle, bonica_rid, party, recipient_state, district, recipient_cfscore, gen_elec_stat, dwnom1, dwnom2, dwdime) %>%
  arrange(district, cycle) %>% ungroup(.)

winner <- left_join(winner, try,
                    by= c("district" = "cong",
                          "cycle"= "year.x")) %>% 
  mutate(nd_death = ifelse(is.na(.$death10 == T), 0, .$death10),
         nd_affect = ifelse(is.na(.$affect10 == T), 0, .$affect10),
         nd_both = ifelse(is.na(.$both == T), 0, .$both))

# construct DV (ideology)
winner <- winner %>% mutate(# cfscore
  ab.cfscore = abs(winner$recipient_cfscore), # absolute cfscore
  dist = case_when(.$party == 100 ~ .$recipient_cfscore - median(.$recipient_cfscore[.$party==100]), # distance from median
                   .$party == 200 ~ .$recipient_cfscore - median(.$recipient_cfscore[.$party==200])),
  # dwdime
  ab.dwdime = abs(.$dwdime),
  dist.dwdime = case_when(.$party == 100 ~ .$dwdime - median(.$dwdime[.$party==100], na.rm=T), 
                          .$party == 200 ~ .$dwdime - median(.$dwdime[.$party==200], na.rm=T)),
  # dwnominate
  ab.dw1 = abs(.$dwnom1),
  dist.dw1 = case_when(.$party == 100 ~ .$dwnom1 - median(.$dwnom1[.$party==100], na.rm=T), 
                       .$party == 200 ~ .$dwnom1 - median(.$dwnom1[.$party==200], na.rm=T))) %>%
  mutate(ab.dist = abs(.$dist),
         ab.dist.dwdime = abs(.$dist.dwdime),
         ab.dist.dw1 = abs(.$dist.dw1))

winner <- winner %>% ungroup(.) %>% mutate(close = as.numeric(close),
                                           nd_death2 = ifelse(.$close < 300 & .$nd_death == 1, 1, 0),
                                           nd_affect2 = ifelse(.$close < 300 & .$nd_affect == 1, 1, 0),
                                           nd_both2 = ifelse(.$close < 300 & .$nd_both == 1, 1, 0))

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
    prob_dd  = sum(candpct * dwdime, na.rm = TRUE) / sum(!is.na(candpct * dwdime))
  ) %>% 
  arrange(year, cong, party) %>% 
  ungroup()


# use this
prob_anl_dan <- left_join(agg, prob,
                          by= c("district" = "cong",
                                "cycle"= "year",
                                "party"="party")) 


# donor ideology analysis

join_w <- join %>%
  group_by(district, party, cycle) %>%
  summarise(
    # overall weighted CF score
    w_cf = sum(amount * contributor_cfscore, na.rm = TRUE) / sum(amount, na.rm = TRUE),
    w_cf_cnt = sum(contributor_cfscore, na.rm = TRUE) / sum(!is.na(contributor_cfscore)),
    # weighted CF score for in-district donors
    w_cf_in = sum(ifelse(con_district == district, amount * contributor_cfscore, 0), na.rm = TRUE) /
      sum(ifelse(con_district == district, amount, 0), na.rm = TRUE),
    
    # weighted CF score for out-of-district donors
    w_cf_out = sum(ifelse(con_district != district, amount * contributor_cfscore, 0), na.rm = TRUE) /
      sum(ifelse(con_district != district, amount, 0), na.rm = TRUE),
    count = n(),
    .groups = "drop"
  )

district_year_party <- cong %>% distinct(cong, year) %>%
  crossing(party = parties) %>% mutate(cong = if_else(cong == "AK00", "AK01", cong)) %>% filter(year <= 2014) %>%
  mutate(across(everything(), ~replace_na(., 0)))

district_year_party <- left_join(district_year_party, join_w, by = c(
  "year" = "cycle", "cong" = "district", "party" = "party"))   %>% dplyr::rename(cycle = year, district = cong)

join_w <- left_join(district_year_party, try,
                    by = c("cycle" = "year.x",
                           "district" = "cong"))

join_w$nd_death <- ifelse(is.na(join_w$death10)==T, 0, join_w$death10)
join_w$nd_affect <- ifelse(is.na(join_w$affect10)==T, 0, join_w$affect10)
join_w$nd_both <- ifelse(is.na(join_w$both)==T, 0, join_w$both)

join_w <- join_w %>% mutate(across(everything(), ~replace_na(., 0)))
join_w$any <- join_w$n_disasters>0
join_w$any_deaths <- !is.na(join_w$deaths)
join_w$any_affected <- !is.na(join_w)

join_w <- join_w %>% ungroup(.) %>% mutate(pct.out = round(join_w$pct.out, 2))


join_w$close <- as.numeric(join_w$close)
join_w <- join_w %>% ungroup(.) %>% mutate(nd_death2 = ifelse(.$close < 300 & .$nd_death == 1, 1, 0),
                                           nd_affect2 = ifelse(.$close < 300 & .$nd_affect == 1, 1, 0),
                                           nd_both2 = ifelse(.$close < 300 & .$nd_both == 1, 1, 0))


feols(w_cf ~ nd_both2 | district^party + cycle^party, data=join_w) 
feols(w_cf_in ~ nd_both2 | district^party + cycle^party, data=join_w) 
feols(w_cf_out ~ nd_both2 | district^party + cycle^party, data=join_w) 

feols(w_cf ~ nd_both2 | district + cycle, data=join_w %>% filter(party==100)) 
feols(w_cf_in ~ nd_both2 | district + cycle, data=join_w%>% filter(party==100)) 
feols(w_cf_out ~ nd_both2 | district + cycle, data=join_w%>% filter(party==100)) 

###------------------------------
# compile reportable results
###------------------------------

# share out district: all positive but only significant to big/disruptive disasters
m_both_amt   <- feols(pct.amt.out ~ nd_both2   | district^party + cycle^party, data = agg)
m_both_cnt   <- feols(pct.out     ~ nd_both2   | district^party + cycle^party, data = agg)

m_death_amt  <- feols(pct.amt.out ~ nd_death2  | district^party + cycle^party, data = agg)
m_death_cnt  <- feols(pct.out     ~ nd_death2  | district^party + cycle^party, data = agg)

m_affect_amt <- feols(pct.amt.out ~ nd_affect2 | district^party + cycle^party, data = agg)
m_affect_cnt <- feols(pct.out     ~ nd_affect2 | district^party + cycle^party, data = agg)

share_out <- bind_rows(
  tidy(m_both_amt)   %>% mutate(model = "Amount", iv = "Deaths + Affected"),
  tidy(m_both_cnt)   %>% mutate(model = "Count",  iv = "Deaths + Affected"),
  
  tidy(m_death_amt)  %>% mutate(model = "Amount", iv = "Deaths"),
  tidy(m_death_cnt)  %>% mutate(model = "Count",  iv = "Deaths"),
  
  tidy(m_affect_amt) %>% mutate(model = "Amount", iv = "Affected"),
  tidy(m_affect_cnt) %>% mutate(model = "Count",  iv = "Affected")
) %>%
  filter(term %in% c("nd_both2", "nd_death2", "nd_affect2")) %>%
  drop_na(estimate)

dwplot(share_out) +
  geom_vline(xintercept = 0, linetype = 2) +
  facet_wrap(~ iv, scales = "free_y") +
  xlab("Effect on share of out-of-district contributions") +
  ylab("") +
  theme_minimal() +
  theme(
    legend.position = "bottom",
    strip.text = element_text(face = "bold")
  )

# count of in/out district: both were hit but absolute number wise out district hit more
m_n_in   <- feols(n.in   ~ nd_both2 | district^party + cycle^party, data = agg)
m_n_out  <- feols(n.out  ~ nd_both2 | district^party + cycle^party, data = agg)

m_amt_in  <- feols(amt.in  ~ nd_both2 | district^party + cycle^party, data = agg)
m_amt_out <- feols(amt.out ~ nd_both2 | district^party + cycle^party, data = agg)

levels_out <- bind_rows(
  tidy(m_n_in)    %>% mutate(model = "In-district",  outcome = "Number"),
  tidy(m_n_out)   %>% mutate(model = "Out-of-district", outcome = "Number"),
  
  tidy(m_amt_in)  %>% mutate(model = "In-district",  outcome = "Amount"),
  tidy(m_amt_out) %>% mutate(model = "Out-of-district", outcome = "Amount")
) %>%
  filter(term == "nd_both2") %>%
  drop_na(estimate)

dwplot(levels_out) +
  geom_vline(xintercept = 0, linetype = 2) +
  facet_wrap(~ outcome, scales = "free_x") +
  xlab("Effect of deaths + affected") +
  ylab("") +
  theme_minimal() +
  theme(
    legend.position = "bottom",
    strip.text = element_text(face = "bold")
  )

# winner ideology: nothing really
m_cf   <- feols(ab.cfscore      ~ nd_both2 | district^party + cycle^party, data = winner)
m_dwd  <- feols(ab.dwdime       ~ nd_both2 | district^party + cycle^party, data = winner)
m_dw1  <- feols(ab.dw1          ~ nd_both2 | district^party + cycle^party, data = winner)

m_dist_cf  <- feols(ab.dist          ~ nd_both2 | district^party + cycle^party, data = winner)
m_dist_dwd <- feols(ab.dist.dwdime   ~ nd_both2 | district^party + cycle^party, data = winner)
m_dist_dw1 <- feols(ab.dist.dw1      ~ nd_both2 | district^party + cycle^party, data = winner)

ideo_out <- bind_rows(
  tidy(m_cf)        %>% mutate(model = "CFscore",   group = "Absolute position"),
  tidy(m_dwd)       %>% mutate(model = "DW-DIME",   group = "Absolute position"),
  tidy(m_dw1)       %>% mutate(model = "DW-NOM 1",  group = "Absolute position"),
  
  tidy(m_dist_cf)   %>% mutate(model = "CFscore",   group = "Distance to district"),
  tidy(m_dist_dwd)  %>% mutate(model = "DW-DIME",   group = "Distance to district"),
  tidy(m_dist_dw1)  %>% mutate(model = "DW-NOM 1",  group = "Distance to district")
) %>%
  filter(term == "nd_both2") %>%
  drop_na(estimate)

dwplot(ideo_out) +
  geom_vline(xintercept = 0, linetype = 2) +
  facet_wrap(~ group, scales = "free_x") +
  xlab("Effect of deaths + affected") +
  ylab("") +
  theme_minimal() +
  theme(
    legend.position = "bottom",
    strip.text = element_text(face = "bold")
  )

# weighted voteshare: just for prob_cf, just democrats

m_cf  <- feols(prob_cf  ~ nd_both2 | district^party + cycle^party, data = prob_anl_dan)
m_dw1 <- feols(prob_dw1 ~ nd_both2 | district^party + cycle^party, data = prob_anl_dan)
m_dw2 <- feols(prob_dw2 ~ nd_both2 | district^party + cycle^party, data = prob_anl_dan)
m_dd  <- feols(prob_dd  ~ nd_both2 | district^party + cycle^party, data = prob_anl_dan)

prob_all <- bind_rows(
  tidy(m_cf)  %>% mutate(model = "CFscore"),
  tidy(m_dw1) %>% mutate(model = "DW-NOM 1"),
  tidy(m_dw2) %>% mutate(model = "DW-NOM 2"),
  tidy(m_dd)  %>% mutate(model = "District distance")
) %>%
  filter(term == "nd_both2") %>%
  drop_na(estimate)

dwplot(prob_all) +
  geom_vline(xintercept = 0, linetype = 2) +
  xlab("Effect of deaths + affected") +
  ylab("") +
  theme_minimal() +
  theme(legend.position = "bottom")



ivs <- c("nd_both2", "nd_death2", "nd_affect2")
dvs <- c("prob_cf", "prob_dw1", "prob_dw2", "prob_dd")
parties <- c(100, 200)

models_party <- crossing(dv = dvs, iv = ivs, party = parties) %>%
  mutate(
    model = pmap(
      list(dv, iv, party),
      ~ feols(
        as.formula(paste0(..1, " ~ ", ..2, " | district + cycle")),
        data = prob_anl_dan %>% filter(party == ..3)
      )
    )
  )

prob_party <- models_party %>%
  mutate(tidy = map(model, tidy)) %>%
  unnest(tidy) %>%
  filter(term %in% ivs) %>%
  mutate(
    model = ifelse(party == 100, "Democrats", "Republicans"),
    iv = recode(term,
                nd_both2   = "Deaths + Affected",
                nd_death2  = "Deaths",
                nd_affect2 = "Affected"
    ),
    outcome = recode(dv,
                     prob_cf  = "CFscore",
                     prob_dw1 = "DW-NOM 1",
                     prob_dw2 = "DW-NOM 2",
                     prob_dd  = "District distance"
    )
  )

dwplot(prob_party) +
  geom_vline(xintercept = 0, linetype = 2) +
  facet_grid(iv ~ outcome, scales = "free_x") +
  xlab("Effect on probability") +
  ylab("") +
  theme_minimal() +
  theme(
    legend.position = "bottom",
    strip.text = element_text(face = "bold"),
    axis.text.y  = element_blank(),
    axis.ticks.y = element_blank()
  ) 

feols(prob_cf ~ nd_both2 | district^party + cycle^party, data=prob_anl_dan)
feols(prob_dw1 ~ nd_both2 | district^party + cycle^party, data=prob_anl_dan)
feols(prob_dw2 ~ nd_both2 | district^party + cycle^party, data=prob_anl_dan)
feols(prob_dd ~ nd_both2 | district^party + cycle^party, data=prob_anl_dan)

feols(prob_cf ~ nd_both2 | district + cycle, data=prob_anl_dan %>% filter(party == 100)) 
feols(prob_cf ~ nd_both2 | district + cycle, data=prob_anl_dan %>% filter(party == 200)) 
feols(prob_cf ~ nd_death2 | district + cycle, data=prob_anl_dan %>% filter(party == 100)) 
feols(prob_cf ~ nd_death2 | district + cycle, data=prob_anl_dan %>% filter(party == 200)) 
feols(prob_cf ~ nd_affect2 | district + cycle, data=prob_anl_dan %>% filter(party == 100)) 
feols(prob_cf ~ nd_affect2 | district + cycle, data=prob_anl_dan %>% filter(party == 200)) 

feols(prob_dw1 ~ nd_both2 | district + cycle, data=prob_anl_dan %>% filter(party == 100)) 
feols(prob_dw1 ~ nd_both2 | district + cycle, data=prob_anl_dan %>% filter(party == 200)) 
feols(prob_dw1 ~ nd_death2 | district + cycle, data=prob_anl_dan %>% filter(party == 100)) 
feols(prob_dw1 ~ nd_death2 | district + cycle, data=prob_anl_dan %>% filter(party == 200)) 
feols(prob_dw1 ~ nd_affect2 | district + cycle, data=prob_anl_dan %>% filter(party == 100)) 
feols(prob_dw1 ~ nd_affect2 | district + cycle, data=prob_anl_dan %>% filter(party == 200)) 

feols(prob_dw2 ~ nd_both2 | district + cycle, data=prob_anl_dan %>% filter(party == 100)) 
feols(prob_dw2 ~ nd_both2 | district + cycle, data=prob_anl_dan %>% filter(party == 200)) 
feols(prob_dw2 ~ nd_death2 | district + cycle, data=prob_anl_dan %>% filter(party == 100)) 
feols(prob_dw2 ~ nd_death2 | district + cycle, data=prob_anl_dan %>% filter(party == 200)) 
feols(prob_dw2 ~ nd_affect2 | district + cycle, data=prob_anl_dan %>% filter(party == 100)) 
feols(prob_dw2 ~ nd_affect2 | district + cycle, data=prob_anl_dan %>% filter(party == 200)) 

feols(prob_dd ~ nd_both2 | district + cycle, data=prob_anl_dan %>% filter(party == 100)) 
feols(prob_dd ~ nd_both2 | district + cycle, data=prob_anl_dan %>% filter(party == 200)) 
feols(prob_dd ~ nd_death2 | district + cycle, data=prob_anl_dan %>% filter(party == 100)) 
feols(prob_dd ~ nd_death2 | district + cycle, data=prob_anl_dan %>% filter(party == 200)) 
feols(prob_dd ~ nd_affect2 | district + cycle, data=prob_anl_dan %>% filter(party == 100)) 
feols(prob_dd ~ nd_affect2 | district + cycle, data=prob_anl_dan %>% filter(party == 200)) 






