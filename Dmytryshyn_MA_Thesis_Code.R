# ============================================================
# Thesis: Determinants of Peace Compliance in Personalist
#         Authoritarian Regimes
# Author: Khrystyna Dmytryshyn
#
# Key specifications:
# - Primary DV:  relapse_5yr (5-year compliance window)
# - TE1 variable: elite_sanctions_intensity (with interaction)
# - TE2 variable: aid_gdp_post_log (with interaction)
# - TE3 variable: milasym_trend_5yr (with interaction)
# ============================================================

rm(list = ls())
options(stringsAsFactors = FALSE)

# ============================================================
# 1) SETUP: PACKAGES, PATHS, YEARS
# ============================================================


# To be able to run this code, download data sets from the following sources:

# GWF-Autocratic-Regimes: https://xmarquez.github.io/democracyData/reference/gwf_all.html
# Extract ZIP, find GWF Autocratic Regimes.xlsx inside GWF-Autocratic-Regimes-1.2/

# MIDB 5.0: https://correlatesofwar.org/data-sets/mids/
# Extract ZIP, find MIDB 5.0.csv, MIDA 5.0.csv

# GSDB_V4: https://www.globalsanctionsdatabase.com/
# Find GSDB_V4.csv

# V-Dem Dataset: https://www.v-dem.net/
# Download R version of the dataset, save as vdem.RData

# AidDataCore_ResearchRelease_Level1_v3.1: https://www.aiddata.org/data/aiddata-core-research-release-level-1-v3-1
# Extract ZIP, folder AidDataCore_ResearchRelease_Level1_v3.1, find AidDataCoreDonorRecipientYearPurpose_ResearchRelease_Level1_v3.1.csv

# NMC-60-abridged: https://correlatesofwar.org/data-sets/national-material-capabilities
# Extract ZIP, find NMC-60-abridged.csv

# gandhi-sumner-estimates: https://dataverse.harvard.edu/dataset.xhtml?persistentId=doi:10.7910/DVN/FB3O9W
# Download dataverse files, find gandhi-sumner-estimates.csv inside dataverse_files/

# GWF+personalism-scores: https://sites.psu.edu/wright/files/2019/11/GWF-time-vary-personalism.zip
# Extract ZIP, find GWF+personalism-scores.csv inside GWF-time-vary-personalism/


PATH_GWF             <- "C:/Users/HP/Downloads/GWF-Autocratic-Regimes-1.2/GWF Autocratic Regimes.xlsx"
PATH_MID             <- "C:/Users/HP/Documents/MID-5-Data-and-Supporting-Materials (1)/MIDB 5.0.csv"
PATH_GSDB            <- "C:/Users/HP/Documents/MID-5-Data-and-Supporting-Materials (1)/GSDB_V4.csv"
PATH_VDEM            <- "C:/Users/HP/Desktop/диплом/vdem.RData"
AIDDATA_DIR          <- "C:/Users/HP/Desktop/диплом/AidDataCore_ResearchRelease_Level1_v3.1"
PATH_NMC             <- "C://Users//HP//Desktop//диплом//NMC-60-abridged.csv"
PATH_GANDHI          <- "C:/Users/HP/Desktop/диплом/dataverse_files/gandhi-sumner-estimates.csv"
PATH_GWF_SCORES      <- "C:/Users/HP/Desktop/диплом/GWF-time-vary-personalism/GWF+personalism-scores.csv"
PATH_AIDDATA_PURPOSE <- "C:/Users/HP/Desktop/диплом/AidDataCore_ResearchRelease_Level1_v3.1/AidDataCoreDonorRecipientYearPurpose_ResearchRelease_Level1_v3.1.csv"

START_YEAR              <- 1960L
PRIOR_CONFLICTS_START   <- 1946L  
FOCAL_END_YEAR          <- 2009L
OBS_END_YEAR            <- 2014L
COV_END_YEAR            <- 2014L
SANCTIONS_POSTWAR_YEARS <- 5L
AID_PRE_YEARS           <- 5L
AID_POST_YEARS          <- 5L
CENSOR_DAYS             <- 1825L

CENSOR_DATE <- as.Date(sprintf("%04d-12-31", OBS_END_YEAR))

pkgs <- c(
  "readxl", "readr", "dplyr", "tidyr", "purrr",
  "countrycode", "haven", "tibble", "lubridate",
  "survival", "sandwich", "lmtest", "modelsummary", "WDI", "ggplot2"
)
new <- pkgs[!(pkgs %in% installed.packages()[,"Package"])]
if (length(new)) install.packages(new)
invisible(lapply(pkgs, library, character.only = TRUE))

stopifnot(
  file.exists(PATH_GWF),
  file.exists(PATH_MID),
  file.exists(PATH_GSDB),
  file.exists(PATH_VDEM),
  dir.exists(AIDDATA_DIR),
  file.exists(PATH_NMC),
  file.exists(PATH_GANDHI),
  file.exists(PATH_GWF_SCORES),
  file.exists(PATH_AIDDATA_PURPOSE)
)

# ============================================================
# 2) CONSTRUCT POPULATION AND UNIT OF ANALYSIS
# ============================================================

# 2.1 GWF regime data 
regimes_raw <- readxl::read_excel(PATH_GWF)

gwf_recode <- c(
  "Cen African Rep" = "Central African Republic",
  "Congo-Brz"       = "Congo - Brazzaville",
  "Congo/Zaire"     = "Congo - Kinshasa",
  "Dominican Rep"   = "Dominican Republic",
  "Guinea Bissau"   = "Guinea-Bissau",
  "Ivory Coast"     = "Côte d'Ivoire",
  "Korea South"     = "South Korea",
  "Korea North"     = "North Korea",
  "Vietnam South"   = "Republic of Vietnam",
  "Myanmar"         = "Myanmar (Burma)"
)

regimes_cy <- regimes_raw %>%
  mutate(
    gwf_startyr = as.integer(gwf_startyr),
    gwf_endyr   = as.integer(ifelse(is.na(gwf_endyr), OBS_END_YEAR, gwf_endyr)),
    categorical_personalism = if_else(
      grepl("personal", tolower(gwf_regimetype)), 1L, 0L
    )
  ) %>%
  rowwise() %>%
  mutate(year = list(gwf_startyr:gwf_endyr)) %>%
  unnest(year) %>%
  ungroup() %>%
  filter(year >= START_YEAR, year <= OBS_END_YEAR) %>%
  select(gwf_country, year, gwf_regimetype, categorical_personalism) %>%
  mutate(gwf_country = dplyr::recode(gwf_country, !!!gwf_recode))

# 2.2 MID dyad episodes 
mid_raw <- read.csv(PATH_MID)
make_date_safe <- function(y, m, d) {
  y <- as.integer(y); m <- as.integer(m); d <- as.integer(d)
  valid  <- !is.na(y) & !is.na(m) & !is.na(d) & m >= 1 & d >= 1
  result <- as.Date(rep(NA, length(y)))
  result[valid] <- as.Date(sprintf("%04d-%02d-%02d",
                                   y[valid], m[valid], d[valid]))
  result
}


mid_force <- mid_raw %>%
  mutate(
    styear   = as.integer(styear),
    stmonth  = as.integer(stmon),
    stday    = as.integer(stday),
    endyear  = as.integer(endyear),
    endmonth = as.integer(endmon),
    endday   = as.integer(endday)
  ) %>%
  filter(hostlev %in% c(4, 5)) %>%
  filter(stmonth >= 1, stday >= 1, endmonth >= 1, endday >= 1) %>%
  mutate(
    st_date  = make_date_safe(styear,  stmonth,  stday),
    end_date = make_date_safe(endyear, endmonth, endday)
  ) %>%
  filter(!is.na(st_date), !is.na(end_date)) %>%
  filter(year(st_date) <= OBS_END_YEAR, year(end_date) >= START_YEAR) %>%
  mutate(
    country_name = countrycode(ccode, origin = "cown",
                               destination = "country.name")
  ) %>%
  filter(!is.na(country_name))

mid_force <- mid_force %>%
  mutate(start_year = year(st_date)) %>%
  left_join(
    regimes_cy,
    by = c("country_name" = "gwf_country", "start_year" = "year")
  ) %>%
  mutate(
    is_autocratic           = !is.na(gwf_regimetype),
    categorical_personalism = if_else(
      is_autocratic, categorical_personalism, NA_integer_
    )
  )

dyads_disp <- mid_force %>%
  select(dispnum, hostlev, st_date, end_date,
         country_name, sidea,
         categorical_personalism) %>%
  distinct() %>%
  group_by(dispnum) %>%
  summarise(
    hostlev      = max(hostlev, na.rm = TRUE),
    st_date      = min(st_date, na.rm = TRUE),
    end_date     = max(end_date, na.rm = TRUE),
    participants = list(country_name),
    sides        = list(as.integer(sidea)),
    pers         = list(categorical_personalism),
    .groups      = "drop"
  ) %>%
  mutate(
    dyad_list = purrr::pmap(
      list(participants, sides, pers),
      function(participants, sides, pers) {
        if (length(participants) < 2) return(NULL)
        pairs <- combn(seq_along(participants), 2, simplify = FALSE)
        purrr::map_dfr(pairs, function(ix) {
          i <- ix[1]; j <- ix[2]
          c1 <- participants[i]; c2 <- participants[j]
          s1 <- sides[i];        s2 <- sides[j]
          p1 <- pers[i];         p2 <- pers[j]
          if (is.na(c1) || is.na(c2)) return(NULL)
          if (c1 <= c2) {
            tibble(
              country1 = c1, country2 = c2,
              sidea_country1 = s1, sidea_country2 = s2,
              personalism_country1 = as.integer(p1),
              personalism_country2 = as.integer(p2)
            )
          } else {
            tibble(
              country1 = c2, country2 = c1,
              sidea_country1 = s2, sidea_country2 = s1,
              personalism_country1 = as.integer(p2),
              personalism_country2 = as.integer(p1)
            )
          }
        })
      }
    )
  ) %>%
  unnest(dyad_list) %>%
  select(dispnum, hostlev, st_date, end_date,
         country1, country2,
         sidea_country1, sidea_country2,
         personalism_country1, personalism_country2)

dyad_spells <- dyads_disp %>%
  arrange(country1, country2, st_date) %>%
  group_by(country1, country2) %>%
  mutate(
    gap_days  = as.integer(st_date - lag(end_date)),
    new_spell = if_else(is.na(gap_days) | gap_days > 1L, 1L, 0L),
    spell_id  = cumsum(new_spell)
  ) %>%
  group_by(country1, country2, spell_id) %>%
  summarise(
    spell_start          = min(st_date),
    spell_end            = max(end_date),
    sidea_country1       = sidea_country1[which.min(st_date)],
    sidea_country2       = sidea_country2[which.min(st_date)],
    personalism_country1 = personalism_country1[which.min(st_date)],
    personalism_country2 = personalism_country2[which.min(st_date)],
    .groups = "drop"
  )

dyad_episodes <- dyad_spells %>%
  arrange(country1, country2, spell_start) %>%
  group_by(country1, country2) %>%
  mutate(
    prev_end    = lag(spell_end),
    gap_days_ep = as.integer(spell_start - prev_end),
    new_episode = if_else(is.na(gap_days_ep) | gap_days_ep > 1L, 1L, 0L),
    episode_id  = cumsum(new_episode)
  ) %>%
  group_by(country1, country2, episode_id) %>%
  summarise(
    episode_start        = min(spell_start),
    episode_end          = max(spell_end),
    sidea_country1       = sidea_country1[which.min(spell_start)],
    sidea_country2       = sidea_country2[which.min(spell_start)],
    personalism_country1 = personalism_country1[which.min(spell_start)],
    personalism_country2 = personalism_country2[which.min(spell_start)],
    .groups = "drop"
  ) %>%
  arrange(country1, country2, episode_start) %>%
  group_by(country1, country2) %>%
  mutate(next_episode_start = lead(episode_start)) %>%
  ungroup()

next_ep_tags <- dyad_episodes %>%
  select(country1, country2, episode_start,
         sidea_country1, sidea_country2) %>%
  rename(
    next_episode_start  = episode_start,
    next_sidea_country1 = sidea_country1,
    next_sidea_country2 = sidea_country2
  )

# 2.3 Autocratic initiator restriction 
episodes <- dyad_episodes %>%
  left_join(next_ep_tags,
            by = c("country1", "country2", "next_episode_start")) %>%
  mutate(
    initiator_country = case_when(
      sidea_country1 == 1L & sidea_country2 == 0L ~ country1,
      sidea_country2 == 1L & sidea_country1 == 0L ~ country2,
      TRUE ~ NA_character_
    ),
    next_initiator_country = case_when(
      is.na(next_episode_start)                              ~ NA_character_,
      next_sidea_country1 == 1L & next_sidea_country2 == 0L ~ country1,
      next_sidea_country2 == 1L & next_sidea_country1 == 0L ~ country2,
      TRUE ~ NA_character_
    ),
    initiator_iso3 = countrycode(initiator_country, "country.name", "iso3c"),
    start_year     = year(episode_start)
  ) %>%
  left_join(
    regimes_cy %>%
      transmute(gwf_country, year,
                gwf_regimetype,
                gwf_personalist = categorical_personalism),
    by = c("initiator_country" = "gwf_country", "start_year" = "year")
  ) %>%
  mutate(
    initiator_autocratic  = !is.na(gwf_regimetype),
    initiator_personalist = if_else(
      initiator_autocratic,
      as.integer(gwf_personalist == 1L),
      NA_integer_
    )
  ) %>%
  filter(
    !is.na(initiator_country),
    !is.na(initiator_iso3),
    initiator_autocratic,
    episode_end < as.Date(sprintf("%04d-12-31", FOCAL_END_YEAR))
  ) %>%
  mutate(
    episode_duration_days = as.integer(episode_end - episode_start) + 1L,
    cold_war              = as.integer(year(episode_start) <= 1989),
    dyad_id               = paste(country1, country2, sep = "--"),
    target_country        = case_when(
      initiator_country == country1 ~ country2,
      initiator_country == country2 ~ country1,
      TRUE ~ NA_character_
    ),
    target_iso3      = countrycode(target_country, "country.name", "iso3c"),
    episode_end_year = year(episode_end)
  )

cat("\nEpisodes after autocratic initiator restriction:", nrow(episodes), "\n")

# 2.4 Relapse outcome variables 
episodes <- episodes %>%
  mutate(
    relapse_event = as.integer(
      !is.na(next_episode_start) &
        next_initiator_country == initiator_country
    ),
    time_to_relapse_days = case_when(
      relapse_event == 1L ~ as.integer(next_episode_start - episode_end),
      TRUE                ~ as.integer(CENSOR_DATE - episode_end)
    ),
    time_to_relapse_days = pmax(time_to_relapse_days, 1L)
  )

na_relapse_check <- episodes %>%
  filter(!is.na(next_episode_start)) %>%
  summarise(
    total_with_next_ep = n(),
    na_next_initiator  = sum(is.na(next_initiator_country)),
    pct_na = round(100 * na_next_initiator / total_with_next_ep, 1)
  )
cat("\n=== RELAPSE CODING DIAGNOSTIC ===\n")
cat("Episodes with next episode:     ", na_relapse_check$total_with_next_ep, "\n")
cat("Next initiator NA:              ", na_relapse_check$na_next_initiator, "\n")
cat("Percentage potentially miscoded:", na_relapse_check$pct_na, "%\n")

# ============================================================
# 3) CONSTRUCT EXPLANATORY VARIABLES
# ============================================================

# 3.1 Personalism constructed in Section 2.3) 
# initiator_personalist is binary: 1 = personalist, 0 = other autocracy

# 3.2 Elite sanctions intensity 
gsdb_raw <- readr::read_csv(PATH_GSDB, show_col_types = FALSE)

gsdb_raw <- gsdb_raw %>%
  mutate(sanctioned_state = case_when(
    sanctioned_state == "Egypt, Arab Rep."                  ~ "Egypt",
    sanctioned_state == "Congo (Brazzaville)"               ~ "Congo - Brazzaville",
    sanctioned_state == "Congo, Democratic Republic of the" ~ "Congo - Kinshasa",
    sanctioned_state == "Myanmar"                           ~ "Myanmar (Burma)",
    sanctioned_state == "Korea, North"                      ~ "North Korea",
    sanctioned_state == "Korea, South"                      ~ "South Korea",
    sanctioned_state == "Ethiopia (excludes Eritrea)"       ~ "Ethiopia",
    sanctioned_state == "Gambia, The"                       ~ "Gambia",
    sanctioned_state == "Yemen, North"                      ~ "Yemen",
    TRUE ~ sanctioned_state
  ))

tier3_keywords <- c(
  "\\bUN\\b", "\\bEU\\b", "\\bEEC\\b",
  "African Union", "ECOWAS",
  "Organization of American States", "Organisation of American States",
  "Organisation of African Unity",
  "League of Arab States",
  "\\bNATO\\b", "Commonwealth",
  "\\bOSCE\\b", "\\bCSCE\\b",
  "\\bSADC\\b", "\\bOIC\\b",
  "\\bOAPEC\\b", "\\bCoCom\\b", "\\bChinCom\\b",
  "\\bG7\\b", "\\bG8\\b",
  "\\bNAFTA\\b", "\\bMERCOSUR\\b", "\\bUNASUR\\b",
  "Pacific Islands Forum",
  "Organization of Eastern Carib",
  "Kimberly Process",
  "Paris Agreement",
  "\\bOASU?\\b"
)
tier3_pattern <- paste(tier3_keywords, collapse = "|")

classify_sender_tier <- function(s) {
  if (is.na(s)) return(1.0)
  if (grepl(tier3_pattern, s, perl = TRUE)) return(2.0)
  n_commas <- nchar(s) - nchar(gsub(",", "", s, fixed = TRUE))
  if (n_commas >= 10) return(2.0)
  if (n_commas >= 2)  return(1.5)
  return(1.0)
}

gsdb_cy <- gsdb_raw %>%
  mutate(
    begin           = as.integer(begin),
    end             = as.integer(end),
    sanctioned_iso3 = countrycode(sanctioned_state, "country.name", "iso3c"),
    trade      = as.integer(as.numeric(trade)     > 0),
    financial  = as.integer(as.numeric(financial) > 0),
    travel     = as.integer(as.numeric(travel)    > 0),
    arms       = as.integer(as.numeric(arms)       > 0),
    military   = as.integer(as.numeric(military)   > 0),
    other      = as.integer(as.numeric(other)      > 0),
    sender_mult = as.integer(as.numeric(sender_mult) > 0),
    sender_tier = sapply(sanctioning_state, classify_sender_tier)
  ) %>%
  filter(!is.na(sanctioned_iso3), !is.na(begin), !is.na(end),
         begin <= end) %>%
  rowwise() %>%
  mutate(year = list(begin:end)) %>%
  unnest(year) %>%
  ungroup() %>%
  filter(year >= START_YEAR, year <= COV_END_YEAR) %>%
  select(sanctioned_iso3, year,
         trade, financial, travel, arms, military, other,
         sender_mult, sender_tier)

episodes <- episodes %>%
  mutate(
    sanc_start_year = year(episode_start),
    sanc_end_year   = year(episode_end %m+% years(SANCTIONS_POSTWAR_YEARS))
  )

sanctions_cases <- episodes %>%
  select(dyad_id, episode_start, initiator_iso3,
         sanc_start_year, sanc_end_year) %>%
  left_join(gsdb_cy,
            by           = c("initiator_iso3" = "sanctioned_iso3"),
            relationship = "many-to-many") %>%
  filter(year >= sanc_start_year, year <= sanc_end_year) %>%
  mutate(
    elite_base      = financial * 0.5 + travel * 0.5,
    elite_weighted  = sender_tier * elite_base,
    all_base        = (trade + financial + travel + arms + military + other) / 6,
    all_weighted    = sender_tier * all_base,
    is_bilateral    = as.integer(sender_mult == 0L),
    is_multilateral = as.integer(sender_mult == 1L)
  )

sanctions_joined <- sanctions_cases %>%
  group_by(dyad_id, episode_start) %>%
  summarise(
    elite_sanctions_intensity    = sum(elite_weighted,               na.rm = TRUE),
    elite_sanctions_bilateral    = sum(elite_base * is_bilateral,    na.rm = TRUE),
    elite_sanctions_multilateral = sum(elite_base * is_multilateral, na.rm = TRUE),
    all_sanctions_intensity      = sum(all_weighted,                 na.rm = TRUE),
    .groups = "drop"
  )

episodes <- episodes %>%
  select(-any_of(c("elite_sanctions_intensity", "all_sanctions_intensity",
                   "elite_sanctions_bilateral", "elite_sanctions_multilateral"))) %>%
  left_join(sanctions_joined, by = c("dyad_id", "episode_start")) %>%
  mutate(
    elite_sanctions_intensity    = replace_na(elite_sanctions_intensity,    0),
    elite_sanctions_bilateral    = replace_na(elite_sanctions_bilateral,    0),
    elite_sanctions_multilateral = replace_na(elite_sanctions_multilateral, 0),
    all_sanctions_intensity      = replace_na(all_sanctions_intensity,      0)
  )

cat("\n=== SANCTIONS DIAGNOSTICS ===\n")
cat("Elite sanctions intensity — mean:", round(mean(episodes$elite_sanctions_intensity), 3), "\n")
cat("Elite sanctions intensity — max: ", round(max(episodes$elite_sanctions_intensity),  3), "\n")
cat("Elite sanctions intensity — SD:  ", round(sd(episodes$elite_sanctions_intensity),   3), "\n")
cat("Episodes with any elite sanctions:", sum(episodes$elite_sanctions_intensity > 0), "\n")

# 3.3 Aid/GDP post-conflict 

aid_raw <- readr::read_csv(
  file.path(AIDDATA_DIR,
            "AidDataCoreDonorRecipientYear_ResearchRelease_Level1_v3.1.csv"),
  show_col_types = FALSE
)
file.exists(file.path(AIDDATA_DIR,
                      "AidDataCoreDonorRecipientYear_ResearchRelease_Level1_v3.1.csv"))

aid_cy <- aid_raw %>%
  transmute(
    recipient_iso3 = countrycode(recipient, "country.name", "iso3c"),
    year           = as.integer(year),
    aid_amount     = suppressWarnings(
      as.numeric(commitment_amount_usd_constant_sum))
  ) %>%
  filter(!is.na(recipient_iso3), !is.na(year), !is.na(aid_amount)) %>%
  filter(year >= START_YEAR, year <= COV_END_YEAR) %>%
  group_by(recipient_iso3, year) %>%
  summarise(aid_amount = sum(aid_amount, na.rm = TRUE), .groups = "drop")

pop_raw <- WDI(country = "all", indicator = "SP.POP.TOTL",
               start = START_YEAR, end = COV_END_YEAR)
pop_cy <- pop_raw %>%
  transmute(
    iso3c      = iso3c,
    year       = as.integer(year),
    population = as.numeric(SP.POP.TOTL)
  ) %>%
  filter(!is.na(iso3c), !is.na(year), !is.na(population))

gdp_total_raw <- WDI(country = "all", indicator = "NY.GDP.MKTP.KD",
                     start = START_YEAR, end = COV_END_YEAR)
gdp_total_cy <- gdp_total_raw %>%
  transmute(
    iso3c     = iso3c,
    year      = as.integer(year),
    gdp_total = as.numeric(NY.GDP.MKTP.KD)
  ) %>%
  filter(!is.na(iso3c), !is.na(year), !is.na(gdp_total), gdp_total > 0)

aid_cy_pc <- aid_cy %>%
  left_join(pop_cy, by = c("recipient_iso3" = "iso3c", "year")) %>%
  filter(!is.na(population), population > 0) %>%
  mutate(aid_pc = aid_amount / population)

aid_cy_gdp <- aid_cy %>%
  left_join(gdp_total_cy, by = c("recipient_iso3" = "iso3c", "year")) %>%
  filter(!is.na(gdp_total), gdp_total > 0) %>%
  mutate(aid_gdp_ratio = aid_amount / gdp_total)

episodes <- episodes %>%
  mutate(
    aid_pre_start_year  = year(episode_start %m-% years(AID_PRE_YEARS)),
    aid_pre_end_year    = year(episode_start %m-% years(1)),
    aid_post_start_year = year(episode_end   + days(1)),
    aid_post_end_year   = year(episode_end   %m+% years(AID_POST_YEARS))
  )

aid_pre_pc <- episodes %>%
  select(dyad_id, episode_start, initiator_iso3,
         aid_pre_start_year, aid_pre_end_year) %>%
  left_join(aid_cy_pc, by = c("initiator_iso3" = "recipient_iso3"),
            relationship = "many-to-many") %>%
  filter(year >= aid_pre_start_year, year <= aid_pre_end_year) %>%
  group_by(dyad_id, episode_start) %>%
  summarise(aid_pc_pre_5y = mean(aid_pc, na.rm = TRUE), .groups = "drop")

aid_post_pc <- episodes %>%
  select(dyad_id, episode_start, initiator_iso3,
         aid_post_start_year, aid_post_end_year) %>%
  left_join(aid_cy_pc, by = c("initiator_iso3" = "recipient_iso3"),
            relationship = "many-to-many") %>%
  filter(year >= aid_post_start_year, year <= aid_post_end_year) %>%
  group_by(dyad_id, episode_start) %>%
  summarise(aid_pc_post_5y = mean(aid_pc, na.rm = TRUE), .groups = "drop")

aid_pre_gdp <- episodes %>%
  select(dyad_id, episode_start, initiator_iso3,
         aid_pre_start_year, aid_pre_end_year) %>%
  left_join(aid_cy_gdp, by = c("initiator_iso3" = "recipient_iso3"),
            relationship = "many-to-many") %>%
  filter(year >= aid_pre_start_year, year <= aid_pre_end_year) %>%
  group_by(dyad_id, episode_start) %>%
  summarise(aid_gdp_pre_5y = mean(aid_gdp_ratio, na.rm = TRUE), .groups = "drop")

aid_post_gdp <- episodes %>%
  select(dyad_id, episode_start, initiator_iso3,
         aid_post_start_year, aid_post_end_year) %>%
  left_join(aid_cy_gdp, by = c("initiator_iso3" = "recipient_iso3"),
            relationship = "many-to-many") %>%
  filter(year >= aid_post_start_year, year <= aid_post_end_year) %>%
  group_by(dyad_id, episode_start) %>%
  summarise(aid_gdp_post_5y = mean(aid_gdp_ratio, na.rm = TRUE), .groups = "drop")

episodes <- episodes %>%
  left_join(aid_pre_pc,   by = c("dyad_id", "episode_start")) %>%
  left_join(aid_post_pc,  by = c("dyad_id", "episode_start")) %>%
  left_join(aid_pre_gdp,  by = c("dyad_id", "episode_start")) %>%
  left_join(aid_post_gdp, by = c("dyad_id", "episode_start")) %>%
  mutate(
    aid_pc_pre_5y      = replace_na(aid_pc_pre_5y,   0),
    aid_pc_post_5y     = replace_na(aid_pc_post_5y,  0),
    aid_gdp_pre_5y     = replace_na(aid_gdp_pre_5y,  0),
    aid_gdp_post_5y    = replace_na(aid_gdp_post_5y, 0),
    aid_gdp_post_log   = log1p(aid_gdp_post_5y),
    aid_pc_change_log  = log1p(aid_pc_post_5y)  - log1p(aid_pc_pre_5y),
    aid_gdp_change_log = log1p(aid_gdp_post_5y) - log1p(aid_gdp_pre_5y),
    aid_pc_post_log    = log1p(aid_pc_post_5y)
  )

# 3.4 Military asymmetry trend (TE3 variable, with interaction) ----
nmc_raw <- readr::read_csv(PATH_NMC, show_col_types = FALSE)

nmc_cy <- nmc_raw %>%
  transmute(
    iso3c = countrycode(stateabb, origin = "cowc", destination = "iso3c"),
    year  = as.integer(year),
    cinc  = as.numeric(cinc)
  ) %>%
  filter(!is.na(iso3c), !is.na(year), !is.na(cinc), cinc > 0)

post_cinc <- episodes %>%
  mutate(end_year = year(episode_end)) %>%
  select(dyad_id, episode_start, initiator_iso3,
         target_iso3, end_year) %>%
  tidyr::crossing(offset = 0:5) %>%
  mutate(year = end_year + offset) %>%
  left_join(nmc_cy %>% rename(cinc_i = cinc),
            by = c("initiator_iso3" = "iso3c", "year")) %>%
  left_join(nmc_cy %>% rename(cinc_t = cinc),
            by = c("target_iso3" = "iso3c", "year")) %>%
  filter(!is.na(cinc_i), !is.na(cinc_t), cinc_i > 0, cinc_t > 0) %>%
  mutate(ratio = log(cinc_i / cinc_t))

milasym_5yr <- post_cinc %>%
  filter(offset >= 1) %>%
  group_by(dyad_id, episode_start) %>%
  filter(n() >= 3) %>%
  summarise(
    milasym_trend_5yr = coef(lm(ratio ~ offset))[["offset"]],
    .groups = "drop"
  )

episodes <- episodes %>%
  left_join(milasym_5yr, by = c("dyad_id", "episode_start")) %>%
  mutate(milasym_trend_5yr = replace_na(milasym_trend_5yr, 0))

cat("\n=== MILITARY ASYMMETRY TREND DIAGNOSTIC ===\n")
cat("5-yr trend (TE3, with interaction):", sum(!is.na(episodes$milasym_trend_5yr)), "\n")

# ============================================================
# 4) CONSTRUCT CONTROL VARIABLES
# ============================================================

# 4.1 Episode duration (already in episodes from Section 2.3) 

# 4.2 Democratization
loaded_names    <- load(PATH_VDEM)
vdem_candidates <- loaded_names[sapply(loaded_names,
                                       function(nm) is.data.frame(get(nm)))]
if (length(vdem_candidates) == 0) stop("No data.frame in V-Dem RData.")
vdem_df <- get(vdem_candidates[which.max(
  sapply(vdem_candidates, function(nm) nrow(get(nm))))])
stopifnot(all(c("country_name", "year", "v2x_polyarchy") %in% names(vdem_df)))

vdem_dem <- vdem_df %>%
  mutate(
    iso3c     = countrycode(country_name, "country.name", "iso3c"),
    year      = as.integer(year),
    polyarchy = as.numeric(v2x_polyarchy)
  ) %>%
  select(iso3c, year, polyarchy) %>%
  filter(!is.na(iso3c))

episodes <- episodes %>%
  mutate(
    dem_pre_start  = year(episode_start %m-% years(5)),
    dem_pre_end    = year(episode_start %m-% years(1)),
    dem_post_start = year(episode_end   + days(1)),
    dem_post_end   = year(episode_end   %m+% years(5))
  )

dem_pre_joined <- episodes %>%
  select(dyad_id, episode_start, initiator_iso3,
         dem_pre_start, dem_pre_end) %>%
  left_join(vdem_dem, by = c("initiator_iso3" = "iso3c"),
            relationship = "many-to-many") %>%
  filter(year >= dem_pre_start, year <= dem_pre_end) %>%
  group_by(dyad_id, episode_start) %>%
  summarise(dem_pre = mean(polyarchy, na.rm = TRUE), .groups = "drop")

dem_post_joined <- episodes %>%
  select(dyad_id, episode_start, initiator_iso3,
         dem_post_start, dem_post_end) %>%
  left_join(vdem_dem, by = c("initiator_iso3" = "iso3c"),
            relationship = "many-to-many") %>%
  filter(year >= dem_post_start, year <= dem_post_end) %>%
  group_by(dyad_id, episode_start) %>%
  summarise(dem_post = mean(polyarchy, na.rm = TRUE), .groups = "drop")

episodes <- episodes %>%
  left_join(dem_pre_joined,  by = c("dyad_id", "episode_start")) %>%
  left_join(dem_post_joined, by = c("dyad_id", "episode_start")) %>%
  mutate(
    dem_pre    = if_else(is.nan(dem_pre),  NA_real_, dem_pre),
    dem_post   = if_else(is.nan(dem_post), NA_real_, dem_post),
    dem_change = dem_post - dem_pre
  )

# 4.3 GDP per capita 
gdp_raw <- WDI(country = "all", indicator = "NY.GDP.PCAP.KD",
               start = START_YEAR, end = COV_END_YEAR)
gdp_cy <- gdp_raw %>%
  transmute(
    iso3c  = iso3c,
    year   = as.integer(year),
    gdp_pc = as.numeric(NY.GDP.PCAP.KD)
  ) %>%
  filter(!is.na(iso3c), !is.na(year), !is.na(gdp_pc))

episodes <- episodes %>%
  mutate(
    gdp_pre_start = year(episode_start %m-% years(5)),
    gdp_pre_end   = year(episode_start %m-% years(1))
  )

gdp_joined <- episodes %>%
  select(dyad_id, episode_start, initiator_iso3,
         gdp_pre_start, gdp_pre_end) %>%
  left_join(gdp_cy, by = c("initiator_iso3" = "iso3c"),
            relationship = "many-to-many") %>%
  filter(year >= gdp_pre_start, year <= gdp_pre_end) %>%
  group_by(dyad_id, episode_start) %>%
  summarise(gdp_pc_pre = mean(gdp_pc, na.rm = TRUE), .groups = "drop")

episodes <- episodes %>%
  left_join(gdp_joined, by = c("dyad_id", "episode_start")) %>%
  mutate(
    gdp_pc_pre     = if_else(is.nan(gdp_pc_pre) | is.na(gdp_pc_pre),
                             NA_real_, gdp_pc_pre),
    log_gdp_pc_pre = log(gdp_pc_pre)
  )

# 4.4 Shared border
shared_border_manual <- tribble(
  ~initiator_country,        ~target_country,            ~shared_border,
  "Algeria",                 "Morocco",                   1,
  "Angola",                  "Congo - Brazzaville",       1,
  "Argentina",               "Chile",                     1,
  "Armenia",                 "Azerbaijan",                1,
  "Bangladesh",              "India",                     1,
  "Burundi",                 "Tanzania",                  1,
  "Cameroon",                "Nigeria",                   1,
  "Chad",                    "Niger",                     1,
  "Chad",                    "Nigeria",                   0,
  "Chad",                    "Sudan",                     1,
  "Chile",                   "Argentina",                 1,
  "Chile",                   "Peru",                      1,
  "China",                   "India",                     1,
  "China",                   "Myanmar (Burma)",            1,
  "China",                   "Russia",                    1,
  "China",                   "Taiwan",                    0,
  "China",                   "United States",             0,
  "China",                   "Vietnam",                   1,
  "Congo - Brazzaville",     "Central African Republic",  1,
  "Congo - Brazzaville",     "Egypt",                     0,
  "Congo - Brazzaville",     "France",                    0,
  "Congo - Brazzaville",     "Morocco",                   0,
  "Congo - Kinshasa",        "Angola",                    1,
  "Congo - Kinshasa",        "Burundi",                   1,
  "Congo - Kinshasa",        "Congo - Brazzaville",       1,
  "Congo - Kinshasa",        "Rwanda",                    1,
  "Congo - Kinshasa",        "Uganda",                    1,
  "Congo - Kinshasa",        "Zambia",                    1,
  "Cuba",                    "Central African Republic",  0,
  "Cuba",                    "Egypt",                     0,
  "Cuba",                    "France",                    0,
  "Cuba",                    "Morocco",                   0,
  "Ecuador",                 "Peru",                      1,
  "Egypt",                   "Cyprus",                    0,
  "Egypt",                   "Israel",                    1,
  "Egypt",                   "Libya",                     1,
  "Egypt",                   "Sudan",                     1,
  "El Salvador",             "Honduras",                  1,
  "Eritrea",                 "Djibouti",                  1,
  "Eritrea",                 "Yemen",                     0,
  "Ethiopia",                "Eritrea",                   1,
  "Ethiopia",                "Kenya",                     1,
  "Ethiopia",                "Somalia",                   1,
  "Ethiopia",                "Sudan",                     1,
  "Ghana",                   "Sierra Leone",              0,
  "Ghana",                   "Togo",                      1,
  "Guinea",                  "Sierra Leone",              1,
  "Honduras",                "El Salvador",               1,
  "Indonesia",               "Malaysia",                  1,
  "Indonesia",               "United Kingdom",            0,
  "Iran",                    "Afghanistan",               1,
  "Iran",                    "Iraq",                      1,
  "Iran",                    "United States",             0,
  "Iraq",                    "Australia",                 0,
  "Iraq",                    "Bahrain",                   0,
  "Iraq",                    "Canada",                    0,
  "Iraq",                    "Egypt",                     0,
  "Iraq",                    "France",                    0,
  "Iraq",                    "Iran",                      1,
  "Iraq",                    "Israel",                    0,
  "Iraq",                    "Italy",                     0,
  "Iraq",                    "Kuwait",                    1,
  "Iraq",                    "Oman",                      0,
  "Iraq",                    "Qatar",                     0,
  "Iraq",                    "Saudi Arabia",              1,
  "Iraq",                    "Syria",                     1,
  "Iraq",                    "Turkey",                    1,
  "Iraq",                    "United Arab Emirates",      0,
  "Iraq",                    "United Kingdom",            0,
  "Iraq",                    "United States",             0,
  "Kenya",                   "Egypt",                     0,
  "Kuwait",                  "Iraq",                      1,
  "Libya",                   "Chad",                      1,
  "Libya",                   "Congo - Kinshasa",          0,
  "Libya",                   "France",                    0,
  "Libya",                   "Tanzania",                  0,
  "Libya",                   "United States",             0,
  "Mali",                    "Burkina Faso",              1,
  "Mauritania",              "Mali",                      1,
  "Morocco",                 "Algeria",                   1,
  "Morocco",                 "Spain",                     1,
  "Mozambique",              "South Africa",              1,
  "Myanmar (Burma)",         "Thailand",                  1,
  "Nicaragua",               "Costa Rica",                1,
  "Nicaragua",               "Honduras",                  1,
  "Nigeria",                 "Côte d'Ivoire",             0,
  "Nigeria",                 "Sierra Leone",              0,
  "Pakistan",                "Afghanistan",               1,
  "Pakistan",                "India",                     1,
  "Pakistan",                "United States",             0,
  "Panama",                  "United States",             0,
  "Paraguay",                "Argentina",                 1,
  "Portugal",                "Malawi",                    0,
  "Portugal",                "Senegal",                   0,
  "Portugal",                "Tanzania",                  0,
  "Portugal",                "Zambia",                    0,
  "Russia",                  "Afghanistan",               0,
  "Rwanda",                  "Congo - Kinshasa",          1,
  "Saudi Arabia",            "Iraq",                      1,
  "Saudi Arabia",            "Israel",                    0,
  "Saudi Arabia",            "Yemen",                     1,
  "Senegal",                 "Gambia",                    1,
  "Senegal",                 "Guinea-Bissau",             1,
  "Senegal",                 "Mauritania",                1,
  "Somalia",                 "Cuba",                      0,
  "Somalia",                 "Ethiopia",                  1,
  "Somalia",                 "France",                    0,
  "Somalia",                 "Kenya",                     1,
  "South Africa",            "Botswana",                  1,
  "South Africa",            "Congo - Kinshasa",          0,
  "South Africa",            "Zambia",                    1,
  "South Korea",             "North Korea",               1,
  "South Korea",             "Vietnam",                   0,
  "Sudan",                   "Chad",                      1,
  "Sudan",                   "Eritrea",                   1,
  "Sudan",                   "Ethiopia",                  1,
  "Syria",                   "Iraq",                      1,
  "Syria",                   "Israel",                    1,
  "Syria",                   "Jordan",                    1,
  "Syria",                   "Turkey",                    1,
  "Syria",                   "United States",             0,
  "Tajikistan",              "Afghanistan",               1,
  "Thailand",                "Laos",                      1,
  "Uganda",                  "Congo - Kinshasa",          1,
  "Uganda",                  "Kenya",                     1,
  "Uganda",                  "Rwanda",                    1,
  "Uganda",                  "Sudan",                     1,
  "United Arab Emirates",    "Iran",                      0,
  "Uzbekistan",              "Afghanistan",               1,
  "Vietnam",                 "Thailand",                  0,
  "Yemen",                   "Saudi Arabia",              1,
  "Zambia",                  "Zimbabwe",                  1
)

episodes <- episodes %>%
  select(-any_of("shared_border")) %>%
  left_join(shared_border_manual,
            by = c("initiator_country", "target_country")) %>%
  mutate(shared_border = replace_na(shared_border, 0L))

cat("Shared border distribution:\n")
print(table(episodes$shared_border))

# 4.5 Regime duration 
regime_duration_tbl <- regimes_raw %>%
  mutate(
    gwf_startyr = as.integer(gwf_startyr),
    gwf_endyr   = as.integer(ifelse(is.na(gwf_endyr), OBS_END_YEAR, gwf_endyr)),
    gwf_country = dplyr::recode(gwf_country, !!!gwf_recode)
  ) %>%
  select(gwf_country, gwf_startyr, gwf_endyr)

episodes <- episodes %>%
  left_join(regime_duration_tbl,
            by = c("initiator_country" = "gwf_country"),
            relationship = "many-to-many") %>%
  filter(episode_end_year >= gwf_startyr,
         episode_end_year <= gwf_endyr) %>%
  mutate(regime_duration = as.integer(episode_end_year - gwf_startyr)) %>%
  group_by(dyad_id, episode_start) %>%
  slice_max(regime_duration, n = 1, with_ties = FALSE) %>%
  ungroup()

# 4.6 Prior conflicts (1946 baseline) 

prior_conflicts_raw <- mid_raw %>%
  mutate(
    styear   = as.integer(styear),
    stmonth  = as.integer(stmon),
    stday    = as.integer(stday),
    endyear  = as.integer(endyear),
    endmonth = as.integer(endmon),
    endday   = as.integer(endday)
  ) %>%
  filter(hostlev %in% c(4, 5)) %>%
  filter(styear >= PRIOR_CONFLICTS_START, styear <= FOCAL_END_YEAR) %>%
  filter(stmonth >= 1, stday >= 1, endmonth >= 1, endday >= 1) %>%
  mutate(
    st_date  = make_date_safe(styear,  stmonth, stday),
    end_date = make_date_safe(endyear, endmonth, endday)
  ) %>%
  filter(!is.na(st_date), !is.na(end_date)) %>%
  mutate(
    country_name = countrycode(ccode, origin = "cown",
                               destination = "country.name")
  ) %>%
  filter(!is.na(country_name))

prior_dyads_raw <- prior_conflicts_raw %>%
  select(dispnum, st_date, end_date, country_name, sidea) %>%
  distinct() %>%
  group_by(dispnum) %>%
  summarise(
    st_date      = min(st_date, na.rm = TRUE),
    end_date     = max(end_date, na.rm = TRUE),
    participants = list(country_name),
    sides        = list(as.integer(sidea)),
    .groups      = "drop"
  ) %>%
  mutate(
    dyad_list = purrr::pmap(
      list(participants, sides),
      function(participants, sides) {
        if (length(participants) < 2) return(NULL)
        pairs <- combn(seq_along(participants), 2, simplify = FALSE)
        purrr::map_dfr(pairs, function(ix) {
          i <- ix[1]; j <- ix[2]
          c1 <- participants[i]; c2 <- participants[j]
          s1 <- sides[i];        s2 <- sides[j]
          if (is.na(c1) || is.na(c2)) return(NULL)
          if (c1 <= c2) {
            tibble(country1 = c1, country2 = c2,
                   sidea_c1 = s1, sidea_c2 = s2)
          } else {
            tibble(country1 = c2, country2 = c1,
                   sidea_c1 = s2, sidea_c2 = s1)
          }
        })
      }
    )
  ) %>%
  unnest(dyad_list) %>%
  select(dispnum, st_date, end_date, country1, country2)

prior_spells <- prior_dyads_raw %>%
  arrange(country1, country2, st_date) %>%
  group_by(country1, country2) %>%
  mutate(
    gap_days  = as.integer(st_date - lag(end_date)),
    new_spell = if_else(is.na(gap_days) | gap_days > 1L, 1L, 0L),
    spell_id  = cumsum(new_spell)
  ) %>%
  group_by(country1, country2, spell_id) %>%
  summarise(
    spell_start = min(st_date),
    spell_end   = max(end_date),
    .groups     = "drop"
  )

prior_episodes_tbl <- prior_spells %>%
  arrange(country1, country2, spell_start) %>%
  group_by(country1, country2) %>%
  mutate(
    prev_end    = lag(spell_end),
    gap_days_ep = as.integer(spell_start - prev_end),
    new_episode = if_else(is.na(gap_days_ep) | gap_days_ep > 1L, 1L, 0L),
    episode_id  = cumsum(new_episode)
  ) %>%
  group_by(country1, country2, episode_id) %>%
  summarise(
    episode_start = min(spell_start),
    episode_end   = max(spell_end),
    .groups       = "drop"
  ) %>%
  ungroup()

# For each focal episode, count prior episodes from 1946 baseline
prior_conflicts_tbl <- episodes %>%
  select(country1, country2, episode_start) %>%
  left_join(
    prior_episodes_tbl %>%
      rename(prior_ep_start = episode_start,
             prior_ep_end   = episode_end),
    by = c("country1", "country2"),
    relationship = "many-to-many"
  ) %>%
  filter(prior_ep_start < episode_start) %>%
  group_by(country1, country2, episode_start) %>%
  summarise(prior_conflicts = n(), .groups = "drop")

episodes <- episodes %>%
  left_join(prior_conflicts_tbl,
            by = c("country1", "country2", "episode_start")) %>%
  mutate(prior_conflicts = replace_na(prior_conflicts, 0L))

cat("\n=== PRIOR CONFLICTS DIAGNOSTIC (1946 baseline) ===\n")
cat("Mean prior conflicts:", round(mean(episodes$prior_conflicts), 2), "\n")
cat("Max prior conflicts: ", max(episodes$prior_conflicts), "\n")

# 4.7 Cold War (already constructed in Section 2.3) 

# 4.8 Baseline military asymmetry ratio (control)
cinc_joined <- episodes %>%
  select(dyad_id, episode_start, initiator_iso3,
         target_iso3, start_year) %>%
  left_join(
    nmc_cy %>% rename(cinc_initiator = cinc),
    by = c("initiator_iso3" = "iso3c", "start_year" = "year")
  ) %>%
  left_join(
    nmc_cy %>% rename(cinc_target = cinc),
    by = c("target_iso3" = "iso3c", "start_year" = "year")
  ) %>%
  mutate(
    cinc_initiator = if_else(cinc_initiator <= 0, NA_real_, cinc_initiator),
    cinc_target    = if_else(cinc_target    <= 0, NA_real_, cinc_target),
    milasym_ratio  = log(cinc_initiator / cinc_target)
  ) %>%
  select(dyad_id, episode_start, cinc_initiator, cinc_target, milasym_ratio)

episodes <- episodes %>%
  left_join(cinc_joined, by = c("dyad_id", "episode_start"))

cat("\n=== MILITARY ASYMMETRY RATIO DIAGNOSTIC (control variable) ===\n")
cat("Static onset ratio (control only):", sum(!is.na(episodes$milasym_ratio)), "\n")
cat("Correlation milasym_ratio vs milasym_trend_5yr: computed after final sample\n")

# ============================================================
# 5) FINAL SAMPLE
# ============================================================
model_vars_final <- c(
  "time_to_relapse_days",
  "relapse_event",
  "initiator_personalist",
  "elite_sanctions_intensity",
  "elite_sanctions_bilateral",
  "elite_sanctions_multilateral",
  "aid_gdp_post_log",
  "aid_pc_change_log",
  "aid_pc_post_log",
  "milasym_ratio",
  "milasym_trend_5yr",
  "dem_change",
  "log_gdp_pc_pre",
  "episode_duration_days",
  "cold_war",
  "prior_conflicts",
  "regime_duration",
  "shared_border",
  "dyad_id"
)

final_data <- episodes %>%
  tidyr::drop_na(dplyr::all_of(model_vars_final)) %>%
  filter(initiator_personalist %in% c(0L, 1L)) %>%
  mutate(
    initiator_personalist = as.integer(initiator_personalist),
    relapse_event         = as.integer(relapse_event),
    relapse_5yr = as.integer(relapse_event == 1 &
                               time_to_relapse_days <= 1825),
    time_cox  = pmin(time_to_relapse_days, CENSOR_DAYS),
    event_cox = as.integer(relapse_event == 1 &
                             time_to_relapse_days <= CENSOR_DAYS)
  )

cat("\n=== FINAL SAMPLE ===\n")
cat("N episodes:           ", nrow(final_data), "\n")
cat("Relapse (5yr):        ", sum(final_data$relapse_5yr),    "\n")
cat("Relapse (ever):       ", sum(final_data$relapse_event),  "\n")
cat("Cox events (<=5yr):   ", sum(final_data$event_cox),      "\n")
cat("Censored (Cox):       ", sum(final_data$event_cox == 0), "\n")
cat("Personalist:          ", sum(final_data$initiator_personalist == 1L), "\n")
cat("Other autocracy:      ", sum(final_data$initiator_personalist == 0L), "\n\n")

cat("Correlation milasym_ratio vs milasym_trend_5yr:",
    round(cor(final_data$milasym_ratio, final_data$milasym_trend_5yr,
              use = "complete.obs"), 3), "\n")

cat("\n=== DESCRIPTIVE STATISTICS ===\n")
desc_vars <- final_data %>%
  select(relapse_5yr, relapse_event,
         initiator_personalist,
         elite_sanctions_intensity,
         aid_gdp_post_log,
         milasym_ratio, milasym_trend_5yr,
         episode_duration_days, cold_war,
         dem_change, log_gdp_pc_pre, prior_conflicts,
         regime_duration, shared_border)
print(summary(desc_vars))
cat("\nRelapse rate 5-year window (primary):", round(mean(final_data$relapse_5yr),   3), "\n")
cat("Relapse rate any time (robustness):  ", round(mean(final_data$relapse_event), 3), "\n")


# ============================================================
# TABLES 1 & 2: Descriptive Statistics and Word export
# ============================================================

library(flextable)
library(officer)

dir.create("Final_tables", showWarnings = FALSE)

desc_vars_list <- c(
  "relapse_5yr", "initiator_personalist",
  "elite_sanctions_intensity", "aid_gdp_post_log",
  "milasym_trend_5yr", "milasym_ratio",
  "episode_duration_days", "cold_war", "dem_change",
  "log_gdp_pc_pre", "prior_conflicts", "regime_duration", "shared_border"
)

desc_var_labels <- c(
  relapse_5yr               = "Relapse Event",
  initiator_personalist     = "Personalist Regime",
  elite_sanctions_intensity = "Elite Sanctions Intensity",
  aid_gdp_post_log          = "Post-conflict Aid (log, % GDP)",
  milasym_trend_5yr         = "Military Asymmetry Trend",
  milasym_ratio             = "Military Asymmetry Ratio",
  episode_duration_days     = "Episode Duration (days)",
  cold_war                  = "Cold War",
  dem_change                = "Democracy Change",
  log_gdp_pc_pre            = "Log GDP per Capita",
  prior_conflicts           = "Prior Conflicts",
  regime_duration           = "Regime Duration",
  shared_border             = "Shared Border"
)

desc_vars_list_t2 <- desc_vars_list[desc_vars_list != "initiator_personalist"]

# TABLE 1
table1 <- bind_rows(lapply(desc_vars_list, function(v) {
  x <- final_data[[v]]
  data.frame(
    Variable = desc_var_labels[v],
    N        = sum(!is.na(x)),
    Mean     = round(mean(x,    na.rm = TRUE), 2),
    SD       = round(sd(x,      na.rm = TRUE), 2),
    Min      = round(min(x,     na.rm = TRUE), 2),
    Median   = round(median(x,  na.rm = TRUE), 2),
    Max      = round(max(x,     na.rm = TRUE), 2),
    stringsAsFactors = FALSE
  )
}))

# TABLE 2
table2 <- bind_rows(lapply(desc_vars_list_t2, function(v) {
  non_p <- final_data %>% filter(initiator_personalist == 0) %>% pull(!!sym(v)) %>% na.omit()
  pers  <- final_data %>% filter(initiator_personalist == 1) %>% pull(!!sym(v)) %>% na.omit()
  data.frame(
    Variable      = desc_var_labels[v],
    Mean_NonP     = round(mean(non_p), 2),
    SD_NonP       = round(sd(non_p),   2),
    Mean_Pers     = round(mean(pers),  2),
    SD_Pers       = round(sd(pers),    2),
    Diff_in_Means = round(mean(pers) - mean(non_p), 2),
    Std_Error     = round(sqrt(var(pers)/length(pers) + var(non_p)/length(non_p)), 2),
    stringsAsFactors = FALSE
  )
}))

n_non_p <- sum(final_data$initiator_personalist == 0)
n_pers  <- sum(final_data$initiator_personalist == 1)
# TABLE 1 export
read_docx() %>%
  body_add_par("Table 1: Descriptive Statistics (N = 214)", style = "heading 1") %>%
  body_add_flextable(
    flextable(table1) %>%
      bold(part = "header") %>%
      bg(part = "header", bg = "#F2F2F2") %>%
      autofit() %>%
      add_footer_lines("Unit of analysis: dyad-level conflict episode. Sample restricted to autocratic initiators, 1960-2009.")
  ) %>%
  print(target = "Final_tables/Table1_Descriptive_Statistics.docx")

cat("Saved: Final_tables/Table1_Descriptive_Statistics.docx\n")

# TABLE 2 export
read_docx() %>%
  body_add_par("Table 2: Conflict Relapse and Regime Type Relations", style = "heading 1") %>%
  body_add_flextable(
    flextable(table2) %>%
      set_header_labels(
        Variable      = "",
        Mean_NonP     = "Mean",
        SD_NonP       = "Std. Dev.",
        Mean_Pers     = "Mean",
        SD_Pers       = "Std. Dev.",
        Diff_in_Means = "Diff. in Means",
        Std_Error     = "Std. Error"
      ) %>%
      add_header_row(
        values    = c("",
                      paste0("Non-Personalist (N=", n_non_p, ")"),
                      paste0("Personalist (N=", n_pers, ")"),
                      "", ""),
        colwidths = c(1, 2, 2, 1, 1)
      ) %>%
      bold(part = "header") %>%
      bg(part = "header", bg = "#D5E8F0") %>%
      autofit() %>%
      add_footer_lines("Note: Diff. in Means = Personalist minus Non-Personalist.")
  ) %>%
  print(target = "Final_tables/Table2_Regime_Type_Relations.docx")

cat("Saved: Final_tables/Table2_Regime_Type_Relations.docx\n")
browseURL("Final_tables/Table1_Descriptive_Statistics.docx")
browseURL("Final_tables/Table2_Regime_Type_Relations.docx")

# Top sanction cases
final_data %>%
  arrange(desc(elite_sanctions_intensity)) %>%
  select(initiator_country, episode_start, episode_end,
         elite_sanctions_intensity, initiator_personalist) %>%
  head(10)

# Zero sanctions cases
final_data %>%
  filter(elite_sanctions_intensity == 0) %>%
  count(initiator_country) %>%
  arrange(desc(n))

# Displaying min and max aid recipients

final_data %>%
  filter(aid_gdp_post_log == max(aid_gdp_post_log) | 
           aid_gdp_post_log == min(aid_gdp_post_log)) %>%
  select(initiator_country, episode_start, episode_end,
         aid_gdp_post_log, aid_gdp_post_5y, initiator_personalist)

# Militart assymetry trend extreme

final_data %>%
  slice_max(milasym_trend_5yr, n = 3) %>%
  bind_rows(final_data %>% slice_min(milasym_trend_5yr, n = 3)) %>%
  select(initiator_country, target_country, episode_start, episode_end,
         milasym_trend_5yr, initiator_personalist)

# Longest regime duration in sample
final_data %>%
  slice_max(regime_duration, n = 5) %>%
  select(initiator_country, episode_start, episode_end,
         regime_duration, initiator_personalist)

# Longest conflict episode
final_data %>%
  slice_max(episode_duration_days, n = 5) %>%
  select(initiator_country, target_country, episode_start, episode_end,
         episode_duration_days, initiator_personalist)

# Most extreme positive (initiator much stronger than target)
final_data %>%
  slice_max(milasym_ratio, n = 5) %>%
  select(initiator_country, target_country, episode_start, episode_end,
         milasym_ratio, initiator_personalist)

# Most extreme negative (initiator much weaker than target)
final_data %>%
  slice_min(milasym_ratio, n = 5) %>%
  select(initiator_country, target_country, episode_start, episode_end,
         milasym_ratio, initiator_personalist)

# ============================================================
# 6) DESCRIPTIVE BASELINE: CHI-SQUARE TEST
# ============================================================
cat("\n=== CHI-SQUARE TEST: Relapse (5yr) x Personalism ===\n")
chi_tab <- table(
  Personalist = final_data$initiator_personalist,
  Relapse_5yr = final_data$relapse_5yr
)
print(chi_tab)
print(chisq.test(chi_tab))
cat("\nRelapse rates by regime type (5-year window):\n")
print(prop.table(chi_tab, margin = 1))

# TABLE 3: Chi-square test
ct <- table(final_data$initiator_personalist, final_data$relapse_5yr)
chi_result <- chisq.test(ct)

table3 <- data.frame(
  `Regime Type`  = c("Non-Personalist (0)", "Personalist (1)"),
  `No Relapse`   = c(ct[1,1], ct[2,1]),
  Relapse        = c(ct[1,2], ct[2,2]),
  Total          = c(rowSums(ct)[1], rowSums(ct)[2]),
  `Relapse Rate` = paste0(round(ct[,2] / rowSums(ct) * 100, 1), "%"),
  check.names    = FALSE
)

read_docx() %>%
  body_add_par(paste0("Table 3: Relapse by Regime Type (Chi-square = ",
                      round(chi_result$statistic, 3), ", df = 1, p = ",
                      round(chi_result$p.value, 2), ")"), style = "heading 1") %>%
  body_add_flextable(
    flextable(table3) %>%
      bold(part = "header") %>%
      bg(part = "header", bg = "#F2F2F2") %>%
      autofit()
  ) %>%
  print(target = "Final_tables/Table3_Chisquare.docx")

cat("Saved: Final_tables/Table3_Chisquare.docx\n")
browseURL("Final_tables/Table3_Chisquare.docx")
# ============================================================
# 7) MAIN MODELS
# ============================================================

# 7.1 LPM without controls 
lpm_no_controls <- lm(
  relapse_5yr ~
    initiator_personalist +
    elite_sanctions_intensity +
    aid_gdp_post_log +
    milasym_trend_5yr +
    initiator_personalist:elite_sanctions_intensity +
    initiator_personalist:aid_gdp_post_log +
    initiator_personalist:milasym_trend_5yr,
  data = final_data
)

cat("\n=== MODEL 1: LPM (No Controls, 5-year window) ===\n")
print(lmtest::coeftest(lpm_no_controls,
                       vcov = sandwich::vcovHC(lpm_no_controls, type = "HC2")))

# 7.2 LPM with controls 
lpm_controls <- lm(
  relapse_5yr ~
    initiator_personalist +
    elite_sanctions_intensity +
    aid_gdp_post_log +
    milasym_trend_5yr +
    initiator_personalist:elite_sanctions_intensity +
    initiator_personalist:aid_gdp_post_log +
    initiator_personalist:milasym_trend_5yr +
    milasym_ratio +
    episode_duration_days + cold_war + dem_change +
    log_gdp_pc_pre + prior_conflicts + regime_duration + shared_border,
  data = final_data
)

cat("\n=== MODEL 2: LPM (Controls, 5-year window) ===\n")
print(lmtest::coeftest(lpm_controls,
                       vcov = sandwich::vcovCL(lpm_controls, cluster = ~dyad_id)))

# 7.3 Cox PH model 
cox_controls <- survival::coxph(
  survival::Surv(time_cox, event_cox) ~
    initiator_personalist +
    elite_sanctions_intensity +
    aid_gdp_post_log +
    milasym_trend_5yr +
    initiator_personalist:elite_sanctions_intensity +
    initiator_personalist:aid_gdp_post_log +
    initiator_personalist:milasym_trend_5yr +
    milasym_ratio +
    episode_duration_days + cold_war + dem_change +
    log_gdp_pc_pre + prior_conflicts + regime_duration + shared_border +
    cluster(dyad_id),
  data = final_data
)

cat("\n=== MODEL 3: Cox PH (Controls, censored at 5yr) ===\n")
print(summary(cox_controls))

cat("\n=== PH ASSUMPTION TEST ===\n")
print(survival::cox.zph(cox_controls))

# Table construction

# Confirming my errors are dyad-cluster (robust)
cox_controls$call

# ============================================================
# TABLE 4: Main Results — Personalism and Conflict Relapse
# ============================================================

library(modelsummary)
library(flextable)
library(officer)

vcov_m1 <- sandwich::vcovHC(lpm_no_controls, type = "HC2")
vcov_m2 <- sandwich::vcovCL(lpm_controls,    cluster = ~dyad_id)

se_row <- data.frame(
  term                = "Std. Errors",
  `LPM (No Controls)` = "HC2",
  `LPM (Controls)`    = "Clustered (dyad)",
  `Cox PH (Controls)` = "Clustered (dyad)",
  check.names = FALSE
)

coef_map <- c(
  "(Intercept)"                                     = "Intercept",
  "initiator_personalist"                           = "Personalist Regime",
  "elite_sanctions_intensity"                       = "Elite Sanctions Intensity",
  "aid_gdp_post_log"                                = "Post-conflict Aid (log, % GDP)",
  "milasym_trend_5yr"                               = "Military Asymmetry Trend",
  "initiator_personalist:elite_sanctions_intensity" = "Personalist × Sanctions Intensity",
  "initiator_personalist:aid_gdp_post_log"          = "Personalist × Aid (log, % GDP)",
  "initiator_personalist:milasym_trend_5yr"         = "Personalist × Military Asym. Trend",
  "milasym_ratio"                                   = "Military Asymmetry Ratio",
  "episode_duration_days"                           = "Episode Duration (days)",
  "cold_war"                                        = "Cold War",
  "dem_change"                                      = "Democracy Change",
  "log_gdp_pc_pre"                                  = "Log GDP per Capita",
  "prior_conflicts"                                 = "Prior Conflicts",
  "regime_duration"                                 = "Regime Duration",
  "shared_border"                                   = "Shared Border"
)

modelsummary(
  list(
    "LPM (No Controls)" = lpm_no_controls,
    "LPM (Controls)"    = lpm_controls,
    "Cox PH (Controls)" = cox_controls
  ),
  vcov     = list(vcov_m1, vcov_m2, NULL),
  coef_map = coef_map,
  stars    = c("†" = 0.10, "*" = 0.05, "**" = 0.01, "***" = 0.001),
  gof_map  = list(
    list(raw = "nobs",          clean = "N",           fmt = 0),
    list(raw = "r.squared",     clean = "R²",          fmt = 3),
    list(raw = "adj.r.squared", clean = "Adj. R²",     fmt = 3),
    list(raw = "nevent",        clean = "Events",      fmt = 0),
    list(raw = "concordance",   clean = "Concordance", fmt = 3)
  ),
  add_rows = se_row,
  title    = "Table 4: Main Results — Personalism and Conflict Relapse",
  output   = "Final_tables/Table4_Main_Results.docx"
)

cat("Saved: Final_tables/Table4_Main_Results.docx\n")

# Figure 2: Predicted Relapse Probability by Sanctions Intensity

library(ggplot2)

sanc_seq_nonp <- seq(
  quantile(final_data$elite_sanctions_intensity[final_data$initiator_personalist == 0], 0.00),
  quantile(final_data$elite_sanctions_intensity[final_data$initiator_personalist == 0], 0.95),
  length.out = 100)

sanc_seq_pers <- seq(
  quantile(final_data$elite_sanctions_intensity[final_data$initiator_personalist == 1], 0.00),
  quantile(final_data$elite_sanctions_intensity[final_data$initiator_personalist == 1], 0.95),
  length.out = 100)

grid_sanc <- bind_rows(
  data.frame(initiator_personalist = 0, elite_sanctions_intensity = sanc_seq_nonp),
  data.frame(initiator_personalist = 1, elite_sanctions_intensity = sanc_seq_pers)
) %>%
  mutate(
    aid_gdp_post_log      = mean(final_data$aid_gdp_post_log),
    milasym_trend_5yr     = mean(final_data$milasym_trend_5yr),
    milasym_ratio         = mean(final_data$milasym_ratio),
    episode_duration_days = mean(final_data$episode_duration_days),
    cold_war              = median(final_data$cold_war),
    dem_change            = mean(final_data$dem_change),
    log_gdp_pc_pre        = mean(final_data$log_gdp_pc_pre),
    prior_conflicts       = mean(final_data$prior_conflicts),
    regime_duration       = mean(final_data$regime_duration),
    shared_border         = median(final_data$shared_border)
  )

preds <- predict(lpm_controls, newdata = grid_sanc, se.fit = TRUE)

grid_sanc <- grid_sanc %>%
  mutate(
    fit    = preds$fit,
    ci_lo  = fit - 1.96 * preds$se.fit,
    ci_hi  = fit + 1.96 * preds$se.fit,
    regime = factor(initiator_personalist,
                    levels = c(0, 1),
                    labels = c("Non-Personalist", "Personalist"))
  )

fig2 <- ggplot(grid_sanc, aes(x = elite_sanctions_intensity, y = fit,
                              color = regime, fill = regime)) +
  geom_hline(yintercept = 0, linetype = "dotted", color = "gray60") +
  geom_ribbon(aes(ymin = ci_lo, ymax = ci_hi), alpha = 0.15, color = NA) +
  geom_line(aes(linetype = regime), linewidth = 1) +
  geom_rug(data = final_data %>%
             mutate(regime = factor(initiator_personalist,
                                    levels = c(0, 1),
                                    labels = c("Non-Personalist", "Personalist"))),
           aes(x = elite_sanctions_intensity, color = regime),
           inherit.aes = FALSE, alpha = 0.4, sides = "b") +
  scale_color_manual(values = c("Non-Personalist" = "#2471a3",
                                "Personalist"     = "#c0392b"),
                     name = "Regime Type") +
  scale_fill_manual(values  = c("Non-Personalist" = "#2471a3",
                                "Personalist"     = "#c0392b"),
                    name = "Regime Type") +
  scale_linetype_manual(values = c("Non-Personalist" = "dashed",
                                   "Personalist"     = "solid"),
                        name = "Regime Type") +
  labs(
    x = "Elite Sanctions Intensity",
    y = "Predicted Probability of Relapse"
  ) +
  theme_minimal(base_size = 12) +
  theme(
    legend.position  = "bottom",
    panel.grid.minor = element_blank()
  )

print(fig2)
ggsave("Final_tables/figure2_sanctions.png", plot = fig2,
       width = 8, height = 5, dpi = 300, bg = "white")
cat("Saved: Final_tables/figure2_sanctions.png\n")

# Figure 3: Predicted Relapse Probability by Post-Conflict Aid

library(ggplot2)

aid_range <- seq(
  min(final_data$aid_gdp_post_log),
  quantile(final_data$aid_gdp_post_log, 0.95),
  length.out = 100)

grid_aid <- bind_rows(
  data.frame(initiator_personalist = 0, aid_gdp_post_log = aid_range),
  data.frame(initiator_personalist = 1, aid_gdp_post_log = aid_range)
) %>%
  mutate(
    elite_sanctions_intensity = mean(final_data$elite_sanctions_intensity),
    milasym_trend_5yr         = mean(final_data$milasym_trend_5yr),
    milasym_ratio             = mean(final_data$milasym_ratio),
    episode_duration_days     = mean(final_data$episode_duration_days),
    cold_war                  = median(final_data$cold_war),
    dem_change                = mean(final_data$dem_change),
    log_gdp_pc_pre            = mean(final_data$log_gdp_pc_pre),
    prior_conflicts           = mean(final_data$prior_conflicts),
    regime_duration           = mean(final_data$regime_duration),
    shared_border             = median(final_data$shared_border)
  )

preds <- predict(lpm_controls, newdata = grid_aid, se.fit = TRUE)

grid_aid <- grid_aid %>%
  mutate(
    fit    = preds$fit,
    ci_lo  = fit - 1.96 * preds$se.fit,
    ci_hi  = fit + 1.96 * preds$se.fit,
    regime = factor(initiator_personalist,
                    levels = c(0, 1),
                    labels = c("Non-Personalist", "Personalist"))
  )

fig3 <- ggplot(grid_aid, aes(x = aid_gdp_post_log, y = fit,
                             color = regime, fill = regime)) +
  geom_hline(yintercept = 0, linetype = "dotted", color = "gray60") +
  geom_ribbon(aes(ymin = ci_lo, ymax = ci_hi), alpha = 0.15, color = NA) +
  geom_line(aes(linetype = regime), linewidth = 1) +
  geom_rug(data = final_data %>%
             mutate(regime = factor(initiator_personalist,
                                    levels = c(0, 1),
                                    labels = c("Non-Personalist", "Personalist"))),
           aes(x = aid_gdp_post_log, color = regime),
           inherit.aes = FALSE, alpha = 0.4, sides = "b") +
  scale_color_manual(values = c("Non-Personalist" = "#2471a3",
                                "Personalist"     = "#c0392b"),
                     name = "Regime Type") +
  scale_fill_manual(values  = c("Non-Personalist" = "#2471a3",
                                "Personalist"     = "#c0392b"),
                    name = "Regime Type") +
  scale_linetype_manual(values = c("Non-Personalist" = "dashed",
                                   "Personalist"     = "solid"),
                        name = "Regime Type") +
  labs(
    x = "Post-conflict Aid (log, % GDP)",
    y = "Predicted Probability of Relapse"
  ) +
  theme_minimal(base_size = 12) +
  theme(
    legend.position  = "bottom",
    panel.grid.minor = element_blank()
  )

print(fig3)
ggsave("Final_tables/figure3_aid.png", plot = fig3,
       width = 8, height = 5, dpi = 300, bg = "white")


cat("Saved: Final_tables/figure3_aid.png\n")

# Figure 4: Adjusted Survival Curves by Regime Type

library(survival)
library(ggplot2)

# Create baseline data at mean/modal values for each regime type
newdata_surv <- data.frame(
  initiator_personalist     = c(0, 1),
  elite_sanctions_intensity = mean(final_data$elite_sanctions_intensity),
  aid_gdp_post_log          = mean(final_data$aid_gdp_post_log),
  milasym_trend_5yr         = mean(final_data$milasym_trend_5yr),
  milasym_ratio             = mean(final_data$milasym_ratio),
  episode_duration_days     = mean(final_data$episode_duration_days),
  cold_war                  = median(final_data$cold_war),
  dem_change                = mean(final_data$dem_change),
  log_gdp_pc_pre            = mean(final_data$log_gdp_pc_pre),
  prior_conflicts           = mean(final_data$prior_conflicts),
  regime_duration           = mean(final_data$regime_duration),
  shared_border             = median(final_data$shared_border)
)

# Get survival curves
surv_fit <- survfit(cox_controls, newdata = newdata_surv)

# Extract into data frame
surv_df <- data.frame(
  time   = rep(surv_fit$time, 2),
  surv   = c(surv_fit$surv[, 1], surv_fit$surv[, 2]),
  upper  = c(surv_fit$upper[, 1], surv_fit$upper[, 2]),
  lower  = c(surv_fit$lower[, 1], surv_fit$lower[, 2]),
  regime = rep(c("Non-Personalist", "Personalist"),
               each = length(surv_fit$time))
)

# Convert to relapse probability (1 - survival)
surv_df <- surv_df %>%
  mutate(
    relapse_prob = 1 - surv,
    upper_prob   = 1 - lower,
    lower_prob   = 1 - upper,
    time_years   = time / 365.25
  )

fig4 <- ggplot(surv_df, aes(x = time_years, y = relapse_prob,
                            color = regime, fill = regime)) +
  geom_ribbon(aes(ymin = lower_prob, ymax = upper_prob),
              alpha = 0.15, color = NA) +
  geom_line(aes(linetype = regime), linewidth = 1) +
  scale_color_manual(values = c("Non-Personalist" = "#2471a3",
                                "Personalist"     = "#c0392b"),
                     name = "Regime Type") +
  scale_fill_manual(values  = c("Non-Personalist" = "#2471a3",
                                "Personalist"     = "#c0392b"),
                    name = "Regime Type") +
  scale_linetype_manual(values = c("Non-Personalist" = "dashed",
                                   "Personalist"     = "solid"),
                        name = "Regime Type") +
  scale_x_continuous(breaks = 0:5, labels = paste0("Year ", 0:5)) +
  scale_y_continuous(limits = c(0, 1), labels = scales::percent) +
  labs(
    x = "Years after Conflict End",
    y = "Cumulative Probability of Relapse"
  ) +
  theme_minimal(base_size = 12) +
  theme(
    legend.position  = "bottom",
    panel.grid.minor = element_blank()
  )

print(fig4)
ggsave("Final_tables/figure4_survival.png", plot = fig4,
       width = 8, height = 5, dpi = 300, bg = "white")
cat("Saved: Final_tables/figure4_survival.png\n")

# ============================================================
# 8) ROBUSTNESS CHECKS
# ============================================================

# 8.1 Alternative personalism measures 

cat("\n=== ROBUSTNESS 8.1-A: G&S BINARY ===\n")
if (file.exists(PATH_GANDHI)) {
  
  gandhi_raw <- readr::read_csv(PATH_GANDHI, show_col_types = FALSE,
                                locale = readr::locale(encoding = "latin1"))
  
  gandhi_cy <- gandhi_raw %>%
    transmute(
      iso3c            = countrycode(country, "country.name", "iso3c"),
      year             = as.integer(year),
      personalism_cont = as.numeric(xhatmean),
      level_personalist = as.integer(xhatmean > 0)
    ) %>%
    filter(!is.na(iso3c), !is.na(year), !is.na(personalism_cont))
  
  episodes_gs <- episodes %>%
    mutate(join_year = start_year - 1L) %>%
    left_join(
      gandhi_cy %>% select(iso3c, year, level_personalist, personalism_cont),
      by = c("initiator_iso3" = "iso3c", "join_year" = "year")
    )
  
  model_vars_gs <- c(
    model_vars_final[model_vars_final != "initiator_personalist"],
    "level_personalist", "personalism_cont"
  )
  
  final_data_gs <- episodes_gs %>%
    tidyr::drop_na(dplyr::all_of(model_vars_gs)) %>%
    filter(level_personalist %in% c(0L, 1L)) %>%
    mutate(
      relapse_5yr       = as.integer(relapse_event == 1 &
                                       time_to_relapse_days <= 1825),
      level_personalist = as.integer(level_personalist),
      relapse_event     = as.integer(relapse_event),
      time_cox          = pmin(time_to_relapse_days, CENSOR_DAYS),
      event_cox         = as.integer(relapse_event == 1 &
                                       time_to_relapse_days <= CENSOR_DAYS)
    )
  
  lpm_gs_binary <- lm(
    relapse_5yr ~
      level_personalist +
      elite_sanctions_intensity +
      aid_gdp_post_log +
      milasym_ratio +
      milasym_trend_5yr +
      level_personalist:elite_sanctions_intensity +
      level_personalist:aid_gdp_post_log +
      level_personalist:milasym_trend_5yr +
      episode_duration_days + cold_war + dem_change +
      log_gdp_pc_pre + prior_conflicts + regime_duration + shared_border,
    data = final_data_gs
  )
  print(lmtest::coeftest(lpm_gs_binary,
                         vcov = sandwich::vcovCL(lpm_gs_binary, cluster = ~dyad_id)))
  
  cat("\n=== ROBUSTNESS 8.1-B: G&S CONTINUOUS ===\n")
  lpm_gs_cont <- lm(
    relapse_5yr ~
      personalism_cont +
      elite_sanctions_intensity +
      aid_gdp_post_log +
      milasym_ratio +
      milasym_trend_5yr +
      personalism_cont:elite_sanctions_intensity +
      personalism_cont:aid_gdp_post_log +
      personalism_cont:milasym_trend_5yr +
      episode_duration_days + cold_war + dem_change +
      log_gdp_pc_pre + prior_conflicts + regime_duration + shared_border,
    data = final_data_gs
  )
  print(lmtest::coeftest(lpm_gs_cont,
                         vcov = sandwich::vcovCL(lpm_gs_cont, cluster = ~dyad_id)))
  
} else {
  message("Gandhi & Sumner file not found — skipping 8.1-A/B.")
}

if (file.exists(PATH_GWF_SCORES)) {
  
  gwf_scores_raw <- readr::read_csv(PATH_GWF_SCORES, show_col_types = FALSE,
                                    locale = readr::locale(encoding = "latin1"))
  
  latent_median <- median(gwf_scores_raw$latent_personalism, na.rm = TRUE)
  
  gwf_latent_cy <- gwf_scores_raw %>%
    transmute(
      iso3c              = countrycode(cowcode, "cown", "iso3c"),
      year               = as.integer(year),
      latent_pers_cont   = as.numeric(latent_personalism),
      latent_pers_binary = as.integer(latent_personalism > latent_median)
    ) %>%
    filter(!is.na(iso3c), !is.na(year), !is.na(latent_pers_cont))
  
  episodes_latent <- episodes %>%
    mutate(join_year = start_year - 1L) %>%
    left_join(
      gwf_latent_cy %>% select(iso3c, year, latent_pers_binary, latent_pers_cont),
      by = c("initiator_iso3" = "iso3c", "join_year" = "year")
    )
  
  model_vars_latent <- c(
    model_vars_final[model_vars_final != "initiator_personalist"],
    "latent_pers_binary", "latent_pers_cont"
  )
  
  final_data_latent <- episodes_latent %>%
    tidyr::drop_na(dplyr::all_of(model_vars_latent)) %>%
    filter(latent_pers_binary %in% c(0L, 1L)) %>%
    mutate(
      relapse_5yr        = as.integer(relapse_event == 1 &
                                        time_to_relapse_days <= 1825),
      latent_pers_binary = as.integer(latent_pers_binary),
      relapse_event      = as.integer(relapse_event),
      time_cox           = pmin(time_to_relapse_days, CENSOR_DAYS),
      event_cox          = as.integer(relapse_event == 1 &
                                        time_to_relapse_days <= CENSOR_DAYS)
    )
  
  cat("\n=== ROBUSTNESS 8.1-C: GWF LATENT BINARY ===\n")
  lpm_latent_binary <- lm(
    relapse_5yr ~
      latent_pers_binary +
      elite_sanctions_intensity +
      aid_gdp_post_log +
      milasym_ratio +
      milasym_trend_5yr +
      latent_pers_binary:elite_sanctions_intensity +
      latent_pers_binary:aid_gdp_post_log +
      latent_pers_binary:milasym_trend_5yr +
      episode_duration_days + cold_war + dem_change +
      log_gdp_pc_pre + prior_conflicts + regime_duration + shared_border,
    data = final_data_latent
  )
  print(lmtest::coeftest(lpm_latent_binary,
                         vcov = sandwich::vcovCL(lpm_latent_binary, cluster = ~dyad_id)))
  
  cat("\n=== ROBUSTNESS 8.1-D: GWF LATENT CONTINUOUS ===\n")
  lpm_latent_cont <- lm(
    relapse_5yr ~
      latent_pers_cont +
      elite_sanctions_intensity +
      aid_gdp_post_log +
      milasym_ratio +
      milasym_trend_5yr +
      latent_pers_cont:elite_sanctions_intensity +
      latent_pers_cont:aid_gdp_post_log +
      latent_pers_cont:milasym_trend_5yr +
      episode_duration_days + cold_war + dem_change +
      log_gdp_pc_pre + prior_conflicts + regime_duration + shared_border,
    data = final_data_latent
  )
  print(lmtest::coeftest(lpm_latent_cont,
                         vcov = sandwich::vcovCL(lpm_latent_cont, cluster = ~dyad_id)))
  
} else {
  message("GWF latent scores file not found — skipping 8.1-C/D.")
}


# ============================================================
# FIGURE 5: G&S Personalism Score Distribution
# ============================================================

fig5 <- ggplot(final_data_gs, aes(x = personalism_cont,
                                  fill = factor(level_personalist,
                                                labels = c("Non-personalist",
                                                           "Personalist")))) +
  geom_histogram(binwidth = 0.2, color = "white", alpha = 0.85) +
  geom_vline(xintercept = 0, linetype = "dashed", color = "red",
             linewidth = 1) +
  geom_vline(xintercept = quantile(final_data_gs$personalism_cont, 0.25),
             linetype = "dotted", color = "gray40") +
  geom_vline(xintercept = quantile(final_data_gs$personalism_cont, 0.75),
             linetype = "dotted", color = "gray40") +
  annotate("text",
           x = quantile(final_data_gs$personalism_cont, 0.25) + 0.05,
           y = 38, label = "P25", size = 3.5, color = "gray40", hjust = 0) +
  annotate("text",
           x = quantile(final_data_gs$personalism_cont, 0.75) + 0.05,
           y = 38, label = "P75", size = 3.5, color = "gray40", hjust = 0) +
  scale_fill_manual(values = c("Non-personalist" = "#5b9bd5",
                               "Personalist"     = "#c0695a"),
                    name = "Classification (threshold = 0)") +
  labs(
    x = "G&S Personalism Score (xhatmean)",
    y = "Count"
  ) +
  theme_minimal(base_size = 12) +
  theme(
    legend.position  = "bottom",
    panel.grid.minor = element_blank()
  )

print(fig5)
ggsave("Final_tables/figure5_gs_distribution.png", plot = fig5,
       width = 8, height = 5, dpi = 300, bg = "white")
cat("Saved: Final_tables/figure5_gs_distribution.png\n")

# ============================================================
# APPENDIX 2: GWF Latent Personalism Models
# ============================================================

vcov_latent_bin  <- sandwich::vcovCL(lpm_latent_binary, cluster = ~dyad_id)
vcov_latent_cont <- sandwich::vcovCL(lpm_latent_cont,   cluster = ~dyad_id)

coef_map_latent <- c(
  "(Intercept)"                                  = "Intercept",
  "latent_pers_binary"                           = "Latent Personalism (binary)",
  "latent_pers_cont"                             = "Latent Personalism (continuous)",
  "elite_sanctions_intensity"                    = "Elite Sanctions Intensity",
  "aid_gdp_post_log"                             = "Post-conflict Aid (log, % GDP)",
  "milasym_trend_5yr"                            = "Military Asymmetry Trend",
  "latent_pers_binary:elite_sanctions_intensity" = "Latent (binary) × Sanctions",
  "latent_pers_binary:aid_gdp_post_log"          = "Latent (binary) × Aid",
  "latent_pers_binary:milasym_trend_5yr"         = "Latent (binary) × Mil. Trend",
  "latent_pers_cont:elite_sanctions_intensity"   = "Latent (cont.) × Sanctions",
  "latent_pers_cont:aid_gdp_post_log"            = "Latent (cont.) × Aid",
  "latent_pers_cont:milasym_trend_5yr"           = "Latent (cont.) × Mil. Trend",
  "milasym_ratio"                                = "Military Asymmetry Ratio",
  "episode_duration_days"                        = "Episode Duration (days)",
  "cold_war"                                     = "Cold War",
  "dem_change"                                   = "Democracy Change",
  "log_gdp_pc_pre"                               = "Log GDP per Capita",
  "prior_conflicts"                              = "Prior Conflicts",
  "regime_duration"                              = "Regime Duration",
  "shared_border"                                = "Shared Border"
)

modelsummary(
  list(
    "LPM — Latent Binary"     = lpm_latent_binary,
    "LPM — Latent Continuous" = lpm_latent_cont
  ),
  vcov     = list(vcov_latent_bin, vcov_latent_cont),
  coef_map = coef_map_latent,
  stars    = c("†" = 0.10, "*" = 0.05, "**" = 0.01, "***" = 0.001),
  gof_map  = list(
    list(raw = "nobs",          clean = "N",       fmt = 0),
    list(raw = "r.squared",     clean = "R²",      fmt = 3),
    list(raw = "adj.r.squared", clean = "Adj. R²", fmt = 3)
  ),
  title  = "Appendix 2: GWF Latent Personalism Models",
  notes  = list(
    "Dyad-clustered standard errors in parentheses.",
    "DV: conflict relapse within 5-year window.",
    "Binary measure dichotomized at sample median (0.427).",
    "Data: Geddes, Wright & Frantz (2018).",
    "† p < 0.10, * p < 0.05, ** p < 0.01, *** p < 0.001"
  ),
  output = "Final_tables/Appendix2_GWF_Latent.docx"
)
cat("Saved: Final_tables/Appendix2_GWF_Latent.docx\n")

# ============================================================
# APPENDIX 3: G&S Personalism Models
# ============================================================

vcov_gs_bin  <- sandwich::vcovCL(lpm_gs_binary, cluster = ~dyad_id)
vcov_gs_cont <- sandwich::vcovCL(lpm_gs_cont,   cluster = ~dyad_id)

coef_map_gs <- c(
  "(Intercept)"                                 = "Intercept",
  "level_personalist"                           = "G&S Personalism (binary)",
  "personalism_cont"                            = "G&S Personalism (continuous)",
  "elite_sanctions_intensity"                   = "Elite Sanctions Intensity",
  "aid_gdp_post_log"                            = "Post-conflict Aid (log, % GDP)",
  "milasym_trend_5yr"                           = "Military Asymmetry Trend",
  "level_personalist:elite_sanctions_intensity" = "G&S (binary) × Sanctions",
  "level_personalist:aid_gdp_post_log"          = "G&S (binary) × Aid",
  "level_personalist:milasym_trend_5yr"         = "G&S (binary) × Mil. Trend",
  "personalism_cont:elite_sanctions_intensity"  = "G&S (cont.) × Sanctions",
  "personalism_cont:aid_gdp_post_log"           = "G&S (cont.) × Aid",
  "personalism_cont:milasym_trend_5yr"          = "G&S (cont.) × Mil. Trend",
  "milasym_ratio"                               = "Military Asymmetry Ratio",
  "episode_duration_days"                       = "Episode Duration (days)",
  "cold_war"                                    = "Cold War",
  "dem_change"                                  = "Democracy Change",
  "log_gdp_pc_pre"                              = "Log GDP per Capita",
  "prior_conflicts"                             = "Prior Conflicts",
  "regime_duration"                             = "Regime Duration",
  "shared_border"                               = "Shared Border"
)

modelsummary(
  list(
    "LPM — G&S Binary"     = lpm_gs_binary,
    "LPM — G&S Continuous" = lpm_gs_cont
  ),
  vcov     = list(vcov_gs_bin, vcov_gs_cont),
  coef_map = coef_map_gs,
  stars    = c("†" = 0.10, "*" = 0.05, "**" = 0.01, "***" = 0.001),
  gof_map  = list(
    list(raw = "nobs",          clean = "N",       fmt = 0),
    list(raw = "r.squared",     clean = "R²",      fmt = 3),
    list(raw = "adj.r.squared", clean = "Adj. R²", fmt = 3)
  ),
  title  = "Appendix 3: Gandhi & Sumner Personalism Models",
  notes  = list(
    "Dyad-clustered standard errors in parentheses.",
    "DV: conflict relapse within 5-year window.",
    "Binary measure dichotomized at zero (xhatmean > 0 = personalist).",
    "Data: Gandhi & Sumner (2020).",
    "† p < 0.10, * p < 0.05, ** p < 0.01, *** p < 0.001"
  ),
  output = "Final_tables/Appendix3_GS_Personalism.docx"
)
cat("Saved: Final_tables/Appendix3_GS_Personalism.docx\n")

cat("N GWF latent sample:", nrow(final_data_latent), "\n")
merged_check <- final_data %>%
  left_join(
    final_data_latent %>% select(dyad_id, episode_start, latent_pers_binary),
    by = c("dyad_id", "episode_start")
  )
cat("Correlation GWF categorical vs latent binary:",
    round(cor(merged_check$initiator_personalist,
              merged_check$latent_pers_binary,
              use = "complete.obs"), 3), "\n")

# 8.2 Conservative specification 

final_data_collapsed <- final_data %>%
  group_by(initiator_country, episode_start) %>%
  arrange(desc(shared_border), target_country) %>%
  slice(1) %>%
  ungroup()

cat("\nN after collapsing multilateral:", nrow(final_data_collapsed), "\n")

final_data_conservative <- final_data_collapsed %>%
  filter(episode_duration_days > 1)

cat("Conservative N:", nrow(final_data_conservative), "\n")
cat("Relapse events (5yr):", sum(final_data_conservative$relapse_5yr), "\n")

cat("\n=== ROBUSTNESS 8.2-A: LPM COLLAPSED ===\n")
lpm_collapsed <- lm(
  relapse_5yr ~
    initiator_personalist +
    elite_sanctions_intensity +
    aid_gdp_post_log +
    milasym_ratio +
    milasym_trend_5yr +
    initiator_personalist:elite_sanctions_intensity +
    initiator_personalist:aid_gdp_post_log +
    initiator_personalist:milasym_trend_5yr +
    episode_duration_days + cold_war + dem_change +
    log_gdp_pc_pre + prior_conflicts + regime_duration + shared_border,
  data = final_data_collapsed
)
print(lmtest::coeftest(lpm_collapsed,
                       vcov = sandwich::vcovCL(lpm_collapsed, cluster = ~dyad_id)))

cat("\n=== ROBUSTNESS 8.2-B: LPM CONSERVATIVE (collapsed + no single-day) ===\n")
lpm_conservative <- lm(
  relapse_5yr ~
    initiator_personalist +
    elite_sanctions_intensity +
    aid_gdp_post_log +
    milasym_ratio +
    milasym_trend_5yr +
    initiator_personalist:elite_sanctions_intensity +
    initiator_personalist:aid_gdp_post_log +
    initiator_personalist:milasym_trend_5yr +
    episode_duration_days + cold_war + dem_change +
    log_gdp_pc_pre + prior_conflicts + regime_duration + shared_border,
  data = final_data_conservative
)
print(lmtest::coeftest(lpm_conservative,
                       vcov = sandwich::vcovCL(lpm_conservative, cluster = ~dyad_id)))

cox_conservative <- survival::coxph(
  survival::Surv(time_cox, event_cox) ~
    initiator_personalist +
    elite_sanctions_intensity +
    aid_gdp_post_log +
    milasym_ratio +
    milasym_trend_5yr +
    initiator_personalist:elite_sanctions_intensity +
    initiator_personalist:aid_gdp_post_log +
    initiator_personalist:milasym_trend_5yr +
    episode_duration_days + cold_war + dem_change +
    log_gdp_pc_pre + prior_conflicts + regime_duration + shared_border +
    cluster(dyad_id),
  data = final_data_conservative
)
print(summary(cox_conservative))
print(survival::cox.zph(cox_conservative))

# ============================================================
# APPENDIX 4: Conservative Specification Models
# ============================================================

vcov_collapsed    <- sandwich::vcovCL(lpm_collapsed,    cluster = ~dyad_id)
vcov_conservative <- sandwich::vcovCL(lpm_conservative, cluster = ~dyad_id)

coef_map_cons <- c(
  "(Intercept)"                                     = "Intercept",
  "initiator_personalist"                           = "Personalist Regime",
  "elite_sanctions_intensity"                       = "Elite Sanctions Intensity",
  "aid_gdp_post_log"                                = "Post-conflict Aid (log, % GDP)",
  "milasym_trend_5yr"                               = "Military Asymmetry Trend",
  "initiator_personalist:elite_sanctions_intensity" = "Personalist × Sanctions Intensity",
  "initiator_personalist:aid_gdp_post_log"          = "Personalist × Aid (log, % GDP)",
  "initiator_personalist:milasym_trend_5yr"         = "Personalist × Military Asym. Trend",
  "milasym_ratio"                                   = "Military Asymmetry Ratio",
  "episode_duration_days"                           = "Episode Duration (days)",
  "cold_war"                                        = "Cold War",
  "dem_change"                                      = "Democracy Change",
  "log_gdp_pc_pre"                                  = "Log GDP per Capita",
  "prior_conflicts"                                 = "Prior Conflicts",
  "regime_duration"                                 = "Regime Duration",
  "shared_border"                                   = "Shared Border"
)

modelsummary(
  list(
    "LPM — Collapsed (N=188)"     = lpm_collapsed,
    "LPM — Conservative (N=152)"  = lpm_conservative,
    "Cox PH — Conservative (N=152)" = cox_conservative
  ),
  vcov     = list(vcov_collapsed, vcov_conservative, NULL),
  coef_map = coef_map_cons,
  stars    = c("†" = 0.10, "*" = 0.05, "**" = 0.01, "***" = 0.001),
  gof_map  = list(
    list(raw = "nobs",          clean = "N",           fmt = 0),
    list(raw = "r.squared",     clean = "R²",          fmt = 3),
    list(raw = "adj.r.squared", clean = "Adj. R²",     fmt = 3),
    list(raw = "nevent",        clean = "Events",      fmt = 0),
    list(raw = "concordance",   clean = "Concordance", fmt = 3)
  ),
  title  = "Appendix 4: Conservative Specification Models",
  notes  = list(
    "Dyad-clustered standard errors in parentheses.",
    "DV: conflict relapse within 5-year window.",
    "Collapsed: one observation per initiating state per episode.",
    "Conservative: collapsed + excluding single-day episodes.",
    "† p < 0.10, * p < 0.05, ** p < 0.01, *** p < 0.001"
  ),
  output = "Final_tables/Appendix4_Conservative.docx"
)
cat("Saved: Final_tables/Appendix4_Conservative.docx\n")

# 8.3 Alternative sanctions operationalization 

cat("\n=== ROBUSTNESS 8.3-A: BILATERAL ELITE SANCTIONS ONLY ===\n")
lpm_bilateral <- lm(
  relapse_5yr ~
    initiator_personalist +
    elite_sanctions_bilateral +
    aid_gdp_post_log +
    milasym_ratio +
    milasym_trend_5yr +
    initiator_personalist:elite_sanctions_bilateral +
    initiator_personalist:aid_gdp_post_log +
    initiator_personalist:milasym_trend_5yr +
    episode_duration_days + cold_war + dem_change +
    log_gdp_pc_pre + prior_conflicts + regime_duration + shared_border,
  data = final_data
)
print(lmtest::coeftest(lpm_bilateral,
                       vcov = sandwich::vcovCL(lpm_bilateral, cluster = ~dyad_id)))

cat("\n=== ROBUSTNESS 8.3-B: MULTILATERAL ELITE SANCTIONS ONLY ===\n")
lpm_multilateral <- lm(
  relapse_5yr ~
    initiator_personalist +
    elite_sanctions_multilateral +
    aid_gdp_post_log +
    milasym_ratio +
    milasym_trend_5yr +
    initiator_personalist:elite_sanctions_multilateral +
    initiator_personalist:aid_gdp_post_log +
    initiator_personalist:milasym_trend_5yr +
    episode_duration_days + cold_war + dem_change +
    log_gdp_pc_pre + prior_conflicts + regime_duration + shared_border,
  data = final_data
)
print(lmtest::coeftest(lpm_multilateral,
                       vcov = sandwich::vcovCL(lpm_multilateral, cluster = ~dyad_id)))

cat("\n=== ROBUSTNESS 8.3-C: ALL SANCTIONS TYPES ===\n")
lpm_allsanc <- lm(
  relapse_5yr ~
    initiator_personalist +
    all_sanctions_intensity +
    aid_gdp_post_log +
    milasym_ratio +
    milasym_trend_5yr +
    initiator_personalist:all_sanctions_intensity +
    initiator_personalist:aid_gdp_post_log +
    initiator_personalist:milasym_trend_5yr +
    episode_duration_days + cold_war + dem_change +
    log_gdp_pc_pre + prior_conflicts + regime_duration + shared_border,
  data = final_data
)
print(lmtest::coeftest(lpm_allsanc,
                       vcov = sandwich::vcovCL(lpm_allsanc, cluster = ~dyad_id)))


# ============================================================
# APPENDIX 5: Alternative Sanctions Operationalization
# ============================================================

vcov_bil  <- sandwich::vcovCL(lpm_bilateral,    cluster = ~dyad_id)
vcov_mul  <- sandwich::vcovCL(lpm_multilateral, cluster = ~dyad_id)
vcov_all  <- sandwich::vcovCL(lpm_allsanc,      cluster = ~dyad_id)

coef_map_sanc <- c(
  "(Intercept)"                                      = "Intercept",
  "initiator_personalist"                            = "Personalist Regime",
  "elite_sanctions_bilateral"                        = "Sanctions (Bilateral Elite)",
  "elite_sanctions_multilateral"                     = "Sanctions (Multilateral Elite)",
  "all_sanctions_intensity"                          = "Sanctions (All Types)",
  "aid_gdp_post_log"                                 = "Post-conflict Aid (log, % GDP)",
  "milasym_trend_5yr"                                = "Military Asymmetry Trend",
  "initiator_personalist:elite_sanctions_bilateral"  = "Personalist × Sanctions (Bilateral)",
  "initiator_personalist:elite_sanctions_multilateral" = "Personalist × Sanctions (Multilateral)",
  "initiator_personalist:all_sanctions_intensity"    = "Personalist × Sanctions (All Types)",
  "initiator_personalist:aid_gdp_post_log"           = "Personalist × Aid (log, % GDP)",
  "initiator_personalist:milasym_trend_5yr"          = "Personalist × Military Asym. Trend",
  "milasym_ratio"                                    = "Military Asymmetry Ratio",
  "episode_duration_days"                            = "Episode Duration (days)",
  "cold_war"                                         = "Cold War",
  "dem_change"                                       = "Democracy Change",
  "log_gdp_pc_pre"                                   = "Log GDP per Capita",
  "prior_conflicts"                                  = "Prior Conflicts",
  "regime_duration"                                  = "Regime Duration",
  "shared_border"                                    = "Shared Border"
)

modelsummary(
  list(
    "Bilateral Elite"    = lpm_bilateral,
    "Multilateral Elite" = lpm_multilateral,
    "All Types"          = lpm_allsanc
  ),
  vcov     = list(vcov_bil, vcov_mul, vcov_all),
  coef_map = coef_map_sanc,
  stars    = c("†" = 0.10, "*" = 0.05, "**" = 0.01, "***" = 0.001),
  gof_map  = list(
    list(raw = "nobs",          clean = "N",       fmt = 0),
    list(raw = "r.squared",     clean = "R²",      fmt = 3),
    list(raw = "adj.r.squared", clean = "Adj. R²", fmt = 3)
  ),
  title  = "Appendix 5: Alternative Sanctions Operationalization",
  notes  = list(
    "Dyad-clustered standard errors in parentheses.",
    "DV: conflict relapse within 5-year window. N = 214.",
    "Main model uses elite sanctions intensity (financial + travel, sender-weighted).",
    "† p < 0.10, * p < 0.05, ** p < 0.01, *** p < 0.001"
  ),
  output = "Final_tables/Appendix5_Sanctions_Robustness.docx"
)
cat("Saved: Final_tables/Appendix5_Sanctions_Robustness.docx\n")

# ============================================================
# FIGURE 6: Sanctions Interaction Coefficient Plot
# ============================================================

# Extract interaction coefficients and CIs across all models
sanc_coef_plot <- bind_rows(
  data.frame(
    model  = "Elite (main)",
    coef   = coef(lpm_controls)["initiator_personalist:elite_sanctions_intensity"],
    se     = sqrt(vcov_m2["initiator_personalist:elite_sanctions_intensity",
                          "initiator_personalist:elite_sanctions_intensity"])
  ),
  data.frame(
    model  = "Bilateral Elite",
    coef   = coef(lpm_bilateral)["initiator_personalist:elite_sanctions_bilateral"],
    se     = sqrt(vcov_bil["initiator_personalist:elite_sanctions_bilateral",
                           "initiator_personalist:elite_sanctions_bilateral"])
  ),
  data.frame(
    model  = "Multilateral Elite",
    coef   = coef(lpm_multilateral)["initiator_personalist:elite_sanctions_multilateral"],
    se     = sqrt(vcov_mul["initiator_personalist:elite_sanctions_multilateral",
                           "initiator_personalist:elite_sanctions_multilateral"])
  ),
  data.frame(
    model  = "All Sanctions Types",
    coef   = coef(lpm_allsanc)["initiator_personalist:all_sanctions_intensity"],
    se     = sqrt(vcov_all["initiator_personalist:all_sanctions_intensity",
                           "initiator_personalist:all_sanctions_intensity"])
  )
) %>%
  mutate(
    ci_lo = coef - 1.96 * se,
    ci_hi = coef + 1.96 * se,
    model = factor(model,
                   levels = c("Elite (main)", "Bilateral Elite",
                              "Multilateral Elite", "All Sanctions Types"))
  )

fig6 <- ggplot(sanc_coef_plot,
               aes(x = model, y = coef, color = model)) +
  geom_hline(yintercept = 0, linetype = "dashed", color = "gray40") +
  geom_pointrange(aes(ymin = ci_lo, ymax = ci_hi),
                  size = 0.8, linewidth = 1) +
  scale_color_manual(values = c("Elite (main)"        = "#1a5276",
                                "Bilateral Elite"     = "#2471a3",
                                "Multilateral Elite"  = "#c0392b",
                                "All Sanctions Types" = "#7d6608"),
                     guide = "none") +
  labs(
    x = "Sanctions Operationalization",
    y = "Interaction Coefficient\n(Personalist × Sanctions Intensity)"
  ) +
  theme_minimal(base_size = 12) +
  theme(
    panel.grid.minor = element_blank(),
    axis.text.x      = element_text(size = 10)
  )

print(fig6)
ggsave("Final_tables/figure6_sanctions_coefplot.png", plot = fig6,
       width = 7, height = 5, dpi = 300, bg = "white")
cat("Saved: Final_tables/figure6_sanctions_coefplot.png\n")

# 8.4 Alternative aid operationalization 

cat("\n=== ROBUSTNESS 8.4-A: AID LEVEL (post-conflict log per capita) ===\n")
lpm_aid_level <- lm(
  relapse_5yr ~
    initiator_personalist +
    elite_sanctions_intensity +
    aid_pc_post_log +
    milasym_ratio +
    milasym_trend_5yr +
    initiator_personalist:elite_sanctions_intensity +
    initiator_personalist:aid_pc_post_log +
    initiator_personalist:milasym_trend_5yr +
    episode_duration_days + cold_war + dem_change +
    log_gdp_pc_pre + prior_conflicts + regime_duration + shared_border,
  data = final_data
)
print(lmtest::coeftest(lpm_aid_level,
                       vcov = sandwich::vcovCL(lpm_aid_level, cluster = ~dyad_id)))

cat("\n=== ROBUSTNESS 8.4-B: AID GDP CHANGE SCORE ===\n")
lpm_aid_gdp <- lm(
  relapse_5yr ~
    initiator_personalist +
    elite_sanctions_intensity +
    aid_gdp_change_log +
    milasym_ratio +
    milasym_trend_5yr +
    initiator_personalist:elite_sanctions_intensity +
    initiator_personalist:aid_gdp_change_log +
    initiator_personalist:milasym_trend_5yr +
    episode_duration_days + cold_war + dem_change +
    log_gdp_pc_pre + prior_conflicts + regime_duration + shared_border,
  data = final_data
)
print(lmtest::coeftest(lpm_aid_gdp,
                       vcov = sandwich::vcovCL(lpm_aid_gdp, cluster = ~dyad_id)))

# ============================================================
# APPENDIX 6: Alternative Aid Operationalization
# ============================================================

vcov_aid_level <- sandwich::vcovCL(lpm_aid_level, cluster = ~dyad_id)
vcov_aid_gdp   <- sandwich::vcovCL(lpm_aid_gdp,   cluster = ~dyad_id)

coef_map_aid <- c(
  "(Intercept)"                                     = "Intercept",
  "initiator_personalist"                           = "Personalist Regime",
  "elite_sanctions_intensity"                       = "Elite Sanctions Intensity",
  "aid_pc_post_log"                                 = "Aid per Capita (log)",
  "aid_gdp_change_log"                              = "Aid/GDP Change (log)",
  "milasym_trend_5yr"                               = "Military Asymmetry Trend",
  "initiator_personalist:elite_sanctions_intensity" = "Personalist × Sanctions Intensity",
  "initiator_personalist:aid_pc_post_log"           = "Personalist × Aid per Capita (log)",
  "initiator_personalist:aid_gdp_change_log"        = "Personalist × Aid/GDP Change (log)",
  "initiator_personalist:milasym_trend_5yr"         = "Personalist × Military Asym. Trend",
  "milasym_ratio"                                   = "Military Asymmetry Ratio",
  "episode_duration_days"                           = "Episode Duration (days)",
  "cold_war"                                        = "Cold War",
  "dem_change"                                      = "Democracy Change",
  "log_gdp_pc_pre"                                  = "Log GDP per Capita",
  "prior_conflicts"                                 = "Prior Conflicts",
  "regime_duration"                                 = "Regime Duration",
  "shared_border"                                   = "Shared Border"
)

modelsummary(
  list(
    "Aid per Capita (log)"  = lpm_aid_level,
    "Aid/GDP Change (log)"  = lpm_aid_gdp
  ),
  vcov     = list(vcov_aid_level, vcov_aid_gdp),
  coef_map = coef_map_aid,
  stars    = c("†" = 0.10, "*" = 0.05, "**" = 0.01, "***" = 0.001),
  gof_map  = list(
    list(raw = "nobs",          clean = "N",       fmt = 0),
    list(raw = "r.squared",     clean = "R²",      fmt = 3),
    list(raw = "adj.r.squared", clean = "Adj. R²", fmt = 3)
  ),
  title  = "Appendix 6: Alternative Aid Operationalization",
  notes  = list(
    "Dyad-clustered standard errors in parentheses.",
    "DV: conflict relapse within 5-year window. N = 214.",
    "Main model uses post-conflict aid as % of GDP (logged).",
    "Aid per capita: logged mean post-conflict aid per capita over 5 years.",
    "Aid/GDP change: logged difference between post- and pre-conflict aid/GDP ratios.",
    "† p < 0.10, * p < 0.05, ** p < 0.01, *** p < 0.001"
  ),
  output = "Final_tables/Appendix6_Aid_Robustness.docx"
)
cat("Saved: Final_tables/Appendix6_Aid_Robustness.docx\n")

# 8.5 Aid sectors

if (file.exists(PATH_AIDDATA_PURPOSE)) {
  
  aid_raw_purpose <- readr::read_csv(PATH_AIDDATA_PURPOSE, show_col_types = FALSE)
  
  aid_typed <- aid_raw_purpose %>%
    transmute(
      recipient_iso3 = countrycode(recipient, "country.name", "iso3c"),
      year           = as.integer(year),
      aid_amount     = suppressWarnings(
        as.numeric(commitment_amount_usd_constant_sum)),
      purpose_code   = as.character(coalesced_purpose_code)
    ) %>%
    filter(!is.na(recipient_iso3), !is.na(year), !is.na(aid_amount),
           aid_amount >= 0, year >= START_YEAR, year <= COV_END_YEAR) %>%
    mutate(
      aid_type = case_when(
        purpose_code %in% c(
          "11000","11100","11105","11120","11130","11182",
          "11220","11230","11240","11320","11330","11420","11430",
          "12000","12005","12100","12110","12181","12182","12191",
          "12220","12230","12240","12250","12261","12281",
          "13000","13005","13010","13020","13030","13040","13081",
          "14000","14005","14010","14015","14020","14030",
          "14040","14050","14081","14082") ~ "social_services",
        purpose_code %in% c(
          "15000","15100","15105","15110","15120","15130","15140",
          "15150","15200","15205","15210","15220","15230",
          "15240","15250","15261") ~ "governance",
        purpose_code %in% c(
          "21005","21010","21020","21030","21040","21050","21061","21081",
          "22000","22005","22010","22020","22030","22040","22081",
          "23000","23005","23010","23020","23030","23040","23050","23055","23081","23082",
          "24000","24005","24010","24020","24030","24040","24081",
          "25010","25020","25081",
          "31000","31100","31105","31110","31120","31130","31140","31150","31181","31182","31191",
          "31205","31210","31220","31281","31282","31291",
          "31300","31305","31310","31320","31330","31381","31382","31391",
          "32000","32105","32110","32120","32130","32140","32181","32182","32191",
          "32200","32205","32210","32220","32281","32310",
          "33100","33105","33110","33120","33130","33140","33181","33210") ~ "economic",
        purpose_code %in% c(
          "70000","72000","72010","72020","72030","72040","72050",
          "73010","74010","91010","93010","99810","99820") ~ "humanitarian",
        purpose_code %in% c(
          "51010","52010","53030","53040","53050",
          "60010","60020","60030","60040") ~ "budget_debt",
        purpose_code %in% c(
          "16010","16020","16030","16050","16081",
          "41000","41005","41010","41020","41030",
          "41040","41050","41081","41082","42010",
          "43010","43030","43040","43050","43081","43082",
          "92000","92005","92010","92020","92030") ~ "other_multisector",
        TRUE ~ NA_character_
      )
    ) %>%
    filter(!is.na(aid_type))
  
  aid_cy_type <- aid_typed %>%
    group_by(recipient_iso3, year, aid_type) %>%
    summarise(aid_amount = sum(aid_amount, na.rm = TRUE), .groups = "drop")
  
  aid_cy_type_gdp <- aid_cy_type %>%
    left_join(gdp_total_cy, by = c("recipient_iso3" = "iso3c", "year")) %>%
    filter(!is.na(gdp_total), gdp_total > 0) %>%
    mutate(aid_gdp = aid_amount / gdp_total)
  
  aid_types_vec <- c("social_services", "governance", "economic",
                     "humanitarian", "budget_debt", "other_multisector")
  
  compute_window_mean_gdp <- function(episodes_df, aid_type_str,
                                      start_col, end_col, out_col) {
    episodes_df %>%
      select(dyad_id, episode_start, initiator_iso3,
             !!sym(start_col), !!sym(end_col)) %>%
      left_join(
        aid_cy_type_gdp %>% filter(aid_type == aid_type_str),
        by = c("initiator_iso3" = "recipient_iso3"),
        relationship = "many-to-many"
      ) %>%
      filter(year >= !!sym(start_col), year <= !!sym(end_col)) %>%
      group_by(dyad_id, episode_start) %>%
      summarise(!!out_col := mean(aid_gdp, na.rm = TRUE), .groups = "drop")
  }
  
  episodes_types <- episodes
  for (atype in aid_types_vec) {
    pre_col  <- paste0("aid_gdp_pre_",        atype)
    post_col <- paste0("aid_gdp_post_",       atype)
    chg_col  <- paste0("aid_gdp_change_log_", atype)
    pre_df   <- compute_window_mean_gdp(episodes_types, atype,
                                        "aid_pre_start_year", "aid_pre_end_year", pre_col)
    post_df  <- compute_window_mean_gdp(episodes_types, atype,
                                        "aid_post_start_year", "aid_post_end_year", post_col)
    episodes_types <- episodes_types %>%
      left_join(pre_df,  by = c("dyad_id", "episode_start")) %>%
      left_join(post_df, by = c("dyad_id", "episode_start")) %>%
      mutate(
        !!pre_col  := replace_na(.data[[pre_col]],  0),
        !!post_col := replace_na(.data[[post_col]], 0),
        !!chg_col  := log1p(.data[[post_col]]) - log1p(.data[[pre_col]])
      )
  }
  
  chg_type_cols <- paste0("aid_gdp_change_log_", aid_types_vec)
  
  pop_pre_joined <- episodes %>%
    select(dyad_id, episode_start, initiator_iso3,
           gdp_pre_start, gdp_pre_end) %>%
    left_join(pop_cy, by = c("initiator_iso3" = "iso3c"),
              relationship = "many-to-many") %>%
    filter(year >= gdp_pre_start, year <= gdp_pre_end) %>%
    group_by(dyad_id, episode_start) %>%
    summarise(pop_pre = mean(population, na.rm = TRUE), .groups = "drop") %>%
    mutate(
      pop_pre     = if_else(is.nan(pop_pre) | is.na(pop_pre), NA_real_, pop_pre),
      log_pop_pre = log(pop_pre)
    )
  
  episodes_types <- episodes_types %>%
    left_join(pop_pre_joined, by = c("dyad_id", "episode_start"))
  
  controls_no_aid <- c("milasym_ratio", "milasym_trend_5yr",
                       "episode_duration_days", "cold_war",
                       "dem_change", "log_gdp_pc_pre", "log_pop_pre",
                       "prior_conflicts", "regime_duration", "shared_border")
  
  fit_aid_sector_model <- function(data, aid_var) {
    d <- data %>% filter(!is.na(.data[[aid_var]]), !is.na(log_pop_pre))
    lpm_formula <- as.formula(paste(
      "relapse_5yr ~ initiator_personalist +", aid_var,
      "+ elite_sanctions_intensity +",
      paste(controls_no_aid, collapse = " + "),
      "+ initiator_personalist:elite_sanctions_intensity +",
      paste0("initiator_personalist:", aid_var)
    ))
    list(
      lpm     = lm(lpm_formula, data = d),
      n       = nrow(d),
      aid_var = aid_var
    )
  }
  
  extract_sector_results <- function(models) {
    lapply(models, function(m) {
      if (is.null(m)) return(NULL)
      av       <- m$aid_var
      int_term <- paste0("initiator_personalist:", av)
      lpm_ct   <- lmtest::coeftest(m$lpm,
                                   vcov = sandwich::vcovCL(m$lpm, cluster = ~dyad_id))
      lpm_mat  <- as.matrix(lpm_ct)
      data.frame(
        aid_type     = gsub("aid_gdp_change_log_", "", av),
        n            = m$n,
        lpm_aid_coef = round(lpm_mat[av,       "Estimate"],   3),
        lpm_aid_p    = round(lpm_mat[av,       "Pr(>|t|)"],   3),
        lpm_int_coef = round(lpm_mat[int_term, "Estimate"],   3),
        lpm_int_p    = round(lpm_mat[int_term, "Pr(>|t|)"],   3),
        stringsAsFactors = FALSE
      )
    }) %>% bind_rows()
  }
  
  # 8.5-A: Main sample
  final_data_types_main <- episodes_types %>%
    tidyr::drop_na(dplyr::all_of(model_vars_final)) %>%
    filter(initiator_personalist %in% c(0L, 1L)) %>%
    mutate(
      relapse_event         = as.integer(relapse_event),
      initiator_personalist = as.integer(initiator_personalist),
      relapse_5yr           = as.integer(relapse_event == 1 &
                                           time_to_relapse_days <= 1825),
      time_cox              = pmin(time_to_relapse_days, CENSOR_DAYS),
      event_cox             = as.integer(relapse_event == 1 &
                                           time_to_relapse_days <= CENSOR_DAYS),
      across(all_of(chg_type_cols), ~ replace_na(.x, 0)),
      log_pop_pre = if_else(is.nan(log_pop_pre) | is.na(log_pop_pre),
                            NA_real_, log_pop_pre)
    )
  
  cat("\n=== ROBUSTNESS 8.5-A: AID SECTORS (main sample, N =",
      nrow(final_data_types_main), ") ===\n")
  
  aid_sector_models_main <- lapply(chg_type_cols, function(v) {
    cat("  Fitting sector model:", v, "\n")
    tryCatch(fit_aid_sector_model(final_data_types_main, v),
             error = function(e) { cat("  ERROR:", e$message, "\n"); NULL })
  })
  names(aid_sector_models_main) <- chg_type_cols
  aid_sector_models_main <- Filter(Negate(is.null), aid_sector_models_main)
  sector_results_main <- extract_sector_results(aid_sector_models_main)
  cat("\n=== KEY RESULTS BY AID SECTOR (MAIN SAMPLE) ===\n")
  print(sector_results_main)
  
  # 8.5-B: Conservative sample
  final_data_types_conservative <- episodes_types %>%
    tidyr::drop_na(dplyr::all_of(model_vars_final)) %>%
    filter(initiator_personalist %in% c(0L, 1L)) %>%
    mutate(
      relapse_event         = as.integer(relapse_event),
      initiator_personalist = as.integer(initiator_personalist),
      relapse_5yr           = as.integer(relapse_event == 1 &
                                           time_to_relapse_days <= 1825),
      time_cox              = pmin(time_to_relapse_days, CENSOR_DAYS),
      event_cox             = as.integer(relapse_event == 1 &
                                           time_to_relapse_days <= CENSOR_DAYS),
      across(all_of(chg_type_cols), ~ replace_na(.x, 0)),
      log_pop_pre = if_else(is.nan(log_pop_pre) | is.na(log_pop_pre),
                            NA_real_, log_pop_pre)
    ) %>%
    group_by(initiator_country, episode_start) %>%
    arrange(desc(shared_border), target_country) %>%
    slice(1) %>%
    ungroup() %>%
    filter(episode_duration_days > 1)
  
  cat("\n=== ROBUSTNESS 8.5-B: AID SECTORS (conservative sample, N =",
      nrow(final_data_types_conservative), ") ===\n")
  
  aid_sector_models_cons <- lapply(chg_type_cols, function(v) {
    cat("  Fitting sector model:", v, "\n")
    tryCatch(fit_aid_sector_model(final_data_types_conservative, v),
             error = function(e) { cat("  ERROR:", e$message, "\n"); NULL })
  })
  names(aid_sector_models_cons) <- chg_type_cols
  aid_sector_models_cons <- Filter(Negate(is.null), aid_sector_models_cons)
  sector_results_cons <- extract_sector_results(aid_sector_models_cons)
  cat("\n=== KEY RESULTS BY AID SECTOR (CONSERVATIVE SAMPLE) ===\n")
  print(sector_results_cons)
  
  # Export Appendix 1 (main) and Appendix 2 (conservative)
  appendix1 <- data.frame(
    `Aid Sector`        = c("Social Services", "Governance", "Economic",
                            "Humanitarian", "Budget/Debt Relief", "Other Multisector"),
    N                   = sector_results_main$n,
    `Main Effect (β)`   = sector_results_main$lpm_aid_coef,
    `Main Effect (p)`   = sector_results_main$lpm_aid_p,
    `Interaction (β)`   = sector_results_main$lpm_int_coef,
    `Interaction (p)`   = sector_results_main$lpm_int_p,
    check.names = FALSE
  )
  
  read_docx() %>%
    body_add_par("Appendix 1: Aid Sector Models (N = 214, Main Sample)",
                 style = "heading 1") %>%
    body_add_flextable(
      flextable(appendix1) %>%
        bold(part = "header") %>%
        bg(part = "header", bg = "#F2F2F2") %>%
        autofit() %>%
        add_footer_lines(c(
          "Main Effect = association between aid sector and relapse for non-personalist regimes.",
          "Interaction = differential effect for personalist regimes relative to non-personalist.",
          "DV: conflict relapse within 5-year window. LPM with dyad-clustered standard errors.",
          "Sample: main analytical sample, dyad-level conflict episodes with autocratic initiators, 1960-2009.",
          "Data: AidData (Tierney et al., 2011). † p < 0.10, * p < 0.05."
        ))
    ) %>%
    print(target = "Final_tables/Appendix1_Aid_Sectors_Main.docx")
  cat("Saved: Final_tables/Appendix1_Aid_Sectors_Main.docx\n")
  
  appendix2 <- data.frame(
    `Aid Sector`        = c("Social Services", "Governance", "Economic",
                            "Humanitarian", "Budget/Debt Relief", "Other Multisector"),
    N                   = sector_results_cons$n,
    `Main Effect (β)`   = sector_results_cons$lpm_aid_coef,
    `Main Effect (p)`   = sector_results_cons$lpm_aid_p,
    `Interaction (β)`   = sector_results_cons$lpm_int_coef,
    `Interaction (p)`   = sector_results_cons$lpm_int_p,
    check.names = FALSE
  )
  
  read_docx() %>%
    body_add_par("Appendix 2: Aid Sector Models (N = 152, Conservative Sample)",
                 style = "heading 1") %>%
    body_add_flextable(
      flextable(appendix2) %>%
        bold(part = "header") %>%
        bg(part = "header", bg = "#F2F2F2") %>%
        autofit() %>%
        add_footer_lines(c(
          "Main Effect = association between aid sector and relapse for non-personalist regimes.",
          "Interaction = differential effect for personalist regimes relative to non-personalist.",
          "DV: conflict relapse within 5-year window. LPM with dyad-clustered standard errors.",
          "Sample: conservative specification (collapsed multilateral, excluding single-day episodes).",
          "Data: AidData (Tierney et al., 2011). † p < 0.10, * p < 0.05."
        ))
    ) %>%
    print(target = "Final_tables/Appendix2_Aid_Sectors_Conservative.docx")
  cat("Saved: Final_tables/Appendix2_Aid_Sectors_Conservative.docx\n")
  
} else {
  message("AidData purpose file not found — skipping aid sector analysis.")
}

# 8.6 Pre-conflict aid endogeneity check 

prior_conflict_check <- final_data_conservative %>%
  select(dyad_id, country1, country2,
         focal_start = episode_start,
         focal_end   = episode_end) %>%
  left_join(
    prior_episodes_tbl %>%
      rename(other_start = episode_start,
             other_end   = episode_end),
    by = c("country1", "country2"),
    relationship = "many-to-many"
  ) %>%
  mutate(
    is_same  = (other_start == focal_start),
    in_pre5  = !is_same & !is.na(other_end) &
      other_end < focal_start &
      other_end >= (focal_start - lubridate::years(5))
  ) %>%
  group_by(dyad_id, focal_start) %>%
  summarise(has_prior_5y = as.integer(any(in_pre5, na.rm = TRUE)),
            .groups = "drop")

final_data_pre_restricted <- final_data_conservative %>%
  left_join(prior_conflict_check,
            by = c("dyad_id", "episode_start" = "focal_start")) %>%
  mutate(has_prior_5y = coalesce(has_prior_5y, 0L)) %>%
  filter(has_prior_5y == 0L)

cat("\n=== ROBUSTNESS 8.6: PRE-CONFLICT AID ENDOGENEITY CHECK ===\n")
cat("N after prior conflict exclusion:", nrow(final_data_pre_restricted), "\n")

pre_aid_df <- final_data_pre_restricted %>%
  mutate(start_year = lubridate::year(episode_start)) %>%
  select(dyad_id, episode_start, initiator_iso3, start_year) %>%
  rowwise() %>%
  mutate(pre_years = list((start_year - 5L):(start_year - 1L))) %>%
  ungroup() %>%
  tidyr::unnest(pre_years) %>%
  rename(year = pre_years) %>%
  left_join(aid_cy_gdp, by = c("initiator_iso3" = "recipient_iso3", "year")) %>%
  group_by(dyad_id, episode_start) %>%
  summarise(aid_gdp_pre5 = mean(aid_gdp_ratio, na.rm = TRUE), .groups = "drop") %>%
  mutate(
    aid_gdp_pre5     = ifelse(is.nan(aid_gdp_pre5), 0, aid_gdp_pre5),
    aid_gdp_pre5_log = log1p(aid_gdp_pre5)
  )

final_data_pre_model <- final_data_pre_restricted %>%
  left_join(pre_aid_df, by = c("dyad_id", "episode_start")) %>%
  mutate(aid_gdp_pre5_log = coalesce(aid_gdp_pre5_log, 0))

lpm_pre <- lm(
  relapse_5yr ~
    initiator_personalist +
    aid_gdp_pre5_log +
    milasym_ratio +
    milasym_trend_5yr +
    initiator_personalist:aid_gdp_pre5_log +
    initiator_personalist:milasym_trend_5yr +
    episode_duration_days + cold_war + dem_change +
    log_gdp_pc_pre + prior_conflicts + regime_duration + shared_border,
  data = final_data_pre_model
)

cox_pre <- survival::coxph(
  survival::Surv(time_cox, event_cox) ~
    initiator_personalist +
    aid_gdp_pre5_log +
    milasym_ratio +
    milasym_trend_5yr +
    initiator_personalist:aid_gdp_pre5_log +
    initiator_personalist:milasym_trend_5yr +
    episode_duration_days + cold_war + dem_change +
    log_gdp_pc_pre + prior_conflicts + regime_duration + shared_border +
    cluster(dyad_id),
  data = final_data_pre_model
)

cat("\n=== ROBUSTNESS 8.6: LPM PRE-CONFLICT AID ===\n")
print(lmtest::coeftest(lpm_pre,
                       vcov = sandwich::vcovCL(lpm_pre, cluster = ~dyad_id)))

cat("\n=== ROBUSTNESS 8.6: COX PRE-CONFLICT AID ===\n")
print(summary(cox_pre))

# 8.7 Uncensored relapse 

cat("\n=== ROBUSTNESS 8.7: UNCENSORED RELAPSE ===\n")
lpm_uncensored <- lm(
  relapse_event ~
    initiator_personalist +
    elite_sanctions_intensity +
    aid_gdp_post_log +
    milasym_ratio +
    milasym_trend_5yr +
    initiator_personalist:elite_sanctions_intensity +
    initiator_personalist:aid_gdp_post_log +
    initiator_personalist:milasym_trend_5yr +
    episode_duration_days + cold_war + dem_change +
    log_gdp_pc_pre + prior_conflicts + regime_duration + shared_border,
  data = final_data
)
print(lmtest::coeftest(lpm_uncensored,
                       vcov = sandwich::vcovCL(lpm_uncensored, cluster = ~dyad_id)))

# ============================================================
# APPENDIX 7: Aid Sectors — Conservative Sample
# ============================================================

appendix7 <- data.frame(
  `Aid Sector`        = c("Social Services", "Governance", "Economic",
                          "Humanitarian", "Budget/Debt Relief", "Other Multisector"),
  N                   = sector_results_cons$n,
  `Main Effect (β)`   = sector_results_cons$lpm_aid_coef,
  `Main Effect (p)`   = sector_results_cons$lpm_aid_p,
  `Interaction (β)`   = sector_results_cons$lpm_int_coef,
  `Interaction (p)`   = sector_results_cons$lpm_int_p,
  check.names = FALSE
)

read_docx() %>%
  body_add_par("Appendix 7: Aid Sector Models (N = 152, Conservative Sample)",
               style = "heading 1") %>%
  body_add_flextable(
    flextable(appendix7) %>%
      bold(part = "header") %>%
      bg(part = "header", bg = "#F2F2F2") %>%
      autofit() %>%
      add_footer_lines(c(
        "Main Effect = association between aid sector and relapse for non-personalist regimes.",
        "Interaction = differential effect for personalist regimes relative to non-personalist.",
        "DV: conflict relapse within 5-year window. LPM with dyad-clustered standard errors.",
        "Sample: conservative specification (collapsed multilateral, excluding single-day episodes).",
        "Data: AidData (Tierney et al., 2011). † p < 0.10, * p < 0.05."
      ))
  ) %>%
  print(target = "Final_tables/Appendix7_Aid_Sectors_Conservative.docx")
cat("Saved: Final_tables/Appendix7_Aid_Sectors_Conservative.docx\n")

# ============================================================
# APPENDIX 8: Pre-conflict Aid Endogeneity Check
# ============================================================

vcov_pre <- sandwich::vcovCL(lpm_pre, cluster = ~dyad_id)

coef_map_pre <- c(
  "(Intercept)"                           = "Intercept",
  "initiator_personalist"                 = "Personalist Regime",
  "aid_gdp_pre5_log"                      = "Pre-conflict Aid (log, % GDP)",
  "milasym_ratio"                         = "Military Asymmetry Ratio",
  "milasym_trend_5yr"                     = "Military Asymmetry Trend",
  "initiator_personalist:aid_gdp_pre5_log"= "Personalist × Pre-conflict Aid",
  "initiator_personalist:milasym_trend_5yr" = "Personalist × Military Asym. Trend",
  "episode_duration_days"                 = "Episode Duration (days)",
  "cold_war"                              = "Cold War",
  "dem_change"                            = "Democracy Change",
  "log_gdp_pc_pre"                        = "Log GDP per Capita",
  "prior_conflicts"                       = "Prior Conflicts",
  "regime_duration"                       = "Regime Duration",
  "shared_border"                         = "Shared Border"
)

modelsummary(
  list(
    "LPM — Pre-conflict Aid"     = lpm_pre,
    "Cox PH — Pre-conflict Aid"  = cox_pre
  ),
  vcov     = list(vcov_pre, NULL),
  coef_map = coef_map_pre,
  stars    = c("†" = 0.10, "*" = 0.05, "**" = 0.01, "***" = 0.001),
  gof_map  = list(
    list(raw = "nobs",          clean = "N",           fmt = 0),
    list(raw = "r.squared",     clean = "R²",          fmt = 3),
    list(raw = "adj.r.squared", clean = "Adj. R²",     fmt = 3),
    list(raw = "nevent",        clean = "Events",      fmt = 0),
    list(raw = "concordance",   clean = "Concordance", fmt = 3)
  ),
  title  = "Appendix 8: Pre-conflict Aid Endogeneity Check",
  notes  = list(
    "Dyad-clustered standard errors in parentheses.",
    "DV: conflict relapse within 5-year window.",
    "Sample restricted to episodes with no prior high-intensity conflict",
    "in the 5 years before onset (N = 82, 18 relapse events).",
    "Sanctions variable excluded from this specification.",
    "† p < 0.10, * p < 0.05, ** p < 0.01, *** p < 0.001"
  ),
  output = "Final_tables/Appendix8_Pre_Conflict_Aid.docx"
)
cat("Saved: Final_tables/Appendix8_Pre_Conflict_Aid.docx\n")

# 8.8 Temporal dynamics (Years 1-5) 

traj_terms <- c(
  "initiator_personalist:elite_sanctions_intensity",
  "initiator_personalist:aid_gdp_post_log",
  "initiator_personalist:milasym_trend_5yr"
)

term_labels_map <- c(
  "initiator_personalist:elite_sanctions_intensity" = "Personalist × Sanctions",
  "initiator_personalist:aid_gdp_post_log"          = "Personalist × Aid (GDP level)",
  "initiator_personalist:milasym_trend_5yr"         = "Personalist × CINC Trend"
)

run_window_lpm <- function(data, days_threshold, yr_label, yr_num) {
  df <- data %>%
    mutate(dv = as.integer(relapse_event == 1 &
                             time_to_relapse_days <= days_threshold))
  n_ev <- sum(df$dv)
  m <- lm(
    dv ~
      initiator_personalist +
      elite_sanctions_intensity +
      aid_gdp_post_log +
      milasym_ratio +
      milasym_trend_5yr +
      initiator_personalist:elite_sanctions_intensity +
      initiator_personalist:aid_gdp_post_log +
      initiator_personalist:milasym_trend_5yr +
      episode_duration_days + cold_war + dem_change +
      log_gdp_pc_pre + prior_conflicts + regime_duration + shared_border,
    data = df
  )
  ct  <- lmtest::coeftest(m, vcov = sandwich::vcovCL(m, cluster = ~dyad_id))
  mat <- as.matrix(ct)
  data.frame(
    year_label = yr_label,
    year_num   = yr_num,
    days       = days_threshold,
    n_total    = nrow(df),
    n_events   = n_ev,
    term       = traj_terms,
    coef       = round(mat[traj_terms, "Estimate"],   4),
    se         = round(mat[traj_terms, "Std. Error"], 4),
    t_stat     = round(mat[traj_terms, "t value"],    3),
    p_value    = round(mat[traj_terms, "Pr(>|t|)"],   4),
    stringsAsFactors = FALSE,
    row.names  = NULL
  )
}

windows_5yr <- list(
  list(days = 365L,  label = "Year 1", num = 1L),
  list(days = 730L,  label = "Year 2", num = 2L),
  list(days = 1095L, label = "Year 3", num = 3L),
  list(days = 1460L, label = "Year 4", num = 4L),
  list(days = 1825L, label = "Year 5", num = 5L)
)

cat("\n=== ROBUSTNESS 8.8: TEMPORAL DYNAMICS (Years 1-5) ===\n")
cat("Focal episodes end <= 2009, observed through 2014.\n\n")

traj_5yr <- dplyr::bind_rows(lapply(windows_5yr, function(w) {
  run_window_lpm(final_data, w$days, w$label, w$num)
})) %>%
  mutate(
    ci_lo        = coef - 1.96 * se,
    ci_hi        = coef + 1.96 * se,
    sig_label    = dplyr::case_when(
      p_value < 0.001 ~ "***",
      p_value < 0.01  ~ "**",
      p_value < 0.05  ~ "*",
      p_value < 0.10  ~ "†",
      TRUE            ~ ""
    ),
    sig_category = factor(dplyr::case_when(
      p_value < 0.05 ~ "Significant (p < 0.05)",
      p_value < 0.10 ~ "Marginal (p < 0.10)",
      TRUE           ~ "Not significant"
    ), levels = c("Significant (p < 0.05)", "Marginal (p < 0.10)", "Not significant")),
    term_label = dplyr::recode(term, !!!term_labels_map)
  )

cat("\n--- Event counts ---\n")
print(as.data.frame(traj_5yr %>%
                      distinct(year_label, n_total, n_events) %>%
                      mutate(relapse_rate = round(n_events / n_total, 3))),
      row.names = FALSE)

cat("\n--- Interaction coefficients (Years 1-5) ---\n")
wide_5yr <- traj_5yr %>%
  mutate(display = paste0(formatC(coef, format="f", digits=3), sig_label)) %>%
  select(term_label, year_label, display) %>%
  tidyr::pivot_wider(names_from = year_label, values_from = display)
print(as.data.frame(wide_5yr), row.names = FALSE)
cat("Key: † p<0.10  * p<0.05  ** p<0.01  *** p<0.001\n")

plot_5yr <- ggplot(traj_5yr, aes(x = year_num, y = coef, group = term_label)) +
  geom_hline(yintercept = 0, linetype = "dashed", color = "gray40", linewidth = 0.5) +
  geom_ribbon(aes(ymin = ci_lo, ymax = ci_hi), alpha = 0.12, fill = "steelblue") +
  geom_line(color = "gray30", linewidth = 0.7) +
  geom_point(aes(color = sig_category, shape = sig_category), size = 3.5) +
  geom_text(aes(label = sig_label), vjust = -1.3, hjust = 0.5,
            size = 3.5, color = "gray20") +
  geom_text(
    data = traj_5yr %>% distinct(year_num, n_events, term_label),
    aes(x = year_num, y = -Inf, label = paste0("n=", n_events)),
    vjust = -0.5, size = 2.8, color = "gray50", inherit.aes = FALSE
  ) +
  facet_wrap(~ term_label, scales = "free_y", ncol = 1) +
  scale_x_continuous(breaks = 1:5, labels = paste0("Year ", 1:5)) +
  scale_color_manual(
    values = c("Significant (p < 0.05)" = "#1a5276",
               "Marginal (p < 0.10)"    = "#2e86c1",
               "Not significant"        = "gray60"),
    name = ""
  ) +
  scale_shape_manual(
    values = c("Significant (p < 0.05)" = 16,
               "Marginal (p < 0.10)"    = 17,
               "Not significant"        = 1),
    name = ""
  ) +
  labs(
    x       = "Years after conflict end (cumulative window)",
    y       = "LPM Coefficient",
    caption = paste0(
      "† p<0.10  * p<0.05  ** p<0.01  *** p<0.001\n",
      "Controls: CINC ratio (onset), episode duration, Cold War, ",
      "democracy change, log GDP pc, prior conflicts, regime duration, shared border."
    )
  ) +
  theme_minimal(base_size = 12) +
  theme(
    legend.position  = "bottom",
    strip.text       = element_text(face = "bold", size = 11),
    strip.background = element_rect(fill = "gray95", color = NA),
    panel.grid.minor = element_blank(),
    plot.caption     = element_text(size = 8, color = "gray50"),
    plot.margin      = margin(10, 15, 10, 10)
  )

print(plot_5yr)
ggsave("Final_tables/figure5_temporal_dynamics.png", plot = plot_5yr,
       width = 8, height = 10, dpi = 300, bg = "white")
cat("Saved: Final_tables/figure5_temporal_dynamics.png\n")

if ("modelsummary" %in% installed.packages()[,"Package"]) {
  yearly_models_5yr <- lapply(windows_5yr, function(w) {
    df <- final_data %>%
      mutate(dv = as.integer(relapse_event == 1 &
                               time_to_relapse_days <= w$days))
    lm(
      dv ~
        initiator_personalist +
        elite_sanctions_intensity +
        aid_gdp_post_log +
        milasym_ratio +
        milasym_trend_5yr +
        initiator_personalist:elite_sanctions_intensity +
        initiator_personalist:aid_gdp_post_log +
        initiator_personalist:milasym_trend_5yr +
        episode_duration_days + cold_war + dem_change +
        log_gdp_pc_pre + prior_conflicts + regime_duration + shared_border,
      data = df
    )
  })
  names(yearly_models_5yr) <- sapply(windows_5yr, function(w) w$label)
  
  yearly_vcov_5yr <- lapply(yearly_models_5yr, function(m)
    sandwich::vcovCL(m, cluster = final_data$dyad_id))
  
  coef_map_traj <- c(
    "initiator_personalist"                           = "Personalist Regime",
    "elite_sanctions_intensity"                       = "Sanctions Intensity",
    "aid_gdp_post_log"                                = "Aid/GDP (log level)",
    "milasym_ratio"                                   = "CINC Ratio (log, control)",
    "milasym_trend_5yr"                               = "CINC Trend (5yr)",
    "initiator_personalist:elite_sanctions_intensity" = "Personalist × Sanctions",
    "initiator_personalist:aid_gdp_post_log"          = "Personalist × Aid",
    "initiator_personalist:milasym_trend_5yr"         = "Personalist × CINC Trend",
    "episode_duration_days"                           = "Episode Duration",
    "cold_war"                                        = "Cold War",
    "dem_change"                                      = "Democracy Change",
    "log_gdp_pc_pre"                                  = "Log GDP pc (pre)",
    "prior_conflicts"                                 = "Prior Conflicts",
    "regime_duration"                                 = "Regime Duration",
    "shared_border"                                   = "Shared Border",
    "(Intercept)"                                     = "Intercept"
  )
  
  modelsummary(
    yearly_models_5yr,
    vcov     = yearly_vcov_5yr,
    coef_map = coef_map_traj,
    stars    = c("†" = 0.10, "*" = 0.05, "**" = 0.01, "***" = 0.001),
    gof_map  = list(
      list(raw = "nobs",          clean = "N",        fmt = 0),
      list(raw = "r.squared",     clean = "R²",       fmt = 3),
      list(raw = "adj.r.squared", clean = "Adj. R²",  fmt = 3)
    ),
    title  = "Appendix 3: Temporal Dynamics (Years 1-5, N = 214)",
    notes  = list(
      "Clustered standard errors by dyad in parentheses.",
      "DV = 1 if same initiator relapses within the indicated window.",
      "milasym_ratio included as control only (no interaction).",
      "Focal episodes end <= 2009, data through 2014.",
      "† p<0.10, * p<0.05, ** p<0.01, *** p<0.001"
    ),
    output = "Final_tables/Appendix3_Temporal_Dynamics.docx"
  )
  cat("Saved: Final_tables/Appendix3_Temporal_Dynamics.docx\n")
}

# ============================================================
# APPENDIX 9: Temporal Dynamics (Years 1-5)
# ============================================================

modelsummary(
  yearly_models_5yr,
  vcov     = yearly_vcov_5yr,
  coef_map = coef_map_traj,
  stars    = c("†" = 0.10, "*" = 0.05, "**" = 0.01, "***" = 0.001),
  gof_map  = list(
    list(raw = "nobs",          clean = "N",       fmt = 0),
    list(raw = "r.squared",     clean = "R²",      fmt = 3),
    list(raw = "adj.r.squared", clean = "Adj. R²", fmt = 3)
  ),
  title  = "Appendix 9: Temporal Dynamics (Years 1-5, N = 214)",
  notes  = list(
    "Dyad-clustered standard errors in parentheses.",
    "DV = 1 if same initiator relapses within the indicated cumulative window.",
    "milasym_ratio included as control only (no interaction).",
    "Focal episodes end <= 2009, data through 2014.",
    "† p < 0.10, * p < 0.05, ** p < 0.01, *** p < 0.001"
  ),
  output = "Final_tables/Appendix9_Temporal_Dynamics.docx"
)
cat("Saved: Final_tables/Appendix9_Temporal_Dynamics.docx\n")

# ============================================================
# APPENDIX 10: Uncensored Relapse
# ============================================================

vcov_uncensored <- sandwich::vcovCL(lpm_uncensored, cluster = ~dyad_id)

coef_map_uncens <- c(
  "(Intercept)"                                     = "Intercept",
  "initiator_personalist"                           = "Personalist Regime",
  "elite_sanctions_intensity"                       = "Elite Sanctions Intensity",
  "aid_gdp_post_log"                                = "Post-conflict Aid (log, % GDP)",
  "milasym_trend_5yr"                               = "Military Asymmetry Trend",
  "initiator_personalist:elite_sanctions_intensity" = "Personalist × Sanctions Intensity",
  "initiator_personalist:aid_gdp_post_log"          = "Personalist × Aid (log, % GDP)",
  "initiator_personalist:milasym_trend_5yr"         = "Personalist × Military Asym. Trend",
  "milasym_ratio"                                   = "Military Asymmetry Ratio",
  "episode_duration_days"                           = "Episode Duration (days)",
  "cold_war"                                        = "Cold War",
  "dem_change"                                      = "Democracy Change",
  "log_gdp_pc_pre"                                  = "Log GDP per Capita",
  "prior_conflicts"                                 = "Prior Conflicts",
  "regime_duration"                                 = "Regime Duration",
  "shared_border"                                   = "Shared Border"
)

modelsummary(
  list("LPM — Uncensored Relapse" = lpm_uncensored),
  vcov     = list(vcov_uncensored),
  coef_map = coef_map_uncens,
  stars    = c("†" = 0.10, "*" = 0.05, "**" = 0.01, "***" = 0.001),
  gof_map  = list(
    list(raw = "nobs",          clean = "N",       fmt = 0),
    list(raw = "r.squared",     clean = "R²",      fmt = 3),
    list(raw = "adj.r.squared", clean = "Adj. R²", fmt = 3)
  ),
  title  = "Appendix 10: Uncensored Relapse Model",
  notes  = list(
    "Dyad-clustered standard errors in parentheses.",
    "DV: any subsequent conflict initiation by same initiator against same target,",
    "regardless of timing (uncensored). N = 214.",
    "† p < 0.10, * p < 0.05, ** p < 0.01, *** p < 0.001"
  ),
  output = "Final_tables/Appendix10_Uncensored_Relapse.docx"
)
cat("Saved: Final_tables/Appendix10_Uncensored_Relapse.docx\n")

# ============================================================
# 9) EXPORT FINAL DATASET
# ============================================================
writexl::write_xlsx(final_data, "dmytryshyn_thesis_data.xlsx")
cat("Dataset exported: dmytryshyn_thesis_data.xlsx\n")

# ============================================================
# SENSITIVITY CHECK: Conflict Outcome as Control
# ============================================================

PATH_MIDA <- "C:/Users/HP/Documents/MID-5-Data-and-Supporting-Materials (1)/MIDA 5.0.csv"

mid_outcome <- read.csv(PATH_MIDA) %>%
  filter(hostlev %in% c(4, 5)) %>%
  mutate(
    styear   = as.integer(styear),
    stmonth  = as.integer(stmon),
    stday    = as.integer(stday),
    endyear  = as.integer(endyear),
    endmonth = as.integer(endmon),
    endday   = as.integer(endday)
  ) %>%
  filter(stmonth >= 1, stday >= 1, endmonth >= 1, endday >= 1) %>%
  mutate(
    st_date  = make_date_safe(styear,  stmonth, stday),
    end_date = make_date_safe(endyear, endmonth, endday)
  ) %>%
  filter(!is.na(st_date), !is.na(end_date))

mid_outcome_clean <- mid_outcome %>%
  mutate(
    outcome_cat = case_when(
      outcome == 1  ~ "victory",
      outcome == 2  ~ "defeat",
      outcome == 3  ~ "yield_loss",
      outcome == 4  ~ "yield_win",
      outcome == 5  ~ "stalemate",
      outcome == 6  ~ "compromise",
      outcome == 7  ~ "released",
      outcome == 8  ~ "unclear",
      outcome == 9  ~ "joins_ongoing",
      outcome == -9 ~ "missing",
      TRUE          ~ "missing"
    )
  ) %>%
  select(dispnum, st_date, end_date, outcome_cat)

final_data_outcome <- final_data %>%
  left_join(mid_outcome_clean,
            by = c("episode_start" = "st_date",
                   "episode_end"   = "end_date"))

cat("\n=== OUTCOME MERGE DIAGNOSTICS ===\n")
cat("N matched:", sum(!is.na(final_data_outcome$outcome_cat)), "\n")
cat("N unmatched:", sum(is.na(final_data_outcome$outcome_cat)), "\n")
cat("\nOutcome distribution:\n")
print(table(final_data_outcome$outcome_cat, useNA = "ifany"))

# ---- Model with all outcome categories (reference = stalemate) ----

lpm_outcome_all <- lm(
  relapse_5yr ~
    initiator_personalist +
    elite_sanctions_intensity +
    aid_gdp_post_log +
    milasym_trend_5yr +
    initiator_personalist:elite_sanctions_intensity +
    initiator_personalist:aid_gdp_post_log +
    initiator_personalist:milasym_trend_5yr +
    milasym_ratio +
    factor(outcome_cat) +
    episode_duration_days + cold_war + dem_change +
    log_gdp_pc_pre + prior_conflicts + regime_duration + shared_border,
  data = final_data_outcome %>%
    filter(!is.na(outcome_cat)) %>%
    mutate(outcome_cat = relevel(factor(outcome_cat), ref = "stalemate"))
)

out_ct_all <- lmtest::coeftest(lpm_outcome_all,
                               vcov = sandwich::vcovCL(lpm_outcome_all,
                                                       cluster = ~dyad_id))

cat("\n=== SENSITIVITY: LPM WITH ALL OUTCOME CATEGORIES ===\n")
cat("Reference category: stalemate\n\n")
print(out_ct_all)

# ---- Key comparison ----

cat("\n=== KEY INTERACTION COMPARISON ===\n")
cat("--- Main model (N = 214) ---\n")
main_ct <- lmtest::coeftest(lpm_controls,
                            vcov = sandwich::vcovCL(lpm_controls, cluster = ~dyad_id))
cat("Sanctions interaction: β =",
    round(main_ct["initiator_personalist:elite_sanctions_intensity", "Estimate"], 4),
    "p =", round(main_ct["initiator_personalist:elite_sanctions_intensity", "Pr(>|t|)"], 4), "\n")
cat("Aid interaction:       β =",
    round(main_ct["initiator_personalist:aid_gdp_post_log", "Estimate"], 4),
    "p =", round(main_ct["initiator_personalist:aid_gdp_post_log", "Pr(>|t|)"], 4), "\n")

cat("\n--- Outcome control model (N = 207) ---\n")
cat("Sanctions interaction: β =",
    round(out_ct_all["initiator_personalist:elite_sanctions_intensity", "Estimate"], 4),
    "p =", round(out_ct_all["initiator_personalist:elite_sanctions_intensity", "Pr(>|t|)"], 4), "\n")
cat("Aid interaction:       β =",
    round(out_ct_all["initiator_personalist:aid_gdp_post_log", "Estimate"], 4),
    "p =", round(out_ct_all["initiator_personalist:aid_gdp_post_log", "Pr(>|t|)"], 4), "\n")

cat("\n--- Outcome category coefficients (vs stalemate) ---\n")
outcome_rows <- grep("factor\\(outcome_cat\\)", rownames(out_ct_all))
print(round(out_ct_all[outcome_rows, ], 4))


final_data_outcome_clean <- final_data_outcome %>%
  filter(!is.na(outcome_cat)) %>%
  mutate(
    outcome_collapsed = case_when(
      outcome_cat %in% c("victory", "yield_win")  ~ "favorable",
      outcome_cat %in% c("defeat", "yield_loss")  ~ "unfavorable",
      outcome_cat == "stalemate"                  ~ "stalemate",
      outcome_cat == "compromise"                 ~ "compromise",
      TRUE                                        ~ "other"
    ),
    outcome_collapsed = relevel(factor(outcome_collapsed), ref = "stalemate")
  )


lpm_outcome_collapsed <- lm(
  relapse_5yr ~
    initiator_personalist +
    elite_sanctions_intensity +
    aid_gdp_post_log +
    milasym_trend_5yr +
    initiator_personalist:elite_sanctions_intensity +
    initiator_personalist:aid_gdp_post_log +
    initiator_personalist:milasym_trend_5yr +
    milasym_ratio +
    outcome_collapsed +
    episode_duration_days + cold_war + dem_change +
    log_gdp_pc_pre + prior_conflicts + regime_duration + shared_border,
  data = final_data_outcome_clean
)

out_ct_collapsed <- lmtest::coeftest(lpm_outcome_collapsed,
                                     vcov = sandwich::vcovCL(lpm_outcome_collapsed,
                                                             cluster = ~dyad_id))

cat("=== SENSITIVITY: COLLAPSED OUTCOME CATEGORIES ===\n")
cat("Reference: stalemate\n")
cat("Favorable = victory + yield_win (N = 11)\n")
cat("Unfavorable = defeat + yield_loss (N = 29)\n")
cat("Compromise (N = 12)\n")
cat("Other = unclear + released + missing (N = 16)\n\n")
print(out_ct_collapsed)

cat("\n=== KEY INTERACTIONS ===\n")
cat("Sanctions: β =",
    round(out_ct_collapsed["initiator_personalist:elite_sanctions_intensity", "Estimate"], 4),
    "p =", round(out_ct_collapsed["initiator_personalist:elite_sanctions_intensity", "Pr(>|t|)"], 4), "\n")
cat("Aid:       β =",
    round(out_ct_collapsed["initiator_personalist:aid_gdp_post_log", "Estimate"], 4),
    "p =", round(out_ct_collapsed["initiator_personalist:aid_gdp_post_log", "Pr(>|t|)"], 4), "\n")
cat("\nOutcome coefficients vs stalemate:\n")
outcome_rows <- grep("outcome_collapsed", rownames(out_ct_collapsed))
print(round(out_ct_collapsed[outcome_rows, ], 4))


lpm_restricted <- lm(
  relapse_5yr ~
    initiator_personalist +
    elite_sanctions_intensity +
    aid_gdp_post_log +
    milasym_trend_5yr +
    initiator_personalist:elite_sanctions_intensity +
    initiator_personalist:aid_gdp_post_log +
    initiator_personalist:milasym_trend_5yr +
    milasym_ratio +
    episode_duration_days + cold_war + dem_change +
    log_gdp_pc_pre + prior_conflicts + regime_duration + shared_border,
  data = final_data_outcome_clean
)

restricted_ct <- lmtest::coeftest(lpm_restricted,
                                  vcov = sandwich::vcovCL(lpm_restricted,
                                                          cluster = ~dyad_id))
cat("Aid interaction on restricted sample (no outcome control):\n")
cat("β =", round(restricted_ct["initiator_personalist:aid_gdp_post_log", "Estimate"], 4),
    "p =", round(restricted_ct["initiator_personalist:aid_gdp_post_log", "Pr(>|t|)"], 4), "\n")

# Aid per capita with outcome control
lpm_aid_pc_outcome <- lm(
  relapse_5yr ~
    initiator_personalist +
    elite_sanctions_intensity +
    aid_pc_post_log +
    milasym_trend_5yr +
    initiator_personalist:elite_sanctions_intensity +
    initiator_personalist:aid_pc_post_log +
    initiator_personalist:milasym_trend_5yr +
    milasym_ratio +
    outcome_collapsed +
    episode_duration_days + cold_war + dem_change +
    log_gdp_pc_pre + prior_conflicts + regime_duration + shared_border,
  data = final_data_outcome_clean
)

# Aid GDP change with outcome control
lpm_aid_gdp_outcome <- lm(
  relapse_5yr ~
    initiator_personalist +
    elite_sanctions_intensity +
    aid_gdp_change_log +
    milasym_trend_5yr +
    initiator_personalist:elite_sanctions_intensity +
    initiator_personalist:aid_gdp_change_log +
    initiator_personalist:milasym_trend_5yr +
    milasym_ratio +
    outcome_collapsed +
    episode_duration_days + cold_war + dem_change +
    log_gdp_pc_pre + prior_conflicts + regime_duration + shared_border,
  data = final_data_outcome_clean
)

ct_pc  <- lmtest::coeftest(lpm_aid_pc_outcome,
                           vcov = sandwich::vcovCL(lpm_aid_pc_outcome,
                                                   cluster = ~dyad_id))
ct_gdp <- lmtest::coeftest(lpm_aid_gdp_outcome,
                           vcov = sandwich::vcovCL(lpm_aid_gdp_outcome,
                                                   cluster = ~dyad_id))

cat("=== AID OPERATIONALIZATION WITH OUTCOME CONTROLS ===\n\n")
cat("--- Main aid (% GDP log) ---\n")
cat("β =", round(out_ct_collapsed["initiator_personalist:aid_gdp_post_log", "Estimate"], 4),
    "p =", round(out_ct_collapsed["initiator_personalist:aid_gdp_post_log", "Pr(>|t|)"], 4), "\n")

cat("\n--- Aid per capita (log) ---\n")
cat("β =", round(ct_pc["initiator_personalist:aid_pc_post_log", "Estimate"], 4),
    "p =", round(ct_pc["initiator_personalist:aid_pc_post_log", "Pr(>|t|)"], 4), "\n")

cat("\n--- Aid GDP change (log) ---\n")
cat("β =", round(ct_gdp["initiator_personalist:aid_gdp_change_log", "Estimate"], 4),
    "p =", round(ct_gdp["initiator_personalist:aid_gdp_change_log", "Pr(>|t|)"], 4), "\n")
getwd()
file.exists("dmytryshyn_thesis_data.xlsx")

