

# 📚 Cargar las librerias

library(httr)
library(jsonlite)
library(rvest)
library(tidyverse)

💾 #cargar y extraer los datos 💀

matches <- "https://match.uefa.com/v5/matches?competitionId=3&limit=55&offset=0&order=ASC&phase=ALL&roundId=2001084&seasonYear=2024"


uefa_matches <- httr::GET(matches, query= list()) %>%
  httr::content() %>% 
  dplyr::tibble(value=.) %>%
  tidyr::unnest_wider(value) %>% 
  tidyr::unnest_wider(c(awayTeam, playerEvents, competition,  group, homeTeam, kickOffTime, matchday, score, round, referees, winner, stadium), names_sep = "_") %>% 
  tidyr::unnest_wider(c(group_metaData, competition_metaData, round_metaData, round_teams, group_teams, playerEvents_scorers, score_regular, score_total), names_sep = "_") %>% 
  unnest_wider(contains("playerEvents_scorers_"), names_sep = "_") %>%
  unnest_wider(contains("playerEvents_scorers_"), names_sep = "_") %>% 
  select(-contains("translations"))

#podría estar haciendo unnest hasta el infinito pero me harté

💿 #data frame para juntar con los datos extraidos

teams_away <- uefa_matches %>% select(team_id = awayTeam_id, countryCode = awayTeam_countryCode, internationalName = awayTeam_internationalName)
teams <- uefa_matches %>% select(team_id = homeTeam_id, countryCode = homeTeam_countryCode, internationalName =  homeTeam_internationalName ) %>% 
  rbind(teams_away) %>% 
  unique() %>% 
  mutate(team_id = as.numeric(team_id))

🏟️ #Función para extrer los datos

id_number <- uefa_matches$id

uefa_stats <- function(id_number){

url <- paste0("https://matchstats.uefa.com/v1/team-statistics/", id_number) %>% 
  httr::GET(query= list()) %>%
  httr::content()

df <- purrr::pluck(url,  2, "statistics") %>%
    dplyr::tibble(value=.) %>%
    tidyr::unnest_wider(value) %>% 
    select(name, value) %>% 
    mutate(team_id = as.numeric(pluck(url,  2, "teamId")),
           match_number = as.numeric(id_number),
           local = TRUE)

df_1 <- purrr::pluck(url,  1, "statistics") %>%
  dplyr::tibble(value=.) %>%
  tidyr::unnest_wider(value) %>% 
  select(name, value) %>% 
  mutate(team_id = as.numeric(pluck(url,  1, "teamId")) ,
         match_number = as.numeric(id_number),
         local = FALSE)  

df_3 <-  rbind(df_1, df) %>% 
left_join(teams, join_by(team_id))

}

uefa_statsDf <- map_df(id_number, uefa_stats)

📝 #escribir csv

write_csv(uefa_statsDf,  "uefa2024.csv")
