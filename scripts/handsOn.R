library(dplyr)
library(tidyr)
library(ggplot2)
library(plotly)
library(lubridate)
library(readr)


# Importe a base de dados do TidyTuesday direto do Github
taylor_album_songs <- read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2023/2023-10-17/taylor_album_songs.csv')
taylor_all_songs <- read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2023/2023-10-17/taylor_all_songs.csv')
taylor_albums <- read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2023/2023-10-17/taylor_albums.csv')

names(taylor_album_songs)

top_n(x = taylor_album_songs, n = 4, wt = track_number)

taylor_album_songs |> 
  top_n(n = 4, wt = track_number)

taylor_albums_selected <- taylor_albums |> 
  select(metacritic_score, user_score, album_name)

levels(as.factor(taylor_albums_selected$album_name))
levels(as.factor(df$album_name))


df <- taylor_album_songs |> 
  select(album_name, track_name, track_number, album_release, danceability) |> 
  mutate(
    release_year = lubridate::year(album_release)
  ) |>  
  # filter(release_year == 2006 | release_year == 2010) |> 
  arrange(desc(track_number)) |> 
  left_join(y = taylor_albums_selected,by = c("album_name"="album_name")) |> 
  group_by(album_name) |> 
  summarise(
    n = n(),
    danceability_media = mean(danceability, na.rm = TRUE),
    danceability_sd = sd(danceability, na.rm = TRUE),
    danceability_max = max(danceability, na.rm = TRUE),
    danceability_min = min(danceability, na.rm = TRUE),
    danceability_mediana = median(danceability, na.rm = TRUE),
    
    user_score_mean = mean(user_score, na.rm = TRUE),
    user_score_sd = sd(user_score)
  )












