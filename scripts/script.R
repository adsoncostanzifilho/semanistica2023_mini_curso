library(readr)
library(dplyr)
library(tidyr)
library(lubridate)
library(ggplot2)
library(plotly)

# Importe a base de dados do TidyTuesday direto do Github
taylor_album_songs <- read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2023/2023-10-17/taylor_album_songs.csv')
taylor_all_songs <- read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2023/2023-10-17/taylor_all_songs.csv')
taylor_albums <- read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2023/2023-10-17/taylor_albums.csv')

# Visualize as primeiras linhas dos dados
taylor_album_songs |> top_n(4, wt = track_number)
taylor_all_songs |> top_n(4, wt = track_number)
taylor_albums |> top_n(4, wt = user_score)



#---- Manipulação de Dados com dplyr
# select, filter, left_join, inner_join, bind_rows, bind_cols, mutate, arrange

names(taylor_album_songs)

# select
df <- taylor_album_songs |>  
  select(album_name, track_number, track_name, album_release, loudness, danceability)

# mutate
df <- df |> 
  mutate(release_year = lubridate::year(album_release))

# filter
df_2010 <- df |> 
  filter(release_year == 2010)

# arrange
df <- df |>  
  arrange(desc(release_year))

# left join
taylor_albums_clean <- taylor_albums |>  
  select(album_name, metacritic_score, user_score)

df_join <- df |>  
  left_join(taylor_albums_clean, by = c("album_name"="album_name"))

#inner join
df_join <- df |>  
  inner_join(taylor_albums_clean[-1,], by = c("album_name"="album_name"))


#---- Transformacao de dados com tidyr

# pivot_longer e pivot_wider

df_long <- df |>  
  group_by(album_name, release_year) |> 
  summarise(n = n()) |> 
  ungroup()

# pivot_wider
df_wider <- df_long |> pivot_wider(id_cols = album_name,names_from = release_year,values_from = n)


# pivot_longer
df_longer_again <- df_wider |>  
  pivot_longer(
    cols = c(`2014`, `2021`, `2019`, `2022`, `2010`, `2006`, `2020`, `2017`),
    names_to = "year",
    values_to = "n"
  ) |> 
  filter(!is.na(n))


#---- Agrupamento e Sumarização com dplyr

# group_by, ungroup, summarise.
df_group <- df |>  
  group_by(album_name) |> 
  summarise(
    danceability_avg = mean(danceability, na.rm = TRUE),
    danceability_median = median(danceability, na.rm = TRUE),
    danceability_sd = sd(danceability, na.rm = TRUE),
    danceability_min = min(danceability, na.rm = TRUE),
    danceability_max = max(danceability, na.rm = TRUE)
  ) |>
  ungroup()


class(df_group)

#---- Visualização com ggplot2 e plotly
plot <- df |> ggplot(aes(x = album_name, y = danceability, fill = album_name)) +
  geom_boxplot() + 
  CSGo::theme_csgo() 

ggplotly(plot)




