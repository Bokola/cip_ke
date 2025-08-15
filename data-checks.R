# browseURL("https://bookdown.org/Maxine/r4ds/comparing-two-data-frames-tibbles.html")
# browseURL("https://stackoverflow.com/questions/51120287/reading-multiple-xlsx-files-each-with-multiple-sheets-purrr")
# regular expressions pattern matching
# browseURL("https://stackoverflow.com/questions/40142682/remove-everything-after-last-space-with-stringr")
# browseURL("https://stackoverflow.com/questions/24465512/regular-expression-to-match-everything-until-the-last-occurrence-of")
# browseURL("https://regex101.com/r/pI4lR5/33")
# browseURL("https://stackoverflow.com/questions/64679380/combine-dataframes-in-two-different-lists-keyed-on-the-element-name-in-r")
# comparedf(out_list[[1]], out_list[[2]])

if(!require(pbwrangler)) devtools::install_github("Bokola/pbwrangler")

library(purrr)
library(ids)
library(pbwrangler)
library(readxl)
library(writexl)
library(dplyr)
library(readr)
library(tidyverse)
library(logr)
library(st4gi)


# log runtime -------------------------------------------------------------

Sys.setenv("LOG_LEVEL" = "ERROR")


# Open the log
log_open("session.log")


# delete all after last underscore

sub("_[^_]+$", "","CIP318107_2_1")

# delete all before last underscore

sub(".*_", '', "CIP318107_2_1")

# delete all after first underscore

sub("_.*", "", "CIP318107_2_1")



# season 2024-2025 --------------------------------------------------------

meta_24_25 <- read_excel(
  list.files(file.path(t_dir, "Season 2024-2025", "Data", "meta-data"),
             full.names = TRUE)[1]
)

meta_24_25_lst <- split(meta_24_25, meta_24_25$trial_name)

season_24_25_a <- pbwrangler::read_workbooks(
  dir = t_dir,
  season = "Season 2024-2025",
  read_subfolder = TRUE,
  sub_folder = "Received data",
  subset = TRUE,
  n = c(1, 4:9, 12),
  # n = 12,
  multiple = TRUE,
  merge = TRUE
)

season_24_25_b <- pbwrangler::read_workbooks(
  dir = t_dir,
  season = "Season 2024-2025",
  read_subfolder = TRUE,
  sub_folder = "Received data",
  subset = TRUE,
  n = c(2, 10:11),
  # n = 3,
  multiple = FALSE,
  merge = FALSE
)

season_24_25 <- c(season_24_25_a, season_24_25_b)


# calculated 

season_24_25_c<- pbwrangler::read_workbooks(
  dir = t_dir,
  season = "Season 2024-2025",
  read_subfolder = TRUE,
  sub_folder = "Received data/SG-calculated",
  subset = FALSE,
  # n = c(2, 10:11),
  # n = 3,
  multiple = FALSE,
  merge = FALSE
)




# join meta data and data

season_24_25 <- join_data_meta(data_lst = season_24_25, meta_lst = meta_24_25_lst)


season_24_25_out <- pre_process_trials(season_24_25) %>% 
  process_trials()


write_season_data(season_24_25_out, season = "Season 2024-2025")


# season 2024 --------------------------------------------------------
season  = "Season 2024"
meta <- read_excel(
  list.files(file.path(t_dir, season, "Data", "meta-data"),
             full.names = TRUE)[1]
)

meta_lst <- split(meta, meta$trial_name)

season_a <- pbwrangler::read_workbooks(
  dir = t_dir,
  season = season,
  read_subfolder = TRUE,
  sub_folder = "Received data",
  subset = TRUE,
  n = c(4:9),
  # n = 6,
  multiple = FALSE,
  merge = FALSE
)




# join meta data and data

season_24<- join_data_meta(data_lst = season_a, meta_lst = meta_lst)


season_24_out <- pre_process_trials(season_24) %>% 
  process_trials()


write_season_data(season_24_out, season = season)



# season 2025 -------------------------------------------------------------

season  = "Season 2025"
meta <- read_excel(
  list.files(file.path(t_dir, season, "Data", "meta-data"),
             full.names = TRUE)[1]
)

meta_lst <- split(meta, meta$trial_name)

season_a <- pbwrangler::read_workbooks(
  dir = t_dir,
  season = season,
  read_subfolder = TRUE,
  sub_folder = "Received data",
  subset = FALSE,
  # n = c(4:9),
  # n = 6,
  multiple = FALSE,
  merge = FALSE,
  update_plot = FALSE
)

names(season_a) <- sub("_[^_]+$", "", names(season_a))


# join meta data and data

season_out<- join_data_meta(data_lst = season_a, meta_lst = meta_lst)


season_out <- pre_process_trials(season_out) %>% 
  process_trials()


write_season_data(season_out, season = season)


log_close()

