if(!require(pbwrangler)) devtools::install_github("Bokola/pbwrangler")

library(purrr)
library(ids)
library(pbwrangler)
library(readxl)
library(writexl)
library(dplyr)
library(readr)
library(tidyverse)

set.seed(as.numeric(today()))

season <- "Season 2024-2025"
season_to_rand <- "Season 2025"
sub_dir <- "Seed inventory"
pat <- "tuber seed"
sheet <- "seed inventory"


# get seeds and design data
f <- list.files(file.path(t_dir, season, sub_dir),
                pattern = pat, full.names = TRUE)
seed <- read_excel(f, sheet = sheet)

# EA21 & BIO-fortified clones

bio <- read_excel(f, sheet = "BIO_Clones")
ea21 <- read_excel(f, sheet = "EA21_Clones")

bw <- read_excel(f, sheet = "BW")[,1:2] %>% filter(n_tubers >= 30)
# 8 clones 
bw_60 <- bw %>% filter(n_tubers >= 60)

df_design <- read_excel(f, sheet = "design", skip = 24) %>% 
  filter(!is.na(trial)) %>% filter(!is.na(design)) #%>%
  # filter(grepl("UON-EA21-AT", trial))

# get trials already randomized

randomized <- list.files(
  file.path(t_dir, season_to_rand, "FieldBook"), pattern = ".xlsx"
) %>% clean_dir_name_c()

df_design <- df_design %>% filter(trial %nin% randomized)
# df_design <- df_design %>% filter(grepl("HERL", trial))

seed_to_rand <- seed %>% filter(grepl("^CIP", geno))
seed_lst <- split(seed_to_rand, seed_to_rand$loc)


# HERL group: UON, MOL, NJO -----------------------------------------------



trials_herl <- c("KE25UON-HERL-IT01", "KE25MOL-HERL-IT01", "KE25NJO-HERL-IT01")
source_herl <- "KE24MOL-HERL-ST01"

# get the 69 to be replicated

dummy <- c("Shangi", "Unica")

uon_dum <- data.frame(geno = sample(dummy, 1))
mol_dem <- data.frame(geno = sample(dummy, 1))
njo_dum <- data.frame(geno = sample(dummy, 2))

df_herl <- seed_lst[[source_herl]] %>% select(geno)
uon_herl <- sample_n(df_herl, size = 69, replace = FALSE) 
mol_herl <- sample_n(
  setdiff(df_herl, uon_herl), size = 69, replace = FALSE
)
njo_herl <- setdiff(df_herl, union(uon_herl, mol_herl))

# add to the 207

uon_herl <- bind_rows(uon_herl, df_herl) %>% bind_rows(uon_dum)
mol_herl <- bind_rows(mol_herl, df_herl) %>% bind_rows(mol_dem)
njo_herl <- bind_rows(njo_herl, df_herl) %>% bind_rows(njo_dum)


# p-rep design

# prep_design <- df_design %>% filter(., grepl("prep", design))
prep_design <- read_excel(f, sheet = "prep")

herl_clone_lst <- list(uon_herl, mol_herl, njo_herl) %>%
  set_names(prep_design$trial) %>%
  map(., function(x) x %>% select(geno) %>% pull())

for(i in seq_len(nrow(prep_design))){
  d <- prep_design[i,]
  trtrepP <- rep(
    c(str_split(d$v_reps, pattern = ",", simplify = T)[1,] %>% as.numeric()),
    c(str_split(d$v_clones, pattern = ",", simplify = T)[1,] %>% as.numeric())
  )
  # trtgroup <- rep(c(1, 11), c(70, 4))
  trgrp <- list(
    c(str_split(d$t_grp, pattern = ",", simplify = T)[1,] %>% as.numeric()),
    c(str_split(d$t_reps, pattern = ",", simplify = T)[1,] %>% as.numeric())
  )
  trtgroup <- rep(
    trgrp[[1]],
    trgrp[[2]]
  )
  block_lst <- list(
    c(str_split(d$block_lst1, pattern = ",", simplify = T)[1,] %>% as.numeric()),
    c(str_split(d$block_lst2, pattern = ",", simplify = T)[1,] %>% as.numeric())
  )
  check <- str_split(d$check, pattern = ",", simplify = T)[1,] %>% 
    trimws()
  dummy <- str_split(d$dummy, pattern = ",", simplify = T)[1,] %>% 
    trimws()
  
  Prep <- rand_Prep(
    tot = d$tot,
    ins = herl_clone_lst[[i]],
    rowD = d$rowD,
    trial = d$trial,
    n_dummies = d$n_dummies,
    loc = d$trial,
    totReps = d$totReps,
    trtrepP = trtrepP,
    trtgroup = trtgroup,
    block_lst = block_lst,
    season = season_to_rand,
    path = t_dir,
    check = check,
    to_add = d$to_add
  )

}

# row-col design

rowcol_design <- df_design %>% filter(., grepl("rowcol", design)) %>% filter(
  rep > 1
)

for(i in seq_len(nrow(rowcol_design))){
  d <- rowcol_design[i,]
  if(grepl("^EA", d$source)){
    df_clones <- ea21 %>% select(geno)
  } else if(grepl("BIO", d$source)){
    df_clones <- bio %>% select(geno)
  } else if(grepl("\\bbw\\b", d$source)) {
    df_clones <- bw
  } else if(grepl("\\bbw_60\\b", d$source)){
    df_clones <- bw_60
  } else{
    df_clones <- seed_lst[[d$source]] %>% select(geno)
  }

  checks <- str_split(d$check, pattern = ",", simplify = T)[1,] %>% 
    trimws()
  dummy <-   check <- str_split(d$dummy, pattern = ",", simplify = T)[1,] %>% 
    trimws()
  
  rowcolD <-   randomize_row_col(
    clones = df_clones,
    trial = d$trial,
    tot = d$tot,
    rowD = d$rowD,
    # rowD = 6,
    rowsinR = d$rowsinR,
    # rowsinR = 6,
    colsinR = d$colsinR,
    # colsinR = 5,
    check = checks,
    dummy = dummy,
    n_dummies = d$n_dummies,
    to_add = d$to_add,
    rep = d$rep,
    path = t_dir,
    season = season_to_rand
  )

}

# multiplication (ST) trials: no rep

st_design <- df_design %>% filter(grepl("norep", design))
# st_toadd <- list(NULL, 4)
# st_rep <- list(0, 4)

st_toadd <- list(NULL)
st_rep <- list(0)
st_dummy <- c("Shangi", "Unica")


for(i in seq_len(nrow(st_design))){
  d <- st_design[i,]
  if(grepl("^EA", d$source)){
    df_clones <- ea21 %>% select(geno)
  }else if(grepl("BIO", d$source)){
    df_clones <- bio %>% select(geno)
  }else{
    df_clones <- seed_lst[[d$source]] %>% select(geno)
  }
  
  if(d$n_dummies > 0){
    df_clones <- bind_rows(df_clones, data.frame(geno = sample(st_dummy, d$n_dummies)))
  }
  
  check <- str_split(d$check, pattern = ",", simplify = T)[1,] %>% 
    trimws()
  
  norepD <-   randomize_noRep(
    ins = df_clones,
    trial = d$trial,
    tot = d$tot,
    rowD = d$rowD,
    check = check,
    to_add = st_toadd[[i]],
    rep = st_rep[[i]],
    path = t_dir,
    season = season_to_rand
  )
  
}
