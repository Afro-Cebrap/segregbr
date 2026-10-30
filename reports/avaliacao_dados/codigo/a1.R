# 0. Setup ----------------------------------------------------------------

# Options
options(scipen = 999) # Disable scientific notation for numbers

# libraries
library(here) # For file path management
library(fs) # For file system operations
library(censobr) # For accessing Brazilian census data
library(tidyverse) # For data manipulation and visualization
library(tidylog) # For logging tidyverse operations
library(geobr) # For accessing Brazilian geographic data
library(arrow) # For reading and writing data in Parquet format
library(sfarrow) # For reading and writing spatial data in Parquet format

# Custom functions to calculate segregation indices
source(here::here("reports","avaliacao_dados","codigo","utils_analysis.R"))

# 1. Inputs ---------------------------------------------------------------

## Parameters
lista_estados <- c("AC", "AL", "AP", "AM", "BA", "CE", "DF", "ES", "GO", 
                   "MA", "MT", "MS", "MG", "PA", "PB", "PR", "PE", "PI", 
                   "RJ", "RN", "RS", "RO", "RR", "SC", "SP", "SE", "TO")


# Get data to be evaluated

df_segreg <- sfarrow::st_read_parquet(
  here::here("outputs","sf_segregation_indices_all_years.parquet")
)

# 1. Coverage of census tracts with no valid information on population -----

t1 <- df_segreg |> 
  st_drop_geometry() |> 
  filter(is.na(unit_type) | unit_type == "muni") |> 
  mutate(valid_tract = if_else(is.na(unit_type),"NotValid","Valid")) |> 
  summarise(
    n = n_distinct(code_tract),
    .by = c(year, valid_tract)
  ) |> 
  pivot_wider(names_from = valid_tract, values_from = n) |> 
  mutate(uf = "Brasil",
         Total = Valid + NotValid,
         prop_NotValid = NotValid / Total * 100) |> 
  select(year, uf, NotValid, Total, prop_NotValid)
  
# adding disaggregation by UF

t1 <- t1 |> 
  bind_rows(
    df_segreg |> 
      st_drop_geometry() |> 
      filter(is.na(unit_type) | unit_type == "muni") |> 
      mutate(valid_tract = if_else(is.na(unit_type),"NotValid","Valid")) |> 
      summarise(
        n = n_distinct(code_tract),
        .by = c(year, name_state, valid_tract)
      ) |> 
      pivot_wider(names_from = valid_tract, values_from = n) |> 
      mutate(uf = name_state,
             Total = Valid + NotValid,
             prop_NotValid = NotValid / Total * 100) |> 
      select(year, uf, NotValid, Total, prop_NotValid)
  ) |> 
  arrange(year, desc(prop_NotValid))

# data for map plot

t1_map <- df_segreg |> 
  filter(is.na(unit_type) | unit_type == "muni") |> 
  mutate(valid_tract = if_else(is.na(unit_type),"NotValid","Valid")) |> 
  select(year, code_tract, zone, code_state, abbrev_state,name_state,valid_tract)

# table with assessment indicators

t1_eval <- df_segreg |> 
  st_drop_geometry() |> 
  filter(is.na(unit_type) | unit_type == "muni") |> 
  mutate(valid_tract = if_else(is.na(unit_type),"NotValid","Valid")) |> 
  select(year, code_muni, name_metro, code_tract, zone, code_state, abbrev_state,name_state, valid_tract)

# 2. Missing information on population size by race -----------------------
# valid census tract


t2 <- df_segreg |>
  st_drop_geometry() |> 
  filter(unit_type == "muni") |> 
  summarise(
    branca = n_distinct(code_tract[n_branca == 0]),
    parda = n_distinct(code_tract[n_parda == 0]),
    preta = n_distinct(code_tract[n_preta == 0]),
    indigena = n_distinct(code_tract[n_indigena == 0]),
    amarela = n_distinct(code_tract[n_amarela == 0]),
    # total_tracts = n_distinct(code_tract),
    .by = c(year)
  ) |> 
  pivot_longer(branca:amarela,
               names_to = "raca",
               values_to = "n") |> 
  left_join(
    df_segreg |>
      st_drop_geometry() |> 
      filter(unit_type == "muni") |> 
      summarise(
        branca = n_distinct(code_tract[n_branca == 0]),
        parda = n_distinct(code_tract[n_parda == 0]),
        preta = n_distinct(code_tract[n_preta == 0]),
        indigena = n_distinct(code_tract[n_indigena == 0]),
        amarela = n_distinct(code_tract[n_amarela == 0]),
        total_tracts = n_distinct(code_tract),
        .by = c(year)
      ) |> 
      mutate(
        branca = branca / total_tracts * 100,
        parda = parda / total_tracts * 100,
        preta = preta / total_tracts * 100,
        indigena = indigena / total_tracts * 100,
        amarela = amarela / total_tracts * 100
      ) |> 
      select(-total_tracts) |> 
      pivot_longer(branca:amarela,
                   names_to = "raca",
                   values_to = "prop"),
    by = join_by(year, raca)
  )

# redo analysis for urban census tract to check whether the issue attenuates

t2_urb <- df_segreg |>
  st_drop_geometry() |> 
  filter(unit_type == "muni", zone %in% c("URBANO","Urbana")) |> 
  summarise(
    branca = n_distinct(code_tract[n_branca == 0]),
    parda = n_distinct(code_tract[n_parda == 0]),
    preta = n_distinct(code_tract[n_preta == 0]),
    indigena = n_distinct(code_tract[n_indigena == 0]),
    amarela = n_distinct(code_tract[n_amarela == 0]),
    # total_tracts = n_distinct(code_tract),
    .by = c(year)
  ) |> 
  pivot_longer(branca:amarela,
               names_to = "raca",
               values_to = "n_urb") |> 
  left_join(
    df_segreg |>
      st_drop_geometry() |> 
      filter(unit_type == "muni", zone %in% c("URBANO","Urbana")) |> 
      summarise(
        branca = n_distinct(code_tract[n_branca == 0]),
        parda = n_distinct(code_tract[n_parda == 0]),
        preta = n_distinct(code_tract[n_preta == 0]),
        indigena = n_distinct(code_tract[n_indigena == 0]),
        amarela = n_distinct(code_tract[n_amarela == 0]),
        total_tracts = n_distinct(code_tract),
        .by = c(year)
      ) |> 
      mutate(
        branca = branca / total_tracts * 100,
        parda = parda / total_tracts * 100,
        preta = preta / total_tracts * 100,
        indigena = indigena / total_tracts * 100,
        amarela = amarela / total_tracts * 100
      ) |> 
      select(-total_tracts) |> 
      pivot_longer(branca:amarela,
                   names_to = "raca",
                   values_to = "prop_urb"),
    by = join_by(year, raca)
  )

t2 <- t2 |> 
  left_join(
    t2_urb,
    by = join_by(year, raca)
  ) |> 
  mutate(
    ratio_urb_total = n_urb / n * 100
  )

rm(t2_urb)

## disaggregation by UF

t2_uf <- df_segreg |>
  st_drop_geometry() |> 
  filter(unit_type == "muni") |> 
  summarise(
    branca = n_distinct(code_tract[n_branca == 0]),
    parda = n_distinct(code_tract[n_parda == 0]),
    preta = n_distinct(code_tract[n_preta == 0]),
    indigena = n_distinct(code_tract[n_indigena == 0]),
    amarela = n_distinct(code_tract[n_amarela == 0]),
    # total_tracts = n_distinct(code_tract),
    .by = c(year, name_state)
  ) |> 
  pivot_longer(branca:amarela,
               names_to = "raca",
               values_to = "n") |> 
  left_join(
    df_segreg |>
      st_drop_geometry() |> 
      filter(unit_type == "muni") |> 
      summarise(
        branca = n_distinct(code_tract[n_branca == 0]),
        parda = n_distinct(code_tract[n_parda == 0]),
        preta = n_distinct(code_tract[n_preta == 0]),
        indigena = n_distinct(code_tract[n_indigena == 0]),
        amarela = n_distinct(code_tract[n_amarela == 0]),
        total_tracts = n_distinct(code_tract),
        .by = c(year, name_state)
      ) |> 
      mutate(
        branca = branca / total_tracts * 100,
        parda = parda / total_tracts * 100,
        preta = preta / total_tracts * 100,
        indigena = indigena / total_tracts * 100,
        amarela = amarela / total_tracts * 100
      ) |> 
      select(-total_tracts) |> 
      pivot_longer(branca:amarela,
                   names_to = "raca",
                   values_to = "prop"),
    by = join_by(year, name_state, raca)
  )

# redo analysis for urban census tract to check whether the issue attenuates

t2_uf_urb <- df_segreg |>
  st_drop_geometry() |> 
  filter(unit_type == "muni", zone %in% c("URBANO","Urbana")) |> 
  summarise(
    branca = n_distinct(code_tract[n_branca == 0]),
    parda = n_distinct(code_tract[n_parda == 0]),
    preta = n_distinct(code_tract[n_preta == 0]),
    indigena = n_distinct(code_tract[n_indigena == 0]),
    amarela = n_distinct(code_tract[n_amarela == 0]),
    # total_tracts = n_distinct(code_tract),
    .by = c(year, name_state)
  ) |> 
  pivot_longer(branca:amarela,
               names_to = "raca",
               values_to = "n_urb") |> 
  left_join(
    df_segreg |>
      st_drop_geometry() |> 
      filter(unit_type == "muni", zone %in% c("URBANO","Urbana")) |> 
      summarise(
        branca = n_distinct(code_tract[n_branca == 0]),
        parda = n_distinct(code_tract[n_parda == 0]),
        preta = n_distinct(code_tract[n_preta == 0]),
        indigena = n_distinct(code_tract[n_indigena == 0]),
        amarela = n_distinct(code_tract[n_amarela == 0]),
        total_tracts = n_distinct(code_tract),
        .by = c(year, name_state)
      ) |> 
      mutate(
        branca = branca / total_tracts * 100,
        parda = parda / total_tracts * 100,
        preta = preta / total_tracts * 100,
        indigena = indigena / total_tracts * 100,
        amarela = amarela / total_tracts * 100
      ) |> 
      select(-total_tracts) |> 
      pivot_longer(branca:amarela,
                   names_to = "raca",
                   values_to = "prop_urb"),
    by = join_by(year, name_state, raca)
  )

t2_uf <- t2_uf |> 
  left_join(
    t2_uf_urb,
    by = join_by(year, name_state, raca)
  ) |> 
  mutate(
    ratio_urb_total = n_urb / n * 100
  ) |> 
  mutate(ratio_urb_total = replace_na(ratio_urb_total, 0)) |> 
  arrange(year, desc(ratio_urb_total))

rm(t2_uf_urb)

# data for map with each missing information

t2_map <- df_segreg |>
  filter(unit_type == "muni") |> 
  mutate(
    missing_branca = if_else(n_branca == 0,1,0),
    missing_parda = if_else(n_parda == 0,1,0),
    missing_preta = if_else(n_preta == 0,1,0),
    missing_indigena = if_else(n_indigena == 0,1,0),
    missing_amarela = if_else(n_amarela == 0,1,0)
  ) |> 
  select(year, code_tract, zone, code_state, abbrev_state,name_state, starts_with("missing_"))

# data for assessing estimates

t2_eval <- df_segreg |>
  st_drop_geometry() |> 
  filter(unit_type == "muni") |> 
  mutate(
    missing_branca = if_else(n_branca == 0,1,0),
    missing_parda = if_else(n_parda == 0,1,0),
    missing_preta = if_else(n_preta == 0,1,0),
    missing_indigena = if_else(n_indigena == 0,1,0),
    missing_amarela = if_else(n_amarela == 0,1,0)
  ) |> 
  select(year, code_muni, name_metro, code_tract, zone, code_state, abbrev_state,name_state, starts_with("missing_"))


# 3. Population size by tract for the subgroup with smaller size ---------
# This indicators gives to us a sense of which subgroup is the one with smaller pop. size
# and how small it is.
# Methodological decision: exclude groups with no information, once they are captured in the
# indicator A.1.2
# MAP: bivariate map with the total population size for the tract and the proportion of the smallest group
racial_groups = c("n_branca","n_preta","n_parda","n_amarela","n_indigena")

t3_df <- df_segreg |> 
  st_drop_geometry() |> 
  filter(unit_type == "muni") |> 
  # attributing high values for zero population subgroups (artifact strategy to avoid them to be included)
  mutate(across(c(n_branca:n_indigena), na_if, 0)) |> 
  mutate(minval = pmap(across(one_of(racial_groups)), ~min(c(...), na.rm = TRUE)),
         minval = as.numeric(minval)) |> 
  # link minimum value to the group
  mutate(minval_label = case_when(minval == n_branca ~ "Branca",
                                  minval == n_preta ~ "Preta",
                                  minval == n_parda ~ "Parda",
                                  minval == n_amarela ~ "Amarela",
                                  minval == n_indigena ~ "Indígena"),
         prop_minval = minval / n_total * 100)

# numero total por uf

t3_uf <- t3_df |> 
  summarise(
    n = n_distinct(code_tract),
    .by = c(year, name_state, minval_label)
  ) |> 
  mutate(
    perc = n / sum(n),
    .by = c(year, name_state)
  )

# data for map

t3_map <- df_segreg |> 
  filter(unit_type == "muni") |> 
  # attributing high values for zero population subgroups (artifact strategy to avoid them to be included)
  mutate(across(c(n_branca:n_indigena), na_if, 0)) |> 
  mutate(minval = pmap(across(one_of(racial_groups)), ~min(c(...), na.rm = TRUE)),
         minval = as.numeric(minval)) |> 
  # link minimum value to the group
  mutate(minval_label = case_when(minval == n_branca ~ "Branca",
                                  minval == n_preta ~ "Preta",
                                  minval == n_parda ~ "Parda",
                                  minval == n_amarela ~ "Amarela",
                                  minval == n_indigena ~ "Indígena"),
         minval_prop = minval / n_total * 100) |> 
  select(year, code_tract, zone, code_state, abbrev_state,name_state, starts_with("minval_"))

# data for assessment indicators

t3_eval <- df_segreg |> 
  st_drop_geometry() |> 
  filter(unit_type == "muni") |> 
  # attributing high values for zero population subgroups (artifact strategy to avoid them to be included)
  mutate(across(c(n_branca:n_indigena), na_if, 0)) |> 
  mutate(minval = pmap(across(one_of(racial_groups)), ~min(c(...), na.rm = TRUE)),
         minval = as.numeric(minval)) |> 
  # link minimum value to the group
  mutate(minval_label = case_when(minval == n_branca ~ "Branca",
                                  minval == n_preta ~ "Preta",
                                  minval == n_parda ~ "Parda",
                                  minval == n_amarela ~ "Amarela",
                                  minval == n_indigena ~ "Indígena"),
         minval_prop = minval / n_total * 100) |> 
  select(year, code_muni, name_metro, code_tract, zone, code_state, abbrev_state,name_state, starts_with("minval_"))

rm(t3_df)

# 4. Heterogeneity of racial groups ---------------------------------------
# To what extent there is a racial heterogeneity in each tract?
# What we capture in our segregation index is a result of whether this groups interacts or not,
# but they cannot interact not because there is a lower proportion of them, but because there is
# not this group in some tracts.
# MAP: univariate map with 1,2,3,4 or 5 as categoric variables.

racial_groups = c("n_branca","n_preta","n_parda","n_amarela","n_indigena")

t4_df <- df_segreg |> 
  st_drop_geometry() |> 
  filter(unit_type == "muni") |> 
  # attributing high values for zero population subgroups (artifact strategy to avoid them to be included)
  mutate(
    tem_branca = if_else(n_branca == 0, 0,1),
    tem_preta = if_else(n_preta == 0, 0,1),
    tem_parda = if_else(n_parda == 0, 0,1),
    tem_amarela = if_else(n_amarela == 0, 0,1),
    tem_indigena = if_else(n_indigena == 0, 0,1),
    heterogeneity_index = tem_branca + tem_preta + tem_parda + tem_amarela + tem_indigena
  )

# numero total por uf

t4_uf <- t4_df |> 
  summarise(
    media = mean(heterogeneity_index),
    erro_padrao = sd(heterogeneity_index),
    cv = erro_padrao / media,
    .by = c(year, name_state)
  )

# data for map

t4_map <- df_segreg |> 
  filter(unit_type == "muni") |> 
  # attributing high values for zero population subgroups (artifact strategy to avoid them to be included)
  mutate(
    tem_branca = if_else(n_branca == 0, 0,1),
    tem_preta = if_else(n_preta == 0, 0,1),
    tem_parda = if_else(n_parda == 0, 0,1),
    tem_amarela = if_else(n_amarela == 0, 0,1),
    tem_indigena = if_else(n_indigena == 0, 0,1),
    heterogeneity_index = tem_branca + tem_preta + tem_parda + tem_amarela + tem_indigena
  ) |> 
  select(year, code_tract, zone, code_state, abbrev_state,name_state, heterogeneity_index)

# data for assessment groups

t4_eval <- df_segreg |> 
  st_drop_geometry() |> 
  filter(unit_type == "muni") |> 
  # attributing high values for zero population subgroups (artifact strategy to avoid them to be included)
  mutate(
    tem_branca = if_else(n_branca == 0, 0,1),
    tem_preta = if_else(n_preta == 0, 0,1),
    tem_parda = if_else(n_parda == 0, 0,1),
    tem_amarela = if_else(n_amarela == 0, 0,1),
    tem_indigena = if_else(n_indigena == 0, 0,1),
    heterogeneity_index = tem_branca + tem_preta + tem_parda + tem_amarela + tem_indigena
  ) |> 
  select(year, code_muni, name_metro, code_tract, zone, code_state, abbrev_state,name_state, heterogeneity_index)

rm(t4_df)
  
# Exporting outputs -------------------------------------------------------

# output of evaluation indicators
t_eval <- t1_eval |> 
  left_join(
    t2_eval,
    by = join_by(year, code_muni, name_metro, code_tract, zone, code_state, abbrev_state,name_state)
  ) |> 
  left_join(
    t3_eval,
    by = join_by(year, code_muni, name_metro, code_tract, zone, code_state, abbrev_state,name_state)
  ) |>
  left_join(
    t4_eval,
    by = join_by(year, code_muni, name_metro, code_tract, zone, code_state, abbrev_state,name_state)
  )

# export table with indicators
write_parquet(t_eval,here::here("reports","avaliacao_dados","A. Validade Interna","indicadores_por_muni.parquet"))
