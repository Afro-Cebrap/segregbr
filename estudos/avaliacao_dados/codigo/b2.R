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
source(here::here("estudos","avaliacao_dados","codigo","utils_analysis.R"))

# 1. Inputs ---------------------------------------------------------------

## Parameters
lista_estados <- c("AC", "AL", "AP", "AM", "BA", "CE", "DF", "ES", "GO", 
                   "MA", "MT", "MS", "MG", "PA", "PB", "PR", "PE", "PI", 
                   "RJ", "RN", "RS", "RO", "RR", "SC", "SP", "SE", "TO")


# Get data to be evaluated

df_segreg <- sfarrow::st_read_parquet(
  here::here("outputs","sf_segregation_indices_all_years.parquet")
)

# Filter only cases for municipalities to avoid duplicates

df_mun <- df_segreg |> 
  st_drop_geometry() |> 
  filter(is.na(unit_type) | unit_type == "muni")

# 1. Correlação entre indicadores -----------------------------------------
df_mun |> colnames()

# dissimilaridade X entropia

t1 <- df_mun |> 
  filter(!is.na(dissimilarity), !is.na(index_h)) |> 
  summarise(
    name_state = "Total",
    cor = cor(dissimilarity,index_h),
    .by = year
  ) |> 
  bind_rows(
    df_mun |> 
      filter(!is.na(dissimilarity), !is.na(index_h)) |> 
      summarise(
        cor = cor(dissimilarity,index_h),
        .by = c(year, name_state)
      )
  )

t1 |> 
  pivot_wider(names_from = year, values_from = cor, names_prefix = "ano_") |> 
  ggplot() +
  aes(x = ano_2010, ano_2022) +
  geom_point(alpha = .4)

# isolamento brancos X pretos e pardos

t2 <- df_mun |> 
  filter(!is.na(iso_branca_branca), !is.na(iso_pp_pp)) |> 
  summarise(
    name_state = "Total",
    cor = cor(iso_branca_branca,iso_pp_pp),
    .by = year
  ) |> 
  bind_rows(
    df_mun |> 
      filter(!is.na(iso_branca_branca), !is.na(iso_pp_pp)) |> 
      summarise(
        cor = cor(iso_branca_branca,iso_pp_pp),
        .by = c(year, name_state)
      )
  )

# exposicao brancos e pp X pp e branco

t3 <- df_mun |> 
  filter(!is.na(exp_branca_pp), !is.na(exp_pp_branca)) |> 
  summarise(
    name_state = "Total",
    cor = cor(exp_branca_pp,exp_pp_branca),
    .by = year
  ) |> 
  bind_rows(
    df_mun |> 
      filter(!is.na(exp_branca_pp), !is.na(exp_pp_branca)) |> 
      summarise(
        cor = cor(exp_branca_pp,exp_pp_branca),
        .by = c(year, name_state)
      )
  )

# Exporting outputs -------------------------------------------------------


