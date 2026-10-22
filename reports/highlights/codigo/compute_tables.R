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
library(sf) # For working with sf objects

# Custom functions to calculate segregation indices
source(here::here("estudos","avaliacao_dados","codigo","utils_analysis.R"))

# 1. Inputs ---------------------------------------------------------------

## Parameters
lista_estados <- c("AC", "AL", "AP", "AM", "BA", "CE", "DF", "ES", "GO", 
                   "MA", "MT", "MS", "MG", "PA", "PB", "PR", "PE", "PI", 
                   "RJ", "RN", "RS", "RO", "RR", "SC", "SP", "SE", "TO")

# Get data to be evaluated

segreg <- sfarrow::st_read_parquet(
  here::here("outputs","sf_segregation_indices_all_years.parquet")
)

# municipalities

segreg_munic <- segreg |> 
  filter(unit_type == "muni")

# metropolitan regions

segreg_metro <- segreg |> 
  filter(unit_type == "metro")

# auxiliar table for name of states and regions by metropolitan areas and municipalities

aux_metro <- segreg_metro |> 
  st_drop_geometry() |> 
  filter(year == 2022, code_tract != "Total") |> 
  select(name_region, name_state, name_metro) |> distinct()

aux_munic <- segreg_munic |> 
  st_drop_geometry() |> 
  filter(year == 2022, code_tract != "Total") |> 
  select(name_region, name_state, name_muni) |> distinct()

# 1. Metropolitan regions ----------------------------------------------------

# N mais segregadas RMs

t1 <- segreg_metro |> 
  st_drop_geometry() |> 
  filter(code_tract == "Total", year == 2022) |> 
  select(name_metro, dissimilarity) |> 
  left_join(
    aux_metro,
    by = join_by(name_metro)
  ) |> 
  select(name_region, name_state, name_metro, dissimilarity)


# 2. Municipalities -------------------------------------------------------

# 2.1 - Municipalities most and lest segregated

t2_1 <- segreg_munic |> 
  st_drop_geometry() |> 
  filter(code_tract == "Total", year == 2022) |> 
  select(name_region, name_state, name_muni, dissimilarity)
