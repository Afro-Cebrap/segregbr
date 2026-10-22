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
  select(name_region, name_state, name_muni, code_muni) |> distinct()

# data with auxiliar indicators from assessment

aux_evaluation <- read_parquet(
  here::here("reports","avaliacao_dados","A. Validade Interna","indicadores_por_muni.parquet")
)

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

###
# 2.1 - Municipalities most and lest segregated
###
# Note: excluding municipalities with less than 3 racial categories, with more than one census tract, and with low Variance Coeficient

t2_1 <- segreg_munic |> 
  st_drop_geometry() |> 
  filter(code_tract == "Total", year == 2022) |> 
  select(name_region, name_state, code_muni, name_muni, dissimilarity)

# excluding municipalities

munic_keep <- aux_evaluation |>
  filter(year == 2022) |> 
  reframe(
    avg = mean(heterogeneity_index, na.rm = T),
    sd = sd(heterogeneity_index, na.rm = T),
    cv = sd / avg,
    .by = c(code_muni)
  )

munic_keep <- munic_keep |> 
  filter(sd != 0) |>
  filter(avg >= 3) |>
  filter(cv <= 0.3)

t2_1 <- t2_1 |> 
  filter(code_muni %in% munic_keep$code_muni)
