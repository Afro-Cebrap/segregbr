library(censobr)
library(dplyr)
library(stringr)
library(geobr)
library(sf)

prepare_data <- function(state_br, tracts_br, year) {
  
  muni_state <- read_municipality(state_br, year = year, simplified = TRUE) %>%
    st_drop_geometry() %>%
    select(code_muni) %>%
    mutate(code_muni = as.character(code_muni))
  
  # choosing variables based on var_name in each year
  if(year == 2010){
    tracts_state <- tracts_br %>%
      filter(code_muni %in% muni_state$code_muni) %>%
      select(
        code_muni = code_muni,
        code_tract = code_tract,
        branca = pessoa03_V002,
        preta  = pessoa03_V003,
        amarela  = pessoa03_V004,
        parda  = pessoa03_V005,
        indigena = pessoa03_V006
      )
  }
  if(year == 2022){
    tracts_state <- tracts_br %>%
      filter(code_muni %in% muni_state$code_muni) %>%
      select(
        code_muni = code_muni,
        code_tract = code_tract,
        branca = raca_V01317,
        preta  = raca_V01318,
        amarela  = raca_V01319,
        parda  = raca_V01320,
        indigena = raca_V01321
      )
  }
  
  tracts_state <- tracts_state %>%
    mutate(
      across(where(is.numeric), ~coalesce(.x, 0)),
      code_muni = as.character(code_muni),
      code_tract = as.character(code_tract),
      tract_total = branca + preta + parda,
      pop_total   = branca + preta + parda + amarela + indigena # full population
    ) %>%
    filter(pop_total > 0) # keep every inhabited tract, not only those with B/P/Pa
  
  metro_state <- read_metro_area(year) %>%
    st_drop_geometry() %>%
    filter(abbrev_state == state_br) %>%
    # drop RIDE areas
    filter(
      !str_detect(name_metro, regex("\\bride\\b", ignore_case = TRUE)),
      !str_detect(type, regex("^(raide|ride)", ignore_case = TRUE))
    ) %>% 
    select(code_muni, name_metro) %>%
    mutate(code_muni = as.character(code_muni))
  
  # harmonizing RM names for 2022
  if(year == 2022){
    metro_state <- metro_state %>% 
      mutate(
        name_metro = str_replace(name_metro, "^Recorte Metropolitano d[eoa]\\s+", "RM ")
      )
  }
  
  tracts_state <- tracts_state %>%
    left_join(metro_state, by = "code_muni")
  
  tracts_all <- bind_rows(
    tracts_state %>%
      mutate(unit_id = code_muni, unit_type = "muni"),
    tracts_state %>%
      filter(!is.na(name_metro)) %>%
      mutate(unit_id = name_metro, unit_type = "metro")
  )
  
  #Tm (Cidade/RM Level)
  tm_df <- tracts_all %>%
    group_by(unit_id, unit_type) %>%
    summarise(
      unit_total = sum(tract_total),
      branca_total = sum(branca),
      preta_total  = sum(preta),
      parda_total  = sum(parda),
      amarela_total  = sum(amarela),
      indigena_total = sum(indigena)
    ) %>%
    mutate(
      tm_branca = branca_total / unit_total,
      tm_preta  = preta_total  / unit_total,
      tm_parda  = parda_total  / unit_total
    )
  
  #Tjm tract level
  tracts_segreg <- tracts_all %>%
    mutate(
      tjm_branca = branca / tract_total,
      tjm_preta  = preta  / tract_total,
      tjm_parda  = parda  / tract_total
    ) %>%
    left_join(tm_df, by = c("unit_id", "unit_type"))
  
  return(tracts_segreg)
}