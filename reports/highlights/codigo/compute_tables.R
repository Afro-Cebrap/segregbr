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
library(spdep) # For spatial analysis with neighbor matrix
library(rgeoda) # For spatial analysis, constructing neighbor matrix

# Custom auxiliary functions
source(here::here("reports","highlights","codigo","utils_analysis.R"))

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

aux_metro_complete <- segreg_metro |> 
  st_drop_geometry() |> 
  filter(code_tract != "Total") |> 
  select(name_region, name_state, name_metro) |> distinct()

aux_munic <- segreg_munic |> 
  st_drop_geometry() |> 
  filter(year == 2022, code_tract != "Total") |> 
  select(name_region, name_state, name_muni,name_metro, code_muni, n_total) |> distinct()

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
  select(name_region, name_state, name_metro, dissimilarity) |> 
  arrange(desc(dissimilarity))


# 2. Municipalities -------------------------------------------------------

###
# 2.1 - Municipalities most and lest segregated
###
# Note: excluding municipalities with less than 3 racial categories, with more than one census tract,
# and with low Variance Coefficient

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
  filter(code_muni %in% munic_keep$code_muni) |> 
  arrange(desc(dissimilarity))

####
# 2.2. Spatial correlation - LISA and Moran
####

# Creating dataset

df_spatial <- segreg_munic |> 
  filter(code_tract == "Total", year == 2022, code_muni %in% t2_1$code_muni) |> 
  select(name_region, name_state, code_muni, name_muni, dissimilarity)

# LISA

t2_2 <- compute_lisa(df_spatial)

# idea to plot map
t2_2 |> 
  ggplot() +
  geom_sf(aes(fill = LISA_map),
          lwd = 0) +
  geom_sf(fill = "transparent",
          colour = "grey90", size = 0.2) +
  scale_fill_manual(
    values = c(
      "Baixo-Baixo" = "#045a8d", "Alto-Baixo" = "#a6bddb","Baixo-Alto" = "#fc9272",
      "Alto-Alto" = "#a50f15","Não significativo" =  "#f0f0f0"
    )) +
  guides(fill = guide_legend(title = glue::glue("LISA Map"))) +
  labs(
    caption = glue::glue("Fonte: IBGE, Censo Demográfico 2022.")
  ) +
  # tira sistema cartesiano
  theme(
    plot.title = element_text(face = "bold", size = 12, hjust = .5, vjust = .5),
    plot.caption = element_blank(),
    legend.title = element_text(face = "bold", size = 12, hjust = 0, vjust = .5),
    legend.text = element_text(size = 12, hjust = 0, vjust = .9),
    legend.position = "right",
    axis.text = element_blank(),
    axis.ticks = element_blank(),
    panel.grid = element_line(color = "#ffffff",linewidth = .01),
    panel.background = element_blank())

####
# 2.3. Neighbors with the highest spatial segregation
####

# creating auxiliar for capitals

aux_capital <- aux_munic |> 
  reframe(
    n_total = sum(n_total),
    .by = c(name_state, code_muni)
  ) |> 
  arrange(name_state, desc(n_total)) |> 
  mutate(order = row_number(),
         .by = c(name_state)) |> 
  filter(order == 1) |> 
  select(-order, -n_total, -name_state)

# choosing capitals

capitals <- t2_1 |> 
  right_join(
    aux_capital,
    by = join_by(code_muni)
  )

# filter tracts from chosen municipalities

t2_3 <- segreg_munic |> 
  st_drop_geometry() |> 
  filter(year == 2022, code_tract != "Total", unit_type == "muni") |> 
  select(code_tract, code_muni, name_muni, name_state, name_region,name_neighborhood, dissimilarity) |> 
  right_join(
    capitals |> select("dissimilarity_muni" = dissimilarity, code_muni),
    by = join_by(code_muni)
  ) |> 
  filter(!is.na(name_neighborhood)) |> 
  arrange(code_muni, desc(dissimilarity)) |> 
  mutate(
    order = row_number(),
    .by = c(code_muni)
  ) |> 
  filter(order == 1) |> 
  select(-order) |> 
  arrange(desc(dissimilarity_muni))

rm(capitals,aux_capital)

# 3. Potential racial interaction -----------------------------------------

###
# 3.1 - Municipalities ranking by residential interaction between Negros x Brancos
###
# Note: excluding municipalities with less than 3 racial categories, with more than one census tract,
# with low Variance Coefficient, and with not a large share of census tract with missing by race group

t3_1 <- segreg_munic |> 
  st_drop_geometry() |> 
  filter(code_tract == "Total", year == 2022) |> 
  select(name_region, name_state, code_muni, name_muni, exp_pp_branca)

# excluding municipalities

munic_keep <- aux_evaluation |>
  filter(year == 2022, valid_tract == "Valid") |> 
  reframe(
    missing_branca = mean(missing_branca, na.rm = T),
    missing_preta = mean(missing_preta, na.rm = T),
    missing_parda = mean(missing_parda, na.rm = T),
    avg = mean(heterogeneity_index, na.rm = T),
    sd = sd(heterogeneity_index, na.rm = T),
    cv = sd / avg,
    .by = c(code_muni)
  )

munic_keep <- munic_keep |> 
  filter(sd != 0) |>
  filter(avg >= 3) |>
  filter(cv <= 0.3) |> 
  filter(missing_preta < .5)

t3_1 <- t3_1 |> 
  filter(code_muni %in% munic_keep$code_muni) |> 
  arrange(desc(exp_pp_branca))

###
# 3.2 - Municipalities ranking by residential interaction between Brancos x Negros
###
# Note: excluding municipalities with less than 3 racial categories, with more than one census tract,
# with low Variance Coefficient, and with not a large share of census tract with missing by race group

t3_2 <- segreg_munic |> 
  st_drop_geometry() |> 
  filter(code_tract == "Total", year == 2022) |> 
  select(name_region, name_state, code_muni, name_muni, exp_branca_pp)

# excluding municipalities

munic_keep <- aux_evaluation |>
  filter(year == 2022, valid_tract == "Valid") |> 
  reframe(
    missing_branca = mean(missing_branca, na.rm = T),
    missing_preta = mean(missing_preta, na.rm = T),
    missing_parda = mean(missing_parda, na.rm = T),
    avg = mean(heterogeneity_index, na.rm = T),
    sd = sd(heterogeneity_index, na.rm = T),
    cv = sd / avg,
    .by = c(code_muni)
  )

munic_keep <- munic_keep |> 
  filter(sd != 0) |>
  filter(avg >= 3) |>
  filter(cv <= 0.3) |> 
  filter(missing_preta < .5)

t3_2 <- t3_2 |> 
  filter(code_muni %in% munic_keep$code_muni) |> 
  arrange(desc(exp_branca_pp))

###
# Correlation between both indicators
###

cor(t3_1$exp_pp_branca, t3_2$exp_branca_pp)

###
# 3.3 - Municipalities ranking by residential isolation among Negro
###
# Note: excluding municipalities with less than 3 racial categories, with more than one census tract,
# with low Variance Coefficient, and with not a large share of census tract with missing by race group

t3_3 <- segreg_munic |> 
  st_drop_geometry() |> 
  filter(code_tract == "Total", year == 2022) |> 
  select(name_region, name_state, code_muni, name_muni, iso_pp_pp)

# excluding municipalities

munic_keep <- aux_evaluation |>
  filter(year == 2022, valid_tract == "Valid") |> 
  reframe(
    missing_branca = mean(missing_branca, na.rm = T),
    missing_preta = mean(missing_preta, na.rm = T),
    missing_parda = mean(missing_parda, na.rm = T),
    avg = mean(heterogeneity_index, na.rm = T),
    sd = sd(heterogeneity_index, na.rm = T),
    cv = sd / avg,
    .by = c(code_muni)
  )

munic_keep <- munic_keep |> 
  filter(sd != 0) |>
  filter(avg >= 3) |>
  filter(cv <= 0.3) |> 
  filter(missing_preta < .5)

t3_3 <- t3_3 |> 
  filter(code_muni %in% munic_keep$code_muni) |> 
  arrange(desc(iso_pp_pp))

###
# 3.4 - Municipalities ranking by residential isolation among Brancos
###
# Note: excluding municipalities with less than 3 racial categories, with more than one census tract,
# with low Variance Coefficient, and with not a large share of census tract with missing by race group

t3_4 <- segreg_munic |> 
  st_drop_geometry() |> 
  filter(code_tract == "Total", year == 2022) |> 
  select(name_region, name_state, code_muni, name_muni, iso_branca_branca)

# excluding municipalities

munic_keep <- aux_evaluation |>
  filter(year == 2022, valid_tract == "Valid") |> 
  reframe(
    missing_branca = mean(missing_branca, na.rm = T),
    missing_preta = mean(missing_preta, na.rm = T),
    missing_parda = mean(missing_parda, na.rm = T),
    avg = mean(heterogeneity_index, na.rm = T),
    sd = sd(heterogeneity_index, na.rm = T),
    cv = sd / avg,
    .by = c(code_muni)
  )

munic_keep <- munic_keep |> 
  filter(sd != 0) |>
  filter(avg >= 3) |>
  filter(cv <= 0.3) |> 
  filter(missing_preta < .5)

t3_4 <- t3_4 |> 
  filter(code_muni %in% munic_keep$code_muni) |> 
  arrange(desc(iso_branca_branca))

###
# Correlation between both indicators
###

cor(t3_3$iso_pp_pp, t3_4$iso_branca_branca) # iso's
cor(t3_1$exp_pp_branca, t3_3$iso_pp_pp)
cor(t3_2$exp_branca_pp, t3_4$iso_branca_branca)


# 4. Changes in the segregation over time ---------------------------------

###
# 4.1 - Ranking of segregation between years for each capital
###

# creating table with Entropy index
t4_1 <- segreg_munic |> 
  st_drop_geometry() |> 
  filter(code_tract == "Total") |> 
  select(year, name_region, name_state, code_muni, name_muni, index_h)

# capitals
aux_capital <- aux_munic |> 
  reframe(
    n_total = sum(n_total),
    .by = c(name_state, code_muni)
  ) |> 
  arrange(name_state, desc(n_total)) |> 
  mutate(order = row_number(),
         .by = c(name_state)) |> 
  filter(order == 1) |> 
  select(-order, -n_total, -name_state)

# choosing capitals

t4_1 <- t4_1 |> 
  right_join(
    aux_capital,
    by = join_by(code_muni)
  )
# create ranking
t4_1 <- t4_1 |> 
  arrange(year, desc(index_h)) |> 
  mutate(order = row_number(),
         .by = c(year))

# comparing years

t4_1 |> 
  filter(year == 2010) |> 
  select(name_region, name_state, name_muni, "H_2010" = index_h, "Ranking_2010" = order) |> 
  left_join(
    t4_1 |> 
      filter(year == 2022) |> 
      select(name_region, name_state, name_muni, "H_2022" = index_h, "Ranking_2022" = order),
    by = join_by(name_region, name_state, name_muni)
  )

###
# 4.2 - Ranking of segregation between years for each metropolitan region based on RM in 2010
###

# creating table with Entropy index
t4_2 <- segreg_metro |> 
  st_drop_geometry() |> 
  filter(code_tract == "Total") |> 
  select(year, name_metro, index_h) |> 
  left_join(
    aux_metro_complete,
    by = join_by(name_metro)
  ) |> 
  arrange(desc(index_h)) |> 
  select(year, name_region, name_state, name_metro, index_h) |> 
  mutate(name_metro = str_remove(name_metro, "^Rm |RM "))

# rms in both years
aux_rms <- 
  reframe(
    n_total = sum(n_total),
    .by = c(name_state, code_muni)
  ) |> 
  arrange(name_state, desc(n_total)) |> 
  mutate(order = row_number(),
         .by = c(name_state)) |> 
  filter(order == 1) |> 
  select(-order, -n_total, -name_state)

# choosing capitals

t4_1 <- t4_1 |> 
  right_join(
    aux_capital,
    by = join_by(code_muni)
  )

# create ranking for comparable RMs

t4_2 <- t4_2 |> 
  filter(year == 2010) |> 
  filter(name_metro %in% t4_2$name_metro[t4_2$year == 2022]) |> 
  select(year, name_region, name_state, name_metro, index_h) |> 
  bind_rows(
    t4_2 |> 
      filter(year == 2022) |> 
      filter(name_metro %in% t4_2$name_metro[t4_2$year == 2010]) |> 
      select(year, name_region, name_state, name_metro, index_h)
  ) |> 
  arrange(year, desc(index_h)) |> 
  mutate(order = row_number(),
         .by = year)

# compare rakings

t4_2 <- t4_2 |> 
  filter(year == 2010) |> 
  select(name_region, name_state, name_metro, "H_2010" = index_h, "Ranking_2010" = order) |> 
  left_join(
    t4_2 |> 
      filter(year == 2022) |> 
      select(name_region, name_state, name_metro, "H_2022" = index_h, "Ranking_2022" = order),
    by = join_by(name_region, name_state, name_metro)
  )
