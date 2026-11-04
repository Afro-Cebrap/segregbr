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
library(ggspatial) # elements for maps
library(ggrepel) # for adding labels to the map
library(glue)
library(forcats)

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

clipr::write_clip(t1)

# distribution of regions
t1 |> 
  summarise(
    n = n_distinct(name_metro),
    .by = name_region
  ) |> 
  mutate(perc = n / sum(n)*100)

# 20 most segregated RMs by region
t1 |> 
  mutate(order = row_number()) |> 
  filter(order <= 20) |> 
  summarise(
    n = n_distinct(name_metro),
    .by = name_region
  ) |> 
  mutate(perc = n / sum(n)*100)

# 20 most segregated RMs by states
t1 |> 
  mutate(order = row_number()) |> 
  filter(order <= 20) |> 
  summarise(
    n = n_distinct(name_metro),
    .by = name_state
  ) |> 
  mutate(perc = n / sum(n)*100)

# 20 less segregated RMs by region
t1 |> 
  mutate(order = row_number()) |> 
  filter(order >= 54) |> 
  summarise(
    n = n_distinct(name_metro),
    .by = name_region
  ) |> 
  mutate(perc = n / sum(n)*100)

segreg_metro |> 
  st_drop_geometry() |>
  filter(code_tract == "Total", year == 2022) |>
  filter(name_metro %in% t1$name_metro) |> 
  select(name_metro, percent_preta_ou_parda) |> 
  right_join(
    t1,
    by = join_by(name_metro)
  ) |> 
  ggplot() +
  aes(x = percent_preta_ou_parda, y = dissimilarity, color = name_region) +
  geom_point(alpha = .5, size = 5) +
  theme_minimal()

segreg_metro |> 
  st_drop_geometry() |>
  filter(code_tract == "Total", year == 2022) |>
  filter(name_metro %in% t1$name_metro) |> 
  select(name_metro, percent_preta_ou_parda) |> 
  right_join(
    t1,
    by = join_by(name_metro)
  ) |> 
  arrange(desc(dissimilarity))

clipr::write_last_clip()

# 2. Municipalities -------------------------------------------------------

###
# 2.1 - Municipalities most and lest segregated
###
# Note: excluding municipalities with less than 3 racial categories, with more than one census tract,
# with low Variance Coefficient, and low presence of Negros (< .1)

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
  # adding race distribution
  left_join(
    segreg_munic |> 
      st_drop_geometry() |>
      filter(code_tract == "Total", year == 2022) |>
      filter(code_muni %in% t2_1$code_muni) |> 
      select(code_muni, percent_preta_ou_parda, n_total),
    by = join_by(code_muni)
  ) |> 
  # drop munic with less than 10% of Negros
  filter(percent_preta_ou_parda >= .1) |> 
  arrange(desc(dissimilarity))

# adding race distribution
t2_1

t2_1 |> 
  ggplot() +
  aes(x = percent_preta_ou_parda, y = dissimilarity, color = name_region) +
  geom_point(alpha = .2, size = 2) +
  theme_minimal()

# focus on cities bigger than 100K of people
t2_1 |> 
  filter(n_total >= 100000) |> 
  arrange(dissimilarity)

t2_1 |> 
  filter(n_total >= 100000) |> 
  arrange(desc(dissimilarity))

clipr::write_last_clip()
####
# 2.2. Spatial correlation - LISA and Moran
####

# Creating dataset

df_spatial <- segreg_munic |> 
  filter(code_tract == "Total", year == 2022) |> 
  select(name_region, name_state, code_muni, name_muni, dissimilarity) |> 
  st_make_valid() |> 
  st_transform(5880) # sirgas 2000

df_lisa <- df_spatial |> 
  filter(code_muni %in% t2_1$code_muni)

# LISA

t2_2 <- compute_lisa(df_lisa)

# idea to plot map

# order for legends and colors
niveis <- c("Alto-Alto", "Alto-Baixo", "Baixo-Alto", "Baixo-Baixo", "Não significativo")
cores  <- c(
  "Alto-Alto"         = "#a50f15",
  "Alto-Baixo"        = "#fc9272",
  "Baixo-Alto"        = "#a6bddb",
  "Baixo-Baixo"       = "#045a8d",
  "Não significativo" = "#f0f0f0"
)

mapa <- t2_2 |>
  mutate(LISA_map = factor(LISA_map, levels = niveis))

# geometry simplification
mapa <- st_simplify(mapa, dTolerance = 500, preserveTopology = TRUE)

# boundaries for states and br
estados <- geobr::read_state(year = 2022) |> 
  st_make_valid() |> 
  st_transform(5880) |> # sirgas 2000
  st_simplify(dTolerance = 500, preserveTopology = TRUE)

brasil <- st_union(estados)

# legend with counting
contagem <- mapa |>
  st_drop_geometry() |>
  count(LISA_map, .drop = FALSE) |>
  mutate(rotulo = glue("{LISA_map} (n = {format(n, big.mark = '.')})"))

rotulos <- setNames(contagem$rotulo, contagem$LISA_map)

# adding labels with names of each state
labels_uf <- estados |>
  st_cast("POLYGON", warn = FALSE) |>
  mutate(area = st_area(geometry)) |>
  group_by(name_state) |>
  slice_max(area, n = 1, with_ties = FALSE) |>
  ungroup() |>
  mutate(geometry = st_point_on_surface(geometry)) 

labels_uf$X <- st_coordinates(labels_uf)[, 1]
labels_uf$Y <- st_coordinates(labels_uf)[, 2]

# creating map

p <- ggplot() +
  # municipios total
  geom_sf(data = df_spatial, fill = NA, colour = "grey90", linewidth = 0.15) +
  # municípios: sem borda (5.570 linhas poluem a escala nacional)
  geom_sf(data = mapa, aes(fill = LISA_map), colour = NA) +
  # estados
  geom_sf(data = estados, fill = NA, colour = "grey25", linewidth = 0.25) +
  # contorno do Brasil
  geom_sf(data = brasil, fill = NA, colour = "black", linewidth = 0.6) +
  scale_fill_manual(
    values = cores,
    labels = rotulos,
    drop   = FALSE,
    name   = "Agrupamento LISA"
  ) +
  geom_text_repel(
    data = labels_uf,
    aes(x = X, y = Y, label = abbrev_state),
    size = 3.2, fontface = "bold", colour = "grey10",
    bg.color = "white", bg.r = 0.15,          # halo branco: legível sobre qualquer cor
    min.segment.length = 0.3,                 # linha só quando o rótulo é deslocado
    segment.colour = "grey30", segment.size = 0.25,
    box.padding = 0.15, point.size = NA,
    max.overlaps = Inf, seed = 123
  ) +
  guides(fill = guide_legend(
    title.position = "top",
    override.aes = list(colour = "grey40", linewidth = 0.2)
  )) +
  # elementos cartográficos
  annotation_scale(
    location = "br", width_hint = 0.2, style = "ticks",
    text_family = "sans", text_cex = 0.8
  ) +
  annotation_north_arrow(
    location = "br", which_north = "true",
    pad_x = unit(0.4, "cm"), pad_y = unit(1.2, "cm"),
    height = unit(1.2, "cm"), width = unit(1.2, "cm"),
    style = north_arrow_minimal(text_size = 8)
  ) +
  labs(
    title    = "Segregação racial nos municípios brasileiros",
    subtitle = "Autocorrelação espacial local (LISA) do índice de dissimilaridade",
    caption  = glue(
      "Fonte: IBGE, Censo Demográfico 2022. Elaboração própria.\n",
      "Projeção: SIRGAS 2000 / Brazil Polyconic. ",
      "Agrupamentos com p < 0,10 (999 permutações)."   # ajuste ao limiar que você usou
    )
  ) +
  theme_void(base_size = 12) +
  theme(
    plot.title      = element_text(face = "bold", size = 15, hjust = 0, margin = margin(b = 3)),
    plot.subtitle   = element_text(size = 11, colour = "grey30", hjust = 0, margin = margin(b = 10)),
    plot.caption    = element_text(size = 8.5, colour = "grey40", hjust = 0, lineheight = 1.2,
                                   margin = margin(t = 10)),
    plot.margin     = margin(12, 12, 12, 12),
    legend.title    = element_text(face = "bold", size = 11),
    legend.text     = element_text(size = 10),
    legend.key.size = unit(0.5, "cm"),
    legend.position = "inside",
    legend.position.inside = c(0.18, 0.22),     # canto inferior esquerdo, área vazia do mapa
    legend.background = element_rect(fill = alpha("white", 0.85), colour = NA),
    plot.background = element_rect(fill = "white", colour = NA)
  )

p

# export it

ggsave(p,
       path = file.path("reports","highlights","output"),
       filename = "mapa_lisa_dissimilaridade.png",
       width = 20, height = 20, units = "cm", dpi = 300, bg = "white")


# t2_2 |> 
#   ggplot() +
#   geom_sf(aes(fill = LISA_map),
#           lwd = 0) +
#   geom_sf(fill = "transparent",
#           colour = "grey90", size = 0.2) +
#   scale_fill_manual(
#     values = c(
#       "Baixo-Baixo" = "#045a8d", "Alto-Baixo" = "#a6bddb","Baixo-Alto" = "#fc9272",
#       "Alto-Alto" = "#a50f15","Não significativo" =  "#f0f0f0"
#     )) +
#   guides(fill = guide_legend(title = glue::glue("LISA Map"))) +
#   labs(
#     caption = glue::glue("Fonte: IBGE, Censo Demográfico 2022.")
#   ) +
#   # tira sistema cartesiano
#   theme(
#     plot.title = element_text(face = "bold", size = 12, hjust = .5, vjust = .5),
#     plot.caption = element_blank(),
#     legend.title = element_text(face = "bold", size = 12, hjust = 0, vjust = .5),
#     legend.text = element_text(size = 12, hjust = 0, vjust = .9),
#     legend.position = "right",
#     axis.text = element_blank(),
#     axis.ticks = element_blank(),
#     panel.grid = element_line(color = "#ffffff",linewidth = .01),
#     panel.background = element_blank())

# results from LISA

t2_2 |> 
  st_drop_geometry() |> 
  filter(name_state == "Minas Gerais",LISA_map == "Baixo-Baixo")

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

t2_3 |> 
  select(name_region, name_state, name_muni, name_neighborhood, dissimilarity_muni)

clipr::write_last_clip()

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
aux_rms <- aux_munic |> 
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
