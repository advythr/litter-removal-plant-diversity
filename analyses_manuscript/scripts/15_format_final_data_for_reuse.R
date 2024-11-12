# Format final data for deposit at Dryad

library(tidyverse)

# Raw hits data ===========================
raw_data <- read.csv("analyses_manuscript/processed_data/long_data.csv")

raw_data_formatted <- raw_data %>%
  select(-year) %>% # redundant with date
  select(collected_date, plot, treatment,
         everything()
         ) %>%
  # Clarify that "plot" is plot ID
  rename("plot_id" = "plot")

write.csv(
  x = raw_data_formatted,
  file = "analyses_manuscript/final_data_for_dryad/raw_hits.csv",
  row.names = FALSE
)

# Species list ===========================
species_list <- read.csv("analyses_manuscript/stats_results/species_list_for_project.csv")

species_list_formatted <- species_list %>%
  filter(known_in_site == "found in 2021-2023 study") %>% # for this published dataset, only the species found in the course of the study is needed
  rename("species_name" = "species.name") %>%
  select(-known_in_site) # this is now obvious

species_list_formatted

write.csv(
  x = species_list_formatted,
  file = "analyses_manuscript/final_data_for_dryad/study_species_list.csv",
  row.names = FALSE
)

# Diversity and composition table ===========================

div_comp <- read.csv("analyses_manuscript/processed_data/analysis_variables.csv")

div_comp_formatted <- div_comp %>%
  # Remove redundant columns
  select(-year_treatment,
         ) %>%
  # Change order of columns to be more logical
  select(
    plot, block, row, column, position, 
    year, treatment,
    years_raked,
    everything()
  ) %>%
  # Clarify that "plot" is plot ID
  rename("plot_id" = "plot") %>%
  # Make the relative abundance column names more explicit, to prevent confusion with areal cover columns
  rename(
    "exotic_forb_relabund" = "exotic_forb",
    "exotic_grass_relabund" = "exotic_grass",
    "native_forb_relabund" = "native_forb",
    "native_shrub_relabund" = "native_shrub",
    "native_succulent_relabund" = "native_succulent"
  ) %>%
  # Move cover columns to before species covers
  select(plot_id:native_succulent_relabund,
         exotic_forb_cover:native_succulent_cover,
         everything()) %>%
  # Remove areal cover variables, this will be a different table
  select(-AMSMEN_cover:-CRACON_cover)
  
head(div_comp_formatted)

write.csv(
  x = div_comp_formatted,
  file = "analyses_manuscript/final_data_for_dryad/plot_div_comp_table.csv",
  row.names = FALSE
)

# Areal cover table ===========================

areal_cover_by_species <- read.csv("analyses_manuscript/processed_data/areal_cover_variables.csv")

areal_cover_by_species_formatted <- areal_cover_by_species %>%
  select(-exotic_forb_cover:-native_succulent_cover) %>%
  # Clarify that "plot" is plot ID
  rename("plot_id" = "plot") %>%
  select(plot_id, everything()) %>%
  arrange(year, treatment)
head(areal_cover_by_species_formatted)

write.csv(
  x = areal_cover_by_species_formatted,
  file = "analyses_manuscript/final_data_for_dryad/plot_areal_cover_by_species_table.csv",
  row.names = FALSE
)

# Plot by species relative abundance matrix ===========================
  
rel_abund_matrix <- read.csv("analyses_manuscript/processed_data/overall_comp_matrix.csv")

rel_abund_matrix_formatted_matrix <- rel_abund_matrix %>%
  # Remove columns redundant with div comp table
  select(-BG, -LIT,
         -year_treatment,
         ) %>%
  # Make site a column to turn this into a site by species matrix
  unite(site, plot, year, treatment, sep = ".") %>%
  column_to_rownames("site")

## Verify that everything adds up to 1, meaning there were no errors

rel_abund_sums <- rel_abund_matrix_formatted %>%
  rowwise() %>%
  mutate(sum = sum(c_across(everything()), na.rm = TRUE)) %>%
  ungroup()

sums <- rel_abund_sums %>% 
  filter(sum != 1)
## Good!

rel_abund_matrix_formatted_table <- rel_abund_matrix_formatted_matrix %>%
  # Turn this back into a matrix as row names can be lost
  rownames_to_column("plot_in_year")
  
write.csv(
  x = rel_abund_matrix_formatted_table,
  file = "analyses_manuscript/final_data_for_dryad/plot_species_relabund_matrix.csv",
  row.names = FALSE
)
