
# Packages
library(here)
library(janitor)
library(tidyverse)
library(sf)
library(RColorBrewer)
library(gridExtra)
library(patchwork)

# Read data
data_orig <- read.csv("/Users/cfree/Dropbox/Chris/UCSB/mentoring/mukta/data.csv", na.strings = c("", "NA")) 
  
# Build data
################################################################################

# Format data
data <- data_orig %>% 
  # Rename
  clean_names() %>%
  # Simplify 
  select(-c(magnitude, mechanism)) %>% 
  # Add habitat type
  mutate(habitat_type = ifelse(habitat_type == "subtropical", "tropical", habitat_type))

# Reduce to unique
data_unique <- data %>% 
  #select variable that are constant within each case study
  select(citation, topic, habitat_type, habitat_subtype, geographic_region) %>% 
  distinct(citation, habitat_type, geographic_region, .keep_all = TRUE)

# Build biome counts
biome_counts <- data %>% 
  # Count habitat types
  count(habitat_type) %>% 
  # Add polar
  add_row(habitat_type = "polar", n = 0) %>% 
  # Add category
  mutate(catg="Biome") %>% 
  # Rename
  rename(habitat=habitat_type)

# Habitat counts
habitat_counts <- data %>% 
  count(habitat_subtype) %>% 
  # Add category
  mutate(catg="Habitat") %>% 
  # Rename
  rename(habitat=habitat_subtype)


# Build assemblages
assemblage_counts <- data %>% 
  count(assemblage_type) %>% 
  # Add category
  mutate(catg="Assemblage") %>% 
  # Rename
  rename(habitat=assemblage_type) %>%
  # Format habitats
  mutate(habitat=gsub("_", " ", habitat) %>% stringr::str_to_sentence(),
         habitat=recode(habitat, "Na"="Not specified")) %>% 
  # Recode assemblages
  mutate(habitat=case_when(grepl("macroalgae|algal", tolower(habitat)) ~ "Macroalgae",
                           grepl("fish|wrasse|pomacentrid", tolower(habitat)) ~ "Fish",
                           grepl("urchin|invert", tolower(habitat)) ~ "Invertebrates",
                           habitat %in% c("Coral", "Hard coral", "Adult coral", "Juvenille coral") ~ "Coral",
                           T ~ habitat)) %>% 
  # Count again
  group_by(catg, habitat) %>% 
  summarize(n=sum(n)) %>% 
  ungroup()

# Merge counts
counts <- bind_rows(biome_counts, habitat_counts, assemblage_counts) %>% 
  # Format category
  mutate(catg=factor(catg, levels=c("Biome", "Habitat", "Assemblage"))) %>% 
  # Format habitats
  mutate(habitat=gsub("_", " ", habitat) %>% stringr::str_to_sentence(),
         habitat=recode(habitat, "Na"="Not specified"))


# Plot data
################################################################################

g <- ggplot(counts, aes(x=n, 
                        y=tidytext::reorder_within(habitat, desc(n), catg))) +
  facet_grid(catg~., space="free_y", scales="free_y") +
  geom_bar(stat="identity") +
  # Labels
  labs(x="Number of occurences", y="") +
  # Scales
  tidytext::scale_y_reordered() +
  # Theme
  theme_classic() +
  theme(strip.background = element_rect(colour=NA, fill=NA))
g










