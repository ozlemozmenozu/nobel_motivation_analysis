# ============================== #
# Nobel Peace & Economics Prize Thematic Analysis (1990–2024)
# Author: Özlem Özmen
# Project: IR 421 Final
# Description: Text, network, and time-series analysis of Nobel Prize justifications
# ============================== #

# === 0. Environment Setup ========================================
rm(list = ls())  # Clear all variables

# === 1. Load Required Packages ===================================
library(readxl)
library(dplyr)
library(tidyr)
library(ggplot2)
library(stringr)
library(purrr)
library(igraph)
library(ggraph)
library(proxy)
library(networkD3)
library(gganimate)
library(transformr)

# === 2. Load and Filter Dataset ==================================
df <- read_excel("nobel.xlsx")  # Ensure 'Year', 'Category', 'Theme' columns exist
df <- df %>% filter(Year >= 1990 & Year <= 2024)

# === 3. Clean and Unnest Theme Data ==============================
df_clean <- df %>%
  rename(themes = Theme) %>%
  mutate(themes = str_replace_all(themes, "\\s+", ""),       # Remove spaces
         themes = str_replace(themes, ",+$", "")) %>%        # Remove trailing commas
  separate_rows(themes, sep = ",") %>%
  filter(themes != "") %>%
  mutate(themes = as.integer(themes)) %>%
  distinct(Year, Category, themes)

# === 4. Yearly Overlap Between Peace and Econ ====================
theme_sets <- df_clean %>%
  group_by(Year, Category) %>%
  summarise(theme_list = list(unique(themes)), .groups = "drop") %>%
  pivot_wider(names_from = Category, values_from = theme_list)

theme_overlap <- theme_sets %>%
  rowwise() %>%
  mutate(
    shared = length(intersect(Economics, Peace)),
    only_econ = length(setdiff(Economics, Peace)),
    only_peace = length(setdiff(Peace, Economics))
  ) %>%
  ungroup()

# === 5. Theme Co-occurrence Network ==============================
theme_pairs <- df_clean %>%
  group_by(Year) %>%
  summarise(pairs = list(as.data.frame(t(combn(sort(unique(themes)), 2)))), .groups = "drop") %>%
  unnest(pairs) %>%
  rename(t1 = V1, t2 = V2) %>%
  mutate(t1 = as.character(t1), t2 = as.character(t2)) %>%
  count(t1, t2, name = "weight")

g <- graph_from_data_frame(theme_pairs, directed = FALSE)
V(g)$degree <- degree(g)

ggraph(g, layout = "fr") +
  geom_edge_link(aes(width = weight), alpha = 0.3) +
  geom_node_point(aes(size = degree), color = "darkorange") +
  geom_node_text(aes(label = name), repel = TRUE, size = 4) +
  theme_void() +
  labs(title = "Theme Co-occurrence Network (1990–2024)")

# === 6. Theme Labels and Visual Time Plot =========================
theme_labels <- c(
  "1" = "MacroFinance & Markets", "2" = "Labour & Employment", "3" = "Development, Poverty & Welfare",
  "4" = "Security, Conflict & Arms Control", "5" = "Governance & Human Rights",
  "6" = "Children & Women Studies", "7" = "Sustainability & Environment", "8" = "Theories & Other Topics"
)

df_viz <- df %>%
  rename(themes = Theme) %>%
  mutate(themes = str_replace_all(themes, "\\s+", ""),
         themes = str_replace(themes, ",+$", "")) %>%
  separate_rows(themes, sep = ",") %>%
  filter(themes != "") %>%
  mutate(themes = as.character(as.integer(themes)),
         theme_label = theme_labels[themes]) %>%
  filter(!is.na(theme_label)) %>%
  distinct(Year, Category, themes, theme_label)

shared_themes <- df_viz %>%
  group_by(Year, themes) %>%
  summarise(cat_count = n_distinct(Category), .groups = "drop") %>%
  filter(cat_count == 2) %>%
  mutate(MatchType = "Both")

marked_data <- df_viz %>%
  left_join(shared_themes %>% select(Year, themes, MatchType), by = c("Year", "themes")) %>%
  mutate(MatchType = ifelse(is.na(MatchType),
                            ifelse(Category == "Economics", "Economics", "Peace"),
                            "Both"))

color_map <- c("Economics" = "red", "Peace" = "blue", "Both" = "green")

ggplot(marked_data, aes(x = Year, y = theme_label, color = MatchType)) +
  geom_point(size = 3, alpha = 0.9) +
  scale_color_manual(values = color_map) +
  labs(
    title = "Themes Addressed by Nobel Peace & Economics Prizes (1990–2024)",
    x = "Year", y = "Theme", color = "Category"
  ) +
  theme_minimal(base_size = 13)

# === 7. Theme Trends: Stacked Area Plot ============================
df_area <- df_viz %>%
  count(Year, theme_label)

ggplot(df_area, aes(x = Year, y = n, fill = theme_label)) +
  geom_area(alpha = 0.8, color = "white") +
  labs(title = "Thematic Distribution Over Time", x = "Year", y = "Total Mentions") +
  scale_x_continuous(breaks = seq(1990, 2024, 5)) +
  theme_minimal(base_size = 13)

# === 8. Theme Trends: Faceted Line Charts ===========================
df_line <- df_viz %>%
  count(Year, theme_label)

ggplot(df_line, aes(x = Year, y = n)) +
  geom_line(color = "darkblue", size = 1) +
  geom_point(color = "red", size = 1.5) +
  facet_wrap(~ theme_label, scales = "free_y") +
  labs(title = "Theme-Specific Time Trends (1990–2024)", x = "Year", y = "Mentions") +
  theme_minimal()

# === 9. Sankey Diagram ==============================================
df_sankey <- df_viz %>%
  count(Year, theme_label, Category, name = "value") %>%
  mutate(Year = as.character(Year))

nodes <- data.frame(name = unique(c(df_sankey$Year, df_sankey$theme_label, df_sankey$Category)))
nodes$id <- 0:(nrow(nodes)-1)
get_id <- function(x) match(x, nodes$name) - 1

link1 <- df_sankey %>%
  select(source = Year, target = theme_label, value) %>%
  mutate(source = get_id(source), target = get_id(target), group = "YearToTheme")

link2 <- df_sankey %>%
  mutate(source = get_id(theme_label), target = get_id(Category), group = Category) %>%
  select(source, target, value, group)

links <- rbind(link1, link2)

nodes$group <- ifelse(nodes$name %in% c("Peace", "Economics"), nodes$name,
                      ifelse(nodes$name %in% df_sankey$Year, "Year", "Theme"))

my_color <- 'd3.scaleOrdinal()
  .domain(["Economics", "Peace", "Year", "Theme"])
  .range(["#e41a1c", "#377eb8", "#999999", "#4daf4a"])'

sankeyNetwork(
  Links = links, Nodes = nodes,
  Source = "source", Target = "target", Value = "value",
  NodeID = "name", NodeGroup = "group", LinkGroup = "group",
  fontSize = 13, nodeWidth = 30, colourScale = my_color
)
