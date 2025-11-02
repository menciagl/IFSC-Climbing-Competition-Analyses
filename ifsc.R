library(readxl)
ifsc <- read_excel("ifsc.xlsx")

# Needed libraries

library(dplyr)
library(ggplot2)
library(rnaturalearth)
library(rnaturalearthdata)
library(sf)
library (plotly)
library(tidyr)


# Plot 1. Top IFSC Champions ----

winners_by_person_cat <- ifsc %>%
  filter(`Overall Winner` != "-") %>%
  group_by(`Overall Winner`, Category) %>%
  summarise(wins = n(), .groups = "drop")

top_winners <- winners_by_person_cat %>%
  group_by(`Overall Winner`) %>%
  summarise(total_wins = sum(wins), .groups = "drop") %>%
  arrange(desc(total_wins)) %>%
  slice_head(n = 5)

top_winners_by_cat <- winners_by_person_cat %>%
  filter(`Overall Winner` %in% top_winners$`Overall Winner`)

top_winners_order <- top_winners$`Overall Winner`

# Plot
ggplot(top_winners_by_cat, aes(x = factor(`Overall Winner`, levels = top_winners_order), 
                               y = wins, fill = Category)) +
  geom_col() +
  geom_text(aes(label = wins), position = position_stack(vjust = 0.5), size = 4, color = "white") +
  scale_fill_manual(values = c(
    "Boulder" = "#CD6BE3",
    "Lead" = "#EBF55D",
    "Speed" = "#6BE3CD"
  )) +
  labs(
    title = "Top 5 IFSC World Cup Champions (2014–2024) by Category",
    x = "Athlete",
    y = "Number of Wins",
    fill = "Category"
  ) +
  theme_minimal(base_size = 14) +
  theme(
    plot.title = element_text(face = "bold", hjust = 0.5),
    axis.text.x = element_text(angle = 45, hjust = 1, size = 14)  # tamaño aumentado
  )


# Map 1. Wins by country ----

world <- ne_countries(scale = "medium", returnclass = "sf")

country_wins <- ifsc %>%
  group_by(Country) %>%
  summarise(
    wins = n(),
    winners = paste(unique(`Overall Winner`), collapse = ", "),
    .groups = "drop"
  )

world_wins <- world %>%
  left_join(country_wins, by = c("name" = "Country"))


world_wins_filtered <- world_wins %>% 
  filter(name != "Antarctica")

ggplot(data = world_wins_filtered) +
  geom_sf(aes(fill = wins), color = "white", size = 0.2) +
  scale_fill_gradient(
    low = "#d0f0c0", high = "#006400", na.value = "grey90",
    name = "Number of Wins"
  ) +
  labs(
    title = "IFSC World Cup Wins by Country",
    subtitle = "2014–2024",
  ) +
  coord_sf(
    xlim = c(-170, 180),  
    ylim = c(-10, 85),   
    expand = FALSE
  ) +
  theme_void(base_size = 14) +
  theme(
    plot.title = element_text(face = "bold", hjust = 0.5),
    plot.subtitle = element_text(hjust = 0.5),
    legend.position = "bottom",
    legend.key.width = unit(2, "cm")
  )



# Plot 2. Countries with the most wins by gender ----

country_gender_wins <- ifsc %>%
  filter(Country != "-") %>% 
  group_by(Country, Gender) %>%
  summarise(
    wins = n(),
    .groups = "drop"
  )

country_order <- country_gender_wins %>%
  group_by(Country) %>%
  summarise(total_wins = sum(wins), .groups = "drop") %>%
  arrange(total_wins) %>%  
  pull(Country)

# Plot
ggplot(country_gender_wins, aes(x = factor(Country, levels = country_order), 
                                y = wins, fill = Gender)) +
  geom_bar(stat = "identity", position = position_stack()) +
  geom_text(aes(label = wins, color = Gender), 
            position = position_stack(vjust = 0.5), 
            fontface = "bold", size = 4) +
  scale_fill_manual(values = c("M" = "#4A90E2",   # Azul para hombres
                               "W" = "#FF69B4")) + # Rosa para mujeres
  scale_color_manual(values = c("M" = "#FFFFFF", "W" = "#FFFFFF"), guide = "none") +  # Cambié colores para mejor contraste
  coord_flip() +
  labs(
    title = "Countries with the Most IFSC World Cup Wins (2014–2024)",
    x = "Country",
    y = "Number of Wins",
    fill = "Gender"
  ) +
  theme_minimal(base_size = 14) +
  theme(
    plot.title = element_text(face = "bold", hjust = 0.5),
    axis.title.y = element_blank()
  )


# Plot 3. Top Countries By Climbing Category ----

country_cat_wins <- ifsc %>%
  filter(Country != "-") %>%
  group_by(Category, Country) %>%
  summarise(wins = n(), .groups = "drop")


top_countries_per_cat <- country_cat_wins %>%
  group_by(Category) %>%
  slice_max(order_by = wins, n = 3) %>% # three countries by category
  ungroup()

top_countries_per_cat <- top_countries_per_cat %>%
  group_by(Category) %>%
  mutate(Country = factor(Country, levels = Country[order(wins)])) %>% 
  ungroup()

category_colors <- c("Boulder" = "#CD6BE3", "Lead" = "#EBF55D", "Speed" = "#6BE3CD")


top_countries_per_cat <- top_countries_per_cat %>% 
  group_by(Category) %>% 
  arrange(desc(wins), .by_group = TRUE) %>%     
  mutate(Country = factor(Country, levels = unique(Country))) %>% 
  ungroup()


ggplot(top_countries_per_cat, aes(x = Country, y = wins, fill = Category)) +
  geom_col(show.legend = FALSE) +
  geom_text(aes(label = wins), hjust = 1.1, fontface = "bold", size = 4, color = "black") +
  scale_fill_manual(values = category_colors) +
  coord_flip() +
  facet_wrap(~Category, scales = "free_y") +
  labs(
    title = "Top Countries by Category - IFSC World Cup (2014–2024)",
    x = "Country",
    y = "Number of Wins"
  ) +
  theme_minimal(base_size = 14) +
  theme(
    plot.title = element_text(face = "bold", hjust = 0.5),
    axis.title.y = element_blank()
  ) +
  ylim(0, max(top_countries_per_cat$wins) + 2)


# Plot 4. Wins By Country and Category ----

heatmap_data <- ifsc %>%
  filter(Country != "-") %>%
  group_by(Country, Category) %>%
  summarise(wins = n(), .groups = "drop") %>%
  pivot_wider(names_from = Category, values_from = wins, values_fill = 0)

heatmap_data_long <- heatmap_data %>%
  pivot_longer(cols = -Country, names_to = "Category", values_to = "wins")


ggplot(heatmap_data_long, aes(x = Category, y = reorder(Country, wins), fill = wins)) +
  geom_tile(color = "white", size = 0.7) +
  scale_fill_gradient(low = "white", high = "#4A90E2") +   # Gradiente azul para intensidad
  labs(
    title = "Heatmap of Wins by Country and Category - IFSC World Cup (2014-2024)",
    x = "Category",
    y = "Country",
    fill = "Number of Wins"
  ) +
  theme_minimal(base_size = 14) +
  theme(
    plot.title = element_text(face = "bold", hjust = 0.5),
    axis.text.x = element_text(color = "black"),
    axis.text.y = element_text(color = "black")
  )



# Plot 5. Mean Age by Category and Gender ----

ifsc <- ifsc %>%
  mutate(
    Year   = as.numeric(Year),
    Age    = as.numeric(Age),
    Height = as.numeric(Height)
  )

df_summary <- ifsc |>
  group_by(Category, Gender) |>
  summarise(mean_age = mean(Age, na.rm = TRUE),
            count = n())

print(df_summary)

ggplot(df_summary, aes(x = Category, y = mean_age, fill = Gender)) +
  geom_col(position = position_dodge(width = 0.5), width = 0.5) +
  geom_text(aes(label = round(mean_age, 1)),       
            position = position_dodge(width = 0.5),
            vjust = -0.7, size = 3) +              
  scale_fill_manual(
    values = c("W" = "#FF80AA", "M" = "#80B1FF"),
    labels = c("W" = "Women", "M" = "Men"),
    name = "Gender"
  ) +
  labs(
    title = "Average Age of IFSC World Cup Champions (2014–2024)",
    x = "Category",
    y = "Average Age"
  ) +
  theme_minimal(base_size = 14) +
  theme(
    plot.title = element_text(face = "bold", hjust = 0.5),
    legend.position = "top",
    panel.grid.major.x = element_blank()
  )

# Plot 6. Evolution of Age (Men) ----

df_yearly <- ifsc %>%
  group_by(Year, Category, Gender) %>%
  summarise(mean_age = mean(Age, na.rm = TRUE), .groups = "drop")

df_men <- df_yearly %>% filter(Gender == "M")
df_women <- df_yearly %>% filter(Gender == "W")

ggplot(df_men, aes(x = factor(Year), y = mean_age, color = Category, group = Category)) +
  geom_line(size = 1) +
  geom_point(size = 2) +
  geom_text(aes(label = round(mean_age, 1)),  # etiqueta con edad redondeada
            size = 3.3,                         # tamaño pequeño
            vjust = -0.6,                       # un poco arriba del punto
            show.legend = FALSE) +  
  scale_color_manual(values = c(
    "Boulder" = "#CD6BE3",
    "Lead"    = "#EBF55D",
    "Speed"   = "#6BE3CD"
  )) +
  labs(
    title = "Evolution of Age - Men Champions (2014–2024)",
    x = "Year",
    y = "Age",
    color = "Category"
  ) +
  theme_minimal(base_size = 14) +
  theme(
    plot.title = element_text(face = "bold", hjust = 0.5),
    legend.position = "bottom",
  )

# Plot 7. Evolution of Age (Women) ----

ggplot(df_women, aes(x = factor(Year), y = mean_age, color = Category, group = Category)) +
  geom_line(size = 1) +
  geom_point(size = 2) +
  geom_text(aes(label = round(mean_age, 1)),  # etiqueta con edad redondeada
            size = 3.3,                       # tamaño pequeño
            vjust = -0.6,                    # un poco arriba del punto
            show.legend = FALSE) +  
  scale_color_manual(values = c(
    "Boulder" = "#CD6BE3",
    "Lead"    = "#EBF55D",
    "Speed"   = "#6BE3CD"
  )) +
  labs(
    title = "Evolution of Age - Women Champions (2014–2024)",
    x = "Year",
    y = "Age",
    color = "Category"
  ) +
  theme_minimal(base_size = 14) +
  theme(
    plot.title = element_text(face = "bold", hjust = 0.5),
    legend.position = "bottom"
  )

# Plot 8. Comparison of Evolution of Age by Gender  ----
men_avg <- df_men %>%
  group_by(Year) %>%
  summarise(mean_age = mean(mean_age)) %>%
  mutate(Gender = "Men")

women_avg <- df_women %>%
  group_by(Year) %>%
  summarise(mean_age = mean(mean_age)) %>%
  mutate(Gender = "Women")

combined_df <- bind_rows(men_avg, women_avg)

ggplot(combined_df, aes(x = factor(Year), y = mean_age, color = Gender, group = Gender)) +
  geom_line(size = 1.5) +
  geom_point(size = 3) +
  geom_text(aes(label = round(mean_age, 1)),
            size = 4,
            vjust = -0.7,
            show.legend = FALSE) +
  scale_color_manual(values = c("Men" = "#80B1FF", "Women" = "#FF80AA")) +  # Azul para hombres, rojo para mujeres
  labs(
    title = "Comparison of Average Age Trends by Gender (2014–2024)",
    x = "Year",
    y = "Average Age",
    color = "Gender"
  ) +
  theme_minimal(base_size = 15) +
  theme(
    plot.title = element_text(face = "bold", hjust = 0.5),
    legend.position = "bottom"
  )

# Plot 9: Average Height of IFSC Champions ----
df_summary_height <- ifsc %>%
  group_by(Category, Gender) %>%
  summarise(
    mean_height = mean(Height, na.rm = TRUE),
    count = n(),
    .groups = "drop"
  )

# Plot
ggplot(df_summary_height, aes(x = Category, y = mean_height, fill = Gender)) +
  geom_col(position = position_dodge(width = 0.5), width = 0.5) +
  geom_text(aes(label = round(mean_height, 1)),
            position = position_dodge(width = 0.5),
            vjust = -0.7, size = 3) +  # Texto encima de las barras
  scale_fill_manual(
    values = c("W" = "#FF80AA", "M" = "#80B1FF"),
    labels = c("W" = "Women", "M" = "Men"),
    name = "Gender"
  ) +
  labs(
    title = "Average Height of IFSC World Cup Champions (2014–2024)",
    x = "Category",
    y = "Average Height (cm)"
  ) +
  theme_minimal(base_size = 14) +
  theme(
    plot.title = element_text(face = "bold", hjust = 0.5),
    legend.position = "top",
    panel.grid.major.x = element_blank()
  )


# Plot 10. Evolution of Height (Men)----
df_yearly_height <- ifsc %>%
  group_by(Year, Category, Gender) %>%
  summarise(mean_height = mean(Height, na.rm = TRUE), .groups = "drop")


df_men_h <- df_yearly_height %>% filter(Gender == "M")
df_women_h <- df_yearly_height %>% filter(Gender == "W")

# MEN

ggplot(df_men_h, aes(x = factor(Year), y = mean_height, color = Category, group = Category)) +
  geom_line(size = 1) +
  geom_point(size = 2) +
  geom_text(aes(label = round(mean_height, 1)),
            size = 3.3,
            vjust = -0.6,
            show.legend = FALSE) +  
  scale_color_manual(values = c(
    "Boulder" = "#CD6BE3",
    "Lead"    = "#EBF55D",
    "Speed"   = "#6BE3CD"
  )) +
  labs(
    title = "Evolution of Height - Men Champions (2014–2024)",
    x = "Year",
    y = "Height (cm)",
    color = "Category"
  ) +
  theme_minimal(base_size = 14) +
  theme(
    plot.title = element_text(face = "bold", hjust = 0.5),
    legend.position = "bottom"
  )

# Plot 11: Evolution of Height (Women)----

ggplot(df_women_h, aes(x = factor(Year), y = mean_height, color = Category, group = Category)) +
  geom_line(size = 1) +
  geom_point(size = 2) +
  geom_text(aes(label = round(mean_height, 1)),
            size = 3.3,
            vjust = -0.6,
            show.legend = FALSE) +  
  scale_color_manual(values = c(
    "Boulder" = "#CD6BE3",
    "Lead"    = "#EBF55D",
    "Speed"   = "#6BE3CD"
  )) +
  labs(
    title = "Evolution of Height - Women Champions (2014–2024)",
    x = "Year",
    y = "Height (cm)",
    color = "Category"
  ) +
  theme_minimal(base_size = 14) +
  theme(
    plot.title = element_text(face = "bold", hjust = 0.5),
    legend.position = "bottom"
  )




