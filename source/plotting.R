library(tidyverse)

# Extract unique Ágazat levels for indexing
unique_agazat <- Tab %>%
  filter(`Típus` %in% c("KSH medián, átlag", "KSH szektorok, átlag", 
                        "ELTE, garantált bér")) %>%
  arrange(-`Bruttó kereset 2024`) %>%
  pull(`Ágazat`)

color1 <- RColorBrewer::brewer.pal(n = 3,"Set1")[1]
line_size <- 0.3

#xxxxxxxxxxxxxxxx
# Barplot and added connected dots -----
#xxxxxxxxxxxxxxxx

# Karok
Karok <- Tab %>% 
  filter(!is.na(Kar)) %>%
  group_by(Kar) %>%
  group_keys() %>% 
  pull()

for(i in Karok) {
  for(j in c("ELTE, becsült átlagbér", "ELTE, becsült mediánbér")) {
    Max <- Tab %>% 
      filter(((Kar == i & `Típus` == j) | `Típus` == "KSH szektorok, átlag") & !is.na(`Bruttó kereset 2024`)) %>% 
      pull(`Bruttó kereset 2024`) %>% 
      max()
    
    Tab %>% 
      filter(`Típus` %in% c("KSH medián, átlag",
                            "KSH szektorok, átlag",
                            "ELTE, garantált bér")) %>%
      ggplot(aes(x = reorder(`Ágazat`, -`Bruttó kereset 2024`),
                 y = `Bruttó kereset 2024`, 
                 fill = `Típus`)) +
      geom_bar(stat = "identity", width = 0.5) +
      scale_fill_brewer(palette = "Set1") +
      scale_y_continuous(breaks = seq(0, Max, 100000),
                         labels = scales::label_number(),
                         expand = c(0, 0),
                         limits = c(0, Max+10000)) +
      
      
      # add dots for Típus == ELTE becsült átlagbér
      geom_point(data = filter(Tab, `Típus` == j & Kar == i),
                 aes(x = `Ágazat`, y = `Bruttó kereset 2024`, shape = `Típus`), 
                 color = color1, size = 1, 
                 inherit.aes = FALSE) +
      # Add a manual scale for shape to control the legend for geom_point
      scale_shape_manual(values = setNames(19, j)) +
      
      # Draw a segment from x = 0 to the geom_point's x value
      geom_segment(data = filter(Tab, `Típus` == j & Kar == i),
                   aes(x = 0, xend = match(`Ágazat`, unique_agazat),
                       y = `Bruttó kereset 2024`, yend = `Bruttó kereset 2024`),
                   color = color1, linetype = "dashed", linewidth = line_size, 
                   inherit.aes = FALSE) +
      
      # Draw a vertical segment from the point to the top of the bar
      geom_segment(data = filter(Tab, `Típus` == j & Kar == i),
                   aes(x = `Ágazat`, xend = `Ágazat`,
                       y = 0, yend = `Bruttó kereset 2024`),
                   color = color1, linetype = "dashed", linewidth = line_size,
                   inherit.aes = FALSE) +
      
      # add horizontal line at Ágazat == Budapesti albétletár, átlag
      geom_hline(data = filter(Tab, `Ágazat` == "Budapesti albétletár, átlag"),
                 aes(yintercept = `Bruttó kereset 2024`, linetype = "solid"), 
                 color = "black", linewidth = line_size, 
                 show.legend = FALSE) +
      annotate("label", x = 0.5, y = Tab %>% 
                 filter(`Ágazat` == "Budapesti albétletár, átlag") %>% 
                 select(`Bruttó kereset 2024`) %>% 
                 pull() + 35000, label = "Átlag budapesti albérletár", 
               size = 3, hjust = 0) +
      
      # Use guides() to control the order of legends
      guides(shape = guide_legend(order = 1, title = NULL),
             fill = guide_legend(order = 2, title = NULL),
             linetype = guide_legend(order = 3, title = NULL)) +
      labs(title = i,
           x = "Ágazat", 
           caption = "Források: KSH, ELTE Közalkalmazotti Tanács, ingatlan.com") +
      
      theme_minimal() +
      theme(axis.text.x = element_text(angle = 45, hjust = 1),
            legend.position = "top",
            legend.spacing.y = unit(0, "cm"))
    
    ggsave(paste0("output/salary_", i, j, ".png"), 
           width = 8, height = 5.5, dpi = 300)
    ggsave(paste0("output/salary_", i, j, ".pdf"), 
           width = 8, height = 5.5, device = cairo_pdf)
  }
}
rm(i, j, Max)

# insert a terminal command to merge pdf-s
system("pdftk output/*.pdf output output/ELTE_keresetek.pdf")
