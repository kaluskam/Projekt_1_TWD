library(dplyr)
library(tidyr)

library(showtext) # czcionka
fam <- "lit"
font_add_google(name = "Lato", family = fam)
showtext_auto()

library(ggplot2)

library(gridExtra)
library(grid)

# Przygotowanie danych do wykresu z bananami
countries <- c("Ecuador", "Philippines", "Columbia", "Costa.Rica", "Guatemala",
               "Belgium.Luxembourg", "Honduras", "USA", "United.Arab.Emirates", 
               "Panama", "Cameroon")

years <- 1994:2005

ekw_qty <- c(3000000, 3600000, 3700000, 4400000, 3700000, 3800000, 3900000, 
             3450000, 4100000, 5100000, 5100000, 5200000)
filp_qty <- c(1000000, 900000, 800000, 8500000, 600000, 720000, 750000,
             2100000, 2000000, 2000000, 2000000, 2000000)
kost_qty <- c(1500000, 1600000, 1600000, 1600000, 2000000, 1800000, 1550000,
             1500000, 1900000, 2100000, 2100000, 1800000)
kol_qty <- c(1500000, 1200000, 1300000, 1400000, 1400000, 1350000, 1550000,
             1500000, 1100000, 1300000, 1300000, 1400000)
gwa_qty <- c(500000, 550000, 500000, 520000, 600000, 540000, 650000,
             700000, 720000, 730000, 800000, 1100000)
bel_qty <- c(500000, 700000, 720000, 700000, 730000, 800000, 810000,
             700000, 600000, 610000, 640000, 7100000)
hon_qty <- c(400000, 500000, 550000, 490000, 500000, 100000, 90000,
             500000, 480000, 470000, 500000, 495000)
usa_qty <- c(400000, 410000, 390000, 390000, 395000, 387000, 391000,
             410000, 394000, 398000, 400000, 392000)
arab_qty <- c(0, 0, 0, 0, 0, 50000, 100000, 0, 0, 0, 250000, 500000)
pana_qty <- c(600000, 550000, 560000, 540000, 395000, 580000, 510000,
              410000, 394000, 399000, 400000, 393000)
kam_qty <- c(100000, 200000, 150000, 160000, 140000, 150000, 210000,
             215000, 207000, 240000, 230000, 225000)

dt <- data.frame(year=years, Ecuador=ekw_qty, Philippines=filp_qty, 
                 `Costa Rica`=kost_qty, Columbia=kol_qty, Guatemala=gwa_qty, 
                 `Belgium/Luxembourg`=bel_qty, Honduras=hon_qty, USA=usa_qty,  
                 `United Arab Emirates`=arab_qty, Panama=pana_qty, Cameroon=kam_qty)

dt_div <- dt %>%
  mutate(countries = dt[countries]/1000)
dt_sel <- dt_div %>% select(1, 13)
dt_countries <- data.frame(year=dt_sel$year, dt_sel$countries)
dt_p <- gather(dt_countries, key="country", value="value", countries)

dt_p <- transform(dt_p,
                  country=factor(country,levels=c("Ecuador", "Philippines", "Columbia", "Costa.Rica", "Guatemala",
                                                  "Belgium.Luxembourg", "Honduras", "USA", "United.Arab.Emirates", 
                                                  "Panama", "Cameroon")))
# Wykres z bananami 
ggplot(dt_p, aes(x=as.factor(year), y=value)) +
  geom_bar(stat="identity", fill="#a31f1c") +
  ylab("number of bananas exported in tonnes") +
  xlab("year") + 
  facet_wrap(~country) +
  scale_x_discrete(guide = guide_axis(angle = 70)) +
  ggtitle("Export of bananas (1994 - 2005)\n") +
  theme_minimal() +
  theme(title = element_text(size = 18, face = "bold"), 
        axis.title = element_text(size = 14, face = "bold"), 
        text = element_text(family = fam, face = "bold", size = 12),
        axis.title.y = element_text(margin = margin(t = 0, r = 20, b = 0, l = 0)),
        axis.title.x = element_blank(),
        strip.background = element_rect(fill="#F5F3F4", color = "#F5F3F4" )) +
  ggsave(filename = "bananas.png", dpi = 80)


# Przygotowanie danych GDP 

asia <- data.frame(country = c("China", "Japan", "India", "South Korea", 
          "Russia", "Indonesia", "Turkey", "Saudi Arabia", "Iran", "Thailand", 
          "UEA", "Hong Kong", "Israel", "Malasya", "Philippines", "Singapore"), 
          value = c(14.84, 5.91, 2.83, 1.86, 1.8, 1.16, 0.97, 0.87, 0.57, 
                      0.53, 0.5, 0.42, 0.4, 0.4, 0.39, 0.39), 
          continent = rep("Asia", 16))
n_america <- data.frame(country = c("USA", "Mexico", "Canada"), 
                        value = c(24.32, 2.09, 1.54), 
                        continent = rep("North America", 3))
s_america <- data.frame(country = c("Brazil", "Argentina", "Venezuela", "Colombia"), 
                        value = c(2.39, 0.79, 0.5, 0.39), 
                        continent = rep("South America", 4))
africa <- data.frame(country = c("Nigeria", "Egypt", "South Africa"), 
                     value = c(0.65, 0.45, 0.42), 
                     continent = rep("Africa", 3))
europe <- data.frame(country = c("Germany", "UK", "France", "Italy", "Spain", 
                                 "Netherlands", "Switzerland", "Sweden", "Poland", 
                                 "Belgium", "Norway", "Austria", "Denmark", "Ireland"),
                     value = c(4.54, 3.85, 3.26, 2.46, 1.62, 1.01, 0.9, 0.67, 
                               0.64, 0.61, 0.52, 0.51, 0.4, 0.38),
                     continent = rep("Europe", 14))
all_data <-rbind(n_america, asia,  europe, s_america, africa, 
                 c("Australia", 1.81, "Australia"))

# Posortowanie danych do wykresu wzgledem panstw
all_data$value <- as.numeric(all_data$value)
sorted <- all_data %>%
  arrange(value)
countries_order <- sorted$country
sorted$country <- factor(sorted$country, levels = countries_order)

# Wykres GDP panstwa
p1 <- ggplot(sorted, aes(x = country, y = value)) +
  geom_bar(stat = "identity", fill = "#a31f1c") + 
  geom_text(aes(label = value), size = 3.5, position = position_dodge(0.9), hjust = -0.3, fontface = "bold", family = fam, color = "#161A1D") +
  ggtitle("GDP by Countries") +
  ylab("%") + 
  theme_minimal() +
  theme(plot.title = element_text(color = "black", face = "bold", size = 20, hjust = 0.5, vjust = 1.5, margin = margin(t = 10)), 
        axis.title = element_text(size = 14, face = "bold"), 
        text = element_text(family = fam, face = "bold", size = 13),
        axis.title.y = element_blank(),
        axis.ticks = element_blank(),
        axis.text.y = element_text(margin = margin(r = 0)),
        legend.title = element_blank(),
        panel.grid.major.y = element_blank()
  ) + coord_flip()

# Przygotowanie danych kontynenty
continents_data <- all_data %>%
  group_by(continent) %>%
  summarise(sum = sum(value)) %>%
  arrange(-sum)

continents_data <- rbind(continents_data, 
                    c("Rest of the World", 9.41))
continents_data$sum <- as.numeric(continents_data$sum)
continents_order <- continents_data$continent
continents_data$continent <- factor(continents_data$continent, 
                                    levels = continents_order)

# Wykres GDP kontynenty
p2 <- ggplot(continents_data, aes(x = continent, y = sum)) +
  geom_bar(stat = "identity", fill = "#a31f1c") + 
  geom_text(aes(label = sum), size = 6, vjust = 1.5, fontface = "bold", family = fam, color = "white") +
  ggtitle("GDP by Continents") +
  theme_minimal() +
  theme(plot.title = element_text(color = "black", face = "bold", size = 20, 
                                  hjust = 0.5, vjust = 1.5, margin = margin(t = 10)), 
        text = element_text(family = fam, face = "bold", size = 13),
        axis.title.y = element_blank(),
        axis.title.x = element_blank(),
        axis.ticks = element_blank(),
        legend.title = element_blank(),
        panel.grid = element_blank(),
        axis.text.y = element_blank(),
        axis.text.x = element_text(size = 12, vjust = 7)
  )

grid.arrange(p1, p2, nrow = 1, top = textGrob("The Global Economy by GDP",
                                              gp=gpar(fontsize=30, family=fam, 
                                              face = "bold")))

library(cowplot)
plot_grid(p1, p2, align = "h", ncol = 2, rel_widths= c(3/5, 2/5))

