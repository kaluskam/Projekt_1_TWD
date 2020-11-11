library(ggplot2)
library(showtext)
library(tidyverse)
library(dplyr)

# Poprawiony wykres przedstawiający przyczyny wypadków
procent_wypadkow <- c(0.3313, 0.2217, 0.1840, 0.1663, 0.0967)
przyczyna <- c("niedostosowanie prędkości\ndo warunków ruchu", "nieudzielenie pierwszeństwa przejazdu",
               "nieustąpienie pierwszeństwa \n przejazdu na przejściu", "niezachownie bezpiecznej odległości \n pomiędzy pojazdami",
               "nieprawidłowe wyprzedzanie")

dane <- data.frame(procent_wypadkow, przyczyna)
font_add_google(name = "Lato", family = "lit")
showtext_auto()

ggplot(dane, aes(x = procent_wypadkow, y = reorder(przyczyna, c((5:1))), fill = procent_wypadkow))+
  geom_bar(stat = "identity", position = "dodge", width = 0.75)+
  theme_minimal()+
  theme(panel.grid.major.y = element_blank(),
        panel.grid.major.x = element_blank(),
        panel.grid.minor.x = element_blank(),
        axis.text.y = element_text(color = "black", face = "bold"), 
        panel.grid.minor.y = element_blank(), legend.position = "none", axis.line.x = element_blank(), axis.text.x = element_blank(),
        text = element_text(family = "lit", size = 12.5, color = "black"), 
        plot.title = element_text(size = 18, hjust = -0.5))+
  ggtitle("Przyczyna wypadków")+
  scale_x_continuous(limits = c(0, 0.5), labels = 
                       scales::percent_format(scale = 100, accuracy = 0.1,))+
  geom_text(aes(label=scales::percent(procent_wypadkow, size = 12)), position=position_dodge(width=1.5), 
            vjust= 0.2, hjust = -0.1, size = 4.3)+
  ylab("")+
  xlab("")+
  scale_fill_continuous(low = "#003c9e", high = "#003c9e")
  
ggsave("samochody.png")

# Poprawiony wykres przedstawiający dzień z życia

liczba_godzin <- c("8:33","8:48","3:40",  "3:35",  "2:39", "2:49", "2:32", "2:29","1:49", "2:08", 
                   "1:14","1:10", "0:47", "0:47", "0:49", "0:44", "0:34", "0:32", "0:29", "0:25",
                   "0:19", "0:19", "0:16", "0:11", "0:11", "0:08", "0:14", "0:08" )
opis <- c("Sleeping", "Work\n related", "Watching TV", "Leisure \n and sports \n (excludes TV)", "Household \n related",
          "Eating \n and drinking", "Personal \n care", "Shopping", "Caring for \nhousehold \n members", "Education",
          "Activities: \n civil, \n religious, \n organizational,  ", "Caring for \n non-household \n members", 
          "Phone call, \n email", "Other \n activities")
rok <- rep(c(2004, 2014), 14)

dataset <- data.frame(stringsAsFactors=FALSE, opis = rep(opis, each = 2), liczba_godzin, year = as.character(rok) )
as.POSIXct(hms::parse_hm(liczba_godzin))

dataset %>% 
  mutate(liczba_godzin = as.POSIXct(hms::parse_hm(liczba_godzin))) %>% 
  ggplot(aes(x = reorder(opis, c(1:28)), y = liczba_godzin, fill = year)) +
    geom_bar(stat = "identity", position = position_dodge())+
      xlab("")+
      ylab("")+
      ggtitle("Time spent on particular activities during the day")+
      theme_minimal()+
      theme(panel.grid.major.y = element_blank(),
            panel.grid.major.x = element_blank(),
            panel.grid.minor.x = element_blank(),
            panel.grid.minor.y = element_blank(),
        axis.text.x = element_text(color = "black"), axis.text.y = element_text(color = "black"),
            text = element_text(family = "lit", size = 12.5, color = "black"),
            panel.grid.minor.x = element_blank(),
        aspect.ratio = 1/4, legend.position = c(0.9, 0.8), legend.title = element_text(size = 11), 
            title = element_text(size = 12, face = "bold"))+
      scale_fill_manual(values = c("#002663", "#003c9e")) #, #001942
  
ggsave("czas_aktywnosci.png")


?as.POSIXct
?ggsave

