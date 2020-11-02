library(ggplot2)


# Funkcja do zawijania tytułu
wrapper <- function(x, ...) 
{
  paste(strwrap(x, ...), collapse = "\n")
}


## Dane do wykresu
liczba_osob <- c(0.589, 0.029, 0.01, 0.231, 0.021, 0.023, 0.061, 0.012, 0.024)
opis = c("wypoczynek, rekreacja, wakacje", "zwiedzanie", "uprawianie turystyki kwalifikowanej", 
         "odwiedziny u krewnych lub znajomych", "uroczystości rodzinne", "zdobywanie umiejętności, kształcenie",
         "zdrowotny", "religijny", "inny")
dane <- data.frame(opis, liczba_osob)



## Odtworzenie wykresu - wersja pierwotna
ggplot(dane, aes(x = "", y = liczba_osob, fill = opis))+
  geom_bar(stat = "identity")+
  coord_polar(theta = "y", start = 0, clip = "off")+
  ggtitle(wrapper("Krajowe wyjazdy wakacyjne (min 5 dni) zrealizowane w gospodarstwach domowych wg celu wyjazdu", width = 52))+
  theme_void()+
  scale_fill_discrete(breaks=c("wypoczynek, rekreacja, wakacje", "zwiedzanie", "uprawianie turystyki kwalifikowanej", 
                               "odwiedziny u krewnych lub znajomych", "uroczystości rodzinne", "zdobywanie umiejętności, kształcenie",
                               "zdrowotny", "religijny", "inny"))+
  theme(legend.title = element_blank())+
  scale_fill_manual(values = c("#000080", "#66CDAA", "#778899", "#BDB76B", "#800080", "#7B68EE", "#F4A460", "#008B8B", "#C71585"))
  
  
?coord_polar

## Wykres poprawiony
ggplot(dane, aes(x = liczba_osob, y = reorder(opis, c((9:1))), fill = opis))+
  geom_bar(stat = "identity", position = "dodge")+
  theme(legend.position = "none")+
  theme(plot.title = element_text(hjust = 1.5))+
  ggtitle(wrapper("Krajowe wyjazdy wakacyjne (min 5 dni) zrealizowane w gospodarstwach domowych wg celu wyjazdu", width = 52))+
  scale_x_continuous(limits = c(0, 0.7), labels = 
                       scales::percent_format(scale = 100, accuracy = 0.1,))+
  geom_text(aes(label=scales::percent(liczba_osob)), position=position_dodge(width=1), 
            vjust=-0.25, hjust = -0.1)+
  ylab("")+
  xlab("")+
  scale_fill_manual(values = c("#000080", "#66CDAA", "#778899", "#BDB76B", "#800080", "#7B68EE", "#F4A460", "#008B8B", "#C71585"))

ggsave("wyjazdy_wakacyjne_wykres.png")

  