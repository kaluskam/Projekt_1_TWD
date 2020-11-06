library(ggplot2)
options(repr.plot.width = 8, repr.plot.height = 8)

# Pakiet do czcionek
library(showtext)
fam <- "lit"

# można wygodnie brać z Google Fonts 
# name -> nazwa z GF
# family -> to co się później w ggplot wpisuje więc lepiej zmienną zrobić
font_add_google(name = "Lato", family = fam)

showtext_auto()

presidents <- c("Ronald\n Reagan", "George\n Bush", "Bill\n Clinton",
                "George\n W. Bush", "Barack\n Obama", "Donald\n Trump")

judgeships <- c(19, 18, 18, 16, 15, 24)

df <- data.frame(presidents, judgeships)
df

ggplot(data = df, aes(x = factor(presidents, level = presidents), y = judgeships)) +
    geom_bar(stat = "identity", width = 0.6, fill = "#a31f1c") +
    geom_text(aes(label = judgeships), vjust = 2, size = 11, fontface = "bold", family = fam, color = "white") + # numerki w słupach
    theme_minimal() + # to chyba ważne
    ggtitle("Appellate judgeships confirmed during \n first congressional term") + 
    theme(text = element_text(family = fam), 
        axis.title.y = element_blank(), # wylacznie pospisu osi y
        axis.text.y = element_blank(), # tekstu y
        axis.ticks.y = element_blank(), # tickow y
        axis.title.x = element_blank(), # podpisu osi x
        axis.text.x = element_text(size = 20, vjust = 6, face = "bold"), # imienia i nazwiska
        axis.ticks.x = element_blank(), # tez zbedne
        panel.grid.major.x = element_blank(), 
        panel.grid.major.y = element_blank(),
        panel.grid.minor.y = element_blank(),
        plot.title = element_text(color = "black", face = "bold", size = 30, hjust = 0.5, vjust = 1.5)) +
    ggsave(filename = "gavels.png", dpi = 192, width = 8, height = 8) # z tym dpi to nie wiem jakie jest optymalne
