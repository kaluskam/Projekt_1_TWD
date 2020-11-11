library(dplyr)
library(tidyr)

library(showtext) # czcionka
fam <- "lit"
font_add_google(name = "Lato", family = fam)
showtext_auto()

library(ggplot2)
dt <- data.frame(x = c("The first", "The second", "The third", "The fourth", "The fifth", 
               "The sixth", "The seventh"), y = c(33.3, 23.0, 17.6, 11.6, 8.2, 4.2, 2.2))

dt$x <- factor(dt$x, levels = c("The first", "The second", "The third", "The fourth", "The fifth", 
                           "The sixth", "The seventh"))

ggplot(dt, aes(x = x, y = as.numeric(y))) +
  geom_bar(fill = "#003c9e", stat = "identity") + # alternatywny kolor #d5433d
  geom_text(aes(label = y), size = 4, vjust = 1.5, fontface = "bold", family = fam, color = "white") +
  ylab("%") +
  theme_minimal() +
  theme(
    text = element_text(size = 12.5, family = fam, face = "bold", color = "black"),
    axis.title.x = element_blank(),
    axis.text.y = element_blank(),
    axis.ticks.y = element_blank(),
    axis.text.x = element_text(color = "black"),
    panel.grid.major.y = element_blank(),
    panel.grid.major.x = element_blank(),
    panel.grid.minor.y = element_blank(),
    aspect.ratio = 1/4
)


library(packcircles)

# Create data
data <- data.frame(group=paste("Group", letters[1:7]), value=sample(seq(1,100),7), color = colors_2) 

# Generate the layout. This function return a dataframe with one line per bubble. 
# It gives its center (x and y) and its radius, proportional of the value
packing <- circleProgressiveLayout(data$value, sizetype='area')

# We can add these packing information to the initial data frame
data <- cbind(data, packing)

# Check that radius is proportional to value. We don't want a linear relationship, since it is the AREA that must be proportionnal to the value
# plot(data$radius, data$value)

# The next step is to go from one center + a radius to the coordinates of a circle that
# is drawn by a multitude of straight lines.
dat.gg <- circleLayoutVertices(packing, npoints=50)
ggplot() + 
  
  # Make the bubbles
  geom_polygon(data = dat.gg, aes(x, y, group = id, fill=as.factor(id)), colour = "black", alpha = 0.6) +
  
  # Add text in the center of each bubble + control its size
  geom_text(data = data, aes(x, y, size=value, label = group), fontface = "bold", family = fam) +
  scale_size_continuous(range = c(1,4)) +
  ggtitle("Size of each group") +
  
  # General theme:
  theme_void() + 
  theme(text = element_text(size = 14, family = fam, face = "bold"),
        legend.position = "none"
        ) +
  coord_equal()

# Make auxilliary data.frame
values <- data.frame("group" = factor(1:length(mapFrance$names)), "color" = color)

# Add color data to gg object by merging
gg$data <- merge(gg$data, values, by = c("group"))

# Plot gg object
gg + 
  geom_polygon(aes(x = long, y = lat, group = group),  fill = gg$data$color, colour = 1) + 
  coord_map(projection = "lambert", parameters = c(lat0 = 41.366005 , lat1 = 51.097523))

ggplot(data, aes(x = group, y = value)) +
  geom_bar(stat = "identity", fill = "#003c9e")  + # alternatywny kolor #d5433d
  #geom_text(aes(label = value), size = 4, vjust = 1.5, fontface = "bold", family = fam, color = "white") +
  ggtitle("Size of each group") +
  theme_minimal() +
  theme(
    text = element_text(size = 14, family = fam, face = "bold"),
    axis.title = element_blank(),
    axis.text.y = element_blank(),
    axis.ticks.y = element_blank(),
    panel.grid.major.y = element_blank(),
    panel.grid.major.x = element_blank(),
    panel.grid.minor.y = element_blank(),
    aspect.ratio = 1/4
  )