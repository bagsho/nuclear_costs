library(tidyverse)
library(hrbrthemes)
library(readxl)

original_data <- read_excel(path="data/original_data.xlsx", 
                            col_types = c("numeric", "numeric", "numeric","numeric"),
                            col_names = c("announcement", "commissioning", "years","cost"),
                            skip=1
                            )

#yılların farklı renklerle gösterimi - ilk deneme

original_data |> 
  ggplot()+
  geom_segment(aes(x=cost,xend=cost,y=announcement,yend=commissioning,colour=announcement))+
  coord_flip()+
  scale_colour_gradient(low = "yellow", high = "red")

# aynı maliyet olan yılları farklı şekillerle gösterdiğim versiyon

original_data |> 
  mutate(shape_id=1) |>
  mutate(shape_id=if_else(announcement==2014,2,1)) |> 
  mutate(cost=if_else(announcement==2014,cost+0.1,cost)) |> 
  ggplot()+
  geom_segment(aes(x=cost,xend=cost,y=announcement,yend=commissioning))+
  geom_point( aes(x=cost, y=announcement,shape = factor(shape_id)), color=rgb(0.2,0.7,0.1,0.5), size=3 ) +
  geom_point( aes(x=cost, y=commissioning,shape = factor(shape_id)), color=rgb(0.7,0.2,0.1,0.5), size=3 ) +
  coord_flip()

# burası esas uğraştığım grafik

original_data |> 
  mutate(cost=if_else(announcement==2014|announcement==2019,cost+0.05,cost)) |> 
  ggplot()+
  geom_segment(aes(x=cost,xend=cost,y=announcement,yend=commissioning))+
  geom_point( aes(x=cost, y=announcement), color=rgb(0.2,0.7,0.1,0.5), size=3 ) +
  geom_point( aes(x=cost, y=commissioning), color=rgb(0.7,0.2,0.1,0.5), size=3 ) +
  #xlim(0,14)+
  scale_x_continuous(
    limits = c(0, 14),
    breaks = seq(from = 0, to = 14, by = 2),
    minor_breaks = seq(0, 14, 2))+
  scale_y_continuous(
    limits = c(2005, 2025),
    breaks = seq(from = 2005, to = 2025, by = 5),
    minor_breaks = seq(2005, 2025, 5))+
  coord_flip()+
  theme_ipsum() +
  theme(legend.position = "none",
        axis.title.x=element_blank(),
        axis.ticks=element_blank()
  ) 

# yıllara göre maliyet artışını hedef alan görselleştirme

original_data |> 
  ggplot(aes(x=announcement,y=cost))+
  geom_line( color="grey") +
  geom_point(aes(size=years,fill=cost),shape=21, color="black", size=3)+
  scale_fill_gradient(low = "yellow", high = "red")+
  theme_ipsum() +
  theme(
    legend.position = "none",
  ) +
  #geom_smooth(method=lm , color="red", fill="#69b3a2", se=TRUE) +
  ggtitle("Flamanville Unit 3 costs and delays")
