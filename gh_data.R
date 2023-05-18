library(tidyverse)
library(ggplot2)
library(ggthemes)
library(plotly)
library(showtext)
library(RCurl)
library(readxl)
library(readr)
library(RColorBrewer)

font_add_google(family = "Roboto", "Roboto")
showtext_auto()

x1 <- "https://raw.githubusercontent.com/shammu119/GuesthouseData/main/guesthouses1.csv"

x <- 1

ghdata <- read.csv(x1) %>% as_tibble() %>% arrange(Atoll, Island, desc(Rooms))

ghdata1 <- ghdata %>% slice_max(Rooms,n=15,with_ties = T)


p <-   ggplot(ghdata1,aes(x=Rooms,y=fct_reorder(Name,Rooms),fill=Island))+
  geom_col()+
    theme_light()+
  labs(
    y="",
    x=""
  )+
    scale_fill_brewer(palette="Dark2")+
    
    theme(
      #Panel Grid
      panel.grid.major.x = element_line(linewidth = 0.25, color = "#D9D9D9"),
      panel.grid.minor.x = element_blank(),
      panel.grid.minor.y = element_blank(),
      panel.grid.major.y = element_blank(),
      
      panel.background = element_rect(fill = "white"),
      panel.border = element_blank(),
      
      legend.title = element_blank(),
      # legend.margin = margin(c(5, 5, 5, 0)),
      legend.background = element_blank(),
      legend.box.background = element_blank(),
      legend.key.size = unit(6, "pt"),
      legend.position = "top",
      legend.justification = "left",
      legend.key = element_rect(colour = NA, fill = NA),
      
      
      legend.text = element_text(
        size = 12 * x,
        color = "#333333",
        margin = margin(r = 1, unit = "pt"),
        family = "Roboto"
      ),
      
      
      plot.margin = margin(20, 15, 20, 15, "pt"),
      
      
      #axis.ticks.length = unit(0.2, "cm"),
      #axis.ticks.y = element_line(color = "#D9D9D9",linewidth = 0.25),
      axis.ticks.y.left = element_blank(),
      axis.ticks.margin = unit(0, "cm"),
      axis.line.y = element_line(color = "#333333", linewidth = 0.25),
      
      plot.subtitle = element_text(
        family = "Roboto",
        face = "italic",
        hjust = 0,
        vjust = 0.5,
        color = "#333333",
        size = 10 * x
      ),
      
      plot.title = element_text(
        family = "Roboto",
        face = "bold",
        hjust = 0,
        vjust = 0.5,
        size = 12 * x
      ),
      
      axis.text.x = element_text(
        vjust = 0.5,
        family = "Roboto",
        face = "bold",
        size = 10 * x,
      ),
      axis.text.y = element_text(
        family = "Roboto",
        face = "bold",
        size = 10 * x,
        vjust = 0.5,
        hjust = 0
      ),
      plot.caption = element_text(
        hjust = 0,
        vjust = -4,
        family = "Roboto",
        size = 6 * x,
        face = "italic"
      )
    )
  
  # annotate("rect", xmin = "2022", xmax = "2025", ymin = -100, ymax = 50000,
  #       alpha = .1,fill = "blue")

  ggsave(
    p,
    filename = "gh.png",
    device = "png",
    scale = 1,
    width = 7,
    height = 5,
    dpi = 96,
    limitsize = F
  )

  