library(ggplot2)
library(Superpower)
library(reshape2)
library(ggplot2)
library(hexSticker)
library(showtext)

## Loading Google fonts (http://www.google.com/fonts)
font_add_google("Supermercado One", "supermercado")
font_add_google("Modak", "modak")
font_add_google("Play", "play")
## Automatically use showtext to render text for future devices
showtext_auto()

sticker_design <- ANOVA_design("2b", n = 30,
                               sd = 1,
                               mu = c(0,.55),
                               plot = FALSE)

power_sticker <- ANOVA_power(sticker_design,
                             verbose = FALSE)



power_sticker2 = ggplot(power_sticker$sim_data, aes(x = anova_a)) +
  scale_x_continuous("", limits=c(0,.5)) + #expand = c(0, 0),
  scale_y_continuous("",limits=c(0,10.5)) +
  annotate('text', x = 0.25, y = 9.5,
           label = "1-beta ",
           parse = TRUE,size=20,
           color = "white") +
  geom_density() +
  geom_segment(aes(x=.05,xend=.05,y=0,yend=8.9), color = "chocolate1") +
  #labs(title = "1 - \U03B2") +
  theme_void() + 
  theme_transparent() +
  theme(
    strip.background = element_blank(),
    strip.text.y = element_blank(),
    axis.text.x = element_blank(),
    axis.text.y = element_blank(),
    axis.ticks = element_blank(),
    text=element_text(family = "play")
  ) 


#plot.title = element_text(size = 65, 
#                          hjust = .5,
#                          color = "white",
#                          margin=margin(0,0,0,0)),


#Plot p-value dist


#power_sticker2 <- power_sticker$plot1
#power_sticker2 <- power_sticker2 + 
#  scale_x_continuous("",expand = c(0, 0)) +
#  scale_y_continuous("",expand = c(0, 0)) +
#  annotate('text', x = 0.5, y = 75,
#           label = "1~-~beta ",
#           parse = TRUE,size=20) +
#  theme(strip.background = element_blank(),
#        strip.text.y = element_blank(),
#        axis.text.x = element_blank(),
#        axis.text.y = element_blank(),
#        axis.ticks = element_blank()) + 
  #theme_void() + 
#  theme_transparent()




sticker(
  power_sticker2,
  package = "Super",
  p_family = "play",
  p_size = 24,
  s_x = .99,
  s_y = .80,
  s_width = .9,
  s_height = .9,
  h_color = "chocolate1"#, #blue4 #chocolate1
  #filename = "baseplot.png"
)
