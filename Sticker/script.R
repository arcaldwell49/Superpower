library(ggplot2)

power_sticker <- ANOVA_power(design_sticker,
                             verbose = FALSE)
power_sticker2 <- power_sticker$plot1
power_sticker2 <- power_sticker2 + 
  scale_x_continuous("",expand = c(0, 0)) +
  scale_y_continuous("",expand = c(0, 0)) +
  annotate('text', x = 0.5, y = 75,
           label = "1~-~beta ",
           parse = TRUE,size=20) +
  theme(strip.background = element_blank(),
        strip.text.y = element_blank(),
        axis.text.x = element_blank(),
        axis.text.y = element_blank(),
        axis.ticks = element_blank()) + 
  #theme_void() + 
  theme_transparent()

library(showtext)
## Loading Google fonts (http://www.google.com/fonts)
font_add_google("Supermercado One", "supermercado")
## Automatically use showtext to render text for future devices
showtext_auto()
library(hexSticker)
sticker(
  power_sticker2,
  package = "Super",
  p_family = "supermercado",
  p_size = 24,
  s_x = .94,
  s_y = .95,
  s_width = 1.45,
  s_height = 1.45,
  h_color = "chocolate1"#,
  #filename = "baseplot.png"
)
