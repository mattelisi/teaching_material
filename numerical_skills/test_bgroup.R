

#grDevices::png(filename = "test_3.png")
plot(0, xlim = c(0, 1), ylim = c(0, 1))
text(0.5, 0.5, expression(bgroup('(',atop(x,y),')')))
#dev.off()


xaringanExtra::use_xaringan_extra(c("tile_view", "animate_css", "tachyons"))

# css: [rladies-fonts, default, custom/style.css]
library(xaringanthemer)
style_solarized_light(colors = c(
  red = "#f34213",
  purple = "#3e2f5b",
  orange = "#ff8811",
  green = "#136f63",
  white = "#FFFFFF",
  blue = " #0000FF"
))

library(ggplot2)
nice_theme <- theme_xaringan()+theme(text=element_text(size=9),panel.border=element_blank(),strip.text=element_text(size=rel(0.8)),axis.text=element_text(size=8),panel.grid.minor=element_blank(),axis.line=element_line(size=.4), axis.title=element_text(size=11), legend.title=element_text(size=11))
theme_set(nice_theme)

library(showtext)
showtext_auto(FALSE)
showtext_auto(TRUE)
plot(0, xlim = c(0, 1), ylim = c(0, 1))
text(0.5, 0.5, expression(bgroup('(',atop(x,y),')')))
