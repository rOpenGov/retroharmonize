library(hexSticker)
library(showtext)
font_add_google("Roboto")

imgurl <- "vignettes/ab_plot1.png"

sticker(imgurl, 
             package="retroharmonize", 
        p_size=14, 
        s_x=1, 
        s_y=.85, 
        s_width=.55,
         p_family= "Roboto",
        filename="logo.png")
