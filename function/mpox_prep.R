
pacman::p_load(tidyverse,readxl, rio, summarytools, knitr, kableExtra, rmarkdown)
options(digits=5)
library(sjmisc)
require(haven)
library(geomtextpath)
library(htmltools)
library(leaflet)
library(leaflet.extras)
library(leaflet.minicharts)
library(leafem)
require(whomapper)
require(whotools)
library(echarts4r)
library(labelled)
library(emojifont)
library(fontawesome)
library(echarty)
# library(ggradar)
# library(ggradar2)
library(janitor)
library(shinyflags)
library(quarto)
library(CoordinateCleaner)
library(htmlwidgets)
library(ggflags)
library(quarto)
library(here)
library(Hmisc)
library(echarts4r)
library(stringr)
library(stringi)
library(tidyverse)
library(dplyr)
library(purrr)
library(grid)
library(lubridate)
library(hrbrthemes)
library(glue)
library(gt)
library(gtExtras)
library(gtsummary)
library(ggthemes)
library(Biostrings)
library(ggplot2)
# library(ggalluvial)
library(scales)
library(RColorBrewer)
library(scales)
library(tibble)
# library(glitr)
library(patchwork)
library(ggnewscale)
library(rstatix)
library(flextable)
library(pastecs)
library(ggpubr)
library(PrettyCols)
library(ggtext)
library(data.table)
# theme(plot.subtitle=element_textbox_simple())
theme_box <- function(...) {
        theme_minimal() +
                theme(plot.title=element_textbox_simple(),
                      plot.subtitle=element_textbox_simple(),
                      # text = element_text(family = "Calibri", color = "#22211d",size=12),
                      axis.line = element_blank(),
                      axis.text.x = element_textbox_simple(),
                      axis.text.y = element_textbox_simple(),
                      # axis.ticks = element_blank(),
                      axis.title.x = element_textbox_simple(),
                      axis.title.y = element_textbox_simple(),
                      # panel.grid.minor = element_line(color = "#ebebe5", size = 0.2),
                      panel.grid.major = element_line(color = "#ebebe5", size = 0.2),
                      panel.grid.minor = element_blank(),
                      plot.background = element_rect(fill = "white", color = NA),
                      panel.background = element_blank(),
                      legend.background = element_rect(fill = "#f5f5f2", color = NA),
                      panel.border = element_blank(),
                      ...
                )
}

palettes <- ggthemes_data[["tableau"]][["color-palettes"]][["regular"]]
pal_tableau20=palettes$`Tableau 20`$value[1:20]
pal_tableau10=palettes$`Tableau 10`$value[1:10]
# pal_dunnr_pastel=dunnr::td_colors$pastel6
# pal_dunnr_dic=dunnr::td_colors$div5


## palettes
pal_who_darkblue=c("#E4E1EC","#B4ABC5","#5C4F80","#3A2E65","#262262")
pal_who_turquoise=c("#F1F9FA","#D3EBED","#8BCED3","#57BFC5","#26BDC0","#00a5a7")
pal_who_orange=c("#FEF5EB","#FDE1C1","#F9B66C","#F59F3D","#F39325","#CC5500")
pal_who_blue=c("#E9F2F8","#BBD8EA","#61A5CD","#358CBF","#009CDE","#002366")
pal_who_purple=c("#F3E5F5","#E1BEE7","#CE93D8","#BA68C8","#AB47BC","#9C27B0","#8E24AA","#7B1FA2","#6A1B9A")

#OSA DATA VISUALIZATION STYLE GUIDE
#https://github.com/USAID-OHA-SI/glitr

pal_osa_denims <-c("#BFDDFF","#A5C5FF","#8BADFE","#7396EE","#5B82D8","#436EC1","#265BAB","#074895","#00347D","#002065","#000C4F")
pal_osa_scooters <-c("#A6FDFF","#8CE4FE","#74CCEC","#5BB5D5","#419FBE","#228AA8","#047491","#005E7A","#004964","#00354f","#00223A")
pal_osa_oldroses <-c("#FFB5BA","#FF989F","#FC7A83","#EE636E","#D8505D","#C33C4C","#AF273D","#990D2E","#7F001C","#630005","#480000")
pal_osa_goldensand <-c("#FFEC6F","#FCCE52","#EAB538","#D2A01E","#BA8B00","#A27600","#8A6200","#734F00","#5E3C00","#4b2900","#3B1500")
pal_osa_moodyblues <-c("#E9DDFF","#CFC3FF","#B5AAF9","#9E94E0","#877EC9","#7069B2","#5A559B","#454185","#2F2E6F","#171d5a","#000A45")
pal_osa_genoas <-c("#A0F2E2","#89DACB","#72C3B4","#5CAC9E","#459688","#2D8073","#0D6C5F","#01564B","#004137","#002e24","#001B0E")
pal_osa_siennas <-c("#FFD4AC","#FFB790","#FD9873","#EC815D","#D56D4B","#BF5A39","#A84728","#923417","#7C2105","#670901","#4C0000")
pal_osa_greys <-c("#E6E7E8","#D5D7D8","#C4C6C8","#B3B5B8","#A3A4A8","#939598","#838484","#747576","#646568","#535356","#414042")
pal_osa_oranges <-c("#FFD4AC","#FFB790","#FD9873","#EC815D","#D56D4B")
pal_osa_yellows <-c("#f9ffd0","#f7fea1","#f7fd71","#FFEC6F","#fcce10")

pal_kelly=c("#f3c300", "#875692", "#f38400", "#a1caf1", "#be0032", "#c2b280", 
            "#848482", "#008856", "#e68fac", "#0067a5", "#f99379", "#604e97", 
            "#f6a600", "#b3446c", "#dcd300", "#882d17", "#8db600", "#654522", 
            "#e25822", "#2b3d26", "lightgrey")

values=c("Senegal","Zambia","DRC","Civlian","Military","Sex worker")
groups=c(rep("Countries",3),rep("Status",3))
colors=c("#D1322B","gold","#0E5AA4","#75A1D3","#1A553F" ,"#D16E73")
led_df<- data.frame(cbind(groups,
                          values,
                          colors))

pal_divergence_blues=c("#436ec1","#8badfe","#bfddff","#FEE08B","#FC8D59","#D73027")
pal_divergence_greens=c("#1A9850","#91CF60","#D9EF8B","#FEE08B","#FC8D59","#D73027")

pal_divergence_blues2=c("#bfddff","#FFFFBF", "#FEE08B","#FDAE61","#F46D43","#D53E4F")


pal_spectral=c("#9E0142", "#D53E4F", "#F46D43", "#FDAE61", "#FEE08B", "#FFFFBF", 
               "#E6F598", "#ABDDA4", "#66C2A5", "#3288BD", "#5E4FA2")
pal_divergence=c("#1A9850","#91CF60","#D9EF8B","#FEE08B","#FC8D59","#D73027")
pal3=c( "#c1232b", "#27727b", "#fcce10", "#e87c25", "#b5c334", "#fe8463", "#9bca63", "#fad860", "#f3a43b", "#60c0dd", "#d7504b", "#c6e579","#f4e001", "#f0805a", "#26c0c0")
pal_classic=c('#5470c6', '#91cc75', '#fac858', '#ee6666', '#73c0de', '#3ba272', '#fc8452', '#9a60b4', '#ea7ccc')
pal_vintage=c("#d87c7c","#919e8b","#d7ab82","#6e7074","#61a0a8","#efa18d","#787464","#cc7e63","#724e58","#4b565b")
pal_dark=c("#dd6b66","#759aa0","#e69d87","#8dc1a9","#ea7e53","#eedd78","#73a373","#73b9bc","#7289ab","#91ca8c","#f49f42")
pal_westeros=c("#2ec7c9","#b6a2de","#5ab1ef","#ffb980","#d87a80","#8d98b3","#e5cf0d","#97b552","#95706d","#dc69aa","#07a2a4","#9a7fd1","#588dd5","#f5994e","#c05050","#59678c","#c9ab00","#7eb00a","#6f5553","#c14089")
pal_essos=c("#893448", "#d95850", "#eb8146", "#ffb248", "#f2d643", "#ebdba4")
pal_wonderland=c("#4ea397","#22c3aa","#7bd9a5","#d0648a","#f58db2","#f2b3c9")
pal_halloween=c("#ff715e","#ffaf51","#ffee51","#8c6ac4","#715c87")
pal_shine=c("#c12e34","#e6b600","#0098d9","#2b821d","#005eaa","#339ca8","#cda819","#32a487")

pal_marine=c("#6e7f80","#536872","#708090","#536878","#36454f")
pal_aqua=c("#c9f0d3","#9ad1c4","#77b1a9","#5f867a","#4b5d67")
pal_reds=c("#ffb8b8","#ed8d8d","#ce5a5a","#b53737","#970c0c")
pal_reds2=c("#F8B195","#F67280","#C06C84","#6C5B7B","#355C7D")
pal_blues <-c("#A5C5FF","#7396EE","#5B82D8","#074895","#000C4F")
pal_roses = c("#ffb5ba", "#ff989f", "#fc7a83", "#ee636e", "#c33c4c")
pal_ocean=c("#c0d2d9","#a1bcc6","#82a6b3","#6390a1","#4f7380")

colorpalette_list<-list(c("#c9f0d3","#9ad1c4","#77b1a9","#5f867a","#4b5d67"),
                        c("#6e7f80","#536872","#708090","#536878","#36454f"),
                        c("#e4e1f1","#c9c3e3","#ada6d5","#9288c7","#776ab9"),
                        c("#92b3ba","#7197a0","#497987","#3c6770","#244f58"),
                        c("#ffb8b8","#ed8d8d","#ce5a5a","#b53737","#970c0c"),
                        c("#eee3e7","#ead5dc","#eec9d2","#f4b6c2","#f6abb6"),
                        c("#ffe9dc","#fce9db","#e0a899","#dfa290","#c99789"),
                        c("#F8B195","#F67280","#C06C84","#6C5B7B","#355C7D"),
                        c("#E5FCC2","#9DE0AD","#45ADA8","#547980","#594F4F"))

# SIEI recommended colors
siei = c("#2057a7", "#c43d4d", "#8980cb", "#e07653", "#1e87a5", "#f2bc40", "#287c6f", "#808080")

# SIEI recommended categorical palettes
denim = c("#2057a7","#1e87a5", "#e07653", "#f2bc40", "#8980cb")
old_rose = c("#c43d4d", "#1e87a5", "#8980cb", "#e07653", "#287c6f")
moody_blue = c("#8980cb", "#287c6f", "#2057a7", "#e07653", "#1e87a5")
burnt_sienna = c("#e07653", "#8980cb", "#f2bc40", "#c43d4d", "#1e87a5")
scooter = c("#1e87a5", "#c43d4d", "#8980cb", "#f2bc40", "#287c6f")
golden_sand = c("#f2bc40", "#2057a7", "#287c6f", "#e07653", "#8980cb")
genoa = c("#287c6f", "#8980cb", "#f2bc40", "#e07653", "#1e87a5")
siei_achv = c("#FF939A", "#FFCAA2", "#5BB5D5", "#E6E6E6")

# SIEI recommended palettes build from base colors
siei_pairs = c("#2057a7", "#BFDDFF", "#c43d4d", "#FF939A", "#8980cb", "#DFD3FF", "#e07653", "#FFCAA2", "#1e87a5", "#83DBFB", "#f2bc40", "#FFDDA2", "#287c6f", "#7ECFC0", "#808080", "#E6E6E6")
denims = c("#bfddff", "#a5c5ff", "#8badfe", "#7396ee", "#5b82d8", "#436ec1", "#265bab", "#074895", "#00347d", "#002065", "#000c4f")

old_roses = c("#ffb5ba", "#ff989f", "#fc7a83", "#ee636e", "#d8505d", "#c33c4c", "#af273d", "#990d2e", "#7f001c", "#630005", "#480000")
moody_blues = c("#e9ddff", "#cfc3ff", "#b5aaf9", "#9e94e0", "#877ec9", "#7069b2", "#5a559b", "#454185", "#2f2e6f", "#171d5a", "#000a45")
burnt_siennas = c("#ffd4ac", "#ffb790", "#fd9873", "#ec815d", "#d56d4b", "#bf5a39", "#a84728", "#923417", "#7c2105", "#670901", "#4c0000")
scooters = c("#a6fdff", "#8ce4fe", "#74ccec", "#5bb5d5", "#419fbe", "#228aa8", "#047491", "#005e7a", "#004964", "#00354f", "#00223a")
golden_sands = c("#ffec6f", "#fcce52", "#eab538", "#d2a01e", "#ba8b00", "#a27600", "#8a6200", "#734f00", "#5e3c00", "#4b2900", "#3b1500")
genoas = c("#a0f2e2", "#89dacb", "#72c3b4", "#5cac9e", "#459688", "#2d8073", "#0d6c5f", "#01564b", "#004137", "#002e24", "#001b0e")
trolley_greys = c("#E6E7E8", "#D5D7D8", "#C4C6C8", "#B3B5B8", "#A3A4A8", "#939598", "#838484", "#747576", "#646568", "#535356", "#414042")
usaid_colors = c("#002a6c", "#ba0c2f", "#212721", "#0067b9", "#a7c6ed", "#6c6463", "#8C8985", "#cfcdc9")
contrast = c("#E4F4EA", "#364352", "#768491", "#C5CAD0", "#BE311F")
compliment = c("#6F472E", "#6F827C", "#E4F4EA", "#E4F4EA", "#E4F4EA")
outbreak_or = c("#F4E5D2", "#FED79C", "#FBA529", "#FF6600")
seablue = c("#4FAFB6", "#98CBD0", "#D5EAEB", "#086EA1")
seablue_text = c("#00949D", "#58B2BA", "#B2D8DD", "#2476A6")

# Vega palettes
category10 = c(
        "#1f77b4", "#ff7f0e", "#2ca02c", "#d62728", "#9467bd", "#8c564b",
        "#e377c2", "#7f7f7f", "#bcbd22", "#17becf"
)

category20 = c(
        "#1f77b4", "#aec7e8", "#ff7f0e", "#ffbb78", "#2ca02c", "#98df8a",
        "#d62728", "#ff9896", "#9467bd", "#c5b0d5", "#8c564b", "#c49c94",
        "#e377c2", "#f7b6d2","#7f7f7f", "#c7c7c7", "#bcbd22", "#dbdb8d",
        "#17becf", "#9edae5"
)

category20b = c(
        "#393b79", "#5254a3", "#6b6ecf", "#9c9ede", "#637939",  "#8ca252",
        "#b5cf6b", "#cedb9c", "#8c6d31", "#bd9e39", "#e7ba52", "#e7cb94",
        "#843c39", "#ad494a", "#d6616b", "#e7969c", "#7b4173",
        "#a55194", "#ce6dbd", "#de9ed6")
category20c = c(
        "#3182bd", "#6baed6", "#9ecae1", "#c6dbef", "#e6550d", "#fd8d3c",
        "#fdae6b", "#fdd0a2", "#31a354", "#74c476", "#a1d99b", "#c7e9c0",
        "#756bb1", "#9e9ac8", "#bcbddc", "#dadaeb", "#636363", "#969696",
        "#bdbdbd", "#d9d9d9")

# Cartodb colors
carto_burgyl = c("#fbe6c5", "#f5ba98", "#ee8a82", "#dc7176", "#c8586c", "#9c3f5d", "#70284a")
carto_sunset = c("#fcde9c", "#faa476", "#f0746e", "#e34f6f", "#dc3977", "#b9257a", "#7c1d6f")
carto_teal = c("#d1eeea", "#a8dbd9", "#85c4c9", "#68abb8", "#4f90a6", "#3b738f", "#2a5674")
carto_dmint = c("#d2fbd4", "#a5dbc2", "#7bbcb0", "#559c9e", "#3a7c89", "#235d72", "#123f5a")
carto_mint = c("#e4f1e1", "#b4d9cc", "#89c0b6", "#63a6a0", "#448c8a", "#287274", "#0d585f")
carto_brown = c("#ede5cf", "#e0c2a2", "#d39c83", "#c1766f", "#a65461", "#813753", "#541f3f")

# Carto diverging
carto_div_armyrose = c("#798234", "#a3ad62", "#d0d3a2", "#fdfbe4", "#f0c6c3", "#df91a3", "#d46780")
carto_div_fall = c("#3d5941", "#778868", "#b5b991", "#f6edbd", "#edbb8a", "#de8a5a", "#ca562c")
carto_div_geyser = c("#008080", "#70a494", "#b4c8a8", "#f6edbd", "#edbb8a", "#de8a5a", "#ca562c")
carto_div_temps = c("#009392", "#39b185", "#9ccb86", "#e9e29c", "#eeb479", "#e88471", "#cf597e")
carto_div_teal = c("#009392", "#72aaa1", "#b1c7b3", "#f1eac8", "#e5b9ad", "#d98994", "#d0587e")
carto_div_tropic = c("#009B9E", "#42B7B9", "#A7D3D4", "#F1F1F1", "#E4C1D9", "#D691C1", "#C75DAB")
carto_div_earth = c("#A16928", "#bd925a", "#d6bd8d", "#edeac2", "#b5c8b8", "#79a7ac", "#2887a1")

#Pantone Colors of the Year (descending)
pantone = c("#FEBE98", "#BE3455", "#6768AB", "#F5DF4D", "#939597", "#0F4C81",
            "#FF6F61", "#5F4B8B", "#88B04B", "#F7CACA", "#93A9D1", "#964F4C",
            "#AD5E99", "#009473", "#DD4124", "#45B5AA", "#D94F70", "#F0C05A",
            "#5A5B9F", "#9B1B30", "#DECDBE", "#53B0AE", "#E2583E", "#7BC4C4",
            "#BF1932", "#C74375", "#9BB7D4")

tmp<- whomapper::pull_who_logo('HQ')

na_strings <- c("NA", "N A", "N / A", "N/A", "N/ A", "Not Available", "NOt available", "")

date_start="2022-01-01"
date_end=Sys.Date()

as_of_date=Sys.Date()
today=as_of_date
base_size = 24
