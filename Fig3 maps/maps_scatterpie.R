### let's try with scatterpie
#install.packages('yulab.utils')
#devtools::install_github("GuangchuangYu/scatterpie")
library(scatterpie)
library(ggmap) ## to draw the map
library(openxlsx)
library(tidyr)
library(dplyr)
library(ggrepel)

## get colors right in case some have zero zero observations
mycolors <- c("mediumpurple", "#4477AA", "#F0E442")
names(mycolors) <- c("C", "S", "W")

## get haplotype data
## read the data ## coordinates and ratios of different haplogroups
#####
coord_data <- read.xlsx("../Fig1 net/Haplogroups Emar.xlsx")
## get latitude and longitude from coordinate string
gsub(" E", "", coord_data$coordinate) %>% strsplit(" N, ") -> latlon
sapply(latlon, "[", 1) %>% as.numeric() -> lat
sapply(latlon, "[", 2) %>% as.numeric() -> lon
## and put them back to the main data frame
coord_data$lat <- lat; coord_data$lon <- lon
## reformat data, count haplogroups, and prepare pies
#####
gather(coord_data, key, value, -c(lat, lon, coordinate, coast, spot, abbreviation, sample, date, primers)) %>% 
  count(coordinate, spot, coast, abbreviation, lat, lon, key, value) %>% 
  spread(value, n, fill = 0) -> pie_data_haplogroup


#ggplot() + geom_scatterpie(aes(x=lon, y=lat, group=spot), data=pie_data_haplogroup,
#                           cols=c("C", "W", "S")) + coord_equal()

bigbox <- c(left=92, right=112, bottom=51, top=57)
bigMap <- get_stadiamap(bigbox, zoom=8, maptype = "stamen_terrain_background", source = "stadia", filename = "bigmap") 

pie_data_haplogroup$sample_size <- pie_data_haplogroup$C + pie_data_haplogroup$W + pie_data_haplogroup$S

ggmap(bigMap) + 
#  geom_scatterpie(data=pie_data_haplogroup, aes(x=lon, y=lat, group=spot), pie_scale = .4, #r=0.05*sample_size), 
#                    cols=c("C", "S", "W"), col='white') + #, alpha=0.3) +
  scale_fill_manual(values = c("mediumpurple", "#4477AA", "#F0E442")) + 
#  geom_label_repel(data=pie_data_haplogroup, aes(x=lon, y=lat, label = abbreviation), #lon+adjust
#                   label.padding = unit(0.1, "lines"), alpha=1, label.size=NA) +
  coord_quickmap() -> largeMap
largeMap



#library(scatterbar)
#scatterbar(data = pie_data_haplogroup)


## Yenisey
Ybox <- c(left=92.2, right=93.6, bottom=55.8, top=56.2)
YMap <- get_stadiamap(Ybox, zoom=10, maptype = "stamen_terrain_background", source = "stadia", filename = "ymap") 



ggmap(YMap) + 
  geom_scatterpie(data=pie_data_haplogroup, aes(x=lon, y=lat, group=spot), pie_scale = .2,#, r=0.05*sample_size), 
                  cols=c("C", "S", "W"), color="white") + #, alpha=0.3) +
  scale_fill_manual(values = mycolors) + 
  geom_label_repel(data=pie_data_haplogroup, aes(x=lon, y=lat, label = abbreviation), #lon+adjust
                   label.padding = unit(0.1, "lines"), alpha=1, label.size=NA) +
  coord_quickmap() -> Ymap
Ymap

## Angara
Abox <- c(left=104.2, right=105.2, bottom=51.7, top=52.2)
AMap <- get_stadiamap(Abox, zoom=10, maptype = "stamen_terrain_background", source = "stadia", filename = "ymap") 

ggmap(AMap) + 
  geom_scatterpie(data=pie_data_haplogroup, aes(x=lon, y=lat, group=spot), pie_scale = .1,#, r=0.05*sample_size), 
                  cols=c("C", "S", "W"), col="white") + #, alpha=0.3) +
  scale_fill_manual(values = c("mediumpurple", "#4477AA", "#F0E442")) + 
  geom_label_repel(data=pie_data_haplogroup, aes(x=lon, y=lat, label = abbreviation), #lon+adjust
                   label.padding = unit(0.1, "lines"), alpha=1, label.size=NA) +
  coord_quickmap() -> Amap
Amap

## Irkutsk
Irkbox <- c(left=104.2, right=104.4, bottom=52.2, top=52.3)
IrkMap <- get_stadiamap(Irkbox, zoom=13, maptype = "stamen_terrain_background", source = "stadia", filename = "ymap") 

ggmap(IrkMap) + 
  geom_scatterpie(data=pie_data_haplogroup, aes(x=lon, y=lat, group=spot), pie_scale = .02,   #r=0.001*sample_size
                  cols=c("C", "S", "W"), col="white") + #, alpha=0.3) +
  scale_fill_manual(values = c("mediumpurple", "#4477AA", "#F0E442")) + 
  geom_label_repel(data=pie_data_haplogroup, aes(x=lon, y=lat, label = abbreviation), #lon+adjust
             label.padding = unit(0.1, "lines"), alpha=1, label.size=NA) +
  coord_quickmap() -> Irkmap
Irkmap


## Olkhon
Obox <- c(left=106.7, right=107.2, bottom=53, top=53.2)
OMap <- get_stadiamap(Obox, zoom=13, maptype = "stamen_terrain_background", source = "stadia", filename = "ymap") 

ggmap(OMap) + 
  geom_scatterpie(data=pie_data_haplogroup, aes(x=lon, y=lat, group=spot), pie_scale = .05,   #r=0.001*sample_size
                  cols=c("C", "S", "W"), col="white") + #, alpha=0.3) +
  scale_fill_manual(values = c("mediumpurple", "#4477AA", "#F0E442")) + 
  geom_label_repel(data=pie_data_haplogroup, aes(x=lon, y=lat, label = abbreviation), #lon+adjust
                   label.padding = unit(0.1, "lines"), alpha=1, label.size=NA) +
  coord_quickmap() -> Omap
Omap



library(ggpubr)
ggarrange(Ymap, Amap, Irkmap, Omap)
ggsave("inlets.svg", width = 10, height = 10, device=svg)


largeMap + 
  geom_rect(xmin=Irkbox["left"], xmax=Irkbox["right"], ymin=Irkbox["bottom"], ymax=Irkbox["top"], colour = "white", fill = "NA") + 
  geom_rect(xmin=Abox["left"], xmax=Abox["right"], ymin=Abox["bottom"], ymax=Abox["top"], colour = "white", fill = "NA") +
  geom_rect(xmin=Ybox["left"], xmax=Ybox["right"], ymin=Ybox["bottom"], ymax=Ybox["top"], colour = "white", fill = "NA") + 
  geom_rect(xmin=Obox["left"], xmax=Obox["right"], ymin=Obox["bottom"], ymax=Obox["top"], colour = "white", fill = "NA")

ggsave("largeMap.svg", device=svg(), width=8, height=8)

# bigMap_data <- ggplot_build(p)
# bigMap_data$data
# #  coord_sf(crs = st_crs(4326))
# 
# library(rosm)
# 
# bounds <- wk::rct(
#   -7476083, 5349058,
#   -6594103, 6243203,
#   crs = osm_crs_native())
# 
# (grd <- osm_raster(bounds, osm_url_spec_example()))
# #> <wk_grd_rct [768 x 768] => [-7514066 5322463 -6574807 6261721] with crs=EPSG:3857>
# #> List of 2
# #>  $ data: 'nativeRaster' int [1:768, 1:768] -2108502 -2108502 -2108502 -2108502 -2108502 -2108502 -2108502 -2108502 -2108502 -2108502 ...
# #>  $ bbox: wk_rct[1:1] [-7514066 5322463 -6574807 6261721]
# #>  - attr(*, "class")= chr [1:2] "wk_grd_rct" "wk_grd"
# plot(grd)
# ggplot() + geom_raster(data = grd)
# 
# 
# world <- map_data('world')
# 
# ggplot(world, aes(long, lat)) +
#   geom_map(map=world, aes(map_id=region), fill=NA, color="black") +
#   geom_scatterpie(aes(x=lon, y=lat, group=spot), data=pie_data_haplogroup, cols=c("C", "W", "S"), pie_scale = 10) + 
#   coord_sf() #coord_sf and coord_quickmap both work fine
# 
# 
# ##https://stackoverflow.com/questions/70823068/scatterpie-pie-plot-circles-not-properly-positioned-over-map
# 
# irkbox <- c(left=104.19, right=104.41, bottom=52.18, top=52.32)
# IrkMap <- get_stadiamap(irkbox, zoom=5, maptype = "stamen_terrain_background", source = "stadia", filename = "irkmap") 
# 
# 
# 
# ################
# 
# 
# library(ggplot2) ## for drawing in general; ggmap should load it but still...
# library(openxlsx) ## to read the data in Excel format
# library(tidyr); library(dplyr) ## for data manipulation
# library(reshape2) ## for the melt function melting data
# library(grid) ## for inset
# library(ggpubr) ## isn't used in the final code but needed if trying to combine
# library(scales) ##for pretty breaks
# ## Loosely based on this:
# ## https://stackoverflow.com/questions/36063043/how-to-plot-barchart-onto-ggplot2-map
# 
# 
# ## useful in-house functions to make pies
# construct_pie_list <- function(pie_data_haplogroup, coord_box, size, adj.col) {
#   lapply(1:length(unique(pie_data_haplogroup$coordinate)), function(x) 
#     if (between(pie_data_haplogroup[x, "lon"],  coord_box[1], coord_box[2]) & 
#         between(pie_data_haplogroup[x, "lat"],  coord_box[3], coord_box[4])) {
#       inset(pie.testplot_list[[x]], 
#             xmin = pie_data_haplogroup[x, "lon"] - size + pie_data_haplogroup[x, adj.col],
#             xmax = pie_data_haplogroup[x, "lon"] + size + pie_data_haplogroup[x, adj.col],
#             ymin = pie_data_haplogroup[x, "lat"] - size,
#             ymax = pie_data_haplogroup[x, "lat"] + size)}) 
# }
# 
# ## ggplot theme
# #####
# mytheme <- theme_set(theme_bw(base_size = 12) + 
#                        theme(text=element_text(family="Arial"))) + 
#   theme(plot.title = element_text(hjust = 0.5, size=14)) 
# 
# 
# ## read the data ## coordinates and ratios of different haplogroups
# #####
# coord_data <- read.xlsx("./Haplogroups Emar.xlsx")
# ## get latitude and longitude from coordinate string
# gsub(" E", "", coord_data$coordinate) %>% strsplit(" N, ") -> latlon
# sapply(latlon, "[", 1) %>% as.numeric() -> lat
# sapply(latlon, "[", 2) %>% as.numeric() -> lon
# ## and put them back to the main data frame
# coord_data$lat <- lat; coord_data$lon <- lon
# 
# 
# ## reformat data, count haplogroups, and prepare pies
# #####
# gather(coord_data[,c("coordinate", "spot", "coast", "lat", "lon", "haplogroup_coi")], key, value, -c(lat, lon, coordinate, coast, spot)) %>% 
#   count(coordinate, spot, coast, lat, lon, key, value) %>% 
#   spread(value, n, fill = 0) -> pie_data_haplogroup
# ## pie_data_haplogroup: melt data to plot
# pie_data_haplogroupm <- melt(pie_data_haplogroup, measure.vars = list("C", "S", "W"))
# ## decide how much to adjust coordinate for the pies
# # pie_data_haplogroup$adjust.B <- ifelse(pie_data_haplogroup$coast=="right coast", 0.015, -0.015)
# # pie_data_haplogroup$adjust.Irk <- ifelse(pie_data_haplogroup$coast=="right coast", 0.005, -0.005)
# # pie_data_haplogroup$adjust.U <- ifelse(pie_data_haplogroup$coast=="right coast", 0.015, -0.015)
# 
# ## these are all pies
# pie.testplot_list <- 
#   lapply(unique(pie_data_haplogroup$coordinate), function(x) { 
#     gt_plot <- ggplotGrob(
#       ggplot(pie_data_haplogroupm[pie_data_haplogroupm$coordinate == x,])+
#         geom_bar(aes(x=coordinate, y=value, fill=variable), col='white', #size=.1, #.5, # size=1, 
#                  stat='identity') +  ## the bar at the bottom is just for the white border
#         geom_bar(aes(x=coordinate, y=value, fill=variable), col='NA', 
#                  stat='identity') + #this is the get these without the white bars
#         theme_minimal() + ## ylim(0, 12) can make equal
#         coord_polar("y", start=0) + 
#         theme(line = element_blank()) +
#         #        scale_fill_manual(values = c("#66BB3C", "#4477AA", "#F0E442")) +
#         theme(legend.position = "none", rect = element_blank(),
#               line = element_blank(), text = element_blank()) 
#     )
#     panel_coords <- gt_plot$layout[gt_plot$layout$name == "panel",]
#     gt_plot[panel_coords$t:panel_coords$b, panel_coords$l:panel_coords$r]
#   })
# 
# 
# 
# #####
# ## Plot background maps of the three places
# ## Irkutsk
# irkbox <- c(left=104.19, right=104.41, bottom=52.18, top=52.32)
# IrkMap <- get_stadiamap(irkbox, zoom=15, maptype = "stamen_terrain_background", source = "stadia", filename = "irkmap") 
# ##terrain-background 10; with higher zoom some parts go missing; maybe it's fixed sometime
# pIrkMap <- ggmap(IrkMap) + 
#   xlab("Longitude") + ylab("Latitude") +
#   ## add arrow for flow direction
#   geom_segment(aes(x = 104.37, y = 52.22, xend = 104.335, yend = 52.235),
#                arrow = arrow(length = unit(0.5, "cm")), 
#                lineend='round', linejoin='bevel', linetype = "solid",
#                col="darkblue") + 
#   ggtitle("Irkutsk") + 
#   mytheme + 
#   scale_x_continuous(limits=c(irkbox['left'], irkbox['right']), expand=c(0,0), n.breaks = 3) + 
#   scale_y_continuous(limits=c(irkbox['bottom'], irkbox['top']), expand=c(0,0), breaks = pretty_breaks(n=3))
# pIrkMap
# 
# 
# ## Angara map upstream of Irkutsk
# upperbox <- c(left=104.25, right=105.05, bottom=51.75, top=52.2)
# #UpperMap <- get_stamenmap(upperbox, zoom=11, maptype = "terrain") 
# UpperMap <- get_stadiamap(upperbox, zoom=11, maptype = "stamen_terrain_background") ## without the Cyrillic label
# pUpper <- ggmap(UpperMap) + 
#   xlab("Longitude") + ylab("Latitude") +
#   #  theme_bw(base_size = 12) + 
#   geom_segment(aes(x = 104.72, y = 51.95, xend = 104.64, yend = 52),
#                arrow = arrow(length = unit(0.5, "cm")), 
#                lineend='round', linejoin='bevel', linetype = "solid",
#                col="darkblue") + 
#   ggtitle ("Angara before Irkutsk") + 
#   mytheme + 
#   scale_x_continuous(limits=c(upperbox['left']+.01, upperbox['right']-.01), expand=c(0,0), n.breaks = 3) + 
#   scale_y_continuous(limits=c(upperbox['bottom']+.01, upperbox['top']-.01), expand=c(0,0), breaks = pretty_breaks(3))
# pUpper
# 
# ## Bratsk map (downstream of Irkutsk)
# #bratskbox <- c(left=101.49, right=102.0, bottom=56, top=56.4)
# bratskbox <- c(left=101.5, right=102.1, bottom=56, top=56.35) ## This looks more balanced
# BratskMap <- get_stadiamap(bratskbox, zoom=11, maptype = "stamen_terrain_background")
# #BratskMap <- get_stamenmap(bratskbox, zoom=11, maptype = "terrain-background")
# pBratsk <- ggmap(BratskMap) + 
#   geom_segment(aes(x = 101.75, y = 56.1, xend = 101.75, yend = 56.2),
#                arrow = arrow(length = unit(0.5, "cm")), 
#                lineend='round', linejoin='bevel', linetype = "solid",
#                col="darkblue") + 
#   ggtitle("Bratsk") + 
#   xlab("Longitude") + ylab("Latitude") +
#   mytheme + 
#   scale_x_continuous(limits=c(bratskbox['left']+.01, bratskbox['right']-.01), expand=c(0,0), breaks = pretty_breaks(n=3)) + 
#   scale_y_continuous(limits=c(bratskbox['bottom']+.01, bratskbox['top']-.01), expand=c(0,0), breaks = pretty_breaks(n=3))
# pBratsk
# 
# 
# ## deal with spot names
# #####
# coord_data$letter <- coord_data$abbreviation ## replaced with my ideas
# spots <- unique(coord_data[, c("lat", "lon", "letter", "coast", "spot", "haplogroup")])
# ## abbreviation=my abbrev, letter=SA's abbreviations
# spots %>% group_by(letter,coast) %>% summarise(lat=mean(lat), lon=mean(lon)) -> uniqSpots
# ## here's another table (haplotype-centered), basically for 
# spots %>% group_by(letter,coast,haplogroup) %>% summarise(lat=mean(lat), lon=mean(lon)) -> haplogrSpots
# addSpotsAndLegend <- function(plotobj, uniqSpots, haplogrSpots) {
#   plotobj + 
#     geom_label(data=uniqSpots, aes(x=lon+adjust, y=lat, label = letter), 
#                label.padding = unit(0.1, "lines"), alpha=1, label.size=NA) + 
#     geom_point(data=haplogrSpots, aes(x=lon, y=lat, fill=haplogroup),
#                pch=21, alpha=0, size = 8) + 
#     scale_fill_manual(values = c("#66BB3C", "#4477AA", "#F0E442"), name="Haplogroup") +
#     guides(fill = guide_legend(override.aes=list(shape=21, alpha=1, col="white"))) +
#     theme(legend.position = "bottom", 
#           legend.background = element_rect(fill="#e1e4db"), legend.key = element_rect(fill = "#e1e4db")) ## to showcase the white borders if needed
# }
# 
# ## Irkutsk: combine pies
# mapwidth <- irkbox["right"]-irkbox["left"]
# pie_annotation_list <- construct_pie_list(pie_data_haplogroup, coord_box = irkbox, size=mapwidth/30, adj.col = "adjust.Irk")
# ## combine map with pies
# pIrkMap.test3 <- Reduce("+", pie_annotation_list, pIrkMap)
# ## padding of labels
# haplogrSpots.cropped <- haplogrSpots[between(haplogrSpots$lon, irkbox["left"], irkbox["right"]) &
#                                        between(haplogrSpots$lat, irkbox["bottom"], irkbox["top"]), ]
# uniqSpots$adjust <- ifelse(uniqSpots$coast=="right coast", 0.018, -0.018)
# addSpotsAndLegend(pIrkMap.test3, uniqSpots, haplogrSpots.cropped)-> pIrkMap.4
# pIrkMap.4
# ## ggsave fonts look weird :(
# #ggsave("Irk_both_pies.png", width = 20, height = 20, units = "cm", device=ragg::agg_png)
# png("Fig4_Irk_both_pies.png", width=15, height=15, units="cm", res=300); print(pIrkMap.4); dev.off()
# 
# ## Angara before Irkutsk ("upper")
# mapwidth <- upperbox["right"]-upperbox["left"]
# pUpper -> pUpper.2
# pUpper.2  
# pie_annotation_list <- construct_pie_list(pie_data_haplogroup, coord_box = upperbox, size=mapwidth/30, adj.col = "adjust.U")
# #uniqSpots$lona <- uniqSpots$lon+uniqSpots$adjust.U
# pUpper.3 <- Reduce("+", pie_annotation_list, pUpper)
# #pUpper.2 + geom_text_repel(data=uniqSpots, aes(x=lon, y=lat, label = letter), col = "black")
# ## padding of labels
# haplogrSpots.cropped <- haplogrSpots[between(haplogrSpots$lon, upperbox["left"], upperbox["right"]) &
#                                        between(haplogrSpots$lat, upperbox["bottom"], upperbox["top"]), ]
# uniqSpots$adjust <- ifelse(uniqSpots$coast=="right coast", mapwidth/13, -mapwidth/13)
# addSpotsAndLegend(pUpper.3, uniqSpots, haplogrSpots.cropped) -> pUpper.4
# pUpper.4
# #ggsave("Upper_both_pies.svg", width = 20, height = 20, units="cm")
# #ggsave("Upper_both_pies.png", width = 5, height = 5)
# png("Fig3_Upper_both_pies.png", width=15, height=15, units="cm", res=300); print(pUpper.4); dev.off()
# 
# ## add pies to the Bratsk map
# mapwidth <- bratskbox["right"]-bratskbox["left"]
# pie_annotation_list <- construct_pie_list(pie_data_haplogroup, coord_box = bratskbox, size=mapwidth/25, adj.col = "adjust.B")
# #uniqSpots$lona <- uniqSpots$lon+uniqSpots$adjust.B
# pBratsk.2 <- Reduce("+", pie_annotation_list, pBratsk)
# ## padding of labels
# uniqSpots$adjust <- ifelse(uniqSpots$coast=="right coast", mapwidth/10, -mapwidth/10)
# haplogrSpots.cropped <- haplogrSpots[between(haplogrSpots$lon, bratskbox["left"], bratskbox["right"]) &
#                                        between(haplogrSpots$lat, bratskbox["bottom"], bratskbox["top"]), ]
# addSpotsAndLegend(pBratsk.2, uniqSpots, haplogrSpots.cropped) -> pBratsk.3
# ## show the final
# pBratsk.3
# #ggsave("Bratsk_both_pies.svg", width = 20, height = 20, units="cm")
# #ggsave("Bratsk_both_pies.png", width = 5, height = 5)
# png("Fig5_Bratsk_both_pies.png", width=15, height=15, units="cm", res=300); print(pBratsk.3); dev.off()
# 
# ## We decided not to arrange all plots into one figure, so the code below is left just in case
# #####
# 
# graphwidths <- c(1/(upperbox[2]-upperbox[1]), 1/(irkbox[2]-irkbox[1]), 1/(bratskbox[2]-bratskbox[1]))
# 
# ## let's plot the legend separately
# legend.shapes.df <- data.frame(x=c(104, 105, 106), value=rep(1,3), variable=c("A", "S", "W"))
# 
# ggplot(legend.shapes.df)+
#   geom_bar(aes(x=x, y=value, fill=variable), col='white', 
#            stat='identity') +  ##position='dodge'
#   theme_minimal() + ## ylim(0, 12) can make equal
#   facet_wrap(~variable) + 
#   coord_polar("y", start=0) + 
#   ggtitle("Haplogroup") + 
#   theme(line = element_blank()) +
#   scale_fill_manual(values = c("#66BB3C", "#4477AA", "#F0E442")) +
#   theme(legend.position = "none", rect = element_blank(), line = element_blank(), 
#         axis.text = element_blank(), axis.title=element_blank(),strip.text = element_text(size = 14),
#         plot.title.position = "plot", plot.title=element_text(hjust = -.3, vjust=-20),
#         plot.margin = margin(c(r=0,t=0,b=2,l=5), unit="cm"))  -> legend.plot
# legend.plot  
# 
# empty.plot <- ggplot(legend.shapes.df) + theme_minimal()
# empty.plot <- 
#   ggplot(legend.shapes.df, aes(x=x, y=value)) + 
#   xlim(100, 110) + ylim(0,1) +  
#   theme_grey() 
# 
# #### pies for the legend
# 
# pie.testplot_list <- 
#   lapply(unique(legend.shapes.df$variable), function(x) { 
#     gt_plot <- ggplotGrob(
#       ggplot(legend.shapes.df[legend.shapes.df$variable==x,])+
#         geom_bar(aes(x=x, y=value, fill=variable), col='white', 
#                  stat='identity') +  ##position='dodge'
#         theme_minimal() + ## ylim(0, 12) can make equal
#         coord_polar("y", start=0) + 
#         theme(line = element_blank()) +
#         scale_fill_manual(values = c("#66BB3C", "#4477AA", "#F0E442")) +
#         theme(legend.position = "none", rect = element_blank(),
#               line = element_blank(), text = element_blank()) 
#     )
#     panel_coords <- gt_plot$layout[gt_plot$layout$name == "panel",]
#     gt_plot[panel_coords$t:panel_coords$b, panel_coords$l:panel_coords$r]
#   })
# 
# ### the following should probably be made into a function...
# pie_annotation_list <- lapply(1:3, function(x) 
#   inset(pie.testplot_list[[x]], 
#         xmin =  legend.shapes.df[x,"x"] - .3,
#         xmax =  legend.shapes.df[x,"x"] + .3,
#         ymin =  0,
#         ymax =  1) )
# 
# Reduce("+", pie_annotation_list, empty.plot)
# 
# ggarrange(pUpper.3, # + theme(legend.position='none'), 
#           pIrkMap.4 + ylab(""), #+ theme(legend.position = 'none') + ylab("") + ggtitle("Irkutsk"), 
#           pBratsk.3 + ylab(""), nrow=1)
# 
# ggarrange(pUpper.4, # + theme(legend.position='none'), 
#           pIrkMap.4, #+ theme(legend.position = 'none') + ylab("") + ggtitle("Irkutsk"), 
#           pBratsk.3 + ylab(""), #+ theme(legend.position = 'none') + ggtitle("Bratsk"), 
#           empty.plot, legend.plot, empty.plot, #nrow = 2, 
#           heights = c(1, 0.1), widths = c(1.1, 1.3, 1, 1, 1, 1)) -> arranged.plots
# arranged.plots
# 
# 
# library(ggsn)
# pUpper.5 <- 
#   pUpper.4 + ggsn::scalebar(location = "bottomright",
#                             x.min=104.6, x.max=105, y.min=51.8, y.max=52.2, 
#                             dist=5, transform = T, dist_unit = 'km', height = 0.025, 
#                             st.dist = 0.05, st.size = 3)
# 
# pIrkMap.5 <- 
#   pIrkMap.4 + ggsn::scalebar(location = "bottomright",
#                              x.min=104.2, x.max=104.4, y.min=52.2, y.max=52.3, 
#                              dist=5, transform = T, dist_unit = 'km', height = 0.025, 
#                              st.dist = 0.05, st.size = 3)
# 
# pBratsk.5 <- 
#   pBratsk.3 + ggsn::scalebar(location = "bottomright",
#                              x.min=101.6, x.max=102.0, y.min=56.05, y.max=56.3, 
#                              dist=5, transform = T, dist_unit = 'km', height = 0.025, 
#                              st.dist = 0.05, st.size = 3)
# 
# arranged.plots <- 
#   ggarrange(pUpper.5, # + theme(legend.position='none'), 
#             pIrkMap.5, #+ theme(legend.position = 'none') + ylab("") + ggtitle("Irkutsk"), 
#             pBratsk.5 + ylab(""), nrow=1) #+ theme(legend.position = 'none') + ggtitle("Bratsk"),
# 
# 
# 
# svg("all_maps_pies_terrain.svg", width = 10, height = 7)
# arranged.plots
# dev.off()
# 
# png("all_maps_pies_terrain.png", width = 10, height = 7, units="in", res=300)
# arranged.plots
# dev.off()
# 
# ##ggsave("all_maps_pies_terrain.svg", width = 25, height = 15, units = "cm")
# ##ggsave("all_maps_pies_terrain_bg.svg", width = 25, height = 15, units = "cm")
# 
# 
# library("OpenStreetMap")
# library(osmdata)
# 
# q <- opq(bbox = c(51.1, 0.1, 51.2, 0.2))
# 
# bbox <- getbb()
# 
# 
# 
# library(sf)
# # Now use maptiles instead, with Mercator to get better results
# #df_merc <- st_transform(df, 3857)
# pie_data_haplogroup <- pie_data_haplogroup[complete.cases(pie_data_haplogroup$coordinate) 
#                                            & !is.na(pie_data_haplogroup$coordinate), ]
# df <- st_as_sf(pie_data_haplogroup,remove = FALSE,
#                coords = c("lon", "lat"),
#                # Add crs
#                crs = "+proj=lonlat"
# )
# df_merc <- st_transform(df, 3857)
# 
# library(maptiles)
# library(tidyterra)
# dc <- get_tiles(x = df, provider = "Esri.WorldTopoMap", zoom = 5)
# 
# 
# ggplot() +
#   geom_spatraster_rgb(data = dc) +
#   geom_scatterpie(data=as.data.frame(df_merc), aes(x=lon, y=lat, group=spot), cols = c("W", "S", "C")) + 
# #  geom_sf(data = df) + 
#   coord_sf() + 
#   scale_fill_manual(values = c("mediumpurple", "#4477AA", "#F0E442"))
# 
# #> Data and map tiles sources: 
# #> © OpenStreetMap contributors
