library(tanggle)
library(dplyr) ## for some data rearrangement
library(phangorn)
library(ggplot2)
library(ggtree)

## read the data = nexus file recorded with SplitsTree4 (!)
#Nnet <- read.nexus.networx("E:/EmarCOI.nex")
Nnet <- read.nexus.networx("EmarCOInet.nex")

pn <- 
  ggsplitnet(Nnet, col="black") + 
  geom_treescale(x=-.04, y=.035, offset=.001) + 
  coord_fixed()
#  geom_tiplab2() + 
#  geom_point(aes(shape="21", color=isTip), size=2)

tips <- pn$data[pn$data$isTip, ]
tips$group <- sapply(tips$label, function(x) strsplit(x, split = "_")[[1]][2])
tips$group[which(is.na(tips$group))] <- "out"

tips %>% count(x, y, group) -> tips.occur

#tips$place <- sapply(tips$label, function(x) strsplit(x, split = "_")[[1]][1])
#tips$place <- ifelse(tips$place == "E" | tips$place == "Evi", "Lake Baikal", "Angara River")


tips$place <- sapply(tips$label, function(x) strsplit(x, split = "_")[[1]][1])
tips$place <- ifelse(startsWith(tips$place, "MK") | tips$place == "Evi", "Lake Baikal", "Angara River")


tips %>% count(x, y, group, place) -> tips.occur
tips.occur <- tips.occur[order(tips.occur$n), ]

pn + 
  geom_point(data = tips.occur, aes(x=x, y=y, fill=group), size=6, shape=21) + 
  scale_fill_manual(values = c("mediumpurple3", "gray8", "gray64","#4477AA" ,"#F0E442"), 
                    name="Haplogroup") +
  expand_limits(x=.03) + 
  scale_color_manual(values=c("white", "black"), name = "Place") + 
  #  scale_color_manual(values = c("#66BB3C", "#D81B60", "grey50", "#4477AA", "#F0E442"), 
  #                    name="Haplogroup") +
#  scale_shape_manual(values = c(21, 22), name = "Place") +
  guides(fill = guide_legend(override.aes=list(shape=21)),
         color = guide_legend(override.aes=list(col="black"))) + 
  theme(legend.position = "left") -> pnCOI
pnCOI








## read the data = nexus file recorded with SplitsTree4 (!)
#Nnet1 <- read.nexus.networx("E:/Emar18s.nex")
Nnet1 <- read.nexus.networx("Emar18snet.nex")

pn1 <- 
  ggsplitnet(Nnet1, col="black") + 
  geom_treescale(x=0, y=0.005, offset=.0001) + 
  coord_fixed()
#  geom_tiplab2() + 
#  geom_point(aes(shape="21", color=isTip), size=2)

tips <- pn1$data[pn1$data$isTip, ]
tips$group <- sapply(tips$label, function(x) strsplit(x, split = "_")[[1]][2])
tips$group[which(is.na(tips$group))] <- "out"

tips %>% count(x, y, group) -> tips.occur

#tips$place <- sapply(tips$label, function(x) strsplit(x, split = "_")[[1]][1])
#tips$place <- ifelse(tips$place == "E" | tips$place == "Evi", "Lake Baikal", "Angara River")


tips$place <- sapply(tips$label, function(x) strsplit(x, split = "_")[[1]][1])
tips$place <- ifelse(startsWith(tips$place, "MK") | tips$place == "Evi", "Lake Baikal", "Angara River")


tips %>% count(x, y, group, place) -> tips.occur
tips.occur <- tips.occur[order(tips.occur$n), ]

pn1 + 
  geom_point(data = tips.occur, aes(x=x, y=y, fill=group), size=6, shape=21) + 
  scale_fill_manual(values = c("mediumpurple3", "gray8", "gray64","navy", "#4477AA","#F0E442"), 
                    name="Haplogroup") +
  #expand_limits(x=.03) + 
  scale_color_manual(values=c("white", "black"), name = "Place") + 
  #  scale_color_manual(values = c("#66BB3C", "#D81B60", "grey50", "#4477AA", "#F0E442"), 
  #                    name="Haplogroup") +
  #  scale_shape_manual(values = c(21, 22), name = "Place") +
  guides(fill = guide_legend(override.aes=list(shape=21)),
         color = guide_legend(override.aes=list(col="black"))) + 
  theme(legend.position = "left") -> pn18s
pn18s






library(ggpubr)
png("Figure_1_draft.png", width = 20, height = 10, res=600, units="cm")
ggarrange(pnCOI+theme(), pn18s,
          labels = c("A", "B"))
dev.off()

svg("Figure_1_draft.svg", width = 7.87, height = 3.94)
ggarrange(pnCOI, pn18s
          labels = c("A", "B"))
dev.off()


