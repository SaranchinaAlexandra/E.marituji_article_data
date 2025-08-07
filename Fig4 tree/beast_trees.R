library(ggplot2)
library(ggtree)
library(treeio)

beast_tree <- read.beast("../4seqs/4seqs.beauti-EmarCOIAnc_w_longer_Ecy.tree")

beast_tree@data$MRCAtextcolor <- ifelse(beast_tree@data$height > 1, "NA", "black")


ggtree(beast_tree) + geom_text(aes(label=node), col = "red") + geom_tiplab() + expand_limits(x=15)

tree1 <- ggtree(beast_tree) + theme_tree2()
tree1 <- revts(tree1)

tree1 + 
#  geom_hilight(node=19, fill="#4477AA") + ## southern
#  geom_hilight(node=18, fill="#F0E442") + ## western
#  geom_hilight(node=20, fill="#D81B60") + ## eastern
#  geom_hilight(node=15, fill="#228B22")+ 
  geom_range("CAheight_0.95_HPD", color="darkgrey", size=2, alpha=.5) +  ## purple was #8b8ef8
#  geom_text2(aes(label=round(as.numeric(posterior), 2), 
#                 subset=as.numeric(posterior)> 0.7, 
#                 x=branch), vjust=0, hjust = 0, color = "grey") + 
  scale_x_continuous(labels = abs, breaks = -10:0) + 
  expand_limits(x=6) + 
  scale_color_manual(values = c("NA", "black"), guide = 'none')  + 
  geom_text(aes(label = round(height, 1), color = MRCAtextcolor), nudge_x = -.2, nudge_y = .15) + 
  geom_tiplab(align=FALSE, linetype='dashed', linesize=.3, size=4.45, fontface = "italic") + 
  theme(axis.text.x = element_text(size=12)) + xlab("mya") -> mrca_tree
mrca_tree

svg("mrca_tree_4seqs.svg", width=12, height=6)
mrca_tree
dev.off()                    
