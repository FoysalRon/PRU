##Set working directory
setwd("path2directory")
getwd()
library(paletteer)
library(microeco)
library(file2meco)
library(ggtext)
#Load data
OTU<-read.csv("asvtablePRU.csv", header=TRUE, row.names=1)
TAX<-read.csv("taxonomyPRU.csv", header=TRUE, row.names=1)
META<-read.csv("metadataPRU.csv", header=TRUE, row.names=1)
class(OTU)
class(TAX)
OTU_data=otu_table(as.matrix(OTU), taxa_are_rows=TRUE)
TAX_data=tax_table(as.matrix(TAX))
META_data=sample_data(META)
physeq<-merge_phyloseq(phyloseq(OTU_data, TAX_data), META_data)
physeq<-t(physeq)

#Transform data for microeco
dataset <- phyloseq2meco(physeq)
dataset

#Set color
MyPalC <- c ("grey30", "#6F63BB", "#8A60B0", "#BA43B4", "#C7519C", "#D63A3A", "#FF7F0E", "#FFAA0E", "#FFBF50", "#BCBD22", "#78A641", "#2CA030", "#12A9A1", "#1BA9C6", "#1F89B1", "#117777", "#799999", "#999960", "#999000", "#774411", "snow3")

#Phylum - pie
t1 <- trans_abund$new(dataset = dataset, taxrank = "Phylum", ntaxa = 7, groupmean = "Groups")
# all pie chart in one row
t1$plot_pie(facet_nrow = 1)
pie1 <- t1$plot_pie(facet_nrow = 2, add_label = TRUE)
pie1

#Save plot
#Save plot
ggsave("Figure_rel-abund_pie.tiff", units = c("in"), width=8, height=8, dpi=300, compression="lzw")

# Genus - bar
t1 <- trans_abund$new(dataset = dataset, taxrank = "Genus", ntaxa = 20)
g1 <- t1$plot_bar(others_color = "grey70", facet = "Groups", legend_text_italic = FALSE, color_values = paletteer_d("ggthemes::Hue_Circle"))
g1 + theme_classic() + theme(axis.title.y = element_text(size = 14))
g1 <- g1 + theme_classic() + theme(axis.text.x = element_text(angle = 45, hjust = 1, size=10)) + theme(legend.text=element_text(size=12, face = "italic"), legend.title = element_text(size=12, face="bold")) + theme(axis.text.x=element_markdown(), legend.text=element_text(face="italic")) + theme(axis.title.y = element_text(size = 12))
g1
g1 <- g1 + scale_fill_manual(values = MyPalC)
g1

# Species - bar
t1 <- trans_abund$new(dataset = dataset, taxrank = "Species", ntaxa = 30)
s1 <- t1$plot_bar(others_color = "grey70", facet = "Groups", legend_text_italic = FALSE, color_values = paletteer_d("ggthemes::Hue_Circle"))
s1 + theme_classic() + theme(axis.title.y = element_text(size = 14))
s1 <- s1 + theme_classic() + theme(axis.text.x = element_text(angle = 45, hjust = 1, size=10)) + theme(legend.text=element_text(size=12, face = "italic"), legend.title = element_text(size=12, face="bold")) + theme(axis.text.x=element_markdown(), legend.text=element_text(face="italic")) + theme(axis.title.y = element_text(size = 12))
s1

#Arrange bar plot
plot_grid(g1, s1, nrow = 1, labels = 'AUTO', rel_widths = c(1.5, 2))

#Save plot
ggsave("Fig1.tiff", units = c("in"), width=14, height=7.5, dpi=300, compression="lzw")
