##Set working directory
setwd("path2directory")
getwd()

## Setup dependencies
library(phyloseq); packageVersion("phyloseq")
library(Biostrings); packageVersion("Biostrings")
library(ggplot2); packageVersion("ggplot2")
library(iNEXT); packageVersion("iNEXT")
library(ape); packageVersion("ape")
library(vegan); packageVersion("vegan")
library(dplyr); packageVersion("dplyr")
library(cowplot); packageVersion("cowplot")
library(plyr); packageVersion("plyr")
library(sjmisc); packageVersion("sjmisc")
library(data.table); packageVersion("data.table")
library(metacoder);packageVersion("metacoder")
library(hilldiv);packageVersion("hilldiv")
library(car);packageVersion("car")
library(lme4);packageVersion("lme4")
library(RColorBrewer);packageVersion("RColorBrewer")
library(tidyverse);packageVersion("tidyverse")
library(ggpubr);packageVersion("ggpubr")

##Load data
ARGabund_df <- read.csv("ARGabund.csv", header=T, sep=",")
head(ARGabund_df)

##Boxplot relative abundance
p_abund <- ggplot(ARGabund_df,aes(x=Groups,y=ARGabund,fill=Groups)) + 
  geom_boxplot() + scale_fill_manual(values = c('#AF8A5F', '#42858C', '#7F8E39')) +
  theme_bw() + 
  theme(axis.text.x = element_text(size = 12, angle = 45, hjust = 1.0)) +
  ylab("Frequency of ARGs (log10)") +
  theme(panel.grid.major = element_blank(),
        panel.grid.minor = element_blank()) +
  geom_pwc(aes(group = Groups), method = "t_test", label = "{p.adj.format}{p.adj.signif}", hide.ns = TRUE, vjust = 0.1) + theme(legend.position = "none")
p_abund

##Set color
cols <- c("#E69F00", "#56B4E9", "#009E73", "#F0E442", "#0072B2", "#D55E00", "#CC79A7")
colAMR <- c('#AF8A5F', '#42858C', '#7F8E39')

##Scatter-plot (regression line)
corr_ARG_type <- ggscatter(ARGabund_df, x="ARGtype", y="ARGabund", color="Groups", add = "reg.line",  size = 5, alpha = 0.9, conf.int = TRUE, cor.method = "pearson", add.params = list(color = "Groups", fill = "Groups")) + theme_classic() + theme (axis.text.x = element_text(size=12, colour = "grey25")) + theme (axis.text.y = element_text(size = 12)) + theme(text = element_text(size = 12)) + theme(panel.background = element_rect(fill = "white"), plot.background = element_rect(fill = "transparent", color = NA), panel.grid.major = element_blank(), panel.grid.minor = element_line(colour = "white")) + theme(legend.position = "bottom") + xlab("ARG type") + ylab("Log10 ARG abundance") + scale_color_manual(values=colAMR) + scale_fill_manual(values=colAMR)
corr_ARG_type <- corr_ARG_type + stat_cor(aes(color = Groups), label.x.npc = 0.35, label.y.npc = 0.2)
corr_ARG_type

corr_ARG_class <- ggscatter(ARGabund_df, x="ARGclass", y="ARGabund", color="Groups", add = "reg.line",  size = 5, alpha = 0.9, conf.int = TRUE, cor.method = "pearson", add.params = list(color = "Groups", fill = "Groups")) + theme_classic() + theme (axis.text.x = element_text(size=12, colour = "grey25")) + theme (axis.text.y = element_text(size = 12)) + theme(text = element_text(size = 12)) + theme(panel.background = element_rect(fill = "white"), plot.background = element_rect(fill = "transparent", color = NA), panel.grid.major = element_blank(), panel.grid.minor = element_line(colour = "white")) + theme(legend.position = "bottom") + xlab("ARG class") + ylab("Log10 ARG abundance") + scale_color_manual(values=colAMR) + scale_fill_manual(values=colAMR)
corr_ARG_class <- corr_ARG_class + stat_cor(aes(color = Groups), label.x.npc = 0.3, label.y.npc = 0.2)
corr_ARG_class

##Abundance - type
#Load data
ARG_T <- read.csv("ARGtype.csv", header=T, sep=",")
total.ARGDepth_df_T <- 
  ARG_T %>% 
  group_by(Groups, Type) %>% 
  summarise(totalARG = sum(Hits),
            numARG = n())
head(total.ARGDepth_df_T)

# calculate other ARG class
arg_type <- 
  ARG_T %>% 
  group_by(Sample, Type) %>%
  summarise(DepthARG = sum(Hits), Groups = unique(Groups))
head(arg_type)

relAbund.df_T <- arg_type %>% 
  group_by(Groups,Type) %>% summarise(avgDepth = mean(DepthARG)) %>% 
  mutate(freq = avgDepth/sum(avgDepth)) %>%
  reshape2::dcast(Type~Groups) %>% 
  arrange(desc(Rural))
head(relAbund.df_T)

otherType <- relAbund.df_T$Type[10:nrow(relAbund.df_T) ]
arg_type$Type[arg_type$Type %in% otherType] <- "Z_Other"
# plot by sample with significance 
plotDat_3bars_T <- arg_type %>% filter(Type != "unclassified") %>%
  group_by(Groups,Type) %>% summarise(avgDepth = mean(DepthARG)) %>% 
  mutate(freq = avgDepth/sum(avgDepth)) %>%
  as.data.frame()
head(plotDat_3bars_T)

plotDat_3bars_T %>% group_by(Groups) %>% summarise(s = sum(freq))

##Plot-bar
p_type<-ggplot(plotDat_3bars_T) +
  geom_col(aes(x=Groups ,y =freq, fill=Type), width = 0.9,color="white") +
  scale_fill_manual(values = mycolor)+
  theme_bw() + theme(axis.text.x = element_text(size = 12, angle = 45, hjust = 1.0),
                     panel.grid = element_blank()) +
  theme(legend.text=element_text(size=12), legend.title = element_text(size=12, face="bold")) + theme(axis.text.x=element_markdown()) + theme(axis.title.y = element_text(size = 12)) +
  ylab("Frequency of AMR-MR")
p_type

##Abundance - class
#Set color
MyPalG <- c ("#40396e", "#6F63BB", "#8A60B0", "#774411", "#8c6946", "#DDAA77", "#0b4a4a", "#117777", "#1F83B4", "#1BA3C6", "#195c1b", "#3b8a3e", "#BCBD22", "#574900", "#857001", "#b09400", "#d1b002", "#821d1d", "#ab2c2c", "#e38484", "#b05302", "#FF7F0E", "#FFAA0E", "#eb9d5b", "#555555")
#Load data
ARG_C <- read.csv("ARGclass.csv", header=T, sep=",")
total.ARGDepth_df_C <- 
  ARG_C %>% 
  group_by(Groups, Class) %>% 
  summarise(totalARG = sum(Hits),
            numARG = n())
head(total.ARGDepth_df_C)

# calculate other ARG class
arg_class <- 
  ARG_C %>% 
  group_by(Sample, Class) %>%
  summarise(DepthARG = sum(Hits), Groups = unique(Groups))
head(arg_class)

relAbund.df_C <- arg_class %>% 
  group_by(Groups,Class) %>% summarise(avgDepth = mean(DepthARG)) %>% 
  mutate(freq = avgDepth/sum(avgDepth)) %>%
  reshape2::dcast(Class~Groups) %>% 
  arrange(desc(Rural))
head(relAbund.df_C)

otherClass <- relAbund.df_C$Class[25:nrow(relAbund.df_C) ]
arg_class$Class[arg_class$Class %in% otherClass] <- "Z_Other"
# plot by sample with significance 
plotDat_3bars_C <- arg_class %>% filter(Class != "unclassified") %>%
  group_by(Groups,Class) %>% summarise(avgDepth = mean(DepthARG)) %>% 
  mutate(freq = avgDepth/sum(avgDepth)) %>%
  as.data.frame()
head(plotDat_3bars_C)

plotDat_3bars_C %>% group_by(Groups) %>% summarise(s = sum(freq))

##Plot-bar
p_class<-ggplot(plotDat_3bars_C) +
  geom_col(aes(x=Groups ,y =freq, fill=Class), width = 0.9,color="white") +
  scale_fill_manual(values = MyPalG)+
  theme_bw() + theme(axis.text.x = element_text(size = 12, angle = 45, hjust = 1.0),
                     panel.grid = element_blank()) +
  theme(legend.text=element_text(size=12), legend.title = element_text(size=12, face="bold")) + theme(axis.text.x=element_markdown()) + theme(axis.title.y = element_text(size = 12)) +
  ylab("Frequency of AMR-MR")
p_class

##Arrange plots
plot_grid(p_type, p_class, labels = c("A", "B"), rel_widths = c(1.2, 2), nrow = 1)
##Save plot
ggsave("Fig 1.tiff", units = c("in"), width=12, height=5, dpi=300, compression="lzw")