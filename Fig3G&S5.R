#######################Differential abundance###########################
########################################################################
ARGabund_class_df <- read.csv("ARGclass.csv", header=T, sep=",")
head(ARGabund_class_df)
list_amino <- c("Aminoglycosides")
pattern <- paste0(mylist, collapse = "|")
df_amino <- ARGabund_class_df %>% 
  separate_rows(Class) %>% 
  group_by(Class) %>% 
  filter(Class %in% list_amino)
ARG_p1 <- ggplot(df_amino, aes(x = Groups, y = Abund, color = Groups)) +
  theme_bw() +
  geom_boxplot(outlier.shape  = NA) +
  geom_jitter(aes(color = Groups), shape=21, alpha=0.75, fill="white", stroke=3.0, size=3.5, position = position_jitterdodge()) +
  labs(x = "", y = "Aminoglycosides (log10)") +
  theme(strip.text = element_text(colour = 'black'), axis.text.x = element_text(angle = 45, size=12, hjust=1), axis.text.y =
          element_text(hjust = 1, size=10), text = element_text(size = 12), axis.title.x = element_text(angle = 0, hjust = 0.5, vjust=0.5, size=12)) +
  scale_fill_manual(values=c('#AF8A5F', '#42858C', '#7F8E39')) +
  scale_color_manual(values=c('#AF8A5F', '#42858C', '#7F8E39')) +
  theme(panel.background = element_rect(fill = "white"), plot.background = element_rect(fill = "transparent", color = NA), panel.grid.major = element_blank(), panel.grid.minor = element_line(colour = "white")) +
  theme(legend.position="none") +
  geom_pwc(aes(group = Groups), method = "t_test", label = "{p.adj.format}{p.adj.signif}", hide.ns = TRUE, vjust = 0.1)
ARG_p1

list_ef <- c("Elfamycins")
pattern <- paste0(mylist, collapse = "|")
df_ef <- ARGabund_class_df %>% 
  separate_rows(Class) %>% 
  group_by(Class) %>% 
  filter(Class %in% list_ef)
ARG_p2 <- ggplot(df_ef, aes(x = Groups, y = Abund, color = Groups)) +
  theme_bw() +
  geom_boxplot(outlier.shape  = NA) +
  geom_jitter(aes(color = Groups), shape=21, alpha=0.75, fill="white", stroke=3.0, size=3.5, position = position_jitterdodge()) +
  labs(x = "", y = "Elfamycins (log10)") +
  theme(strip.text = element_text(colour = 'black'), axis.text.x = element_text(angle = 45, size=12, hjust=1), axis.text.y =
          element_text(hjust = 1, size=10), text = element_text(size = 12), axis.title.x = element_text(angle = 0, hjust = 0.5, vjust=0.5, size=12)) +
  scale_fill_manual(values=c('#AF8A5F', '#42858C', '#7F8E39')) +
  scale_color_manual(values=c('#AF8A5F', '#42858C', '#7F8E39')) +
  theme(panel.background = element_rect(fill = "white"), plot.background = element_rect(fill = "transparent", color = NA), panel.grid.major = element_blank(), panel.grid.minor = element_line(colour = "white")) +
  theme(legend.position="none") +
  geom_pwc(aes(group = Groups), method = "t_test", label = "{p.adj.format}{p.adj.signif}", hide.ns = TRUE, vjust = 0.1)
ARG_p2

list_mls <- c("MLS")
pattern <- paste0(mylist, collapse = "|")
df_mls <- ARGabund_class_df %>% 
  separate_rows(Class) %>% 
  group_by(Class) %>% 
  filter(Class %in% list_mls)
ARG_p3 <- ggplot(df_mls, aes(x = Groups, y = Abund, color = Groups)) +
  theme_bw() +
  geom_boxplot(outlier.shape  = NA) +
  geom_jitter(aes(color = Groups), shape=21, alpha=0.75, fill="white", stroke=3.0, size=3.5, position = position_jitterdodge()) +
  labs(x = "", y = "MLS (log10)") +
  theme(strip.text = element_text(colour = 'black'), axis.text.x = element_text(angle = 45, size=12, hjust=1), axis.text.y =
          element_text(hjust = 1, size=10), text = element_text(size = 12), axis.title.x = element_text(angle = 0, hjust = 0.5, vjust=0.5, size=12)) +
  scale_fill_manual(values=c('#AF8A5F', '#42858C', '#7F8E39')) +
  scale_color_manual(values=c('#AF8A5F', '#42858C', '#7F8E39')) +
  theme(panel.background = element_rect(fill = "white"), plot.background = element_rect(fill = "transparent", color = NA), panel.grid.major = element_blank(), panel.grid.minor = element_line(colour = "white")) +
  theme(legend.position="none") +
  geom_pwc(aes(group = Groups), method = "t_test", label = "{p.adj.format}{p.adj.signif}", hide.ns = TRUE, vjust = 0.1)
ARG_p3

list_tet <- c("Tetracyclines")
pattern <- paste0(mylist, collapse = "|")

df_tet <- ARGabund_class_df %>% 
  separate_rows(Class) %>% 
  group_by(Class) %>% 
  filter(Class %in% list_tet)

ARG_p4 <- ggplot(df_tet, aes(x = Groups, y = Abund, color = Groups)) +
  theme_bw() +
  geom_boxplot(outlier.shape  = NA) +
  geom_jitter(aes(color = Groups), shape=21, alpha=0.75, fill="white", stroke=3.0, size=3.5, position = position_jitterdodge()) +
  labs(x = "", y = "Tetracyclines (log10)") +
  theme(strip.text = element_text(colour = 'black'), axis.text.x = element_text(angle = 45, size=12, hjust=1), axis.text.y =
          element_text(hjust = 1, size=10), text = element_text(size = 12), axis.title.x = element_text(angle = 0, hjust = 0.5, vjust=0.5, size=12)) +
  scale_fill_manual(values=c('#AF8A5F', '#42858C', '#7F8E39')) +
  scale_color_manual(values=c('#AF8A5F', '#42858C', '#7F8E39')) +
  theme(panel.background = element_rect(fill = "white"), plot.background = element_rect(fill = "transparent", color = NA), panel.grid.major = element_blank(), panel.grid.minor = element_line(colour = "white")) +
  theme(legend.position="none") +
  geom_pwc(aes(group = Groups), method = "t_test", label = "{p.adj.format}{p.adj.signif}", hide.ns = TRUE, vjust = 0.1)
ARG_p4

ARGabund_class_df$Class <- gsub(" ", "", ARGabund_class_df$Class)
list_cat <- c('Cationicantimicrobialpeptides')
df_cat <- ARGabund_class_df %>% 
  separate_rows(Class) %>% 
  group_by(Class) %>% 
  filter(Class %in% list_cat)
ARG_p5 <- ggplot(df_cat, aes(x = Groups, y = Abund, color = Groups)) +
  theme_bw() +
  geom_boxplot(outlier.shape  = NA) +
  geom_jitter(aes(color = Groups), shape=21, alpha=0.75, fill="white", stroke=3.0, size=3.5, position = position_jitterdodge()) +
  labs(x = "", y = "Cationic antimicrobial peptides (log10)") +
  theme(strip.text = element_text(colour = 'black'), axis.text.x = element_text(angle = 45, size=12, hjust=1), axis.text.y =
          element_text(hjust = 1, size=10), text = element_text(size = 12), axis.title.x = element_text(angle = 0, hjust = 0.5, vjust=0.5, size=12)) +
  scale_fill_manual(values=c('#AF8A5F', '#42858C', '#7F8E39')) +
  scale_color_manual(values=c('#AF8A5F', '#42858C', '#7F8E39')) +
  theme(panel.background = element_rect(fill = "white"), plot.background = element_rect(fill = "transparent", color = NA), panel.grid.major = element_blank(), panel.grid.minor = element_line(colour = "white")) +
  theme(legend.position="none") +
  geom_pwc(aes(group = Groups), method = "t_test", label = "{p.adj.format}{p.adj.signif}", hide.ns = TRUE, vjust = 0.1)
ARG_p5

#########Select only Rural n Urban data############
df_RU <- ARGabund_class_df %>% 
  group_by(Groups) %>% 
  filter(Groups %in% c("Rural", "Urban"))
#Remove space & symbols from class
df_RU$Class <- gsub(" ", "", df_RU$Class)
df_RU$Class <- gsub("-", "", df_RU$Class)
#Filter and plot other AMR that only present in rural and urban 
list_amino <- c('Aminocoumarins')
df_amino_RU <- df_RU %>% 
  separate_rows(Class) %>% 
  group_by(Class) %>% 
  filter(Class %in% list_amino)

ARG_p6 <- ggplot(df_amino_RU, aes(x = Groups, y = Abund, color = Groups)) +
  theme_bw() +
  geom_boxplot(outlier.shape  = NA) +
  geom_jitter(aes(color = Groups), shape=21, alpha=0.75, fill="white", stroke=3.0, size=3.5, position = position_jitterdodge()) +
  labs(x = "", y = "Aminocoumarins (log10)") +
  theme(strip.text = element_text(colour = 'black'), axis.text.x = element_text(angle = 45, size=12, hjust=1), axis.text.y =
          element_text(hjust = 1, size=10), text = element_text(size = 12), axis.title.x = element_text(angle = 0, hjust = 0.5, vjust=0.5, size=12)) +
  scale_fill_manual(values=c('#AF8A5F', '#42858C', '#7F8E39')) +
  scale_color_manual(values=c('#AF8A5F', '#42858C', '#7F8E39')) +
  theme(panel.background = element_rect(fill = "white"), plot.background = element_rect(fill = "transparent", color = NA), panel.grid.major = element_blank(), panel.grid.minor = element_line(colour = "white")) +
  theme(legend.position="none") +
  geom_pwc(aes(group = Groups), method = "t_test", label = "{p.adj.format}{p.adj.signif}", hide.ns = TRUE, vjust = 0.1)
ARG_p6


list_cpr <- c('Copperresistance')
df_cpr_RU <- df_RU %>% 
  separate_rows(Class) %>% 
  group_by(Class) %>% 
  filter(Class %in% list_cpr)
ARG_p7 <- ggplot(df_cpr_RU, aes(x = Groups, y = Abund, color = Groups)) +
  theme_bw() +
  geom_boxplot(outlier.shape  = NA) +
  geom_jitter(aes(color = Groups), shape=21, alpha=0.75, fill="white", stroke=3.0, size=3.5, position = position_jitterdodge()) +
  labs(x = "", y = "Copper resistance (log10)") +
  theme(strip.text = element_text(colour = 'black'), axis.text.x = element_text(angle = 45, size=12, hjust=1), axis.text.y =
          element_text(hjust = 1, size=10), text = element_text(size = 12), axis.title.x = element_text(angle = 0, hjust = 0.5, vjust=0.5, size=12)) +
  scale_fill_manual(values=c('#AF8A5F', '#42858C', '#7F8E39')) +
  scale_color_manual(values=c('#AF8A5F', '#42858C', '#7F8E39')) +
  theme(panel.background = element_rect(fill = "white"), plot.background = element_rect(fill = "transparent", color = NA), panel.grid.major = element_blank(), panel.grid.minor = element_line(colour = "white")) +
  theme(legend.position="none") +
  geom_pwc(aes(group = Groups), method = "t_test", label = "{p.adj.format}{p.adj.signif}", hide.ns = TRUE, vjust = 0.1)
ARG_p7

list_oxa <- c('Oxazolidinone')
df_oxa_RU <- df_RU %>% 
  separate_rows(Class) %>% 
  group_by(Class) %>% 
  filter(Class %in% list_oxa)
ARG_p11 <- ggplot(df_oxa_RU, aes(x = Groups, y = Abund, color = Groups)) +
  theme_bw() +
  geom_boxplot(outlier.shape  = NA) +
  geom_jitter(aes(color = Groups), shape=21, alpha=0.75, fill="white", stroke=3.0, size=3.5, position = position_jitterdodge()) +
  labs(x = "", y = "Oxazolidinone (log10)") +
  theme(strip.text = element_text(colour = 'black'), axis.text.x = element_text(angle = 45, size=12, hjust=1), axis.text.y =
          element_text(hjust = 1, size=10), text = element_text(size = 12), axis.title.x = element_text(angle = 0, hjust = 0.5, vjust=0.5, size=12)) +
  scale_fill_manual(values=c('#AF8A5F', '#42858C', '#7F8E39')) +
  scale_color_manual(values=c('#AF8A5F', '#42858C', '#7F8E39')) +
  theme(panel.background = element_rect(fill = "white"), plot.background = element_rect(fill = "transparent", color = NA), panel.grid.major = element_blank(), panel.grid.minor = element_line(colour = "white")) +
  theme(legend.position="none") +
  geom_pwc(aes(group = Groups), method = "t_test", label = "{p.adj.format}{p.adj.signif}", hide.ns = TRUE, vjust = 0.1)
ARG_p11

list_rifa <- c('Rifampin')
df_rifa_RU <- df_RU %>% 
  separate_rows(Class) %>% 
  group_by(Class) %>% 
  filter(Class %in% list_rifa)
ARG_p12 <- ggplot(df_rifa_RU, aes(x = Groups, y = Abund, color = Groups)) +
  theme_bw() +
  geom_boxplot(outlier.shape  = NA) +
  geom_jitter(aes(color = Groups), shape=21, alpha=0.75, fill="white", stroke=3.0, size=3.5, position = position_jitterdodge()) +
  labs(x = "", y = "Rifampin (log10)") +
  theme(strip.text = element_text(colour = 'black'), axis.text.x = element_text(angle = 45, size=12, hjust=1), axis.text.y =
          element_text(hjust = 1, size=10), text = element_text(size = 12), axis.title.x = element_text(angle = 0, hjust = 0.5, vjust=0.5, size=12)) +
  scale_fill_manual(values=c('#AF8A5F', '#42858C', '#7F8E39')) +
  scale_color_manual(values=c('#AF8A5F', '#42858C', '#7F8E39')) +
  theme(panel.background = element_rect(fill = "white"), plot.background = element_rect(fill = "transparent", color = NA), panel.grid.major = element_blank(), panel.grid.minor = element_line(colour = "white")) +
  theme(legend.position="none") +
  geom_pwc(aes(group = Groups), method = "t_test", label = "{p.adj.format}{p.adj.signif}", hide.ns = TRUE, vjust = 0.1)
ARG_p12

list_mdr <- c('Multidrugresistance')
df_mdr_RU <- df_RU %>% 
  separate_rows(Class) %>% 
  group_by(Class) %>% 
  filter(Class %in% list_mdr)
ARG_p13 <- ggplot(df_mdr_RU, aes(x = Groups, y = Abund, color = Groups)) +
  theme_bw() +
  geom_boxplot(outlier.shape  = NA) +
  geom_jitter(aes(color = Groups), shape=21, alpha=0.75, fill="white", stroke=3.0, size=3.5, position = position_jitterdodge()) +
  labs(x = "", y = "Multi-drug resistance (log10)") +
  theme(strip.text = element_text(colour = 'black'), axis.text.x = element_text(angle = 45, size=12, hjust=1), axis.text.y =
          element_text(hjust = 1, size=10), text = element_text(size = 12), axis.title.x = element_text(angle = 0, hjust = 0.5, vjust=0.5, size=12)) +
  scale_fill_manual(values=c('#AF8A5F', '#42858C', '#7F8E39')) +
  scale_color_manual(values=c('#AF8A5F', '#42858C', '#7F8E39')) +
  theme(panel.background = element_rect(fill = "white"), plot.background = element_rect(fill = "transparent", color = NA), panel.grid.major = element_blank(), panel.grid.minor = element_line(colour = "white")) +
  theme(legend.position="none") +
  geom_pwc(aes(group = Groups), method = "t_test", label = "{p.adj.format}{p.adj.signif}", hide.ns = TRUE, vjust = 0.1)
ARG_p13

list_mer <- c('Mercuryresistance')
df_mer_RU <- df_RU %>% 
  separate_rows(Class) %>% 
  group_by(Class) %>% 
  filter(Class %in% list_mer)
ARG_p14 <- ggplot(df_mer_RU, aes(x = Groups, y = Abund, color = Groups)) +
  theme_bw() +
  geom_boxplot(outlier.shape  = NA) +
  geom_jitter(aes(color = Groups), shape=21, alpha=0.75, fill="white", stroke=3.0, size=3.5, position = position_jitterdodge()) +
  labs(x = "", y = "Mercury resistance (log10)") +
  theme(strip.text = element_text(colour = 'black'), axis.text.x = element_text(angle = 45, size=12, hjust=1), axis.text.y =
          element_text(hjust = 1, size=10), text = element_text(size = 12), axis.title.x = element_text(angle = 0, hjust = 0.5, vjust=0.5, size=12)) +
  scale_fill_manual(values=c('#AF8A5F', '#42858C', '#7F8E39')) +
  scale_color_manual(values=c('#AF8A5F', '#42858C', '#7F8E39')) +
  theme(panel.background = element_rect(fill = "white"), plot.background = element_rect(fill = "transparent", color = NA), panel.grid.major = element_blank(), panel.grid.minor = element_line(colour = "white")) +
  theme(legend.position="none") +
  geom_pwc(aes(group = Groups), method = "t_test", label = "{p.adj.format}{p.adj.signif}", hide.ns = TRUE, vjust = 0.1)
ARG_p14

##Arrange plots
library(cowplot); packageVersion("cowplot")
t_row <- plot_grid(ARG_p3, ARG_p1, ARG_p7, ARG_p6, rel_widths = c(2, 2, 1.5, 1.5), nrow=1)
m_row <- plot_grid(ARG_p2, ARG_p4, ARG_p5, rel_widths = c(2, 2, 2), nrow=1)
b_row <- plot_grid(ARG_p11, ARG_p12, ARG_p13, ARG_p14, rel_widths = c(2, 2, 2, 2), nrow=1)
plot_grid(t_row, m_row, b_row, rel_widths = c(2, 2, 2), ncol=1)

##Save plot
ggsave("Figure_2.tiff", units = c("in"), width=8, height=12, dpi=500, compression="lzw")