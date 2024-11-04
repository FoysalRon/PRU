############Circular plot for AMR#############
#Load library
library(circlize); packageVersion('circlize')
library(ggplot2); packageVersion('ggplot2')
#read the csv file
df <- read.csv("cirDD.csv", row.names = 1)
head(df)
#convert the table to a martix
df <- as.matrix(df)
#create a chord diagram
chordDiagram(df)
#create a chord diagram but without labeling 
chordDiagram(df, annotationTrack = "grid", preAllocateTracks = 1)
#add the labels and axis
circos.trackPlotRegion(track.index = 2, panel.fun = function(x, y) {
  xlim = get.cell.meta.data("xlim")
  ylim = get.cell.meta.data("ylim")
  sector.name = get.cell.meta.data("sector.index")
  
  #print labels 
  circos.text(mean(xlim), ylim[1] + 2.5, sector.name, 
              facing = "clockwise", niceFacing = TRUE, adj = c(0, 0.5), cex=0.6)
  
  #print axis
  circos.axis(h = "top", labels.cex = 0.5, major.tick.percentage = 0.2, 
              sector.index = sector.name, track.index = 2)
}, bg.border = NA)

#assign color to each group of strains and variable (for Kp)
col = c(Pristine='#AF8A5F', Rural='#42858C', Urban='#7F8E39',
        mcyA= "#BC3C29FF", mcyB= "#BC3C29FF", mcyC= "#BC3C29FF", mcyD= "#BC3C29FF", mcyE= "#BC3C29FF",
        mcyF= "#BC3C29FF", mcyG= "#BC3C29FF", mcyH= "#BC3C29FF",
        mcyI= "#BC3C29FF", mcyJ= "#BC3C29FF", 
        merA_A= "grey", merA_S= "grey", merA_P= "grey", merA_B= "grey" , LtxA= "#E18727FF", LtxB= "#E18727FF", LtxD= "#E18727FF", AnaF= "#7876B1FF", SxtA= "#E17597", SilC= "grey")

#create a chord diagram but without labeling 
chordDiagram(df, grid.col = col, annotationTrack = "grid", preAllocateTracks = 1)
#add the labels and axis
circos.trackPlotRegion(track.index = 2, panel.fun = function(x, y) {
  xlim = get.cell.meta.data("xlim")
  ylim = get.cell.meta.data("ylim")
  sector.name = get.cell.meta.data("sector.index")
  
  #print labels in bigger font
  circos.text(mean(xlim), ylim[1] + 2.5, sector.name, 
              facing = "clockwise", niceFacing = TRUE, adj = c(0, 0.5), cex=0.8)
  
  #print axis
  circos.axis(h = "top", labels.cex = 0.5, major.tick.percentage = 0.2, 
              sector.index = sector.name, track.index = 2)
}, bg.border = NA)

#saving the plot (high definition)
dev.copy(tiff,'Fig-S6.tiff', width=8, height=8, units="in", res=500)
dev.off()