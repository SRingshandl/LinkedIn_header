library("RColorBrewer")

#generate random values in a matrix
data <- matrix(runif(800, min = 0, max = 100), nrow = 200, ncol = 4)

#generate a heatmap, get the clustering data from it and sort data
heatorder <- heatmap(data)
heatorder_row <- heatorder$rowInd
heatorder_col <- heatorder$colInd
data <- data[heatorder_row,heatorder_col]

#get heatmap image without axes, ... surrounding the plot
#run this until you're happy with the outcome
png("heatmap.png", width = 1584, height = 396, units = "px", type = "cairo")
par(mar = c(0,0,0,0))
image(data, col = brewer.pal(8, "Blues"))
dev.off()

#add "transparency" gradient to image turning the left side white. Linear gradient
#stupidly slow but you only have to run it once
#around two minutes...
img <- readPNG("heatmap.png")

for(y in 1:y_dim){
  for(x in 1:x_dim){
    hex_col <- rgb(img[y,x,][1],img[y,x,][2],img[y,x,][3])
    colfunc <- colorRampPalette(c(hex_col, "white"))
    percentage <- as.integer(x/x_dim*100)
    if(percentage == 100){
      percentage <- 99
    }
    new_col <- col2rgb(colfunc(100)[100-percentage])/256
    img[y,x,][1] <- new_col[1]
    img[y,x,][2] <- new_col[2]
    img[y,x,][3] <- new_col[3]
  }
}

#save newly generated image with gradient
png("heatmap_gradient.png", width = 1584, height = 396, units = "px", type = "cairo")
par(mar = c(0,0,0,0))
grid::grid.raster(img)
dev.off()

#run header.R after this!

