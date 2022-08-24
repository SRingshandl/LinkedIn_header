library(png)

x_dim <- 1584
y_dim <- 396

#defines whether lines are drawn
line_distance_limit <- 8
color_bonus <- 3
type_bonus <- 3
size_bonus <- 3
size_bonus_limit <- 6.5

#if you've run heatmap.R beforehand you should have a heatmap_gradient.png image in your working directory
#this defines the background. Use another image if you want.
img <- readPNG("heatmap_gradient.png")


measure_distance <- function(x1, y1, x2, y2){
  distance <- sqrt(abs(x1-x2) + abs(y2-y1))
  return(distance)
}

df <- data.frame(X = numeric(),
                 Y = numeric(),
                 SIZE = numeric(),
                 COLOR = numeric(),
                 TYPE = numeric())

#create points in df
for(i in 1:50){
  df[nrow(df) + 1,] <- data.frame(
    rnorm(1, mean = 0.5 * x_dim, sd = 0.1 * x_dim),
    rnorm(1, mean = 0.5 * y_dim, sd = 0.1 * x_dim),
    sample(c(2,3,4,5),1),
    sample(c("black", "blue4", "red2", "green4"),1),
    sample(c(21,rep(19,10)),1))
}

for(i in 1:50){
  df[nrow(df) + 1,] <- data.frame(
    rnorm(1, mean = 0.1 * x_dim, sd = 0.1 * x_dim),
    rnorm(1, mean = 0.3 * y_dim, sd = 0.1 * x_dim),
    sample(c(2,3,4,5),1),
    sample(c("black", "blue4", "red2", "green4"),1),
    sample(c(21,rep(19,10)),1))
}

for(i in 1:15){
  df[nrow(df) + 1,] <- data.frame(
    rnorm(1, mean = 0.85 * x_dim, sd = 0.1 * x_dim),
    rnorm(1, mean = 0.17 * y_dim, sd = 0.1 * x_dim),
    sample(c(2,3,4,5),1),
    sample(c("black", "blue4", "red2", "green4"),1),
    sample(c(21,rep(19,10)),1))
}

#clean outliers
remove_rows <- NULL
for(i in 1:nrow(df)){
  if(df$X[i] < 0 || df$X > x_dim || df$Y[i] < 0 || df$Y > y_dim){
    remove_rows <- c(remove_rows, i)
    }
}
df <- df[-remove_rows,]

#clean points too close
remove_rows <- NULL
for(i in 1:(nrow(df)-1)){
  if(i %in% remove_rows){next}
  for(k in (i+1):nrow(df)){
    if(k %in% remove_rows){next}
    dist <- measure_distance(df$X[i], df$Y[i], df$X[k], df$Y[k])
    min_dist <- (df$SIZE[i] + df$SIZE[k])
    if(dist < min_dist){
      remove_rows <- c(remove_rows, k)
    }
  }
}
df <- df[-remove_rows,]



#par(mar = c(0.4,0.4,0.4,0.4))
png("header.png", width = 1584, height = 396, units = "px", type = "cairo")
par(mar = c(0,0,0,0))
plot(1,1,
     xlim = c(0,x_dim),
     ylim = c(0,y_dim),
     axes = F,
     type = "n",
     xlab = "",
     ylab = "",
     xaxs="i", yaxs="i")

#apply background
lim <- par()
rasterImage(img, lim$usr[1], lim$usr[3], lim$usr[2], lim$usr[4])
grid()

#draw stuff
for(i in 1:nrow(df)){
  for(k in 1:nrow(df)){
    dist <- measure_distance(df$X[i], df$Y[i], df$X[k], df$Y[k])
    bonus <- 0
    if(df$COLOR[i] == df$COLOR[k]){
      bonus <- bonus + color_bonus
    }
    if(df$TYPE[i] == df$TYPE[k]){
      bonus <- bonus + type_bonus
    }
    if(df$SIZE[i] + df$SIZE[k] >= size_bonus_limit){
      bonus <- bonus + size_bonus
    }
    
    if((line_distance_limit + bonus) > dist){
      lines(c(df$X[i], df$X[k]), c(df$Y[i], df$Y[k]), col="black", lwd = 1)
    }
  }
}

points(df$X, df$Y, cex = df$SIZE, col = df$COLOR, pch = df$TYPE, lwd = 5, bg = "white")
box(col = "black")

#find points in dataframe by enabling this and comparing X and Y coordinates
#text(df$X, df$Y+20, labels = paste0(as.integer(df$X),"_", as.integer(df$Y)))

dev.off()
