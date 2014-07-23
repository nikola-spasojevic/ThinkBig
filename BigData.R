bigData <- function(directory)
{
  data <- read.csv2(directory, colClasses = "character", header = TRUE,strip.white=TRUE)
  colnames(data) <- c('cif','total','sector','amount')  
  data <- subset(data,sector!='Internal (no class.)')
  data <- subset(data,sector!='Non-classified institutions') 
  data$cif <- as.numeric(data$cif)
  data$amount <- as.numeric(data$amount)
  data$total <- as.numeric(data$total)
  data$pct <- data$amount/data$total
  sectors <- sort(unique(data$sector))
  
  amount <- subset(data, select = c("cif", "total"))
  amount <- amount[!duplicated(amount[c("cif", "total")]),]
  
  pivot <- cast(data, cif~sector, value = "pct")
  pivot[is.na(pivot)] <- 0
  pivot <- data.frame(pivot)
  pivot$cif <- 1:nrow(pivot)
  pivot.pca <- prcomp(pivot[, -1], scale = TRUE, center = TRUE, cor = TRUE) 
  #rownames(pivot.pca$rotation) <- sectors
  
  pivot.x <- pivot.pca$x
  pivot.x <- remove_outliers(pivot.x)
  pivot.x[is.na(pivot.x)]<-0
  #all(colnames(pivot[,-1]) == sectors) 
  
  barchart(signif(sort(abs(pivot.pca$rotation[,1]),decreasing=TRUE)[1:10],2),
           main="Relative Variance per Sector (1st Component)",
           xlab="Variance",
           col=c("darkblue"))  
  scatterplot3d(pivot.x[,1:3],  highlight.3d = TRUE, col.axis = "blue",
                col.grid = "lightblue", pch = 20, xlab = "PC 1", 
                ylab = "PC 2", zlab = "PC 3")  
  
  myclust <- kmeans(pivot.x, 2)
  distances <- dist(pivot.x, method="euclidean")
  mycmdscale <- cmdscale(distances, 2) 
  plot(mycmdscale, cex=0)
  points(mycmdscale, col=myclust$cluster)
  mycmdscale <- cmdscale(distances, 3)
  s3d <- scatterplot3d(mycmdscale, color=myclust$cluster)
  plot3d(mycmdscale, col=myclust$cluster, size=5)
     
  rows.first <- pivot[pivot$"Medical...pharm..prod." > 0.2, 1]
  pivot.first <- pivot.x[rows.first, 1:3] 
  rows.second <- pivot[pivot$"Food...soft.drinks." > 0.2, 1]
  pivot.second <- pivot.x[rows.second, 1:3]
  pivot.second <- remove_outliers(pivot.second)
  rows.third <- pivot[pivot$"Capital.goods..misc" > 0.2, 1]
  pivot.third <- pivot.x[rows.third, 1:3]
  s3d1 <- scatterplot3d(pivot.first, col.axis = "blue",
                col.grid = "lightblue", pch = 20, xlab = "PC 1", 
                ylab = "PC 2", zlab = "PC 3")
  s3d1$points3d(pivot.second, pch = 20, col = "red")
  s3d1$points3d(pivot.third, pch = 20, col = "green")
  
  #plot(pivot.pca, type = "l", main="Variance Distribution per Component", xlab="Principle Component", ylab="Variance") 
}
