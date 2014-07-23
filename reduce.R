reduce <- function(A)
{
  svd1 <- svd(scale(A))
  plot(svd1$d^2/sum(svd1$d^2), pch = 19, xlab = "Singular vector", ylab = "Variance explained")
  
  u <- sing$u[, 1:dim]
  u <- remove_outliers(u)
  myclust <- kmeans(pivot.x, 2)
  
  scatterplot3d(u,  highlight.3d = TRUE, col.axis = "blue",
                col.grid = "lightblue", pch = 20, xlab = "PC 1", 
                ylab = "PC 2", zlab = "PC 3")
}
