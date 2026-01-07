# Step 1: Load built-in iris dataset
data(iris)

# Step 2: export CSVs – keep as you already have
write.csv(iris, "iris.csv", row.names = FALSE)
iris_numeric <- iris[, 1:4]
write.csv(iris_numeric, "iris_without_species.csv", row.names = FALSE)

# Step 3: Use petal features for clustering (simple & matches our plot)
df <- iris[, c("Petal.Length", "Petal.Width")]

# Step 4: Run K-means
set.seed(123)
km <- kmeans(df,
             centers = 3,
             nstart  = 20)

cat("Cluster sizes:\n")
print(km$size)

cat("\nCluster centers:\n")
print(km$centers)

cm <- table(iris$Species, km$cluster)
cat("\nConfusion matrix (Species vs cluster):\n")
print(cm)

cat("\nCovariance Matrix (Petal.Length & Petal.Width):\n")
cov_matrix <- cov(df)
print(cov_matrix)

cat("\nCorrelation Matrix (Petal.Length & Petal.Width):\n")
cor_matrix <- cor(df)
print(cor_matrix)

png("correlation_matrix.png", width = 600, height = 600)

image(1:ncol(cor_matrix),
      1:nrow(cor_matrix),
      cor_matrix,
      col = colorRampPalette(c("blue", "white", "red"))(100),
      axes = FALSE,
      xlab = "",
      ylab = "",
      main = "Correlation Matrix")

axis(1, at = 1:ncol(cor_matrix), labels = colnames(cor_matrix))
axis(2, at = 1:nrow(cor_matrix), labels = rownames(cor_matrix))

text(expand.grid(1:ncol(cor_matrix), 1:nrow(cor_matrix)),
     labels = round(cor_matrix, 2))

dev.off()

cat("\nFinal K-means Values:\n")

cat("Total Within-Cluster Sum of Squares (tot.withinss):\n")
print(km$tot.withinss)

cat("\nBetween-Cluster Sum of Squares (betweenss):\n")
print(km$betweenss)

cat("\nTotal Sum of Squares (totss):\n")
print(km$totss)

cat("\nRatio of Between SS to Total SS:\n")
print(km$betweenss / km$totss)

png("kmeans_clusters.png")

plot(df$Petal.Length, df$Petal.Width,
     col  = km$cluster,
     pch  = 19,
     xlab = "Petal Length",
     ylab = "Petal Width",
     main = "K-means Clusters (k = 3)")

points(km$centers[,"Petal.Length"],
       km$centers[,"Petal.Width"],
       pch = 8,
       cex = 2,
       lwd = 2,
       col = 1:3)

text(km$centers[,"Petal.Length"],
     km$centers[,"Petal.Width"],
     labels = paste0("C", 1:3),
     pos = 3)

dev.off()

png("species_actual.png")

plot(df$Petal.Length, df$Petal.Width,
     col  = "black",
     pch  = 19,
     xlab = "Petal Length",
     ylab = "Petal Width",
     main = "Actual Species")

dev.off()

plot(df$Petal.Length, df$Petal.Width,
     col  = km$cluster, pch = 19,
     xlab = "Petal Length", ylab = "Petal Width",
     main = "K-means Clusters (k = 3)")

points(km$centers[,"Petal.Length"],
       km$centers[,"Petal.Width"],
       pch = 8, cex = 2, lwd = 2, col = 1:3)

plot(df$Petal.Length, df$Petal.Width,
     col  = "black", pch = 19,
     xlab = "Petal Length", ylab = "Petal Width",
     main = "Actual Species")

cat("\n--- TEST PRINT: CODE EXECUTED SUCCESSFULLY ---\n")

