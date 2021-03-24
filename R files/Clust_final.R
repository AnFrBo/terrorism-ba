#Clustering

'I Dummy coding
II Hierarchical Clustering
III Fuzzy C-Means'

'I Dummy Coding'

#selected variables who should be coded as dummies
t1_dummy <- t1[, c("vicinity", "multiple", "suicide", "attacktype1_txt", 
                   "weaptype1_txt", "weapsubtype1_txt", "targgroup")]
  #t2_dummy <- t2[, c("vicinity", "multiple", "suicide", "attacktype1_txt", 
  #                 "weaptype1_txt", "weapsubtype1_txt", "targgroup")]
  #t3_dummy <- t3[, c("vicinity", "multiple", "suicide", "attacktype1_txt", 
  #                 "weaptype1_txt", "weapsubtype1_txt", "targgroup")]
  #t4_dummy <- t4[, c("vicinity", "multiple", "suicide", "attacktype1_txt", 
  #                 "weaptype1_txt", "weapsubtype1_txt", "targgroup")]
  #t5_dummy <- t5[, c("vicinity", "multiple", "suicide", "attacktype1_txt", 
  #                 "weaptype1_txt", "weapsubtype1_txt", "targgroup")]

#transform them to factor variables
t1_fac <- data.frame(lapply(t1_dummy, as.factor))
  #t2_fac <- data.frame(lapply(t2_dummy, as.factor))
  #t3_fac <- data.frame(lapply(t3_dummy, as.factor))
  #t4_fac <- data.frame(lapply(t4_dummy, as.factor))
  #t5_fac <- data.frame(lapply(t5_dummy, as.factor))

#make dummies and delete one of the two leveled variables because they are redundant
t1_dummy <- createDummyFeatures(t1_fac)
t1_dummy <- t1_dummy[,-c(1,3,5)]
  #t2_dummy <- createDummyFeatures(t2_fac)
  #t2_dummy <- t2_dummy[,-c(1,3,5)]
  #t3_dummy <- createDummyFeatures(t3_fac)
  #t3_dummy <- t3_dummy[,-c(1,3,5)]
  #t4_dummy <- createDummyFeatures(t4_fac)
  #t4_dummy <- t4_dummy[,-c(1,3,5)]
  #t5_dummy <- createDummyFeatures(t5_fac)
  #t5_dummy <- t5_dummy[,-c(1,3,5)]

'II Hierarchical Clustering'

start_time <- Sys.time()
d <- dist.binary_jac(t1_dummy) #dummy df
end_time <- Sys.time()
end_time - start_time #takes around 6 min. to run

#not enough computational power in order to run code for each time frame
  #d2 <- dist.binary_jac(t2_dummy)
  #d3 <- dist.binary_jac(t3_dummy)
  #d4 <- dist.binary_jac(t4_dummy)
  #d5 <- dist.binary_jac(t5_dummy)

#run cluster analysis with first time period t1
  #d=a dissimilarity structure as produced by dist

#ward.D2
hclust <- hclust(d, method = "ward.D2") 
png("wardD2.png", units="in", width=8, height=5, res=300)
plot <- plot(hclust, labels = F, hang = -1, sub = "", xlab = "",
             main = list("Ward", 
                         font = 1, cex = 1.6))
dev.off()

#single linkage
hclust_sgl <- hclust(d, method = "single")
png("single.png", units="in", width=8, height=5, res=300)
plot <- plot(hclust_sgl, labels = F, hang = -1, sub = "", xlab = "",
             main = list("Single Linkage",
                         font = 1, cex = 1.6))
dev.off()

#complete linkage
hclust_com <- hclust(d, method = "complete")
png("complete.png", units="in", width=8, height=5, res=300)
plot <- plot(hclust_com, labels = F, hang = -1, sub = "", xlab = "",
             main = list("Complete Linkage",
                         font = 1, cex = 1.6))
dev.off()

#draw red lines around selected clusters
rect.hclust(hclust, k=2, border="red") 
groups <- cutree(hclust, k=2)
t1_hclust <- cbind(t1_fac, groups)

rm(t1_fac, t1_dummy)
  #t2_fac, t3_fac, t4_fac, t5_fac, t1_dummy, t2_dummy, t3_dummy, t4_dummy, t5_dummy

'III Fuzzy C-Means Clustering'

#conduct fuzzy c-means clustering with two clusters
start_time <- Sys.time()
t1_fuzzy2 <- fanny(d, 2, diss = T)
end_time <- Sys.time()
end_time - start_time #takes about 10 min. to run

#shows silhouette plot for two clusters
png("silhouette.png", units="in", width=8, height=5, res=300)
fviz_silhouette(t1_fuzzy2, palette = "Greys",
                ggtheme = theme_classic())
dev.off() #alt. color palette: YlOrRd

#conduct fuzzy c-means clustering with five clusters
start_time <- Sys.time()
t1_fuzzy5 <- fanny(d, 5, diss = T)
end_time <- Sys.time()
end_time - start_time #takes about 30 min. to run

#shows silhouette plot for five clusters
fviz_silhouette(t1_fuzzy5, palette = "jco",
                ggtheme = theme_minimal())

#compares hotbeds (if loaded) with clusters
comp_clust <- table(t1_fuzzy$clustering, t1_hclust$groups)
t1_fuzzy$coeff #very fuzzy clustering
t1_fuzzy$membership #really close

table(t2_hclust$groups, t2$hotbed) #distribution hotbeds