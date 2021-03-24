'Heatmap 4'

t4t5_heatmap <- data.frame(aggregate(t4t5_freq, by= list(unique.values = t4t5_freq$tactics), FUN= length))
t4t5_heatmap <- t4t5_heatmap[which(t4t5_heatmap$Freq >= 50), 1]
t4t5_heatmap_name <- as.data.frame(droplevels(t4t5_heatmap))
colnames(t4t5_heatmap_name)[1] <- "tactics"

t4t5_freq$iyear <- as.numeric(as.character(t4t5_freq$iyear))

t4t5_freq$tactics <- as.character(t4t5_freq$tactics)

y <- 1
for(i in 1:nrow(t4t5_heatmap_name)) {
  df <- t4t5_freq[which(t4t5_freq$tactics %in% t4t5_heatmap_name$tactics[i]), ]
  #df <- arrange(df, -xtfrm(region_txt), decreasing = T)
  
  #df$tactics <- droplevels(df$tactics)
  name <- paste("tactics_hm1", y, sep ="")
  assign(name, df)
  
  y <- y+1
}

t4t5_list <- lapply(ls(pattern="tactics_hm1"), function(x) get(x))

mycol <- brewer.pal(3,"YlOrRd")
#Farbpaletten: https://davetang.org/muse/2010/12/06/making-a-heatmap-with-r/

create_heatmap <- function(list_element, df) {
  
  heatmap <- ggplot(data = list_element, mapping = aes(x = iyear, y = region_txt)) +
    geom_tile(aes(fill = relfreq)) +
    scale_fill_gradientn(colours = mycol, na.value = "white") +
    #alt: scale_fill_gradient(low = "yellow", high = "red")
    labs(x="",y="",fill = "rel. frequency", title=title) +
    theme(plot.title = element_text(size = rel(1.2), hjust = 0, vjust = 2, 
                                    face = "bold"),
          panel.background=element_rect(fill="white", colour="grey"), 
          legend.position= c(-0.45, 0.9), legend.title = element_text(size = rel(0.7), face = "bold"))
  
  histo <- ggplot(list_element, mapping = aes(x = iyear)) +
    geom_histogram(binwidth = 0.4) +
    labs(x="",y="") 
  
  histo_t1 <- ggplot(df, mapping = aes(x = iyear)) +
    geom_histogram(binwidth = 0.4) +
    labs(x="",y="")
  
  figure <- ggdraw() +
    draw_plot(heatmap, x = 0, y = 0, width = 0.7, height = 0.95) +
    draw_plot(histo_t1, x = 0.69, y = 0.15, width = 0.25, height = .3) +
    draw_plot(histo, x = 0.7, y = 0.5, width = 0.25, height = 0.3) +
    draw_plot_label(label = c("Number of overall attacks in 2008-2017", "Number of attacks specified in the title", "Only frequencies >0 are considered for the analysis"), size = 10,
                    x = c(0.66, 0.66, 0.62), y = c(0.48, 0.84, 0.1))
  
  ggsave(paste("heatmap", t, ".png", sep = ""), plot = figure, dpi = 150, width = 15, height = 8, units = "in", type = "cairo")
}

t <- 411
for (i in t4t5_list) {
  title <- i[[2]][[1]]
  create_heatmap(i, t4t5_freq)
  t <- t+1
}

