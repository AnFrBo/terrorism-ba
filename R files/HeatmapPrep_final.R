t1t2_xtabs <- xtabs( ~ tactics + iyear + region_txt, t1t2)
t1t2_xtabs <- as.data.frame.table(t1t2_xtabs)
t1t2_freq <- t1t2_xtabs[which(t1t2_xtabs$Freq > 1), ]

t2t3_xtabs <- xtabs( ~ tactics + iyear + region_txt, t2t3)
t2t3_xtabs <- as.data.frame.table(t2t3_xtabs)
t2t3_freq <- t2t3_xtabs[which(t2t3_xtabs$Freq > 1), ]

t3t4t5_xtabs <- xtabs( ~ tactics + iyear + region_txt, t3t4t5)
t3t4t5_xtabs <- as.data.frame.table(t3t4t5_xtabs)
t3t4t5_freq <- t3t4t5_xtabs[which(t3t4t5_xtabs$Freq > 1), ]

t4t5_xtabs <- xtabs( ~ tactics + iyear + region_txt, t4t5)
t4t5_xtabs <- as.data.frame.table(t4t5_xtabs)
t4t5_freq <- t4t5_xtabs[which(t4t5_xtabs$Freq > 1), ]

nyear <- data_selected %>% count(iyear)

t1t2_freq <- merge(x = t1t2_freq, y = nyear, by = "iyear", all.x = TRUE)
t2t3_freq <- merge(x = t2t3_freq, y = nyear, by = "iyear", all.x = TRUE)
t3t4t5_freq <- merge(x = t3t4t5_freq, y = nyear, by = "iyear", all.x = TRUE)
t4t5_freq <- merge(x = t4t5_freq, y = nyear, by = "iyear", all.x = TRUE)

t1t2_freq$relfreq <- t1t2_freq$Freq/t1t2_freq$n
t2t3_freq$relfreq <- t2t3_freq$Freq/t2t3_freq$n
t3t4t5_freq$relfreq <- t3t4t5_freq$Freq/t3t4t5_freq$n
t4t5_freq$relfreq <- t4t5_freq$Freq/t4t5_freq$n

