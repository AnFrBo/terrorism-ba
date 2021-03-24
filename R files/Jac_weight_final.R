'Jaccard Matrix (weigthed)'

#the source code comes originally from https://github.com/cran/ade4/blob/master/R/dist.binary.R
#and only the marked (with ###START and ####END) part was adapted

dist.binary_jac <- function (df, diag = FALSE, upper = FALSE) {
  if (!(inherits(df, "data.frame") | inherits(df, "matrix"))) 
    stop("df is not a data.frame or a matrix")
  df <- as.matrix(df)
  if(!is.numeric(df))
    stop("df must contain  numeric values")
  if (any(df < 0)) 
    stop("non negative value expected in df")
  nlig <- nrow(df)
  d.names <- row.names(df)
  if(is.null(d.names))
    d.names <- 1:nlig
  nlig <- nrow(df)
  df <- as.matrix(1 * (df > 0))
  
  #########START
  
  #split in two level variables and >2 level variables
  df_s1 <- df[,1:3]
  df_s2 <- df[,-c(1:3)]
  
  df_s1_t <- t(df_s1)
  a1 <- df_s1 %*% df_s1_t
  b1 <- df_s1 %*% (1- df_s1_t)
  c1 <- (1-df_s1) %*% df_s1_t
  
  df_s2_t <- t(df_s2)
  a2 <- df_s2 %*% df_s2_t
  b2 <- df_s2 %*% (1 - df_s2_t)
  c2 <- (1-df_s2) %*% df_s2_t
  
  #weighting of obs. of 0/1 - 1/0 of >2 level variables
  a <- a1+a2
  b <- b1+(0.5*b2)
  c <- c1+(0.5*b2)
  d <- a/(a+b+c)
  
  #########END
  
  #transform to distance
  d <- sqrt(1 - d)
  d <- as.dist(d)
  
  attr(d, "Size") <- nlig
  attr(d, "Labels") <- d.names
  attr(d, "Diag") <- diag
  attr(d, "Upper") <- upper
  #attr(d, "method") <- METHODS[method]
  attr(d, "call") <- match.call()
  class(d) <- "dist"
  return(d)
}

