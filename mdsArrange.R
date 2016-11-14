
mdsArrange <- function(d){
  
  library(tibble)
  
  dst <- dist(t(scale(d)))
  dN <- dimnames(d)[[2]]
  dst.m <- as.matrix(dst)
  dimnames(dst.m) <- list(dN, dN)
  dst.cmd <- cmdscale(dst.m, k=2)
  x <- dst.cmd[,1]           
  y <- dst.cmd[,2]
  
  data_frame(ids = names(x), x = x, y = y)
  
}