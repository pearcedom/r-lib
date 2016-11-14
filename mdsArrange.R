
mdsArrange <- function(d){

  dst <- dist(t(scale(d)))
  dN <- dimnames(d)[[2]]
  dst.m <- as.matrix(dst)
  dimnames(dst.m) <- list(dN, dN)
  dst.cmd <- cmdscale(dst.m, k=2)

  data.frame(ids = names(x), x = dst.cmd[,1], y = dst.cmd[,2])

}
