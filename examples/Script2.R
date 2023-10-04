percolate <- function(N=10, p=1/2, draw=T, dual=F){
  P <-create.percolation(N,N+1, p=p, open=F, boundary.l=1)
  if(draw){
    plot.percolation(P, dual=F, col="blue", open=F)
  }
  C <- find.left.cluster(P)
  if(draw){
    plot.percolation(cluster2edges(C,P), lattice=F, lattice.dual=F, add=T, col="blue", lwd=2, open=F)
  }
  connected <- check.connected.lr(C)
  if (dual&&draw) {
    P.dual <- create.percolation.dual(P, open=F, boundary.t=1)
    plot.percolation(P.dual, dual=T, lattice=F, lattice.dual=F, add=T, col="red", open=F)
    C.dual <- find.top.cluster(P.dual)
    plot.percolation(cluster2edges(C.dual,P.dual), dual=T, lattice=F, lattice.dual=F, add=T, col="red", lwd=2, open=F)
    connected.dual <- check.connected.tb(C.dual)
  }
  return(connected)
}
