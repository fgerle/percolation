N<-50

plot.lattice(N,N+1, col="black", lty=1, open=F)
plot.lattice(N,N+1, col="darkgrey", lty=1, open=F, add=T, dual=T)

P <-create.percolation(N,N+1, p=1/2, open=F, boundary.l=1)

plot.percolation(P, dual=F, col="blue", open=F)

C <- find.left.cluster(P)

plot.percolation(cluster2edges(C,P), lattice=F, lattice.dual=F, add=T, col="blue", lwd=2, open=F)

connected <- check.connected.lr(C)

P.dual <- create.percolation.dual(P, open=F, boundary.t=1)
plot.percolation(P.dual, dual=T, lattice=F, lattice.dual=F, add=T, col="red", open=F)
C.dual <- find.top.cluster(P.dual)
plot.percolation(cluster2edges(C.dual,P.dual), dual=T, lattice=F, lattice.dual=F, add=T, col="red", lwd=2, open=F)
connected.dual <- check.connected.tb(C.dual)



