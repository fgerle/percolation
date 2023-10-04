
## This is a simple script which provides functions for plotting and analyzing 
#  2-dimensional percolation on a rectangular lattice
#
#  (C) Fabian Gerle 2016

#' Create a new Percolation
#'
#' This function generates a new random percolation
#' @param N Number of rows
#' @param K Number of columns
#' @param p Probability to draw an edge
#' @param open Specify the boundary
#' @param boundary.l boundary condition for the left boundary
#' @param boundary.b boundary condition for the bottom boundary
#' @param boundary.r boundary condition for the right boundary
#' @param boundary.t boundary condition for the top boundary
#' @keywords percolation new create
#' @export
#' @examples
#' create.percolation()
create.percolation <- function(#
			N=10,#		Number of rows
			K=10,#		Number of columns
			p=0.5,#		Probability to draw an edge
			open=TRUE,#
			boundary.l=2,#	boundary condition for the left, bottom right and top
			boundary.b=2,#	boundary respectively. Choose 1,0 or 2 for random 
			boundary.r=2,#	boundary conditions.
			boundary.t=2) {
# Initialize an empty vector
  edges <- NULL

# In the first N*(K+1) components store the horizontal edges starting from 
# the bottom left edge and fill the boundaries according to the boundary conditions.
  if (!open) {
    boundary.l <- 0
    boundary.r <- 0
    boundary.t <- 2
    boundary.b <- 2
  }
  if (open) {
  for (i in 1:N) {
    if(boundary.l==0) {
      edges <- c(edges,0)
    } else if(boundary.l==1) {
      edges <- c(edges,1)
    } else if(boundary.l==2) {
      edges <- c(edges,rbinom(1,1,p))
    }
    edges <- c(edges,rbinom(K-1, 1, p))
    if(boundary.r==0) {
      edges <- c(edges,0)
    } else if(boundary.r==1) {
      edges <- c(edges,1)
    } else if(boundary.r==2) {
      edges <- c(edges,rbinom(1,1,p))
    }
  }

# In the next (N+1)*K components store the vertical edges, again starting from
# the bottom left edge and fill the boundaries according to the boundary conditions.
  if(boundary.b==0) {
    edges <- c(edges,rep(0,K))
  } else if(boundary.b==1) {
    edges <- c(edges,rep(1,K))
  } else if(boundary.b==2) {
    edges <- c(edges,rbinom(K,1,p))
  }
  edges <- c(edges,rbinom((N-1)*K,1,p))
  if(boundary.t==0) {
    edges <- c(edges,rep(0,K))
  } else if(boundary.t==1) {
    edges <- c(edges,rep(1,K))
  } else if(boundary.t==2) {
    edges <- c(edges,rbinom(K,1,p))
  }
  } else {  # Create a closed lattice
    for (i in 1:N) {
      edges <- c(edges,rbinom(K-1, 1, p))
  }

# In the next (N+1)*K components store the vertical edges, again starting from
# the bottom left edge and fill the boundaries according to the boundary conditions.
  for (i in 1:((N-1)*K)) {
  if (((i%%K)!=1)) {
    edges <- c(edges,rbinom(1,1,p))
  } else{
    edges <- c(edges,1)
  }
}
}
# Additionally store the edge-probability, and the dimensions as attributes in the vector
  attributes(edges)$p = p
  attributes(edges)$N = N
  attributes(edges)$K = K
  attributes(edges)$open = open
  attributes(edges)$dual = FALSE

# Return the vector
  return(edges)
}#


## Create the dual percolation to a given percolation
create.percolation.dual <- function(#
			percolation,#	The percolation to which the dual shall be created
			open =TRUE,#
			boundary.l=2,#
			boundary.b=2,# 
			boundary.r=2,#
			boundary.t=2) {

## Setup some variables containing the information from the percolation
  A <- percolation
  K <- attr(A, "K")
  N <- attr(A, "N")
  open <- attr(A, "open")
  p <- 1-attr(A, "p")

## Initialize the edges
  edges <- NULL

## Fill the edge vector first with the horizontal edges
  if (!open){
    K <- K
    boundary.t <- 0
    boundary.b <- 0
    boundary.r <- 0
    boundary.l <- 0
  }
  if (open) {
  for (i in 1:(N+1)) {
    if(boundary.l==0) {
      edges <- c(edges,0)
    } else if(boundary.l==1) {
      edges <- c(edges,1)
    } else if(boundary.l==2) {
      edges <- c(edges,rbinom(1,1,p))
    }
    for (j in 1:(K)) {
      edges <- c(edges, as.integer(!A[N*(K+1)+j+(i-1)*(K)]))
    }
    if(boundary.r==0) {
      edges <- c(edges,0)
    } else if(boundary.r==1) {
      edges <- c(edges,1)
    } else if(boundary.r==2) {
      edges <- c(edges,rbinom(1,1,p))
    }
  }

## And then with the vertical edges
  if(boundary.b==0) {
    edges <- c(edges,rep(0,K+1))
  } else if(boundary.b==1) {
    edges <- c(edges,rep(1,K+1))
  } else if(boundary.b==2) {
    edges <- c(edges,rbinom(K+1,1,p))
  }
  for (i in 1:((N)*(K+1))) {
    edges <- c(edges,!A[i])
  }
  if(boundary.t==0) {
    edges <- c(edges,rep(0,K+1))
  } else if(boundary.t==1) {
    edges <- c(edges,rep(1,K+1))
  } else if(boundary.t==2) {
    edges <- c(edges,rbinom(K+1,1,p))
  }
} else {
  edges <- rbinom(K-2,1,p)#rep(1,K-2)
  for (i in 1:(N-1)) {
    for (j in 1:(K-2)) {
      edges <- c(edges, as.integer(!A[(K-1)*(N-1)+i*(K)+j]))
    }
  }
  edges <- c(edges, rep(1,K-2))
  
  ## And then with the vertical edges
  
  for (i in 1:((N)*(K-1))) {
    edges <- c(edges,!A[i])
  }

}
## And add the edge-probability and dimensions as attributes
  if (open) {
  attributes(edges)$N = N+1
  attributes(edges)$K = K+1
  } else {
    attributes(edges)$N = N+1
    attributes(edges)$K = K-1
  }
  attributes(edges)$p = 1-p
  attributes(edges)$open=open
  attributes(edges)$dual=TRUE
## Return edges
  return(edges)
}#


## Initialize the graphical output by creating an empty plot of the right dimensions
init.plot <- function(#
			N,#
			K,#
			plot=TRUE#  actually do open a plot or only calculate the scaling factor
			) {

## We will plot into a 10 by 10 square so first calculate the ratios in x and y direction 
  xrat <- 10/K;
  yrat <- 10/N;

## Choose the scaling ratio and set the x and y dimensions of the plot accordingly
  ratio <- min(xrat,yrat)
  xdim <- K*ratio
  ydim <- N*ratio
  s <- ratio
  par(mar=c(1,1,1,1)-1)
  if (plot){
    plot(NULL, NULL, xlim=c(0,xdim+s), ylim=c(0,ydim+s), axes=FALSE, asp=1)
  }

return(s)
}


## Draw the Lattice
plot.lattice <- function(#
			N,#	Number of rows
			K,#	Number of colums
			dual=FALSE,#	draw the dual lattice
			add=FALSE,#	add the lattice to an existing plot
			col="lightgrey",# 	color for the lattice
			lty=1,#		linetype
			open=TRUE,#   draw an open or closed lattice
			psc=1.5) {#	scaling constant for the Points.

  if (!add) {
    s <- init.plot(N,K,plot=TRUE)
  } else {
    s <- init.plot(N,K,plot=FALSE)
  }

  if (!dual) {
    if (open) {
      ## Initiate vectors for horizontal lines
      x0.horiz <- rep(0,N+1)
      x1.horiz <- rep(K+1,N+1)
      y0.horiz <- 1:(N)
      y1.horiz <- 1:(N)
      
      ## Initiate vectors for vert lines
      x0.vert <- 1:(K)
      x1.vert <- 1:(K)
      y0.vert <- rep(0,K+1)
      y1.vert <- rep(N+1,K+1)
    } else {
      ## Initiate vectors for horizontal lines
      x0.horiz <- rep(1,N)
      x1.horiz <- rep(K,N)
      y0.horiz <- 1:(N)
      y1.horiz <- 1:(N)
      
      ## Initiate vectors for vert lines
      x0.vert <- 1:(K)
      x1.vert <- 1:(K)
      y0.vert <- rep(1,K)
      y1.vert <- rep(N,K)
      
    }

  
    ## Initiate vectors for gridpoints
    points.y <- rep(1:N,K)
    points.x <- rep(1:K,each=N)

    ## Draw points
    points(points.x*s, points.y*s, pch=16, col=col,cex =s*psc);
    

    ## Draw horizontal lines
    segments(x0.horiz*s, y0.horiz*s, x1.horiz*s, y1.horiz*s, col=col, lty=lty);
   
    ## Draw horizontal lines
    segments(x0.vert*s, y0.vert*s, x1.vert*s, y1.vert*s, col=col, lty=lty);
  
  
  } else {
    if (open) {
      ## Initiate vectors for horizontal grid lines
      x0.horiz.dual <- rep(0,N+1)
      x1.horiz.dual <- rep(K+1,N+1)
      y0.horiz.dual <- 1:(N+1)-1/2;
      y1.horiz.dual <- 1:(N+1)-1/2;
      
      ## Initiate vectors for vert grid lines
      x0.vert.dual <- 1:(K+1)-1/2;
      x1.vert.dual <- 1:(K+1)-1/2;
      y0.vert.dual <- rep(0,K+1)
      y1.vert.dual <- rep(N+1,K+1)
      ## Initiate vectors for gridpoints
      points.y.dual <- rep(1:(N+1),K+1)-1/2
      points.x.dual <- rep(1:(K+1),each=(N+1))-1/2
    } else {
      ## Initiate vectors for horizontal grid lines
      x0.horiz.dual <- rep(1,N)+1/2
      x1.horiz.dual <- rep(K-1,N)+1/2
      y0.horiz.dual <- 1:(N+1)-1/2;
      y1.horiz.dual <- 1:(N+1)-1/2;
      
      ## Initiate vectors for vert grid lines
      x0.vert.dual <- 1:(K-1)+1/2;
      x1.vert.dual <- 1:(K-1)+1/2;
      y0.vert.dual <- rep(1,K)-1/2
      y1.vert.dual <- rep(N,K)+1/2
      ## Initiate vectors for gridpoints
      points.y.dual <- rep(1:(N+1),K-1)-1/2
      points.x.dual <- rep(1:(K-1),each=(N+1))+1/2
    }
    
    segments(x0.horiz.dual*s, y0.horiz.dual*s, x1.horiz.dual*s, y1.horiz.dual*s, col=col, lty=lty);
    segments(x0.vert.dual*s, y0.vert.dual*s, x1.vert.dual*s, y1.vert.dual*s, col=col, lty=lty);
    points(points.x.dual*s, points.y.dual*s, pch=16, col="white" ,cex =s*psc);
    points(points.x.dual*s, points.y.dual*s, pch=1, col=col,cex =s*psc);
    
  }
}#

## Function for Plotting percolations (or more generally partial lattices)
plot.percolation <- function(#
			percolation,#
			dual=FALSE,#
			lattice=TRUE,#
			lattice.dual= TRUE,#
			add=FALSE,#
			col="blue",#
			col.dual="red",#
			col.lattice="lightgrey",#
			lty = 1,#
			lwd = 1,#
			lty.dual = 3,#
			open=T,#
			psc=1.5#
			) {
  
  A <- percolation
  open <- attr(percolation,"open")

  if (dual) {
    if (open) {
    dimN <- attr(percolation,"N") - 1
    dimK <- attr(percolation,"K") - 1
    } else {
      dimN <- attr(percolation,"N")
      dimK <- attr(percolation,"K")
    }
    lty.dual <- lty
  } else {
    dimN <- attr(percolation,"N")
    dimK <- attr(percolation,"K")
  }

    N <- attr(percolation,"N")
    K <- attr(percolation,"K")
  
  p <- attr(percolation,"p")

  s <- init.plot(dimN, dimK, plot=!add)

  if (lattice) {
    plot.lattice(dimN,dimK,dual=F,add=T,col=col.lattice,lty=1,psc=psc, open=open)
  }

  if (lattice.dual) {
    plot.lattice(dimN,dimK,dual=T,add=T,col=col.lattice,lty=3,psc=psc,open=open)
  }

  dual.offset <- 1/2*s
 
  ## Initiate vectors for horizontal grid lines
  x0.horiz <- NULL
  x1.horiz <- NULL
  y0.horiz <- NULL
  y1.horiz <- NULL
  if (open){
  for (i in 1:(N*(K+1))) {
    if (A[i]==1) {
      x0.horiz <- c(x0.horiz,((i-1)%%(K+1)));
      x1.horiz <- c(x1.horiz,((i-1)%%(K+1))+1);
      y0.horiz <- c(y0.horiz,((i-1)%/%(K+1))+1);
      y1.horiz <- c(y1.horiz,((i-1)%/%(K+1))+1);
    }  
  }
  } else {
    for (i in 1:(N*(K-1))) {
      if ((A[i]==1)) {
        x0.horiz <- c(x0.horiz,((i-1)%%(K-1)+1));
        x1.horiz <- c(x1.horiz,((i-1)%%(K-1))+2);
        y0.horiz <- c(y0.horiz,((i-1)%/%(K-1))+1);
        y1.horiz <- c(y1.horiz,((i-1)%/%(K-1))+1);
      }  
    }
  }
  
  
  ## Initiate vectors for vertical grid lines
  x0.vert <- NULL
  x1.vert <- NULL
  y0.vert <- NULL
  y1.vert <- NULL
  if (open) {
  for (i in 1:(K*(N+1))) {
    if (A[i+(N*(K+1))]==1) {
      y0.vert <- c(y0.vert,((i-1)%/%(K)));
      y1.vert <- c(y1.vert,((i-1)%/%(K))+1);
      x0.vert <- c(x0.vert,((i-1)%%(K)+1));
      x1.vert <- c(x1.vert,((i-1)%%(K)+1));
    }  
  }
  } else {
    for (i in 1:((K)*(N-1))) {
      if (A[i+(N*(K-1))]==1) {
        y0.vert <- c(y0.vert,((i-1)%/%(K))+1);
        y1.vert <- c(y1.vert,((i-1)%/%(K))+2);
        x0.vert <- c(x0.vert,((i-1)%%(K)+1));
        x1.vert <- c(x1.vert,((i-1)%%(K)+1));
      }
    }
  }
  
  if (open){
     dual.offset.h <- 1/2*s
     dual.offset.v <- 1/2*s
  } else {
    dual.offset.h <- -1/2*s
    dual.offset.v <- 1/2*s
  }
  
  if (dual){
    segments(x0.horiz*s-dual.offset.h, y0.horiz*s-dual.offset.v, x1.horiz*s-dual.offset.h, y1.horiz*s-dual.offset.v, col=col.dual, lty=lty.dual, lwd=lwd);
    segments(x0.vert*s-dual.offset.h, y0.vert*s-dual.offset.v, x1.vert*s-dual.offset.h, y1.vert*s-dual.offset.v, col=col.dual, lty=lty.dual, lwd=lwd);
  } else {
    segments(x0.horiz*s, y0.horiz*s, x1.horiz*s, y1.horiz*s, col=col, lty=lty, lwd=lwd);
    segments(x0.vert*s, y0.vert*s, x1.vert*s, y1.vert*s, col=col, lty=lty, lwd=lwd);
  }
  
}#


## Function for checking, if two vertices are neighbors in a given percolation
are.neighbors <- function(percolation, vertexA, vertexB) {#
  x1 <- vertexA[1]
  y1 <- vertexA[2]
  x2 <- vertexB[1]
  y2 <- vertexB[2]
  x <- min(x1,x2)
  X <- max(x1,x2)
  Y <- max(y1,y2)
  y <- min(y1,y2)
  N <- attr(percolation,"N")
  K <- attr(percolation,"K") 
  open <- attr(percolation,"open")
  if (!open) {
    N <- N
    K <- K
  }
  if (open){
  if ((abs(x1-x2)>1)|(abs(y1-y2)>1)|((x1==x2)&(y1==y2))|((abs(x1-x2)==1)&(abs(y1-y2)==1))) {
    return(0)
  }else{
    if (abs(x1-x2)==1) {
      if ((y<=0)|(Y>=(N+1))|(x<0)|(X>K+1)) {
        return(0)
      } else {
        return(percolation[(y-1)*(K+1)+x+1])
      }
    }
    if (abs(y1-y2)==1) {
      if((x<=0)|(X>=(K+1))|(y<0)|(Y>N+1)) {
        return(0)
      } else {
        return(percolation[N*(K+1) + y*(K)+x])
      }
    }
  }
  } else {
    if (((X-x)>1)|((Y-y)>1)|((x1==x2)&(y1==y2))|((abs(x1-x2)==1)&(abs(y1-y2)==1))) {
      return(0)
    }else{
      if ((X-x)==1) {
        if ((y<0)|(Y>N)|(x<0)|(X>K)) {
          return(0)
        } else {
          return(percolation[(y)*(K-1)+x+1])
        }
      }
      if ((Y-y)==1) {
        if((x<0)|(X>K)|(y<0)|(Y>N)) {
          return(0)
        } else {
          return(percolation[N*(K-1) + y*(K)+x+1])
        }
      }
    }
  }
}#

has.neighbor <- function(cluster, vertex) {

}


## Fills a given point configuration (matrix) with all points that are in the same cluster as one of
## given points. Returns a matrix.
fill.cluster <- function(percolation, cluster) {#
  N <- attr(percolation,"N")
  K <- attr(percolation,"K") 
  open <- attr(percolation,"open")
  if (!open) {
    K <- K-2
    N <- N-2
  }
  cluster1 <- matrix(0, K+2, N+2)
  while (sum(cluster1)!=sum(cluster)) {
    cluster1 <- cluster
    for (i in 1:(K+2)) {
      for (j in 1:(N+2)) {
        if (cluster[i,j]==1) {
          if ((i+1)<=(K+2)){
          cluster[i+1,j] <- cluster[i+1,j]|are.neighbors(percolation, c(i-1,j-1),c(i,j-1)) 
          }
          cluster[i-1,j] <- cluster[i-1,j]|are.neighbors(percolation, c(i-1,j-1),c(i-2,j-1))
          if ((j+1) <= (N+2)) {
          cluster[i,j+1] <- cluster[i,j+1]|are.neighbors(percolation, c(i-1,j-1),c(i-1,j))
          }
          cluster[i,j-1] <- cluster[i,j-1]|are.neighbors(percolation, c(i-1,j-1),c(i-1,j-2))
        }
      }
    } 
    }
  return(cluster)
}#

fill.cluster2 <- function(percolation, cluster) {#
  N <- attr(percolation,"N")
  K <- attr(percolation,"K") 
  cluster1 <- matrix(0, K+2, N+2)
  while (sum(cluster1)!=sum(cluster)) {
    cluster1 <- cluster
    i1 <- NULL
    j1 <- NULL
    for (i in 1:(K+1)) {
      for (j in 1:(N+1)) {
       if (cluster[i,j]) {
         i1 <- c(i1,i)
         j1 <- c(j1,j)
       }
      }
    }
    for (i in i1) {
      for (j in j1) {
        cluster[i+1,j] <- cluster[i+1,j]|are.neighbors(percolation, c(i-1,j-1),c(i,j-1)) 
        cluster[i-1,j] <- cluster[i-1,j]|are.neighbors(percolation, c(i-1,j-1),c(i-2,j-1))
        cluster[i,j+1] <- cluster[i,j+1]|are.neighbors(percolation, c(i-1,j-1),c(i-1,j))
        cluster[i,j-1] <- cluster[i,j-1]|are.neighbors(percolation, c(i-1,j-1),c(i-1,j-2))
      }
    }
  }
  return(cluster)
}#

## Turn a point configuration into a vector containing only the edges from a percolation
cluster2edges <- function(cluster,percolation) {#
  edges <- NULL
  sites <- as.vector((cluster))
  K <- length(cluster[,1])
  N <- length(cluster[1,])
  open <- attr(percolation,"open")
  if (open) {
  for (i in (K+1):((N-1)*(K))) {
    if (i%%(K) != 0) {
      edges <- c(edges, sites[i]&sites[i+1])
    }
  }
  for (i in 1:((N-1)*(K))) {
    if ((i%%(K) != 0)&(i%%(K) != 1)) {
      edges <- c(edges, sites[i]&sites[i+K])
    }
  }
  edges <- as.integer(edges&percolation)
  #edges <- as.integer(edges)
  attributes(edges)$p = 0.5
  attributes(edges)$N = N-2
  attributes(edges)$K = K-2
  attributes(edges)$open = open
  } else {
    for (i in 1:(N*K)) {
      if (i%%(K) != 0) {
        edges <- c(edges, sites[i]&sites[i+1])
      }
    }
    for (i in 1:((N-1)*K)) {
      #if ((i%%(K-1) != 0)&(i%%(K-1) != 1)) {
        edges <- c(edges, sites[i]&sites[i+(K)])
      #}
    }
    edges <- as.integer(edges&percolation)
    attributes(edges)$p = 0.5
    attributes(edges)$N = N
    attributes(edges)$K = K
    attributes(edges)$open = open
  }
  return(edges)
}#


## Find all points connected to the top boundary
find.top.cluster <- function(percolation, algorithm=1) {#
  N <- attr(percolation,"N")
  K <- attr(percolation,"K") 
  if (!attr(percolation,"open")) {
    K <- K-2
    N <- N-2
  }
  cluster <- matrix(0, K+2, N+2)
  for (i in 1:K) {
    cluster[i+1,N+1] <- percolation[i+N*K+N*(K+1)]
  }
  if (algorithm == 1) {
     cluster <- fill.cluster(percolation, cluster)
  } else {
     cluster <- fill.cluster2(percolation, cluster)
  }
  return(cluster)
}#


find.cluster <- function(percolation, point=c(1,1), algorithm=1) {#
  N <- attr(percolation,"N")
  K <- attr(percolation,"K") 
  if (!attr(percolation,"open")) {
    K <- K-2
    N <- N-2
  }
  cluster <- matrix(0, K+2, N+2)
  cluster[point[1],point[2]] <- 1
  if (algorithm == 1) {
    cluster <- fill.cluster(percolation, cluster)
  } else {
    cluster <- fill.cluster2(percolation, cluster)
  }
  return(cluster)
}#

## Find all points connected to the left boundary
find.left.cluster <- function(percolation, algorithm=1) {#
  N <- attr(percolation,"N")
  K <- attr(percolation,"K") 
  open <- attr(percolation,"open")
  if (!open) {
    K <- K-2
    N <- N-2
  }
  cluster <- matrix(0, K+2, N+2)
  if (open){
  for (i in 1:N) {
    cluster[1,i+1] <- percolation[1+(i-1)*(K+1)]
  }
  } else {
    cluster[1,] <- 1
  }
  if (algorithm == 1) {
     cluster <- fill.cluster(percolation, cluster)
  } else {
     cluster <- fill.cluster2(percolation, cluster)
  }
  return(cluster)
}#

## Check if a cluster connects from left to right
check.connected.lr <- function(cluster) {
l <- sum(cluster[1,])
r <- sum(cluster[length(cluster[,1]),])

return((l>0)&(r>0))
}

## Check if a cluster connects top left to bottom
check.connected.tb <- function(cluster) {
t <- sum(cluster[,1])
b <- sum(cluster[,length(cluster[1,])])

return((t>0)&(b>0))
}

## A testing function
percolation <- function(#
			N=10,#
			K=10,#
			p=1/2,#
			dual=T,#
			lattice=T,#
			col="blue",#
			col.dual="red",#
			col.lattice="lightgrey",#
			psc=1.5,#
			timer=FALSE
			) {
  
  create.timer <- system.time(A <- create.percolation(N,K,p))[[3]]
  dual.timer <- system.time(A.dual <- create.percolation.dual(A))[[3]]
  plot.timer <- system.time(plot.percolation(A, lattice=T, lattice.dual = F, dual=F,col=col, psc=psc, col.lattice = col.lattice))[[3]]
  plot.timer <- plot.timer + system.time(plot.percolation(A.dual, lattice=F, lattice.dual = T, dual=T, add=T, lty=1,col.dual=col.dual, psc=psc, col.lattice = col.lattice))[[3]]
  find.timer <- system.time(cluster <- find.top.cluster(A))[[3]]
  print(check.connected.tb(cluster))
  A.left <- cluster2edges(cluster,A)
  #print(cluster)
  plot.percolation(A.left, lattice=F, lattice.dual=F, dual=F, add=T, lwd=3, lty=1, col=col)
  find.dual.timer <- system.time(cluster.dual <- find.left.cluster(A.dual))[[3]]
  A.left.dual <- cluster2edges(cluster.dual,A.dual)
  plot.percolation(A.left.dual, lattice=F, lattice.dual=F, dual=T, add=T, lwd=3, lty=1, col.dual=col.dual)
  time <- c(create.timer,dual.timer,plot.timer,find.timer,find.dual.timer, find.timer+find.dual.timer)
  names(time) <- c("create", "create dual", "plot", "find", "find dual", "total find")
  if (timer) {return(time)}
}#

percolation.stats <- function(runs=10, N=10, K=10, p=c(1/2), Leftright=TRUE, runtime=FALSE) {
  ret <- NULL
  for (j in 1:length(p)){
    success <- 0
  for (i in 1:runs) {
    A <- create.percolation(N,K,p[j])
    if (Leftright) {
      cluster <- find.left.cluster(A)
      success <- success + check.connected.lr(cluster)
    } else {
      cluster <- find.top.cluster(A)
      success <- success + check.connected.tb(cluster)
    }
  }
  ret<-c(ret,success)
  }
  return(ret)
}

percolation.benchmark <- function(N=10,K=10,p=1/2,runs=100) {
  t<- NULL
  for (i in 1:runs) {
    t <- c(t,percolation(N,K,p,timer=T)[[6]])
  }
  return(t)
}

test <- function(N=10, K=11) {
  A <- create.percolation(N,K,1/2, open=F)
  A.cluster <- find.left.cluster(A)
  A.left <- cluster2edges(A.cluster,A)
  plot.percolation(A, open=F)
  plot.percolation(A.left, lattice=F, lattice.dual=F, dual=F, add=T, lwd=3, lty=1, col="blue", open=F)
  A.dual <- create.percolation.dual(A)
  plot.percolation(A.dual, lattice=F, lattice.dual=F, dual=T, add=T, lwd=1, lty=1, col="red", open=F)
  A.dual.cluster <- find.top.cluster(A.dual)
  A.dual.top <- cluster2edges(A.dual.cluster,A.dual)
  plot.percolation(A.dual.top, lattice=F, lattice.dual=F, dual=T, add=T, lwd=3, lty=1, col.dual="red", open=F)
  
  return(A)
}

new.percolation <- function(N=10, K=11,  p=1/2, col="blue", open=F, lwd=1, lty=1, lattice.dual=T, lattice=T, col.lattice="lightgrey") {
  A <- create.percolation(N,K,p, open=open)
  plot.percolation(A, col=col, open=open, lwd=lwd, lty=lty, lattice.dual=lattice.dual, lattice = lattice, col.lattice = col.lattice )
  return(A)
}

add.dual <- function(percolation, col="red", lwd=1, lty=1) {
  A.dual <- create.percolation.dual(percolation)
  plot.percolation(A.dual, dual=T, col.dual=col, lwd=lwd, lty=lty, lattice.dual=F, lattice = F, add=T)
  return(A.dual)
}

add.cluster <- function(percolation, point = c(0,0), lty=3, lwd=3, col="blue", col.dual="red") {
 x <- point[1]
 y <- point[2]
 A <- percolation
 N <- attr(percolation,"N")
 K <- attr(percolation,"K") 
 open <- attr(percolation,"open")
 dual <- attr(percolation,"dual")
 boundary <- FALSE
 
 if (dual) {
   point[1] <- y
   point[2] <- x
 }
 
 if ((x<=0)|(x>K)|(x<=0)|(x>N)){
  boundary=TRUE 
 }
 if (boundary) {
   if(dual) {
     A.cluster <- find.top.cluster(A)
     plot.percolation(cluster2edges(A.cluster,A), add=T, col=col, col.dual=col.dual, lty=lty, lwd=lwd, lattice=F, lattice.dual=F, dual=T)
   } else {
     A.cluster <- find.left.cluster(A)
     plot.percolation(cluster2edges(A.cluster,A), add=T, col=col, col.dual=col.dual, lty=lty, lwd=lwd, lattice=F, lattice.dual=F, dual=F)
   }
 } else {
   if(dual) {
     A.cluster <- find.cluster(A,point)
     plot.percolation(cluster2edges(A.cluster,A), add=T, col=col, col.dual=col.dual, lty=lty, lwd=lwd, lattice=F, lattice.dual=F, dual=T)
   } else {
     A.cluster <- find.cluster(A,point)
     plot.percolation(cluster2edges(A.cluster,A), add=T, col=col, col.dual=col.dual, lty=lty, lwd=lwd, lattice=F, lattice.dual=F, dual=F)
   }
 }
 return(A.cluster)
}



