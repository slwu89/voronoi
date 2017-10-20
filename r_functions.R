# from 'soiltexture'

TT.polygon.area <- function(pol.x,pol.y){  # Index range:
  i <- 1:length(pol.x)
  #
  # Close the polygon
  pol.x <- c(pol.x,pol.x[1]) 
  pol.y <- c(pol.y,pol.y[1]) 
  #
  # Calculate the area
  A <- 0.5 * sum( pol.x[i] * pol.y[i+1] - pol.x[i+1] * pol.y[i] )
  #
  return( A ) 
  ### Returns a single numerical: area of the polygon.
} 

TT.polygon.centroids <- function(pol.x,pol.y){  #
  i <- 1:length(pol.x)
  # Calculate the area:
  A <- TT.polygon.area( 
    pol.x = pol.x, 
    pol.y = pol.y  
  )   #
  #
  # Close the polygon
  pol.x <- c(pol.x,pol.x[1]) 
  pol.y <- c(pol.y,pol.y[1]) 
  #
  # Calculate the area
  Cx <- (1/(6*A)) * sum( (pol.x[i]+pol.x[i+1]) * (pol.x[i]*pol.y[i+1] - pol.x[i+1]*pol.y[i]) )  
  Cy <- (1/(6*A)) * sum( (pol.y[i]+pol.y[i+1]) * (pol.x[i]*pol.y[i+1] - pol.x[i+1]*pol.y[i]) )  
  #
  return( c("x"=Cx,"y"=Cy) ) 
  ### Returns a vector of 2 numericals: x and y coordinates of 
  ### the polygon's centroid. Vector items are names "x" and "y". 
}  

allocate <- function (names, s, w, outer, target, maxIteration = 200, debug = FALSE, 
          dplot = FALSE, debugCell = FALSE) {
  count <- 1
  debugPlot <<- debugPlotGen()
  repeat {
    k <- awv(s, w, outer, debug, debugCell)
    areas <- lapply(k, area.poly)
    if (debug) {
      drawRegions(list(names = names, k = k, s = s, w = w, 
                       a = areas, t = target), debug)
      info <- rbind(area = round(unlist(areas)/sum(unlist(areas)), 
                                 4), target = round(target, 4), weight = round(w, 
                                                                               1))
      colnames(info) <- names
      print(info)
    }
    if (count > maxIteration || breaking(unlist(areas), target, 
                                         debug = debug, dplot = dplot)) {
      return(list(names = names, k = k, s = s, w = w, a = areas, 
                  t = target))
    }
    else {
      w <- adjustWeights(w, unlist(areas), target)
      s <- shiftSites(s, k)
      w <- shiftWeights(s, w)
    }
    count <- count + 1
  }
}


adjustWeights <- function (w, a, target) {
  a <- ifelse(a == 0, 0.01 * sum(a), a)
  normA <- a/sum(a)
  w + mean(abs(w)) * ((target - normA)/target)
}


shiftSites <- function (s, k) {
  newSites <- mapply(function(poly, x, y) {
    if (length(poly@pts)) {
      pts <- getpts(poly)
      TT.polygon.centroids(pts$x, pts$y)
    }
    else {
      c(x = x, y = y)
    }
  }, k, as.list(s$x), as.list(s$y), SIMPLIFY = FALSE)
  list(x = sapply(newSites, "[", "x"), y = sapply(newSites, 
                                                  "[", "y"))
}



shiftWeights <- function (s, w) 
{
  n <- length(s$x)
  for (i in 1:n) {
    for (j in 1:n) {
      if (i != j) {
        f = sqrt((s$x[i] - s$x[j])^2 + (s$y[i] - s$y[j])^2)/(abs(w[i]) + 
                                                               abs(w[j]))
        if (f > 0 && f < 1) {
          w[i] <- w[i] * f
          w[j] <- w[j] * f
        }
      }
    }
  }
  w
}
