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
