surface.explore <- function(fun.call=NULL, x=NULL, y=NULL, 
                            xlim=c(-Inf, Inf), 
                            ylim=c(-Inf, Inf), 
                            zlim=c(-Inf, Inf), 
                            new=F, 
                            truncate=F, 
                            max.grid.density=100,
                            graph="persp",
                            ...) {
  
  ## Function to explore surface of a 2-dimensional function
  ## fun.call: Must be specified as a string. The function must contain
  ##            take "x[i]" and "y[j]" as arguments, e.g.:
  ##            fun.call="prod(x[i], y[j])"
  ## truncate: Instead of converting z values outside zlim to NA, 
  ##           convert them to the bounds instead. This can make the graph
  ##           prettier.
  ## new: Create a new graph!
  ## max.grid.density: By default, it tries to choose values that are equally 
  ##                   spread between the limits. This controls the density of 
  ##                    the grid lines. 
  
  surface.exists <- with(.GlobalEnv, exists(".surface", inherits=F))
  if(!(surface.exists) || new) {
    length.x <- length(x)
    length.y <- length(y)
    f <- matrix(NA, length.x, length.y)
    for(i in 1:length.x) {
      for(j in 1:length.y) {
        if(x[i] >= xlim[1] && x[i] <= xlim[2] && 
             y[j] >= ylim[1] && y[j] <= ylim[2]) {
          f[i,j] <- eval(parse(text=fun.call), 
                         envir=list(x=x,y=y,i=i,j=j), enclos=.GlobalEnv)
        }
      }
    }
    surface <- list(f=f, x=x, y=y, fun.call=fun.call)
    .surface <<- surface
    x.todraw <- x
    y.todraw <- y
    
  }
  else {
     surface<- get(x=".surface", envir=.GlobalEnv, inherits=F)
     if(!is.null(fun.call)) {
       if(surface$fun.call != fun.call) stop("fun.call has changed!")
     }
     if(!is.null(x)) {
       xlim[1] <- max(c(min(x), xlim[1]))
       xlim[2] <- min(c(max(x), xlim[2]))
       x.todraw <- x[x >= xlim[1] & x <= xlim[2]]
     }
     else x.todraw <- surface$x[surface$x >= xlim[1] & surface$x <= xlim[2]]
     if(!is.null(y)) {
       ylim[1] <- max(c(min(y), ylim[1]))
       ylim[2] <- min(c(max(y), ylim[2]))
       y.todraw <- y[y >= ylim[1] & y <= ylim[2]]
     }
     else y.todraw <- surface$y[surface$y >= ylim[1] & surface$y <= ylim[2]]
     
     ### Select roughly equally spaced subset only 
     ideal.x <- seq(from=min(x.todraw), to=max(x.todraw), 
                    length.out=max.grid.density)
     ideal.y <- seq(from=min(y.todraw), to=max(y.todraw), 
                    length.out=max.grid.density)
     
     rounded.x <- nearest.neighbour(ideal.x, x.todraw, selectvector=T)
     rounded.y <- nearest.neighbour(ideal.y, y.todraw, selectvector=T)
     
     x.todraw <- x.todraw[rounded.x]
     y.todraw <- y.todraw[rounded.y]
     
     ### The new surface object as a union of old and new (i.e.: .todraw)
     new.x <- sort(union(surface$x, x.todraw))
     new.y <- sort(union(surface$y, y.todraw))
     new.f <- matrix(NA, length(new.x), length(new.y))
     
     if(is.null(fun.call)) fun.call <- surface$fun.call
     
     oldpos.x <- findpos(!is.na(match(new.x, surface$x)))
     oldpos.y <- findpos(!is.na(match(new.y, surface$y)))
     new.f[oldpos.x, oldpos.y] <- surface$f
     
     extra.x <- setdiff(new.x, surface$x)
     extra.y <- setdiff(new.y, surface$y)
     
     n.extra.x <- length(extra.x)
     n.extra.y <- length(extra.y)
     
     if(n.extra.x > 0) {
       newpos.x <- findpos(!is.na(match(new.x, extra.x)))
       x <- extra.x
       y <- surface$y
       for(i in 1:n.extra.x) {
         for(j in 1:length(surface$y)) {
           if(x[i] >= xlim[1] && x[i] <= xlim[2] && 
                y[j] >= ylim[1] && y[j] <= ylim[2]) {
             new.f[newpos.x[i], oldpos.y[j]] <- eval(parse(text=fun.call),
                                                     envir=list(x=x,y=y,i=i,j=j), 
                                                     enclos=.GlobalEnv)
           }
         }
       }
     }

     if(n.extra.y > 0) {
       newpos.y <- findpos(!is.na(match(new.y, extra.y)))
       y <- extra.y
       x <- surface$x
       for(i in 1:length(surface$x)) {
         for(j in 1:n.extra.y) {
           if(x[i] >= xlim[1] && x[i] <= xlim[2] && 
                y[j] >= ylim[1] && y[j] <= ylim[2]) {
             new.f[oldpos.x[i], newpos.y[j]] <- eval(parse(text=fun.call),
                                                     envir=list(x=x,y=y,i=i,j=j), 
                                                     enclos=.GlobalEnv)
           }
         }
       }
     }
     
     if(n.extra.y > 0 && n.extra.x > 0) {
       y <- extra.y
       x <- extra.x
       for(i in 1:n.extra.x) {
         for(j in 1:n.extra.y) {
           if(x[i] >= xlim[1] && x[i] <= xlim[2] && 
                y[j] >= ylim[1] && y[j] <= ylim[2]) {
             new.f[newpos.x[i], newpos.y[j]] <- eval(parse(text=fun.call),
                                                     envir=list(x=x,y=y,i=i,j=j), 
                                                     enclos=.GlobalEnv)
           }
         }
       }
     }
     
     surface <- list(f=new.f, x=new.x, y=new.y, fun.call=fun.call)
     .surface <<- surface
     
  }

  ### Draw subset
  rowmax <- apply(surface$f,MARGIN=1,FUN=max,na.rm=T)
  colmax <- apply(surface$f,MARGIN=2,FUN=max,na.rm=T)
  rowmin <- apply(surface$f,MARGIN=1,FUN=min,na.rm=T)
  colmin <- apply(surface$f,MARGIN=2,FUN=min,na.rm=T)
  
  x.subset <- !is.na(match(surface$x, x.todraw)) & 
    rowmax >= zlim[1] & rowmin <= zlim[2]
  y.subset <- !is.na(match(surface$y, y.todraw)) &
    colmax >= zlim[1] & colmin <= zlim[2]
  
  subset.x <- surface$x[x.subset]
  subset.y <- surface$y[y.subset]
  subset.f <- surface$f[x.subset, y.subset]
  if(truncate==F) subset.f[subset.f < zlim[1] | subset.f > zlim[2]] <- NA
  else {
    subset.f[subset.f < zlim[1]] <- zlim[1]
    subset.f[subset.f > zlim[2]] <- zlim[2]
  }
  if(graph=="persp") {
    persp(subset.x, subset.y, subset.f, ticktype="detailed", ...)
  }
  else {
    do.call(graph, list(x=subset.x, y=subset.y, z=subset.f, ...))
  }
  

  return(invisible(list(f=subset.f, x=subset.x, y=subset.y, fun.call=fun.call)))

}
