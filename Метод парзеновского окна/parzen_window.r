{\rtf1\ansi\ansicpg1252\cocoartf2577
\cocoatextscaling0\cocoaplatform0{\fonttbl\f0\fnil\fcharset0 HelveticaNeue;}
{\colortbl;\red255\green255\blue255;}
{\*\expandedcolortbl;;}
\paperw11900\paperh16840\margl1440\margr1440\vieww11520\viewh8400\viewkind0
\pard\tx560\tx1120\tx1680\tx2240\tx2800\tx3360\tx3920\tx4480\tx5040\tx5600\tx6160\tx6720\pardirnatural\partightenfactor0

\f0\fs26 \cf0 euclideanDistance <- function(u,v) \{\
  sqrt(sum((u - v)^2))\
\}\
\
Rect <- function(r) \{\
  (abs(r) <= 1) * 0.5\
\}\
\
Tria <- function(r) \{\
  (abs(r) <= 1) * (1 - abs(r))\
\}\
\
Epan <- function(r) \{\
  (abs(r) <= 1) * (1 - r^2)\
\}\
\
Quar <- function(r) \{\
  (abs(r) <= 1) * (1 - r^2)^2\
\}\
\
Gaus <- function(r) \{\
  dnorm(r)\
\}\
\
parsen <- function(x, z, h, F) \{\
  m <- dim(x)[1]\
  n <- dim(x)[2]-1\
  classes <- rep(0, length(names(table(x[,n+1]))))\
  names(classes) <- names(table(x[,n+1]))\
  for(i in 1:m)\{\
    y <- x[i, n+1]\
    dist <- euclideanDistance(x[i,1:n],z)\
    w <- F(dist/h)\
    classes[y] <- classes[y] + w\
  \}\
  if(sum(classes) > 0) \{\
    class <- names(which.max(classes))\
  \} else \{\
    class <- "unknown"\
  \}\
  return(class)\
\}\
\
LOO <- function(x, F) \{\
  m <- dim(x)[1]\
  n <- dim(x)[2] - 1\
  params <- seq(0.1,2,0.05)\
  mark <- rep(0,length(params))\
  for(h in 1:length(params)) \{\
    for(i in 1:m) \{\
      x1 <- x[-i,]\
      class1 <- parsen(x1,x[i,1:n], params[h], F)\
      class2 <- x[i,n+1]\
      if(class1 != class2)  mark[h] <- mark[h] + 1/m\
    \}\
  \}\
  plot(params, mark, type = l)\
  minPoint <- c(params[which.min(mark)], round(min(mark),4))\
  text <- paste("h = ",minPoint[1],"\\nLOO = ",minPoint[2])\
  points(minPoint[1], minPoint[2], pch=19, col="firebrick1", bg="black")\
  text(minPoint[1] + 0.1,minPoint[2] + 0.1,labels=text, col="firebrick1")\
\}\
\
xl <- iris[, 3:5]\
colors <- c( "setosa"="red", "versicolor"="green3", "virginica"="blue", "unknown"="grey" )\
\
FindParsen <- function(z1, z2, h, F) \{\
  points(z1,z2, pch = 21, col=colors[parsen(xl,c(z1,z2),h,F)])\
\}\
\
PlotMapParsen <- function(h, F) \{\
  plot(iris[, 3:4], pch = 21, bg = colors[iris$Species], col = colors[iris$Species], asp = 1) \
  for (i in seq(1,7,0.1)) \{\
    for (j in seq(0,3,0.1))\{\
      FindParsen(i,j,h,F)\
    \}\
  \}\
\}}