TwoPolys <- function(X,             # vector of X values
                     Y,             # vector of Y values
                     meanY     # divide between upper polygons and lower polygons
                                    # either a constant value or else a vector with 1 value for each value of X
                    ) {
    # is meanY a constant?
    Rflag <- length(meanY)>1
    
    cat("Rflag: ",Rflag,"\n")
    # Build explicit return along possibly curved meanY
    
    if(Rflag) {
        pReturn <- data.frame(X[length(X):1],meanY[length(meanY):1])
        names(pReturn) <- c("X","Y")
        pReturn$meanY <- pReturn$Y
        pReturn$upper <- pReturn$Y
        pReturn$lower <- pReturn$Y
        pReturn$X <- as.numeric(pReturn$X)
    } else meanY <- rep(meanY,length(X))  # fill in meanY for data.frame below

# sort into X order
     DF <- data.frame(X,Y,meanY)[order(X),] 
 # add on points at meanY at xmin and Xmax
    left <- data.frame(X=DF$X[1],Y=DF$meanY[1],meanY=DF$meanY[1])
    z <- nrow(DF)
    right <- data.frame(X=DF$X[z],Y=DF$meanY[z],meanY=DF$meanY[z])
     DF <- rbind(left,DF,right)
# compute differences
     DF$deltaX <- c(NA,diff(DF$X))
     DF$deltaY <- c(NA,diff(DF$Y))
     DF$deltaYm <- c(NA,diff(DF$meanY))
     DF$devY <- DF$Y - DF$meanY
     n <- nrow(DF) 
# flag where y crosses meanY     
     flag <- c(NA,DF$devY[2:n]*DF$devY[1:n-1]<0)
# subset to just where new points are needed & compute them    
     crosspts <- na.omit(DF[flag,])
     Xhat <- crosspts$X - (crosspts$Y-crosspts$meanY)/(crosspts$deltaY-crosspts$deltaYm)*crosspts$deltaX
     Yhat <- crosspts$Y-crosspts$deltaY*(crosspts$X-Xhat)
# add new points back to DF
     newpts <- data.frame(X=Xhat,Y=Yhat,meanY=Yhat) # Y, meanY and for thta matter upper & lower all Yhat at crosses
     allpts <- rbind(newpts,DF[,1:3])
# reorder to put them in the right places
     allpts <- allpts[order(allpts$X),]
#     plot(Y~X,data=allpts,type="l")
# compute polygons
allpts$upper <- pmax(allpts$Y,allpts$meanY)
allpts$lower <- pmin(allpts$Y,allpts$meanY)
# add back on return points if necessary
if (Rflag) TwoPolys <- rbind(allpts,pReturn) else TwoPolys <- allpts
}
