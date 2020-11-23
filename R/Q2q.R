#' getqxt: obtain the age specific mortality surface
#'
#' getqxt interpolate the age specific mortality rates for a set of life tables
#'
#' @param Qxt A surface of Five-ages mortality rates which should be a numerical matrix containing mortality rates without age identification column and time identification row
#'
#' @param nag the number of age groups
#'
#' @param t the number of years
#'
#' @return qxt a matrix containing the age-specific mortality rates for age x in rows and for year t in columns
#' @return lxt a matrix containing the age evolution of survivorship for the year t
#' @return dxt a matrix containing the theoretical deaths occured at age x and year t
#' @return qxtl the age specific mortality rates interpolated using the Lagrange method for each year t
#' @return qxtk the age specific mortality rates interpolated using the Karup-king method for each year t
#' @return jonct_ages a vector containing, for each year t, the ages where qxtk and qxtl have been joined
#'
#' @examples getqxt(matrix(rep(c(0.12, seq(0.05, 0.5,by=0.05)), 5), byrow=FALSE, ncol=5), 11, 5)
#'
#' @author Farid FLICI
#' @export
getqxt <- function(Qxt, nag ,t){

  if(             t==1                               ) { Qxt <- matrix(Qxt, ncol=1)}

  if((is.matrix(Qxt)==FALSE)|(is.numeric(Qxt)==FALSE)) { stop("Warning: Qxt needs to be a numerical matrix !") }else{

    if(nag != nrow(Qxt) ){ stop("Warning: The number of age groups (nag) doesn't meet the number of rows in Qxt")     }else{

      if( t != ncol(Qxt)){ stop("Warning: The number of years doesn't meet the number of columns in Qxt")     }else{
        N     <- nrow(Qxt)
        X_max <- (N-2)*5
        x_max <- X_max + 4
        n     <- x_max + 1
        p     <- ncol(Qxt)
        rownames(Qxt) <- c(0,1, seq(5,X_max, by=5))

        # Lagrange coefficients
        ## First group Coefficients
        C1<-matrix(     c( 0.56203  , 0.7176  ,-0.4784   , 0.283886 ,-0.100716 , 0.0156   ,
                           0.273392 , 1.047199,-0.531911 , 0.2992   ,-0.103747 , 0.015867 ,
                           0.096491 , 1.1088  ,-0.328533 , 0.1728   ,-0.058358 , 0.0088)  ,    byrow=TRUE, ncol=6)
        rownames(C1) <- c("u2","u3", "u4" )
        colnames(C1) <- c("x=1", "x=5", "x=10", "x=15", "x=20", "x=25")
        ## Second group Coefficients
        C2<-matrix(     c(-0.041667 , 0.798   , 0.354667 ,-0.152    , 0.048    ,-0.007    ,
                          -0.048872 , 0.5616  , 0.6656   ,-0.240686 , 0.072758 ,-0.0104   ,
                          -0.037281 , 0.3332  , 0.888533 ,-0.2448   , 0.070148 ,-0.0098   ,
                          -0.018379 , 0.1408  , 1.001244 ,-0.160914 , 0.043116 ,-0.005867),    byrow=TRUE, ncol=6)
        rownames(C2) <- c("u6","u7", "u8", "u9" )
        colnames(C2) <- c("x=1", "x=5", "x=10", "x=15", "x=20", "x=25")
        ## Third group Coefficients
        C3<-matrix(      c(0.008064 ,-0.07392 , 0.88704  , 0.22176  ,-0.04928  , 0.006336 ,
                           0.011648 ,-0.09984 , 0.69888  , 0.46592  ,-0.08736  , 0.010752 ,
                           0.010752 ,-0.08736 , 0.46592  , 0.69888  ,-0.0998399, 0.011648 ,
                           0.006336 ,-0.04928 , 0.22176  , 0.88704  ,-0.07392  , 0.008064),    byrow=TRUE, ncol=6)
        rownames(C3) <- c("u(5m+1)","u(5m+2)", "u(5m+3)", "u(5m+4)" )
        colnames(C3) <- c("x=5m-10", "x=5m-5", "x=5m", "x=5m+5", "x=5m+10", "x=5m+15")
        Lxt     <- matrix(rep(NA)   , nrow=(N+1), ncol=p)
        Lxt[1,] <- matrix(rep(10000), ncol=p    , nrow=1)

        for (i in 1:N)   {    Lxt[(i+1),] <- Lxt[i,]*(matrix(rep(1), ncol=p, nrow=1)-matrix(Qxt[i,], nrow=1))     }
        rownames(Lxt)  <- c(0,1, seq(5,(X_max+5), by=5))
        lxtl           <- matrix(rep(NA), ncol=p, nrow=(n+1))
        rownames(lxtl) <- c(0:(x_max + 1))
        lxtl[1:2,] <- Lxt[1:2,]

        for (i in 1:(N - 1))   {   lxtl[(5*i+1),] <- Lxt[(i+2),]     }

        lxtl[3:5 ,] <- C1%*%as.matrix(Lxt[2:7, ])
        lxtl[7:10,] <- C2%*%as.matrix(Lxt[2:7, ])
        for (i in 2:(N - 4)){      lxtl[(2+i*5):(5+i*5),] <- C3%*%as.matrix(Lxt[i:(i+5), ])            }
        qxtl           <- matrix(rep(NA), ncol=p, nrow=(n-10))
        rownames(qxtl) <- c(0:(x_max - 10))
        for (i in 1: (n-10)){ qxtl[i,] <- (lxtl[(i),]-lxtl[(i+1),])/lxtl[(i),] }
        # Interpolation using the karup-king method
        ## first group of coefficients
        k1 <- matrix(       c( 0.344,	-0.208,	 0.064,
                               0.248,	-0.056,	 0.008,
                               0.176,	 0.048,	-0.024,
                               0.128,	 0.104,	-0.032,
                               0.104,	 0.112,	-0.016),     byrow=TRUE, ncol=3)
        #middle group coefficients
        k2  <- matrix(      c( 0.064,	 0.152,	-0.016,
                               0.008,	 0.224,	-0.032,
                              -0.024,	 0.248,	-0.024,
                              -0.032,	 0.224,	 0.008,
                              -0.016,	 0.152,	 0.064),     byrow=TRUE, ncol=3)
        #last group coefficients
        k3  <- matrix(      c(-0.016,	 0.112,	 0.104,
                              -0.032,	 0.104,	 0.128,
                              -0.024,	 0.048,	 0.176,
                               0.008, -0.056,	 0.248,
                               0.064, -0.208,  0.344),     byrow=TRUE, ncol=3)
        Dxt                <- matrix(rep(NA), ncol=p, nrow=(N -1))
        Dxt[1,]            <- Lxt[1,]-Lxt[3,]
        Dxt[2:(N - 1),   ] <- as.matrix(Lxt[3:N,])-as.matrix(Lxt[4:(N + 1),])
        ## estimating dx
        dxt <- matrix(rep(NA), ncol=p, nrow=n)
        dxt[1:5,]        <- k1%*%Dxt[1:3, ]
        dxt[(n - 4):n, ] <- k3%*%Dxt[ (N - 3) : (N - 1), ]
        for (s in 2:(N - 2)){     dxt[(1+5*(s-1)):(5+5*(s-1)), ] <- k2%*%Dxt[ (s-1):(s+1), ]     }
        dxt[dxt[ ]<0 ]<- 0
        ## deduce lxk and qxk
        lxtk     <- matrix( rep(NA)  , ncol=p, nrow=(n+1))
        lxtk[1,] <- matrix(rep(10000), ncol=p, nrow=1    )
        for(i in 1:n)    {   lxtk[(i+1), ]  <-  lxtk[i, ] - dxt[i, ]                 }
        qxtk <- as.matrix(dxt)/as.matrix(lxtk[1:(nrow(lxtk)-1),])
        # Jonction:
        if(t==1){
                   err_vec <- matrix(rep(NA), ncol=1, nrow=(n - 29))
                   for (x in 11:(n-19)){     err               <- (log(qxtk[(x-2):(x+2), ])-log(qxtl[(x-2):(x+2) , ]) )
                   err_vec[(x-20),]  <- t(err)%*%err    }

                   jonct_age <- which.min(err_vec)+11
                   qx<-matrix(c(qxtl[1:(jonct_age-1), ], qxtk[jonct_age:n, ]), ncol=1)
                   rownames(qx) <-c(0:x_max)

                   lx <- matrix(rep(NA), nrow=(n+1),ncol=1 )
                   lx[1,]                         <- 10000
                   for (i in 1: n) { lx[ (i+1) , ] <-lx[i,]*(1-qx[i,])  }
                   rownames(lx) <-c(0:(x_max+1))

                   dx <- matrix(rep(NA), nrow=n,ncol=1 )
                   dx <- lx[1:n,] * qx
                   rownames(dx) <-c(0:x_max)

                   qxk <- qxtk
                   qxl <- qxtl

                   list(qx=qx, lx=lx, dx=dx, qxk=qxk, qxl=qxl, jonct_age=jonct_age)

        }else{
                   err_mat <- matrix(rep(NA), ncol=p, nrow=(n - 29))
                   for (x in 11:(n-19)){
                   err                <- (log(qxtk[(x-2):(x+2), ])-log(qxtl[(x-2):(x+2) , ]) )
                   err_mat[(x-20), ]  <-  colSums( err*err)                                       }

                   jonct_ages <- matrix(rep(NA), nrow=1, ncol=p )

                   for (t in 1:p){  jonct_ages[ ,t] <- which.min(err_mat[,t])+11    }

                   qxt <- matrix(rep(NA), ncol=p, nrow=n    )
                   for (t in 1: p){   qxt[,t]<-matrix(c(qxtl[1:(jonct_ages[,t]-1), t], qxtk[jonct_ages[,t]:n, t]), ncol=1)  }
                   rownames(qxt) <- c(0: x_max)

                   lxt <- matrix(rep(NA), nrow=(n+1),ncol=p )
                   lxt[1,]                          <- matrix(rep(10000), ncol=1, nrow=p)
                   for (i in 1: n) { lxt[ (i+1) , ] <-lxt[i,]*as.matrix(1-qxt[i,])  }
                   rownames(lxt) <-c(0:(x_max+1))

                   dxt <- matrix(rep(NA), nrow=n,ncol=p )
                   dxt <- lxt[1:n,] * qxt
                   rownames(dxt) <-c(0:x_max)

                   list(qxt=qxt, lxt=lxt, dxt=dxt, qxtk=qxtk, qxtl=qxtl, jonct_ages=jonct_ages)

        }


      }
    }
  }
}

#' getqx
#'
#' It interpolate the age specific mortality rates
#' @param Qx Five-ages mortality rates which can be a vector created using  or a column of a numerical matrix
#'
#' @param nag number of age groups
#' @return qx age-specific mortality rates
#' @return lx a vector containing the age evolution of survivorship
#' @return dx a vector containing the theoretical deaths occured at age x
#' @return qxtl age specific mortality rates interpolated using the Lagrange method
#' @return qxtk age specific mortality rates interpolated using the Karup-king method
#' @return jonct_age the age where qxk and qxl have been joined
#'
#' @examples
#' getqx(c(0.12, seq(0.05, 0.8,by=0.05)), 17)
#'
#' @author Farid FLICI
#' @export
getqx <- function(Qx,nag) {

  getqxt(Qxt=Qx, nag, t=1)

}

