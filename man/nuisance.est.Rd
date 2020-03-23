\name{nuisance.est}
\alias{nuisance.est}
\title{Estimate nuisance parameters in the stopping rule.}
\description{
The function estimates the nuisance parameters required in the stopping rule, through a trainig sample.
}
\usage{
nuisance.est(training.sample)
}
\arguments{
  \item{training.sample}{
A historical dataset without change points. 
}
}
\value{
Returns a list of estimated nuisance parameters. See below for more detail.
  \item{mu.hat}{
The sample mean of the training sample.
}
  \item{M.hat}{
The estimated M dependence.
}
  \item{cor.hat}{
A value used to obtain the standard deviation of the test statistic in the stopping rule.  
}
}
\references{
Li, L. and Li, J. (2020) "Online Change-Point Detection in High-Dimensional Covariance Structure with Application to Dynamic Networks." arXiv:1911.07762.
}
\examples{
\donttest{
p<-200;n0<-200
M<-2

Gam1<-diag(1,p,p)

data_Mat<-matrix(0,n0,p)
L<-M+1
Z<-matrix(rnorm(p*(n0+L-1)),p*(n0+L-1),1)
vec.coef<-1/rep(c(L:1),each=p)

 	for(j in 1:n0){
 	Gam.mat<-t(apply(Gam1,1,rep,L))*matrix(vec.coef,ncol=L*p,nrow=p,byrow=TRUE)	
 	data_Mat[j,]<-matrix((Gam.mat\%*\%Z[((j-1)*p+1):((j+L-1)*p),]),1,p,byrow=FALSE)	
 	}

training.sample<-data_Mat
nuisance.results<-nuisance.est(training.sample)
mu<-nuisance.results$mu.hat
M<-nuisance.results$M.hat
cor<-nuisance.results$cor.hat
}
}

