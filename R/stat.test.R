stat.test <-
function(M,data,cor){
n<-dim(data)[1]
data.mat<-(data%*%t(data))^2
C.mat<-B.mat(n,M)
stat<-(n)^(-2)*sum(C.mat*data.mat)
var<-0

	for(i in 1:(2*M+1)){

		for(j in 1:(2*M+1)){
		h1<-i-(M+1);h2<-j-(M+1)
		id1<-max(1,1-h1):min(n,n-h1)
		id2<-max(1,1+h2):min(n,n+h2)
		id3<-max(1,1+h1):min(n,n+h1)
		id4<-max(1,1-h2):min(n,n-h2)	
		var<-var+sum(C.mat[id1,id2]*C.mat[id3,id4]*(cor[i,j])^2)
	      }	
	}
	
sd.stat<-(4*var/(n^4))^0.5	
stat.test<-stat/sd.stat
return(stat.test)
}
