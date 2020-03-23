cor.est <-
function(M,data.whole){
n<-dim(data.whole)[1]
a<-data.whole%*%t(data.whole)
correlation.est<-matrix(0,(2*M+1),(2*M+1))
out<-0
storage.mode(out)<-"double"
storage.mode(a)<-"double"
n<-as.integer(n)
M<-as.integer(M)
#dyn.load("variance3.dll")
result4<-.Fortran("code3",n,M,a,out=out)

	for(i in 1:(2*M+1)){

		for(j in 1:(2*M+1)){
    h1<-i-(M+1);h2<-j-(M+1)
    h1<-as.integer(h1)
    h2<-as.integer(h2)
    h3<-abs(h1); h4<-abs(h2)
    h3<-as.integer(h3)
    h4<-as.integer(h4)
		#dyn.load("variance1.dll")   		
		result1<-.Fortran("code1",n,M,h1,h2,a,out=out)
		#dyn.load("variance2.dll")
		result2<-.Fortran("code2",n,M,h3,a,out=out)
		result3<-.Fortran("code2",n,M,h4,a,out=out)		
		correlation.est[i,j]<-result1[[6]]-result2[[5]]-result3[[5]]+result4[[4]]		
		}
	
	}

return(correlation.est)
}
