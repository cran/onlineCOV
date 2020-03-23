stopping.rule <-
function(ARL,H,mu,M,cor,old.data,new.data){

arl <-function(Ths){

	integrand<-function(x){
	g<-2*log(x/H)+(1/2)*log(log(x/H))+log(4/sqrt(pi))-Ths*sqrt(2*log(x/H))
	f<-exp(-2*exp(g))
    	return(f)	
	}

result = (integrate(integrand,H,Inf)$value+H)-ARL
return(result)
}

a = round(uniroot(arl,c(1,4),tol=0.0001)$root,digits = 2)

data_M = rbind(old.data,new.data)
n.M = dim(data_M)[1]
data.o = data_M[(n.M-H+1):n.M,]
data = t(t(data.o)-mu)

n<-dim(data)[1]; p<-dim(data)[2]
value<-0; Decision<-0 
	
	if(n>2*(M+2)){
	value<-stat.test(M,data,cor)
 
		if((value)^2>a^2){Decision<-1}	
	
	}

result = list()
result$decision<-Decision
result$old.updated = data.o

return(result)		
}
