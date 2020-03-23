B.mat <-
function(n,M){
B.mat<-matrix(0,n,n)

	for (t in (M+2):(n-M-2)){
	A.mat<-matrix(0,n,n)	
	A.mat[1:t,1:t]<-(n-t-M)/(t-M-1)
	A.mat[1:t,(t+1):n]<--((t-M)*(n-t-M))/(t*(n-t)-M*(M+1)/2)
	A.mat[(t+1):n,1:t]<--((t-M)*(n-t-M))/(t*(n-t)-M*(M+1)/2)
	A.mat[(t+1):n,(t+1):n]<-(t-M)/(n-t-M-1)
	B.mat<-B.mat+A.mat
	}

	for(i in 1:(2*M+1)){
    	h<-i-(M+1)
    	B.mat[row(B.mat)==col(B.mat)-h]<-0
    	}

return(B.mat)
}
