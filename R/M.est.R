M.est <-
function(data_M,M_threshold=0.05){
n <- dim(data_M)[1]
m <- NULL

  if (n>150){

    for (i in 1:10){
    left <- sample(1:(n-51),1)
    right <- sample((left+50):n,1)
    data <- data_M[left:right,]
    n.r<-dim(data)[1]
    a<-data%*%t(data)
    out<-0
    storage.mode(out)<-"double"
    storage.mode(a)<-"double"
    n.r<-as.integer(n.r)

      for(H in 1:10){        
      H<-as.integer(H)
      #dyn.load("variance3.dll")
      result4<-.Fortran("code3",n.r,H,a,out=out)
      h1<--H;h2<-H
      h1<-as.integer(h1)
      h2<-as.integer(h2)
      h3<-abs(h1); h4<-abs(h2)
      h3<-as.integer(h3)
      h4<-as.integer(h4)
      #dyn.load("variance1.dll")	
      result1<-.Fortran("code1",n.r,H,h1,h2,a,out=out)
      #dyn.load("variance2.dll")
      result2<-.Fortran("code2",n.r,H,h3,a,out=out)
      result3<-.Fortran("code2",n.r,H,h4,a,out=out)		
      off.diag<-result1[[6]]-result2[[5]]-result3[[5]]+result4[[4]]
      
      h1<-0;h2<-0
      h1<-as.integer(h1)
      h2<-as.integer(h2)
      h3<-abs(h1); h4<-abs(h2)
      h3<-as.integer(h3)
      h4<-as.integer(h4)
      #dyn.load("variance1.dll")    		
      result1<-.Fortran("code1",n.r,H,h1,h2,a,out=out)
      #dyn.load("variance2.dll") 
      result2<-.Fortran("code2",n.r,H,h3,a,out=out)
      result3<-.Fortran("code2",n.r,H,h4,a,out=out)		
      on.diag<-result1[[6]]-result2[[5]]-result3[[5]]+result4[[4]]
      
        if(abs(off.diag/on.diag) < M_threshold ){
        m[i] <- max(0,(H-1))
        break
				}

			}

    }

  }
  else
  {
	data <- data_M
  n.r<-dim(data)[1]
  a<-data%*%t(data)
  out<-0
  storage.mode(out)<-"double"
  storage.mode(a)<-"double"
  n.r<-as.integer(n.r)
    
		for(H in 1:min(10,floor(((n-4)/3)))){
		H<-as.integer(H)
    #dyn.load("variance3.dll")
    result4<-.Fortran("code3",n.r,H,a,out=out)
    h1<--H;h2<-H
    h1<-as.integer(h1)
    h2<-as.integer(h2)
    h3<-abs(h1); h4<-abs(h2)
    h3<-as.integer(h3)
    h4<-as.integer(h4)
    #dyn.load("variance1.dll")	    		
    result1<-.Fortran("code1",n.r,H,h1,h2,a,out=out)
    #dyn.load("variance2.dll")	
    result2<-.Fortran("code2",n.r,H,h3,a,out=out)
    result3<-.Fortran("code2",n.r,H,h4,a,out=out)		
    off.diag<-result1[[6]]-result2[[5]]-result3[[5]]+result4[[4]]
      
    h1<-0;h2<-0
    h1<-as.integer(h1)
    h2<-as.integer(h2)
    h3<-abs(h1); h4<-abs(h2)
    h3<-as.integer(h3)
    h4<-as.integer(h4)
    #dyn.load("variance1.dll")  	    		
    result1<-.Fortran("code1",n.r,H,h1,h2,a,out=out)
    #dyn.load("variance2.dll")
    result2<-.Fortran("code2",n.r,H,h3,a,out=out)
    result3<-.Fortran("code2",n.r,H,h4,a,out=out)		
    on.diag<-result1[[6]]-result2[[5]]-result3[[5]]+result4[[4]]

      if(abs(off.diag/on.diag) < M_threshold){
      m <- c(m,max(0,(H-1)))
      break
			}

    }

  }

ifelse (is.null(m), m <- 0, m <- round(mean(m,na.rm = TRUE)))
#print(paste("dominant.temporal.dependence_M =",m))
return(m)
}
