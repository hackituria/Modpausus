Modpausu<-function(zen1, zen2, mo){
  
  #zen1^zen2 mod mo
  
  seg<-vector()
    
  while(zen2>1){
    
    cat(zen1,"^",zen2,"mod",mo,"\n")
    
    zat<-zatitzailetxiki(zen2)
    
    if(zat==0){
      seg[length(seg)+1]<-zen1      
      zen2<-zen2-1
      zat<-2
    }
    
    zen2<-zen2/zat
    
    cat("(",zen1,"^",zat,")","^",zen2,"*",seg,"mod",mo,"\n")
    
    zen1<-mod(zen1^zat,mo)
    
  
    
  }
  seg[length(seg)+1]<-zen1
  biderketak(seg,mo)
  


}


zatitzailetxiki<-function(zen){
  
  zatitzaile<-2
  aurk<-0
 
  
  while(aurk==0 && zatitzaile<=3){
    
    if(zen%%zatitzaile==0){
      aurk<-1
    }else{
      zatitzaile<-zatitzaile+1
      
    }
    
  }
  if(aurk==0){
   
    zatitzaile<-0
  }
return(zatitzaile)
}

mod<-function(x,y){
  
  z<-floor(x/y)
  return(x-z*y)
}

biderketak<-function(seg,mod){
  
  asi<-1
  
  while(length(seg)>0){
    
    asi<-mod(asi*seg[1],mod)
    seg<-seg[-1]
    
    cat(asi,"*",seg,"mod",mod,"\n")
  }
}
