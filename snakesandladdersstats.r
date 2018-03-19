playgamestats<-function(x){
  grid<-rep(0,100)
  pos<-rep(0,x)
  turns<-0
  while(any(pos>=100)==FALSE){
    turns<-turns+1
    for (i in 1:x){
      dr<-sample(6,1)
      pos[i]<-pos[i]+dr
      if (pos[i]>100){pos[i]<-100}
      grid[pos[i]]<-grid[pos[i]]+1
      if (pos[i]==1){pos[i]<-38}
      if (pos[i]==4){pos[i]<-14}
      if (pos[i]==9){pos[i]<-31}
      if (pos[i]==21){pos[i]<-42}
      if (pos[i]==28){pos[i]<-84}
      if (pos[i]==36){pos[i]<-44}
      if (pos[i]==51){pos[i]<-67}
      if (pos[i]==71){pos[i]<-91}
      if (pos[i]==80){pos[i]<-100}
      if (pos[i]==16){pos[i]<-6}
      if (pos[i]==47){pos[i]<-26}
      if (pos[i]==49){pos[i]<-11}
      if (pos[i]==56){pos[i]<-53}
      if (pos[i]==62){pos[i]<-19}
      if (pos[i]==64){pos[i]<-60}
      if (pos[i]==87){pos[i]<-24}
      if (pos[i]==93){pos[i]<-73}
      if (pos[i]==95){pos[i]<-75}
      if (pos[i]==98){pos[i]<-78}
    }
  }
  info<-list(turns=turns,gridvisits=grid)
  return(info)
}
