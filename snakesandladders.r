playgame<-function(x){
  writeLines(paste("There are",x, "players"))
  pos<-rep(0,x)
  turns<-0
  while(any(pos>=100)==FALSE){
    turns<-turns+1
    for (i in 1:x){
      dr<-sample(6,1)
      writeLines(paste("Player ",i," is on square ",pos[i],". They roll a ",dr, sep=""))
      pos[i]<-pos[i]+dr
      if (pos[i]>100){pos[i]<-100}
      if (pos[i]==1){pos[i]<-38;writeLines("They climb a ladder from square 1 to 38")}
      if (pos[i]==4){pos[i]<-14;writeLines("They climb a ladder from square 4 to 14")}
      if (pos[i]==9){pos[i]<-31;writeLines("They climb a ladder from square 9 to 31")}
      if (pos[i]==21){pos[i]<-42;writeLines("They climb a ladder from square 21 to 42")}
      if (pos[i]==28){pos[i]<-84;writeLines("They climb a ladder from square 28 to 84")}
      if (pos[i]==36){pos[i]<-44;writeLines("They climb a ladder from square 36 to 44")}
      if (pos[i]==51){pos[i]<-67;writeLines("They climb a ladder from square 51 to 67")}
      if (pos[i]==71){pos[i]<-91;writeLines("They climb a ladder from square 71 to 91")}
      if (pos[i]==80){pos[i]<-100;writeLines("They climb a ladder from square 80 to 100")}
      if (pos[i]==16){pos[i]<-6;writeLines("They slide down a snake from square 16 to 6")}
      if (pos[i]==47){pos[i]<-26;writeLines("They slide down a snake from square 47 to 26")}
      if (pos[i]==49){pos[i]<-11;writeLines("They slide down a snake from square 49 to 11")}
      if (pos[i]==56){pos[i]<-53;writeLines("They slide down a snake from square 56 to 53")}
      if (pos[i]==62){pos[i]<-19;writeLines("They slide down a snake from square 62 to 19")}
      if (pos[i]==64){pos[i]<-60;writeLines("They slide down a snake from square 64 to 60")}
      if (pos[i]==87){pos[i]<-24;writeLines("They slide down a snake from square 87 to 24")}
      if (pos[i]==93){pos[i]<-73;writeLines("They slide down a snake from square 93 to 73")}
      if (pos[i]==95){pos[i]<-75;writeLines("They slide down a snake from square 95 to 75")}
      if (pos[i]==98){pos[i]<-78;writeLines("They slide down a snake from square 98 to 78")}
      writeLines(paste("Player",i,"is now on square",pos[i]))
    }
  }
  writeLines(paste("The finshing positions are",paste(pos,collapse=" ")))
  writeLines(paste("Player",which(pos>=100),"wins"))
  writeLines(paste("It took",turns,"goes to win the game"))
}
