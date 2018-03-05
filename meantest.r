# C1618613 & C1626682 #
## Please run whole document for meantest() to work ##

library(lawstat)
library(EnvStats)
library(Deducer)

#one sample students t test
ttest1<-function(x,null=0,alternative="ne",conf=0.95){
  n<-length(x)
  denomt<-sqrt(var(x)/n) #denominator of test statistic
  tstat<-((mean(x)-null)/denomt)
  atstat<-abs(tstat) #necessary for two tailed case
  if (alternative=="ne"){ 
    pvalue<-1-(pt(atstat,df=(n-1))-pt(-atstat, df=(n-1))) #1-the dfference between the upper tail and lower tail 
    hypothesis<-paste("μ =",null,"against μ ≠",null)
    confint<-c(mean(x)-abs(denomt)*qt(1-(1-conf)/2,df=(n-1)),mean(x)+abs(denomt)*qt(1-(1-conf)/2,df=(n-1))) #dormant confidence interval function
  }
  if (alternative=="l"){ 
    pvalue<-pt(tstat,df=(n-1)) #the lower tail of the test statistic 
    hypothesis<-paste("μ =",null,"against μ <",null)
    confint<-c(-Inf,mean(x)+abs(denomt)*qt(conf,df=(n-1)))
  }
  if (alternative=="g"){
    pvalue<-pt(tstat,df=(n-1),lower.tail = FALSE) #the upper tail of the test statistic 
    hypothesis<-paste("μ =",null,"against μ >",null)
    confint<-c(mean(x)-abs(denomt)*qt(conf,df=(n-1)),+Inf)
  }
  list<-list(statistic=tstat, p.value=pvalue, method="t test",hypothesis=hypothesis,conf.int=confint,x.mean=mean(x))
  return(list) #Provides the reqiured statistical data in a list format
}

#Two sample students t test
ttest2<-function(x,y,null=0,alternative="ne", conf=0.95){
  n1<-length(x)
  n2<-length(y)
  v <- (((var(x)/n1)+(var(y)/n2))^2)/((((var(x)/n1)^2)/(n1-1))+(((var(y)/n2)^2)/(n2-1))) #degrees of freedom
  denomt<-sqrt((var(x)/n1)+(var(y)/n2))
  tstat<-((mean(x)-mean(y)-null)/denomt)
  atstat<-abs((mean(x)-mean(y)-null)/denomt) #used for two tailed case 
  if (alternative=="ne"){
    pvalue<-1-(pt(atstat,df=(v))-pt(-atstat, df=(v))) #1 - (the difference between the lower tail and upper tail) 
    hypothesis<-paste("Δ =",null,"against Δ ≠",null)
    confint<-c((mean(x)-mean(y))-abs(denomt)*qt(1-(1-conf)/2,df=(v)),(mean(x)-mean(y))+abs(denomt)*qt(1-(1-conf)/2,df=(v)))
  }
  if (alternative=="l"){  
    pvalue<-pt(tstat,df=(v)) #the lower tail with v df
    hypothesis<-paste("Δ =",null,"against Δ <",null)
    confint<-c(-Inf,(mean(x)-mean(y))+abs(denomt)*qt(conf,df=(v)))
  }
  if (alternative=="g"){  
    pvalue<-pt(tstat,df=(v),lower.tail = FALSE) #the upper tail with v df
    hypothesis<-paste("Δ =",null,"against Δ >",null)
    confint<-c((mean(x)-mean(y))-abs(denomt)*qt(conf,df=(v)),+Inf)
  }
  list<-list(statistic=tstat, p.value=pvalue, method="Welch two sample t test",hypothesis=hypothesis,conf.int=confint,x.mean=mean(x),y.mean=mean(y))
  return(list)
}

ttestev<-function(x,y,null=0,alternative="ne", conf=0.95){
  n1<-length(x)
  n2<-length(y)
  Sp <-sqrt((n1-1)/(n1+n2-2)*var(x)^2+((n2-1)/(n1+n2-2))*var(y)^2) #pooled standard deviation
denomt<-(Sp)*sqrt((1/n1)+(1/n2))
tstat<-((mean(x)-mean(y)-null)/denomt)
atstat<-abs((mean(x)-mean(y)-null)/denomt)
if (alternative=="ne"){
  pvalue<-1-(pt(atstat,df=(n1+n2-2))-pt(-atstat, df=(n1+n2-2)))
  hypothesis<-paste("Δ =",null,"against Δ ≠",null)
  confint<-c((mean(x)-mean(y))-abs(denomt)*qt(1-(1-conf)/2,df=(n1+n2-2)),(mean(x)-mean(y))+abs(denomt)*qt(1-(1-conf)/2,df=(n1+n2-2)))
}
if (alternative=="l"){
  pvalue<-pt(tstat,df=(n1+n2-2))
  hypothesis<-paste("Δ =",null,"against Δ <",null)
  confint<-c(-Inf,(mean(x)-mean(y))+abs(denomt)*qt(conf,df=(n1+n2-2)))
}
if (alternative=="g"){
  pvalue<-pt(tstat,df=(n1+n2-2),lower.tail = FALSE)
  hypothesis<-paste("Δ =",null,"against Δ >",null)
  confint<-c((mean(x)-mean(y))-abs(denomt)*qt(conf,df=(n1+n2-2)),+Inf)
}
list<-list(statistic=tstat, p.value=pvalue, method="Two Sample T-test with Pooled Variance",hypothesis=hypothesis,conf.int=confint,x.mean=mean(x),y.mean=mean(y))
return(list)
}

wtest1<-function(x,null=0,alternative="ne",conf=0.95){
  nx<-c(x[x!=null]) #eliminate values equal to mean
  d<-(nx-null) #difference between data and null value
  w<-sign(d)*rank(abs(d))
  v<-sum(w[w>0]) #calculate test statistic, separated by above/below 0
  n<-length(nx)
  tb<-n*(n+1)/4 #expected value
  se<-sqrt((n*(n+1)*(2*n+1))/24) #standard deviation
  if (alternative=="ne"){
    cv<-v-sign(v)*0.5 #continuity correction
    z<-(cv-tb)/se
    pvalue<-2*(min(pnorm(z),pnorm(z,lower.tail=FALSE)))
    hypothesis<-paste("μ =",null,"against μ ≠",null)
  }
  if (alternative=="l"){
    cv<-v+0.5
    z<-(cv-tb)/se #z value for normal distribution
    pvalue<-pnorm(z)
    hypothesis<-paste("μ =",null,"against μ <",null)
  }
  if (alternative=="g"){
    cv<-v-0.5
    z<-(cv-tb)/se
    pvalue<-pnorm(z,lower.tail=FALSE)
    hypothesis<-paste("μ =",null,"against μ >",null)
  }
  list<-list(p.value=pvalue,statistic=v,hypothesis=hypothesis,method="Wilcoxon signed rank test",x.mean=mean(x),z=z)
  return(list)
}

setClass( "mt", representation("list"))

#set a class method for nice looking outlook
setMethod("show","mt",function(object){
  cat(object$data,"\n")
  cat("Method used is",object$m,"\n")
  cat("Hypothesis:",object$h,"\n")
  cat("The test statistic value is",object$statistic,"\n")
  cat("The P value for this test is",object$p,"\n")
  cat("\n")
  cat(object$c)
})

meantest<-function(x,y=NULL,paired=FALSE,null=0,alternative="ne"){
  gp<-TRUE #used for graph plot, false if graph cannot be drawn
  g<-seq(-4,4,0.1) #used as x axis values to plot distribution
  if (is.null(y)){
    st<-shapiro.test(x)$p.value
    if (st>=0.05){
      ndf<-(length(x)-1) #df
      sc<-paste("The data is normally distributed (Shapiro P value ",round(st,digits=3),")",sep="")
      m<-"One Sample T Test"
      t<-ttest1(x,null=null,alternative=alternative)$statistic #retrieve data from pre written function
      p<-ttest1(x,null=null,alternative=alternative)$p.value
      h<-ttest1(x,null=null,alternative=alternative)$hypothesis
      plt<-plot(g,dt(g,df=ndf),type="l",main = "P value of null hypothesis",ylab="",xlab=paste("Follows t distribution with",ndf,"df")) #plot appropriate distribution
      abline(v=t,col="red",lwd=2) #plot test statistic
      legend(1.5,0.35,legend="P value of H0",fill="light blue",bty="n")
      if (alternative=="ne"){
        if (t<0){ #separate to be able to plot p value
          polygon(c(seq(max(t,-4),-4,-0.1),seq(-4,max(t,-4),0.1)),c(rep(0,length(seq(max(t,-4),-4,-0.1))),dt(seq(-4,max(t,-4),0.1),df=ndf)),col="light blue")
          polygon(c(seq(min(-t,4),4,0.1),seq(4,min(-t,4),-0.1)),c(rep(0,length(seq(min(-t,4),4,0.1))),dt(seq(4,min(-t,4),-0.1),df=ndf)),col="light blue")
          #polygon function to plot p value
        }
        if (t>=0){
          polygon(c(seq(min(t,4),4,0.1),seq(4,min(t,4),-0.1)),c(rep(0,length(seq(min(t,4),4,0.1))),dt(seq(4,min(t,4),-0.1),df=ndf)),col="light blue")
          polygon(c(seq(max(-t,-4),-4,-0.1),seq(-4,max(-t,-4),0.1)),c(rep(0,length(seq(max(-t,-4),-4,-0.1))),dt(seq(-4,max(-t,-4),0.1),df=ndf)),col="light blue")
        }
      }
      if (alternative=="l"){
        polygon(c(seq(max(t,-4),-4,-0.1),seq(-4,max(t,-4),0.1)),c(rep(0,length(seq(max(t,-4),-4,-0.1))),dt(seq(-4,max(t,-4),0.1),df=ndf)),col="light blue")
      }
      if (alternative=="g"){
        polygon(c(seq(min(t,4),4,0.1),seq(4,min(t,4),-0.1)),c(rep(0,length(seq(min(t,4),4,0.1))),dt(seq(4,min(t,4),-0.1),df=ndf)),col="light blue")
      }
    }
    if (st<0.05){
      sym<-symmetry.test(x)$p.value
      if (sym>=0.05){
        sc<-paste("The data is symmetric (Symmetry P value ",round(sym,digits=3),"), but not normal",sep="")
        m<-"Wilcox Signed Rank Test"
        t<-wtest1(x,null=null,alternative=alternative)$statistic
        p<-wtest1(x,null=null,alternative=alternative)$p.value
        h<-wtest1(x,null=null,alternative=alternative)$hypothesis
        z<-wtest1(x,null=null,alternative=alternative)$z
        plt<-plot(g,dnorm(g),type="l",main="P value of null hypothesis",ylab="",xlab="Follows normal distribution")
        abline(v=z,col="red",lwd=2)
        legend(1.5,0.35,legend="P value of H0",fill="light blue",bty="n")
        if (alternative=="ne"){
          if (z<0){
            polygon(c(seq(max(z,-4),-4,-0.1),seq(-4,max(z,-4),0.1)),c(rep(0,length(seq(max(z,-4),-4,-0.1))),dnorm(seq(-4,max(z,-4),0.1))),col="light blue")
            polygon(c(seq(min(-z,4),4,0.1),seq(4,min(-z,4),-0.1)),c(rep(0,length(seq(min(-z,4),4,0.1))),dnorm(seq(4,min(-z,4),-0.1))),col="light blue")
          }
          if (z>=0){
            polygon(c(seq(min(z,4),4,0.1),seq(4,min(z,4),-0.1)),c(rep(0,length(seq(min(z,4),4,0.1))),dnorm(seq(4,min(z,4),-0.1))),col="light blue")
            polygon(c(seq(max(-z,-4),-4,-0.1),seq(-4,max(-z,-4),0.1)),c(rep(0,length(seq(max(-z,-4),-4,-0.1))),dnorm(seq(-4,max(-z,-4),0.1))),col="light blue")
          }
        }
        if (alternative=="l"){
          polygon(c(seq(max(z,-4),-4,-0.1),seq(-4,max(z,-4),0.1)),c(rep(0,length(seq(max(z,-4),-4,-0.1))),dnorm(seq(-4,max(z,-4),0.1))),col="light blue")
        }
        if (alternative=="g"){
          polygon(c(seq(min(z,4),4,0.1),seq(4,min(z,4),-0.1)),c(rep(0,length(seq(min(z,4),4,0.1))),dnorm(seq(4,min(z,4),-0.1))),col="light blue")
        }
      }
      if (sym<0.05){
        gp<-FALSE
        sc<-paste("The data is not symmetric, and not normal (Shapiro P value ",round(st,digits=3),"), (Symmetry P value ",round(sym,digits=3),")",sep="")
        m<-"Permutation Test"
        if (alternative=="l"){ #need to split into 3 cases, to match different alternative notation
          t<-oneSamplePermutationTest(x,mu=null,alternative="less")$statistic
          p<-oneSamplePermutationTest(x,mu=null,alternative="less")$p.value
          h<-paste("μ =",null,"against μ <",null)
        }
        if (alternative=="g"){
          t<-oneSamplePermutationTest(x,mu=null,alternative="greater")$statistic
          p<-oneSamplePermutationTest(x,mu=null,alternative="greater")$p.value
          h<-paste("μ =",null,"against μ >",null)
        }
        if (alternative=="ne"){
          t<-oneSamplePermutationTest(x,mu=null,alternative="two.sided")$statistic
          p<-oneSamplePermutationTest(x,mu=null,alternative="two.sided")$p.value
          h<-paste("μ =",null,"against μ ≠",null)
        }
      }
    }
  }
  if (!is.null(y)){
    if (paired){
      x<-x-y #calculate difference of two samples
      if (all(x==x[1])){
        return(paste("All differences are",x[1])) #prevent error when same datasets are used
      }
      st<-shapiro.test(x)$p.value
      if (st>=0.05){
        ndf=(length(x)-1)
        sc<-paste("The difference data is normally distributed (Shapiro P value ",round(st,digits=3),")",sep="")
        m<-"One Sample T Test"
        t<-ttest1(x,null=null,alternative=alternative)$statistic
        p<-ttest1(x,null=null,alternative=alternative)$p.value
        plt<-plot(g,dt(g,df=ndf),type="l",main = "P value of null hypothesis",ylab="",xlab=paste("Follows t distribution with",ndf,"df"))
        abline(v=t,col="red",lwd=2)
        legend(1.5,0.35,legend="P value of H0",fill="light blue",bty="n")
        if (alternative=="ne"){
          h<-paste("Δ =",null,"against Δ ≠",null)
          if (t<0){
            polygon(c(seq(max(t,-4),-4,-0.1),seq(-4,max(t,-4),0.1)),c(rep(0,length(seq(max(t,-4),-4,-0.1))),dt(seq(-4,max(t,-4),0.1),df=ndf)),col="light blue")
            polygon(c(seq(min(-t,4),4,0.1),seq(4,min(-t,4),-0.1)),c(rep(0,length(seq(min(-t,4),4,0.1))),dt(seq(4,min(-t,4),-0.1),df=ndf)),col="light blue")
          }
          if (t>=0){
            polygon(c(seq(min(t,4),4,0.1),seq(4,min(t,4),-0.1)),c(rep(0,length(seq(min(t,4),4,0.1))),dt(seq(4,min(t,4),-0.1),df=ndf)),col="light blue")
            polygon(c(seq(max(-t,-4),-4,-0.1),seq(-4,max(-t,-4),0.1)),c(rep(0,length(seq(max(-t,-4),-4,-0.1))),dt(seq(-4,max(-t,-4),0.1),df=ndf)),col="light blue")
          }
        }
        if (alternative=="l"){
          h<-paste("Δ =",null,"against Δ <",null)
          polygon(c(seq(max(t,-4),-4,-0.1),seq(-4,max(t,-4),0.1)),c(rep(0,length(seq(max(t,-4),-4,-0.1))),dt(seq(-4,max(t,-4),0.1),df=ndf)),col="light blue")
        }
        if (alternative=="g"){
          h<-paste("Δ =",null,"against Δ >",null)
          polygon(c(seq(min(t,4),4,0.1),seq(4,min(t,4),-0.1)),c(rep(0,length(seq(min(t,4),4,0.1))),dt(seq(4,min(t,4),-0.1),df=ndf)),col="light blue")
        }
      }
      if (st<0.05){
        sym<-symmetry.test(x)$p.value
        if (sym>=0.05){
          sc<-paste("The difference data is symmetric (Symmetry P value ",round(sym,digits=3),"), but not normal",sep="")
          m<-"Wilcox Signed Rank Test"
          t<-wtest1(x,null=null,alternative=alternative)$statistic
          p<-wtest1(x,null=null,alternative=alternative)$p.value
          z<-wtest1(x,null=null,alternative=alternative)$z
          plt<-plot(g,dnorm(g),type="l",main="P value of null hypothesis",ylab="",xlab="Follows normal distribution")
          abline(v=z,col="red",lwd=2)
          legend(1.5,0.35,legend="P value of H0",fill="light blue",bty="n")
          if (alternative=="ne"){
            h<-paste("Δ =",null,"against Δ ≠",null)
            if (z<0){
              polygon(c(seq(max(z,-4),-4,-0.1),seq(-4,max(z,-4),0.1)),c(rep(0,length(seq(max(z,-4),-4,-0.1))),dnorm(seq(-4,max(z,-4),0.1))),col="light blue")
              polygon(c(seq(min(-z,4),4,0.1),seq(4,min(-z,4),-0.1)),c(rep(0,length(seq(min(-z,4),4,0.1))),dnorm(seq(4,min(-z,4),-0.1))),col="light blue")
            }
            if (z>=0){
              polygon(c(seq(min(z,4),4,0.1),seq(4,min(z,4),-0.1)),c(rep(0,length(seq(min(z,4),4,0.1))),dnorm(seq(4,min(z,4),-0.1))),col="light blue")
              polygon(c(seq(max(-z,-4),-4,-0.1),seq(-4,max(-z,-4),0.1)),c(rep(0,length(seq(max(-z,-4),-4,-0.1))),dnorm(seq(-4,max(-z,-4),0.1))),col="light blue")
            }
          }
          if (alternative=="l"){
            h<-paste("Δ =",null,"against Δ <",null)
            polygon(c(seq(max(z,-4),-4,-0.1),seq(-4,max(z,-4),0.1)),c(rep(0,length(seq(max(z,-4),-4,-0.1))),dnorm(seq(-4,max(z,-4),0.1))),col="light blue")
          }
          if (alternative=="g"){
            h<-paste("Δ =",null,"against Δ >",null)
            polygon(c(seq(min(z,4),4,0.1),seq(4,min(z,4),-0.1)),c(rep(0,length(seq(min(z,4),4,0.1))),dnorm(seq(4,min(z,4),-0.1))),col="light blue")
          }
        }
        if (sym<0.05){
          gp<-FALSE
          sc<-paste("The data is not symmetric, and not normal (Shapiro P value ",round(st,digits=3),"), (Symmetry P value ",round(sym,digits=3),")",sep="")
          m<-"Permutation Test"
          if (alternative=="l"){
            t<-oneSamplePermutationTest(x,mu=null,alternative="less")$statistic
            p<-oneSamplePermutationTest(x,mu=null,alternative="less")$p.value
            h<-paste("Δ =",null,"against Δ <",null)
          }
          if (alternative=="g"){
            t<-oneSamplePermutationTest(x,mu=null,alternative="greater")$statistic
            p<-oneSamplePermutationTest(x,mu=null,alternative="greater")$p.value
            h<-paste("Δ =",null,"against Δ >",null)
          }
          if (alternative=="ne"){
            t<-oneSamplePermutationTest(x,mu=null,alternative="two.sided")$statistic
            p<-oneSamplePermutationTest(x,mu=null,alternative="two.sided")$p.value
            h<-paste("Δ =",null,"against Δ ≠",null)
          }
        }
      }
    }
    if (paired==FALSE){
      gp<-FALSE
      xst<-shapiro.test(x)$p.value
      yst<-shapiro.test(y)$p.value
      if (xst>=0.05 & yst>=0.05){ #both x and y are normal
        sc<-paste("Both samples are normally distributed")
        if (var.test(x,y)$p.value>=0.05){ #test x and y for equal variance 
          t<-ttestev(x=x,y=y,null=null,alternative=alternative)$statistic
          p<-ttestev(x=x,y=y,null=null,alternative=alternative)$p.value
          h<-ttestev(x=x,y=y,null=null,alternative=alternative)$hypothesis
          m<-ttestev(x=x,y=y,null=null,alternative=alternative)$method
        } else {
          t<-ttest2(x=x,y=y,null=null,alternative=alternative)$statistic
          p<-ttest2(x=x,y=y,null=null,alternative=alternative)$p.value
          h<-ttest2(x=x,y=y,null=null,alternative=alternative)$hypothesis
          m<-ttest2(x=x,y=y,null=null,alternative=alternative)$method
        }
      } else { 
        xsym<-symmetry.test(x)$p.value
        ysym<-symmetry.test(y)$p.value
        if (xsym>=0.05 & ysym>=0.05){ #both x and y are symmetric 
          sc<-"Both samples are symmetric, at least one is not normally distributed"
          m<-"Two Sample Wilcox Sign Rank Test"
          if (alternative=="ne"){
            t<-wilcox.test(x=x,y=y,mu=null,alternative="two.sided")$statistic
            p<-wilcox.test(x=x,y=y,mu=null,alternative="two.sided")$p.value
            h<-paste("Δ =",null,"against Δ ≠",null)
          } 
          if (alternative=="l"){
            t<-wilcox.test(x=x,y=y,mu=null,alternative="lower")$statistic
            p<-wilcox.test(x=x,y=y,mu=null,alternative="lower")$p.value  
            h<-paste("Δ =",null,"against Δ <",null)  
          }
          if (alternative=="g"){
            t<-wilcox.test(x=x,y=y,mu=null,alternative="greater")$statistic
            p<-wilcox.test(x=x,y=y,mu=null,alternative="greater")$p.value  
            h<-paste("Δ =",null,"against Δ >",null)  
          } 
        } else{
          if (null!=0){ #if the null is not equal to 0
            return("At least one sample is neither normal nor symmetric, null must be 0")
          }else{
            sc<-paste("At least one of the samples is neither normal nor symmetric")
            m<-"Two Sample Permutation Test"
            if (alternative=="ne"){
              t<-perm.t.test(x=x,y=y,alternative="two.sided")$statistic
              p<-perm.t.test(x=x,y=y,alternative="two.sided")$p.value
              h<-paste("Δ =",null,"against Δ ≠",null)
            }
            if (alternative=="l"){
              t<-perm.t.test(x=x,y=y,alternative="lower")$statistic
              p<-perm.t.test(x=x,y=y,alternative="lower")$p.value  
              h<-paste("Δ =",null,"against Δ <",null)  
            }
            if (alternative=="g"){
              t<-perm.t.test(x=x,y=y,alternative="greater")$statistic
              p<-perm.t.test(x=x,y=y,alternative="greater")$p.value  
              h<-paste("Δ =",null,"against Δ >",null)  
            } 
          }
        }  
      }   
    }
    
  }
  if (gp==FALSE){
    plt<-plot(1, type="n", xlab="", ylab="", xlim=c(-4, 4), ylim=c(0, 1)) #plot output if graph cannot be plotted
    text(0,0.5,labels = "Plot is not available")
  }
  if (p<0.05){
    c<-"There is sufficient evidence to reject the null hypothesis" #outputs conclusion
  }
  if (p>=0.05){
    c<-"There is not significant evidence to reject the null hypothesis"
  }
  ls<-list(data=sc,statistic=t,p.value=p,hypothesis=h,method=m,c=c,plt)
  new("mt",ls) #outputs data according to class made earlier
}

