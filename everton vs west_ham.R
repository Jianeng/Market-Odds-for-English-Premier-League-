library(nleqslv)
library(skellam)
library(knitr)
library(ggplot2)

#####   Data  #####
data<-read.csv("everton.csv")
data<-data[,c(2:7,9:13)]
data[is.na(data)]<-Inf
data[,-1]<-1/(1+data[,-1])
data[,-1]<-apply(data[,-1],2,function (x) x/sum(x))

goalx<-c(13,56)/90 #everton
goaly<-c(78,81,89)/90 #west ham
theta_MM_1=rep(0,10)
theta_MM_2=rep(0,10)




#####   Estimation  #####
m<-dim(data)[1]
N<-10000
for (i in 1:10)
{
  Z<-rep(data[,1],round(N*data[,i+1]))
  
  #calibrate odds
  if (2<i&i<8) Z=Z-1
  if (i>7&i<10) Z=Z-2
  
  target=function(theta,V=var(Z),E=mean(Z))
  {
    result=(V-theta[1]-theta[2])^2+(E-theta[1]+theta[2])^2 +(max(-theta[1],0)^2+max(-theta[2],0)^2)*100000
    return(result)
  }
  output=optim(c(1,1),target)
  theta_MM_1[i]=output$par[1]
  theta_MM_2[i]=output$par[2]
}
#theta_MM_1 = c(theta_MM_1,0)
#theta_MM_2 = c(theta_MM_2,0)
t = c(0,10,20,30,40,45,55,65,75,85,90)/90
IV = sqrt(theta_MM_1+theta_MM_2)
everton = cbind(t,theta_MM_1,theta_MM_2,IV)
everton = t(everton)


#####   Comparison   ##### 

p1 = matrix(NA,10,10) #data
p2 = matrix(NA,10,10) #skellam
for (t in 2:11)
{
  for (i in -4:5)
  {
    p1[i+5,t-1] = sum(data[which(data[,1]==i),t])
    p2[i+5,t-1] = dskellam(i,lambda1=theta_MM_1[t-1],lambda2 = theta_MM_2[t-1])
    if(3<t&t<9)
      p2[i+5,t-1] = dskellam(i-1,lambda1=theta_MM_1[t-1],lambda2 = theta_MM_2[t-1])
    if(8<t&t<11)
      p2[i+5,t-1] = dskellam(i-2,lambda1=theta_MM_1[t-1],lambda2 = theta_MM_2[t-1])
  }
  
}
p1 = p1[,-6]
p2 = p2[,-6]
p1 = round(p1*100,2)
p2 = round(p2*100,2)

Time = c("t = 0", "t = 0.11", "t = 0.22", "t = 0.33", "t = 0.44", 
"t = 0.61", "t = 0.72", "t = 0.83", "t = 0.94")
Probability = c(c(p1),c(p2))
Difference = rep(-4:5,18)
Time = rep(Time,each=10)
Time = c(Time,Time)
Type = c(rep("Market Implied Prob.",90),rep("Skellam Implied Prob.",90))

Comparison = data.frame(Probability,Difference,Time,Type)

a = ggplot(data=Comparison, aes(x=Difference, y=Probability, color=Type,shape=Type)) +
  geom_line()+
  geom_point()+
  facet_wrap(~Time,nrow = 3,ncol = 3)+
  theme_bw()+
  scale_x_continuous(breaks=-4:5,labels = -4:5)+
  labs(title="Market Implied Probability vs Skellam Implied Probability",
       y="Probability (%)",x="Score Difference")+
  theme(legend.position="bottom")
  
setEPS()
postscript("comparison.eps",width = 8,height = 8)
a
dev.off()

#####   Probability   ##### 
n<-90000
#x<-c(0,10,20,30,40,45,55,65,75,85,90)
#y1<-c(theta_MM_1,0)
#y2<-c(theta_MM_2,0)
x<-c(0,10,20,30,34,40,45,55,65,75,85,90)
y1<-c(theta_MM_1[1:4],theta_MM_1[4],theta_MM_1[5:10],0)
y2<-c(theta_MM_2[1:4],theta_MM_2[4],theta_MM_2[5:10],0)
theta_x<-spline(x,y1,n)$y
theta_y<-spline(x,y2,n)$y
t<-seq(0,1,length.out=n)
x<-rep(0,n)
y<-rep(0,n)
if(length(goalx)>0)
  for(i in 1:length(goalx))
    x<-x+c(rep(0,floor(goalx[i]*n)),rep(1,n-floor(goalx[i]*n)))
if(length(goaly)>0)
  for(i in 1:length(goaly))
    y<-y+c(rep(0,floor(goaly[i]*n)),rep(1,n-floor(goaly[i]*n)))

p_win<-pskellam(y-x,lambda1 = theta_x,lambda2 = theta_y,lower.tail = FALSE)*100
p_draw<-dskellam(y-x,lambda1 = theta_x,lambda2 = theta_y)*100
p_lose<-100-p_win-p_draw

iv<-sqrt(theta_x+theta_y)

#####   Probability Plot   ##### 
pdf(file = "Everton vs West Ham.pdf",width = 10,height = 7)
par(xpd=TRUE,mar=c(10,4,4,4))
plot(t,100-p_win,type="l",ylim=c(0,100),xaxs="i",yaxs="i",lab=c(10,10,12),
     main = "Everton vs. West Ham (SAT, 05 MAR 2016)",
     xlab = "Time",ylab="Probability (%)")
lines(t,p_lose,col="red")
mtext("Implied Volatility",side=4,line=3)
axis(4,labels = seq(0,2,0.2),at=seq(0,100,10))


polygon(c(rev(t), t), c(rev(p_lose),100-p_win), col = "dodgerblue4", border = NA)
polygon(c(rev(t), t), c(rev(100-p_win),rep(100,n)), col = "firebrick3", border = NA)
polygon(c(rev(t), t), c(rev(rep(0,n)),p_lose), col = "goldenrod1", border = NA)
lines(t,iv*50,lwd=2.5)
legend(0.12,-21,legend=c("Implied Volatility","Everton Wins","West Ham Wins","Draw"),horiz=TRUE,
       fill=c("black","firebrick3","goldenrod1","dodgerblue4"),angle=45)

text(85/90,96,"Goal:W")
par(xpd=FALSE)
abline(v=13/90,lty="dotted",lwd=2) 
text(13/90,90,"Goal:E")
abline(v=56/90,lty="dotted",lwd=2)
text(56/90,90,"Goal:E")
abline(v=78/90,lty="dotted",lwd=2)
text(78/90,81,"Goal:W")
abline(v=81/90,lty="dotted",lwd=2)
text(81/90,88,"Goal:W")
abline(v=89/90,lty="dotted",lwd=2)
abline(v=34/90,lty="dotted",lwd=2)
text(34/90,70,"Red Card:E")
abline(v=0.5,lty="dotted",lwd=2)
text(0.5,60,"Half Time")
dev.off() 

##### Histogram #####

## Full time
prob<-matrix(0,6,6)
for (i in 1:6)
  for (j in 1:6)
    prob[i,j]=dpois(i-1,lambda = everton[1])*dpois(j-1,lambda = everton[2])
percent=round(prob*100,2)
margin1=apply(percent,1,sum)
margin2=apply(percent,2,sum)
estimate<-cbind(margin2,percent)
estimate<-rbind(c(NA,margin1),estimate)
kable(estimate,"latex",align = 'c')

prob = dskellam(-5:5,lambda1 = everton[1,1],lambda2 = everton[1,2])







histo_full<-rep(0,11)  # full
for (i in 1:11) 
  histo_full[i]=dskellam(i-6,lambda1 = everton[1,1],lambda2 = everton[1,2])
par(xpd=TRUE,mar=c(4,4,4,4))
barplot(beside=TRUE, 
        height=histo_full*100,
        names.arg=-5:5,
        border="black",
        col=c(rep("goldenrod1",5),"dodgerblue4",rep("firebrick3",5)),
        ylim = c(0,25),ylab = "Probability (%)",xlab = "Score Difference",
        main = "Probability of Score Difference - Before 1st Half"
)  
#legend(1,-5,legend=c("Everton Wins","West Ham Wins","Draw"),horiz=TRUE,
#       fill=c("firebrick3","goldenrod1","dodgerblue4"))
text(2.5,20,"West Ham Wins = 23.03%")
text(11,20,"Everton Wins = 57.47%")
text(6.5,23,"Draw = 19.50%")

## Half time
histo_half<-rep(0,11)  # half
for (i in 1:11) 
  histo_half[i]=dskellam(i-7,lambda1 = everton[6,1],lambda2 = everton[6,2])
par(xpd=TRUE,mar=c(4,4,4,4))
barplot(beside=TRUE, 
        height=histo_half*100,
        names.arg=-5:5,
        border="black",
        col=c(rep("goldenrod1",5),"dodgerblue4",rep("firebrick3",5)),
        ylim=c(0,35),ylab = "Probability (%)",xlab = "Score Difference",
        main = "Probability of Score Difference - Before 2nd Half"
)  
text(2.5,28,"West Ham Wins = 15.74%")
text(10.5,28,"Everton Wins = 61.08%")
text(6.5,32,"Draw = 23.18%")
##### simulation #####

## Full time
l<-90
N=1000
simu_full=matrix(0,N,l)
set.seed(123)
for(i in 1:N)
  {x<-cumsum(rpois(90,lambda = everton[1,1]/90))
   y<-cumsum(rpois(90,lambda = everton[1,2]/90))
   simu_full[i,]=x-y
}
t=seq(0,1,length.out = l)
plot(t,simu_full[1,],ylim = c(-6,8),"l",col=1,lwd=1.5,
     ylab = "Score Difference",xlab = "Time",main = "Game Simulations - Before 1st Half",
     ,lab=c(10,10,12))
for(j in 2:N)
  lines(t,simu_full[j,],ylim = c(-6,6),"l",col=j,lwd=1.5)

## Half time
l<-90
N=1000
simu_half=matrix(0,N,l)
simu_half[,13:45]=1
set.seed(123)
for(i in 1:N)
{x<-cumsum(rpois(45,lambda = everton[6,1]/90))
y<-cumsum(rpois(45,lambda = everton[6,2]/90))
simu_half[i,46:90]=x-y+1
}
t=seq(0,1,length.out = l)
plot(t,simu_half[1,],ylim = c(-5,5),"l",col=1,lwd=1.5,
     ylab = "Score Difference",xlab = "Time",main = "Game Simulations - Before 2nd Half",
     ,lab=c(10,10,12))
for(j in 2:N)
  lines(t,simu_half[j,],ylim = c(-6,6),"l",col=j,lwd=1.5)

################## Figure 1
pdf("figure1.pdf",height = 12,width = 12)
par(mfrow=c(2,2))
barplot(beside=TRUE, 
        height=histo_full*100,
        names.arg=-5:5,
        border="black",
        col=c(rep("goldenrod1",5),"dodgerblue4",rep("firebrick3",5)),
        ylim = c(0,25),ylab = "Probability (%)",xlab = "Score Difference",
        main = "Probability of Score Difference - Before 1st Half"
)  
#legend(1,-5,legend=c("Everton Wins","West Ham Wins","Draw"),horiz=TRUE,
#       fill=c("firebrick3","goldenrod1","dodgerblue4"))
text(3.5,20,"West Ham Wins = 23.03%")
text(11.5,20,"Everton Wins = 57.47%")
text(6.5,23,"Draw = 19.50%")

plot(t,simu_full[1,],ylim = c(-6,8),"l",col=1,lwd=1.5,
     ylab = "Score Difference",xlab = "Time",main = "Game Simulations - Before 1st Half",
     ,lab=c(10,10,12))
for(j in 2:N)
  lines(t,simu_full[j,],ylim = c(-6,6),"l",col=j,lwd=1.5)

barplot(beside=TRUE, 
        height=histo_half*100,
        names.arg=-5:5,
        border="black",
        col=c(rep("goldenrod1",5),"dodgerblue4",rep("firebrick3",5)),
        ylim=c(0,35),ylab = "Probability (%)",xlab = "Score Difference",
        main = "Probability of Score Difference - Before 2nd Half"
)  
text(3.5,28,"West Ham Wins = 15.74%")
text(11.5,28,"Everton Wins = 61.08%")
text(6.5,32,"Draw = 23.18%")

plot(t,simu_half[1,],ylim = c(-5,5),"l",col=1,lwd=1.5,
     ylab = "Score Difference",xlab = "Time",main = "Game Simulations - Before 2nd Half",
     ,lab=c(10,10,12))
for(j in 2:N)
  lines(t,simu_half[j,],ylim = c(-6,6),"l",col=j,lwd=1.5)
dev.off()


