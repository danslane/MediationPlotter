#Set your work directory here
setwd("") 

# Load the image as an R object with the "JPEG" package
library(jpeg)
library(extrafont)
library(mediation)
my_image<-readJPEG("MediationPlots.jpg")
#load sample data set
D<-mtcars


###Run a simple mediation w/ 'mediation' package####
#First run to two OLS 
fit1<-lm(wt~disp,data=D)
fit2<-lm(mpg~wt+disp,data=D)

##Run mediation test###

#Set number of bootstrap simulations
sims<-100

med.out<- mediate(fit1, fit2, treat = "disp", mediator = "wt",boot=T,sims = sims,conf.level =.95)
summary(med.out)

###PLOT#####

#Create Base plot#
par(mar = c(0,0,0,0),family="Arial") # set zero margins on all 4 sides
plot(x = NULL, y = NULL, xlim = c(0,1584), ylim = c(0,891), pch = '',
     xaxt = 'n', yaxt = 'n', xlab = '', ylab = '', xaxs = 'i', yaxs = 'i',
     bty = 'n') # plot empty figure
rasterImage(my_image, xleft = 0, ybottom = 0, xright = 1584, ytop = 891) # plot jpeg

#Extract terms for plot from mediation object#
dv<-med.out[["model.y"]][["terms"]][[2]]
med<-med.out$call[[7]]
iv<-med.out$call[[6]]
title<-paste("Mediation model for",dv) 

a1<-round(med.out[["model.m"]][["coefficients"]][[paste(med.out$call[[6]])]],2)
b1<-round(med.out[["model.y"]][["coefficients"]][[paste(med.out$call[[7]])]],2)
c1<-round(med.out[["model.y"]][["coefficients"]][[paste(med.out$call[[6]])]],2)

pvalA<-summary(med.out[["model.m"]])$coefficients[which(rownames(summary(med.out[["model.m"]])$coefficients)==med.out$call[[6]]),4]  
pvalB<-summary(med.out[["model.y"]])$coefficients[which(rownames(summary(med.out[["model.y"]])$coefficients)==med.out$call[[7]]),4]  
pvalC<-summary(med.out[["model.y"]])$coefficients[which(rownames(summary(med.out[["model.y"]])$coefficients)==med.out$call[[6]]),4]  

#add stars for p-values
pa<-""
pb<-""
pc<-""
if (pvalA<.05){pa<-"*"}
if (pvalA<.01){pa<-"**"}
if (pvalA<.001){pa<-"***"}
if (pvalB<.05){pb<-"*"}
if (pvalB<.01){pb<-"**"}
if (pvalB<.001){pb<-"***"}
if (pvalC<.05){pc<-"*"}
if (pvalC<.01){pc<-"**"}
if (pvalC<.001){pc<-"***"}

a<-paste(" = ",a1,pa,sep="")
b<-paste(" = ",b1,pb,sep="")
c<-paste(" = ",c1,pc,sep="")

#Plot coefficients
text(1584/2,229, paste("c'",c),cex=.9)
text(435,500,paste("a'",a),pos=2,cex=.9)
text(1170,500,paste("b'",b),pos=4,cex=.9)

#Plot variable names
text(240,185,dv)
text(797,719,med)
text(1335,185,iv)

#Add main title
text(10,845,title,pos=4)

#Report indirect effect
tsplit <- function( string , split ){
  require( stringr )
  blurb <- paste( string )
  blurbs <- strsplit( blurb , paste(split) )
  annot <- bquote( paste( bold( .( blurbs[[1]][1] ) ) , .(split) , .(blurbs[[1]][2]) , sep = "" ) )
  return( annot )
}
PE <- paste("PE = ",rd(med.out$d.avg,3),", 95% CI [", rd(med.out$d.avg.ci[1] ,3),", "
                    , rd(med.out$d.avg.ci[2] ,3),"]",sep="")
text((1584/2),(891/2),PE,font=2)

#Add notes
notes<-paste("Note.","N","=",med.out[["nobs"]],",","Bootstrap samples =",med.out[["sims"]])
text(50,50,notes,pos=4,cex=.7)

