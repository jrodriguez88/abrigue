################################################################################
####                   Reproducibility code for: 
## Models of Soil Organic Matter Decomposition: the SoilR package, version 1.0
# Authors: Carlos A. Sierra, Markus Mueller, Susan Trumbore
##############################################################     08/2012
# Published in Geosci. Model Dev. 
################################################################################

#First, load the SoilR package
library(SoilR)

#Figure 2
attr(ICBMModel,"ex") #Shows the code
attr(ICBMModel,"ex")() #Runs the example and shows Figure 2

#Figure 3
attr(TwopFeedbackModel, "ex") #Shows the code
attr(TwopFeedbackModel, "ex")() #Runs the example and shows Figure 3

#Figure 4. Implementing the RothC model from scratch
attr(RothCModel, "ex") #Shows the code
attr(RothCModel, "ex")() #Runs the example and shows Figure 4

#Figure 5
library(sp)
library(raster)
library(ncdf4)
library(colorspace)
mypal=rev(sequential_hcl(10))

setwd("../Data/") #Set to appropriate directory where accompanying data is stored

PETb=(brick("PET_PT.WATCH.MonthlyMean.1980.2001.nc"))
Pb=(brick("Precip.WATCH.MonthlyMean.1950.2001.nc"))
Tbair=(brick("Tair.WATCH.MonthlyMean.1950.2001.nc"))

PET=as.array(PETb)
P=as.array(Pb)
Tair=as.array(Tbair)

TCent1=fT.Century1(Temp=Tair-273)
WCent=fW.Century(PPT=P,PET=PET)

CDIm=TCent1*WCent
CDI=brick(CDIm)

CDIa=calc(CDI,mean)


plot(CDIa,ylab="Latitude",xlab="Longitude",col=mypal,xaxt="n",yaxt="n")

################################################################################
#Note: This code was tested on R version 2.14.1
