require(XML)
x<-xmlParse('C:/users/trinh/downloads/TF_Xcalibur.gaml')
sapply(getNodeSet(x,'//experiment/trace/Xdata/Ydata'),xmlValue)
#similar to:
xpathSApply(x,'//experiment/trace/Xdata/Ydata',xmlValue)
getNodeSet(x,'//experiment/trace/Xdata/Ydata') #get the node set

basepk<-xpathSApply(x,'//experiment/trace/Xdata/Ydata/parameter[@name="basePk_intensity"]',xmlValue)
pkmass<-xpathSApply(x,'//experiment/trace/Xdata/Ydata/parameter[@name="scan_base_peak_mass"]',xmlValue)
plot(pkmass,basepk,type='h',lwd=1,xlim = c(100,375))

#=====================================================================
lcra<-'C:/users/trinh/downloads/ascii files/C15373_5_4509_11.lcra'
d<-xmlParse(lcra)
xmlRoot(d)[[4]]
names.XMLNode(xmlRoot(d)[['Data']][['RESULT']])
xmlSize(xmlRoot(d)[['Data']][['RESULT']][['RESULTFILES']])

getNodeSet(d,'//RESULT/RESULTFILES/valuesPeaks')
xpathSApply(d,'//RESULT/RESULTFILES/*/child::node()',xmlValue)






#=============================================##########################
library(mzR)
x<-'c:/users/trinh/downloads/ascii files/C14175_4509_11_C14175_4509_11_5_inj2_(4)_2017-05-25_04-48-30-PM_at_05-42-12-PM.mzML'
aa<-openMSfile(x)
runInfo(aa)  
instrumentInfo(aa)
pl<-peaks(aa,10)
peaksCount(aa)
plot(pl[,1],pl[,2],type='h',lwd=1)
plot(peaks(aa)[[1]],type='h',lwd=1)
chromatogramsInfo(x)
fileName(aa)

