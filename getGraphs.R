#/usr/bin/Rscript/
library(stringr)
library(data.table)
source("compress.R")
baseurl="http://www.vets.ucar.edu/vg/isabeldata/"

getData=function(tstep=2,tmax=43,step=10,path="data/"){
        a=Sys.time()
	tseq=seq(1,tmax,tstep)
	if(!file.exists(path)) dir.create(path)
	if(!file.exists(path)){
	        print(c("Unable to create directory",path,"choose another one"))
	        return(NA)
	}
	
	slice=list()
	fullDb=data.table(i=NA,x=NA,y=NA,z=NA,value=NA,variable=NA,t=NA)
	fullDb=fullDb[-1,]
	cm=0
	for( t in tseq){
	        cm=cm+1
	        tt=str_pad(t,2,pad="0")
	      #  print(c(t,"of",max(tseq)))
	        
	        lfile=paste(path,"TCf",tt,".bin.gz",sep="")
	    #    print(c(path,lfile,!file.exists(lfile)))
	        lsfile=paste(path,"TCf",tt,".step.",step,".bin.gz",sep="")
	        rfile=paste(baseurl,"TCf",tt,".bin.gz",sep="")
	        if(!file.exists(lfile) && !file.exists(lsfile)) {
	                print(c("Downloading remote file:",rfile,"to",lfile))
	                download.file(rfile,lfile)
	        }
	        if(!file.exists(lsfile)){
	                temp=compress(t=t,step,"temperature",path)
	        }else{
	                temp=uncompress(t=t,step,"temperature",path)
	        }
	        temp$variable="temperature"
	        colnames(temp)[5]="value"
	        temp$t=cm
	        lfile=paste(path,"Pf",tt,".bin.gz",sep="")
	        lsfile=paste(path,"Pf",tt,".step.",step,".bin.gz",sep="")
	        rfile=paste(baseurl,"Pf",tt,".bin.gz",sep="")
	        if(!file.exists(lfile) && !file.exists(lsfile)) {
	                print(c("Downloading remote file:",rfile,"to",lfile))
	                download.file(rfile,lfile)
	        }
	        if(!file.exists(lsfile)){
	                press=compress(t=t,step,"pressure",path)
	        }
	        else{
	                press=uncompress(t=t,step,"pressure",path)
	        }
	        press$variable="pressure"
	        colnames(press)[5]="value"
	        press$t=cm
	        fullDb=rbind(fullDb,temp)
	        fullDb=rbind(fullDb,press)
	        fullDb
	}
	b=Sys.time()
	print(paste("Took",b-a,"secs to collect the data"))
	fullDb
}

	
#manipulate(makeGrid(db,zControl),zControl=slider(1,100))
