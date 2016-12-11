#!/usr/bin/Rscript
source("functions.R")
compress=function(t,step,target="temperature",reload=F,path=""){
     #   print("packing File")
        
tt=str_pad(t,2,pad="0")
if(target=="temperature"){
        fname=paste(path,"TCf",tt,sep="")
        }
else{
        fname=paste(path,"Pf",tt,sep="")
}
ufname=paste(fname,".bin.gz",sep="")
cfname=paste(fname,".step.",step,".bin.gz",sep="")

if(!reload){
        if(!file.exists(ufname)){
                print(c(ufname,"does not exists"))
                return(NA)
        }
	fdb=loadDb(ufname,target,step)
	size=saveDb(fdb,cfname,target)
	fdb
        }
else
        {
        if(!file.exists(cfname)){
                print(c(cfname,"does not exists!"))
                return(NA)       
        }
        ndb=loadDb(cfname,target)
        ndb
        }
}

uncompress=function(t,step,target="temperature",path=""){
     #   print("Un")
  compress(t,step,target,T,path)
}
