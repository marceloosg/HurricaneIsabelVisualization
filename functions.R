library(data.table)
library(plyr)
library(manipulate)
library(ggplot2)
cutDb=function(db,step=10,reload=F)
{
        ldim=c(500,500,100)
        if(reload){
                ldim=ldim/step
                step=1
        }
        grids=list()
        grids$x=seq(1,ldim[1],step)
        grids$y=seq(1,ldim[2],step)
        grids$z=seq(1,ldim[3],step)
        grids$fit=expand.grid(x=grids$x,y=grids$y,z=grids$z)
        gridsdb=data.table(grids$fit)
        gridsdb$i=gridsdb$x+ldim[1]*(gridsdb$y-1)+ldim[1]*ldim[2]*(gridsdb$z-1)
        gridsdb=merge(gridsdb,db,all.x=T,by="i")
        gridsdb
}
makeFImage=function(db,mx,my){
        library(ggplot2)
        ggplot(db, aes(x=x, y=y, z = value,fill=value)) +
                geom_tile()+geom_point(x=mx,y=my,col="white")+
                scale_fill_distiller(palette="Spectral",
                                     na.value="black")+
                geom_contour(bins=4,color="white", alpha=1,na.rm=T)
}

makeImage=function(db,target,mx,my){
        library(ggplot2)
        
        ggplot(db, aes_string(x="x", y="y", z = target,fill=target)) +
                geom_tile()+geom_point(x=mx,y=my,pch=3,size=5,col="white")+
                scale_fill_distiller(palette="Spectral", na.value="black") +
                geom_contour(bins=4,color="white", alpha=1,na.rm=T)
}
loadDb=function(filen,target,step=10){
        defaultn=500*500*100
        reload=F
        if(length(grep("step",filen))>0){
                step=as.integer(gsub("\\..*","",gsub(".*step\\.","",filen)))
                defaultn=defaultn/step^3
                reload=T
        }
        if(length(grep(".gz$",filen))>0){
                to.read=gzfile(filen,"rb")
        }
        else{
                to.read=file(filen,"rb")
        }
        out=readBin(to.read,double(),size=4,n=defaultn,endian="swap")
        out[which(out>10000)]=NA
        close(to.read)
        library(data.table)
        db=data.table(i=(1:length(out)), w=out)
        colnames(db)=c("i",target)
        db=cutDb(db,step=step,reload)
        db
}
saveDb=function(fdb,filen,target){
        if(length(grep(".gz$",filen))>0){
                to.write=gzfile(filen,"wb")
        }
        else{
                to.write=file(filen,"wb")
        }
        writeBin(object=fdb[[target]],con=to.write,size=4,endian="swap")
        close(to.write)
        dim(fdb)[1]*4
}
getFilter=function(fullDb,step=10,tstep=2){

        manipulate(choose(fullDb,x,y,z,t,variable),
                x=slider(1,500/step,35),
                y=slider(1,500/step,35),
                z=slider(1,100/step,1),
                t=slider(1,48/tstep,1),
                variable= picker("temperature" = "temperature", 
                               "pressure" = "pressure",
                               "both"="both")
                        )
}
choose=function(fullDb,mx,my,mz,mt,mvariable){
        if(mvariable=="both"){
                db=fullDb[x==mx &
                               y==my &
                               z==mz,]
                db=db[complete.cases(db),]
                ggplot(data=db,
                aes(x=t,y=value))+
                        geom_point()+
                        geom_line(aes(col=variable))+
                        facet_grid(variable ~ .,scale="free",switch="both")
        }else{
                db=fullDb[z==mz & t==mt & variable==mvariable,]
                makeImage(db,"value",mx,my)
        }
}