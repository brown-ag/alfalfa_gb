#Methods file

#input generator functions
makeSelectorFit <- function(n,pval,params,outpath) {
  buffer=c(selector_template,fit_template,atmo_template,prof_template,h1d_template)
  fnames=c("SELECTOR.IN","FIT.IN","ATMOSPH.IN","PROFILE.DAT","HYDRUS1D.DAT")
  values=c(pval)
  for(p in 1:length(params)) {
    for(qq in 1:length(buffer))
      buffer[qq]=gsub(paste("%",toupper(params[p]),"%",sep=""),values[p],buffer[qq])
  }
  for(qq in 1:length(buffer)) {
    writeChar(buffer[qq],paste(outpath,"\\",n,"\\",fnames[qq],sep=""))
  }
}

makeMasterBatch <- function(nsim,sidr) {
  buffer="@echo off\n"
  for(kk in 1:(nsim)) {
    buffer=paste(buffer,"cd .\\",kk,"\n",sep="")  
    buffer=paste(buffer,"call sim.bat\n", sep="")  
    #buffer=paste(buffer,"cd ..\n", sep="")  
  }
  write(buffer,paste(sidr,"master.bat",sep=""))
}

#batch file format
#@echo S:\Andrew\Code\alfalfa_gb_git\Simulations\2\1 > "C:\hydrus1d\Level_01.dir"
#C:
#cd C:\hydrus1d\
#H1D_clci<return.txt
#buffer=paste(buffer,"\"C:\\hydrus1d\\H1D_clci.exe\" .\\",kk,"\n", sep="")  
makeBatch <- function(last_index,n,outpath,simdir) {
  buffer="@echo off\n" 
  wd=gsub("/","\\",getwd(),fixed=TRUE)
  for(kk in 1:(last_index)) {
    buffer=paste(buffer,"@echo ",simdir,n,"\\",kk," > C:\\hydrus1d\\Level_01.dir\nC:\ncd C:\\hydrus1d\\\nH1D_clci<return.txt\n",substr(simdir,0,1),":\ncd ",simdir,"\n", sep="") 
  }                   
  write(buffer,paste(outpath,"\\sim.bat",sep=""))
}

getSeq <- function(range, disc){
  return(seq(range[1],range[2],(range[2]-range[1])/disc))
}

getParams <- function(qq) {
  glib=(gsub("range_","",rownames(qq)))
  return(glib)
}

makeGrid <- function(qq,d) {
  gnames=gsub("range_","d_",rownames(qq))
  buf=list()
  for(p in 1:length(rownames(qq))) {
    lux=qq[p,2:3]
    buf[[p]]=getSeq(as.matrix(lux),d)
  }
  return(expand.grid(buf))
}

#optimization script functions
readNodeFile <- function(id,sim,simdir) {
  fn="OBS_NODE.out"
  fl="Node"
  return(readH1DFile(id,sim,fn,4,list(a="1.234",b="1.234",c="1.234",d="1.234"),12,1,simdir))
}

readFitOutFile <- function(id,sim,simdir,fp) {
    fn="FIT.out"
    fl="Node"
    buf=(readH1DFile(id,sim,fn,7,list(a="1.234",b="1.234",c="1.234",d="1.234",e="1.234",f="1.234",g="1.234"),121,1,simdir))
	return(buf[1:fp,c(2,4)])
}

readFitIn <- function(id,sim,simdir) {
  fn="FIT.IN"
  return(readH1DFile(id,sim,fn,5,list(a="1.234",b="1.234",c="1.234",d="1.234",e="1.234"),16,3,simdir))
}
fee=""
#H1D FILE reader
readH1DFile <- function(id,sim,fname,cols,whatt,skipp,trimm,simdir) {
  obs_node_fname=paste(simdir,sim,"\\",id,"\\",fname,sep="")
  print(paste(sim, id))
  con=file(obs_node_fname, open="r")
  flag=FALSE
  iyx=suppressWarnings(scan(con,what=whatt,skip=skipp,fill=TRUE))
  close(con)
  mat=matrix(unlist(iyx),ncol=cols)
  mat=mat[1:(length(mat[,1])-trimm),]
  class(mat)="numeric"
  return(mat)
}

getFitPoints <- function(x,fit,int=15) {
  x=array(x)
  mmin=0
  mmax=0
  buf=c()
  for(i in 1:length(fit)) {
    mmax=int*i
    interval=(x[((x<=mmax) + (x>mmin))-1])
    buf=c(buf,rep(i,length(interval)))
    mmin=mmax
  }
  return(buf)
}

makeLimit <- function(tplist) {
  xbar=tplist[1,1:length(tplist[1,])]
  inter=2*tplist[2,1:length(tplist[1,])]
  buf=lapply(1:length(xbar),function(i) return(c(xbar[i]-inter[i],xbar[i]+inter[i])))
  #shell.exec(paste("cp .\\LIMITS.IN .\\LIMITS",strftime(Sys.time(),format="%d%m%Y:%H%M%S"),".IN",sep=""))
  buf2=list(params,xbar)
  names(buf)=params
  #write.csv(buf,"LIMITS.IN")
  #write.csv(buf2,"CENTERS.IN")
}

#Miscellaneous borrowed
filled.contour2 <-
  function (x = seq(0, 1, length.out = nrow(z)),
            y = seq(0, 1, length.out = ncol(z)), z, xlim = range(x, finite = TRUE), 
            ylim = range(y, finite = TRUE), zlim = range(z, finite = TRUE), 
            levels = pretty(zlim, nlevels), nlevels = 20, color.palette = cm.colors, 
            col = color.palette(length(levels) - 1), plot.title, plot.axes, 
            key.title, key.axes, asp = NA, xaxs = "i", yaxs = "i", las = 1, 
            axes = TRUE, frame.plot = axes,mar, ...) 
  {
    # modification by Ian Taylor of the filled.contour function
    # to remove the key and facilitate overplotting with contour()
    if (missing(z)) {
      if (!missing(x)) {
        if (is.list(x)) {
          z <- x$z
          y <- x$y
          x <- x$x
        }
        else {
          z <- x
          x <- seq.int(0, 1, length.out = nrow(z))
        }
      }
      else stop("no 'z' matrix specified")
    }
    else if (is.list(x)) {
      y <- x$y
      x <- x$x
    }
    if (any(diff(x) <= 0) || any(diff(y) <= 0)) 
      stop("increasing 'x' and 'y' values expected")
    mar.orig <- (par.orig <- par(c("mar", "las", "mfrow")))$mar
    on.exit(par(par.orig))
    w <- (3 + mar.orig[2]) * par("csi") * 2.54
    par(las = las)
    mar <- mar.orig
    plot.new()
    par(mar=mar)
    plot.window(xlim, ylim, "", xaxs = xaxs, yaxs = yaxs, asp = asp)
    if (!is.matrix(z) || nrow(z) <= 1 || ncol(z) <= 1) 
      stop("no proper 'z' matrix specified")
    if (!is.double(z)) 
      storage.mode(z) <- "double"
    .Internal(filledcontour(as.double(x), as.double(y), z, as.double(levels), 
                            col = col))
    if (missing(plot.axes)) {
      if (axes) {
        title(main = "", xlab = "", ylab = "")
        Axis(x, side = 1)
        Axis(y, side = 2)
      }
    }
    else plot.axes
    if (frame.plot) 
      box()
    if (missing(plot.title)) 
      title(...)
    else plot.title
    invisible()
  }

filled.contour3 <-
  function (x = seq(0, 1, length.out = nrow(z)),
            y = seq(0, 1, length.out = ncol(z)), z, xlim = range(x, finite = TRUE), 
            ylim = range(y, finite = TRUE), zlim = range(z, finite = TRUE), 
            levels = pretty(zlim, nlevels), nlevels = 20, color.palette = cm.colors, 
            col = color.palette(length(levels) - 1), plot.title, plot.axes, 
            key.title, key.axes, asp = NA, xaxs = "i", yaxs = "i", las = 1, 
            axes = TRUE, frame.plot = axes,mar, ...) 
  {
    # modification by Ian Taylor of the filled.contour function
    # to remove the key and facilitate overplotting with contour()
    # further modified by Carey McGilliard and Bridget Ferris
    # to allow multiple plots on one page
    
    if (missing(z)) {
      if (!missing(x)) {
        if (is.list(x)) {
          z <- x$z
          y <- x$y
          x <- x$x
        }
        else {
          z <- x
          x <- seq.int(0, 1, length.out = nrow(z))
        }
      }
      else stop("no 'z' matrix specified")
    }
    else if (is.list(x)) {
      y <- x$y
      x <- x$x
    }
    if (any(diff(x) <= 0) || any(diff(y) <= 0)) 
      stop("increasing 'x' and 'y' values expected")
    # mar.orig <- (par.orig <- par(c("mar", "las", "mfrow")))$mar
    # on.exit(par(par.orig))
    # w <- (3 + mar.orig[2]) * par("csi") * 2.54
    # par(las = las)
    # mar <- mar.orig
    plot.new()
    # par(mar=mar)
    plot.window(xlim, ylim, "", xaxs = xaxs, yaxs = yaxs, asp = asp)
    if (!is.matrix(z) || nrow(z) <= 1 || ncol(z) <= 1) 
      stop("no proper 'z' matrix specified")
    if (!is.double(z)) 
      storage.mode(z) <- "double"
    .Internal(filledcontour(as.double(x), as.double(y), z, as.double(levels), 
                            col = col))
    if (missing(plot.axes)) {
      if (axes) {
        title(main = "", xlab = "", ylab = "")
        Axis(x, side = 1)
        Axis(y, side = 2)
      }
    }
    else plot.axes
    if (frame.plot) 
      box()
    if (missing(plot.title)) 
      title(...)
    else plot.title
    invisible()
  }

filled.legend <-
  function (x = seq(0, 1, length.out = nrow(z)), y = seq(0, 1, 
                                                         length.out = ncol(z)), z, xlim = range(x, finite = TRUE), 
            ylim = range(y, finite = TRUE), zlim = range(z, finite = TRUE), 
            levels = pretty(zlim, nlevels), nlevels = 20, color.palette = cm.colors, 
            col = color.palette(length(levels) - 1), plot.title, plot.axes, 
            key.title, key.axes, asp = NA, xaxs = "i", yaxs = "i", las = 1, 
            axes = TRUE, frame.plot = axes, ...) 
  {
    # modification of filled.contour by Carey McGilliard and Bridget Ferris
    # designed to just plot the legend
    if (missing(z)) {
      if (!missing(x)) {
        if (is.list(x)) {
          z <- x$z
          y <- x$y
          x <- x$x
        }
        else {
          z <- x
          x <- seq.int(0, 1, length.out = nrow(z))
        }
      }
      else stop("no 'z' matrix specified")
    }
    else if (is.list(x)) {
      y <- x$y
      x <- x$x
    }
    if (any(diff(x) <= 0) || any(diff(y) <= 0)) 
      stop("increasing 'x' and 'y' values expected")
    #  mar.orig <- (par.orig <- par(c("mar", "las", "mfrow")))$mar
    #  on.exit(par(par.orig))
    #  w <- (3 + mar.orig[2L]) * par("csi") * 2.54
    #layout(matrix(c(2, 1), ncol = 2L), widths = c(1, lcm(w)))
    #  par(las = las)
    #  mar <- mar.orig
    #  mar[4L] <- mar[2L]
    #  mar[2L] <- 1
    #  par(mar = mar)
    # plot.new()
    plot.window(xlim = c(0, 1), ylim = range(levels), xaxs = "i", 
                yaxs = "i")
    rect(0, levels[-length(levels)], 1, levels[-1L], col = col)
    if (missing(key.axes)) {
      if (axes) 
        axis(4)
    }
    else key.axes
    box()
  }
