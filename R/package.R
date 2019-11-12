library(devtools)
library("ggplot2")
library("gridExtra")
library("gtable")
library("grid")
library("ggplotify")


# Make folders if they don't exit
directory <- function(){
if(!dir.exists("tables")) {dir.create("tables")}
if(!dir.exists("tables/Rates")) {dir.create("tables/Rates")}
if(!dir.exists("tables/RevRequirements")) {dir.create("tables/RevRequirements")}
if(!dir.exists("tables/KeyAssumptions")) {dir.create("tables/KeyAssumptions")}
if(!dir.exists("tables/CostOfService")) {dir.create("tables/CostOfService")}
}

read <- function(filename){
  read.csv(paste0(filename,".csv"),check.names = F)
}


shorten <- function(filename){
  gsub(" ","\n",colnames(filename))
}


design <- function(filename){
  bold<-c(which(filename$design1=="bold"))
  blue<-c(which(filename$design1=="blue"))
  italic<-c(which(filename$design1=="italic"))
  red<-c(which(filename$design1=="red"))
  cbind(bold,blue,italic,red)

}



trim <- function(filename,lrow="a",lcol="a"){
  if(lrow=="a"&lcol=="a"){
    lrow=which(filename$design1=="end")
    lcol=which(colnames(filename)=="design1")
    filename[1:lrow-1,1:lcol-1]
  }
  else if(lrow=="a"){
    filename[1:nrow(filename),1:lcol]}
  else if(lcol=="a"){
    filename[1:lrow,1:ncol(filename)]}
  else{
    filename[1:lrow,1:lcol]
  }
}


table <- function(fname,d=NULL,reg=T,type="",Ititle=""){

  rdnred <- rgb(166,9,61, maxColorValue = 255)
  rdnblue <- rgb(0,62,110, maxColorValue = 255)
  rdngrey <- rgb(188,190,192, maxColorValue = 255)
  rdnaqua <- rgb(100,196,169, maxColorValue = 255)
  rdnora <- rgb(239,72,72, maxColorValue = 255)
  rdngreen <- rgb(173,209,76, maxColorValue = 255)
  c2 <- c(rdnblue,rdnred)
  c6 <- c(rdnblue,rdngrey,rdnaqua,rdnred,rdnora,rdngreen)
  bg2 <- c("snow1", "snow2")
  f3 <- c("black", "white", "red")

  greyrows<-seq(2,nrow(fname),2)
  bolds <- d[,1]
  its <- d[,4]
  blues <- d[,2]
  reds <- d[,3]

  bold <- matrix("plain",nrow(fname),ncol(fname))
  bold[bolds,1:ncol(fname)] <- c("bold")
  bold[its,1:ncol(fname)] <- c("italic")

  bcell<- matrix(c(bg2[1]),nrow(fname),ncol(fname))
  bcell[greyrows,1:ncol(fname)] <- c(bg2[2])
  bcell[blues,1:ncol(fname)] <- c(c2[1])

  fontcol <- matrix(c(f3[1]),nrow(fname), ncol(fname))
  fontcol[reds,1:ncol(fname)] <- c(f3[3])

  #Setting design characteristics

  gdltheme <- ttheme_minimal(
    core=list(bg_params = list(fill = bcell, col=F),
              fg_params=list(fontface=bold,hjust=1, x=.96, col=fontcol)),
    colhead=list(fg_params=list(col=f3[2], fontface="bold"),
                 bg_params=list(fill=c2[1], col=f3[2])))
  ###print etc.###
  fgrid <- tableGrob(theme = gdltheme, format(fname), rows=NULL)
  fgrid <- gtable_add_grob(fgrid, grobs=segmentsGrob(
    x0=unit(0,"npc"),
    y0=unit(0,"npc"),
    x1=unit(1,"npc"),
    y1=unit(0,"npc"),
    gp=gpar(lwd=4.0,col=c2[1])),
    t=nrow(fname)+1,b=nrow(fname)+1,l=1,r=ncol(fname))

  h3 = grid::convertHeight(sum(fgrid$heights), "in", TRUE)
  w3 = grid::convertWidth(sum(fgrid$widths), "in", TRUE)
  fname <- as.character(names(fname))
  if(type=="rates"){
    ggsave(filename = paste0("tables/Rates/",Ititle,".jpg",sep=""), fgrid, height = h3, width = w3)
  } else if (type=="cos"){
    ggsave(filename = paste0("tables/CostOfService/",Ititle,".jpg",sep=""), fgrid, height = h3, width = w3)
  } else if (type=="revreq"){
    ggsave(filename = paste0("tables/RevRequirements/",Ititle,".jpg",sep=""), fgrid, height = h3, width = w3)
  } else if (type=="key"){
    ggsave(filename = paste0("tables/KeyAssumptions/",Ititle,".jpg",sep=""), fgrid, height = h3, width = w3)
  } else {
    ggsave(filename = paste0(Ititle,".jpg",sep=""), fgrid, height = h3, width = w3)
  }
}

