#This script is applied to generate PCA plots based on the gradients of different feature.
#The values of each feature are represented by color gradient.
#Written by Shi Huang, 20121029

# setwd("~/Documents/TrainingData")
#--------------------------------------------------------
# Load the packages used in this script
#--------------------------------------------------------
p <- c("clusterSim","ade4","MASS","cluster","fpc","fmsb","squash")
usePackage <- function(p) {
    if (!is.element(p, installed.packages()[,1]))
        install.packages(p, dep = TRUE, repos = "http://cran.us.r-project.org")
    suppressWarnings(suppressMessages(invisible(require(p, character.only = TRUE))))
}
invisible(lapply(p, usePackage))
#--------------------------------------------------------
# Define the filename you want to analyze
#--------------------------------------------------------
prefix="Fig_6b6c_2b_"
#--------------------------------------------------
file.opts<-list(
                prefix=prefix,
                filename=paste(prefix,"abd.txt",sep=""),
                metadata.filename=paste(prefix,"metadata.txt",sep=""),
                group.type="Timepoint",
                clinical.type="PC1_ordination",
                Host.type="Host"
               )
#--------------------------------------------------------
filename<-file.opts$filename
metadata.filename<-file.opts$metadata.filename
#--------------------------------------------------------
#--------------------------------------------------
# Set the output path 
#--------------------------------------------------
   outpath<-paste("./",prefix,".Feb08/",sep="")
   dir.create(outpath)
#-------------------------------
# Data input
#-------------------------------
    g<-read.table(filename,header=T,row.names=1)
    g<-g[order(rownames(g)),]
    print(paste("The number of variables : ", ncol(g) ,sep=""))
    #-------------------------------filtering taxa with zero variance
    g<-g[,which(apply(g,2,var)!=0)]
    print(paste("The number of variables (removed variables with zero variance) : ", ncol(g) ,sep=""))
    #-------------------------------
    gmat<-data.matrix(g)
#-------------------------------
# Metadata of training data input
#-------------------------------
    metadata<-read.table(metadata.filename,header=T,sep="\t",row.names=1)
    metadata<-metadata[order(rownames(metadata)),]
    group<-as.factor(metadata[,file.opts$group.type]); names(group)<-rownames(metadata)   
    clin<-metadata[,file.opts$clinical.type]; names(clin)<-rownames(metadata) 
    Host<-as.factor(metadata[,file.opts$Host.type]); names(Host)<-rownames(metadata) 
#--------------------------------------------------
#PCoA using ade4 package
#--------------------------------------------------
gmat<-gmat[,which(colMeans(gmat)>0.0001)]
df<-data.frame(gmat)
df.bc <- vegan::vegdist(df, method = "bray")
df.pcoa <- dudi.pco(df.bc,scan=FALSE, nf=5)
#--------------------------------------------------
Axis1<-df.pcoa$li[,1]
Axis2<-df.pcoa$li[,2]
#--------------------------------------------------

###################################################
# Bacterial Correlation with PC1
###################################################
cutoff=0.7
#------------------function: cor.p
cor.p.cols<-function(cols)
{p<-cor.test(Axis1,cols,method="spearman",exact=FALSE)$p.value; p}
#--------------------------------------------------
#------------------function: cor.rho
cor.rho.cols<-function(cols)
{rho<-cor(Axis1,cols,method="spearman"); rho }
#--------------------------------------------------
cor.p.adj<-p.adjust(apply(gmat,2,cor.p.cols),method="bonferroni",ncol(gmat))
cor.rho<-apply(gmat,2,cor.rho.cols)
#--------------------------------------------------
pdf(paste(outpath,"Corr.PC1.",cutoff,".pdf",sep=""),30,15)
par(mfrow = c(4,10))
for(i in 1:ncol(gmat)) {
par(mar = c(4,4,2,2))
if(cor.p.adj[i]<0.2&&cor.rho[i]>cutoff) 
{
plot(Axis1, gmat[,i], xlab="PC1",cex.lab=1.25,cex=4*cor.rho[i],  ylab="Phylogenetic abundance",col="red", pch=20)
scatterutil.sub(colnames(gmat)[i],2,possub ="topleft")
mtext(paste("rho=",round(cor.rho[i],2),"\n p.adj=",round(cor.p.adj[i],2),sep=""),outer=FALSE,line=-3,adj=0.9,col="red")
abline(glm(gmat[,i]~Axis1))
}else if(cor.p.adj[i]<0.2&&cor.rho[i]<(-cutoff)){
plot(Axis1, gmat[,i], xlab="PC1",cex.lab=1.25,cex=4*(-cor.rho[i]),  ylab="Phylogenetic abundance",col="springgreen3", pch=20)
scatterutil.sub(colnames(gmat)[i],2,possub ="topleft")
mtext(paste("rho=",round(cor.rho[i],2),"\n p.adj=",round(cor.p.adj[i],2),sep=""),outer=FALSE,line=-3,adj=0.9,col="springgreen3")
abline(glm(gmat[,i]~Axis1))
}else{
plot(Axis1, gmat[,i], xlab="PC1",cex.lab=1.25, ylab="Phylogenetic abundance",col=rgb(0,0.75,1),pch=20)
scatterutil.sub(colnames(gmat)[i],2,possub ="topleft")
mtext(paste("rho=",round(cor.rho[i],2),"\n p.adj=",round(cor.p.adj[i],2),sep=""),outer=FALSE,line=-3,adj=0.9,col=rgb(0,0.75,1))
}
}
dev.off()
###################################################
# PCA plot based on gradients of all features
###################################################
cor<-cbind(cor.rho,cor.p.adj)
cor.sig<-cor[which(abs(cor[,1])>cutoff & cor[,2]<0.2),]
num_sig<-nrow(cor.sig)
pdf(paste(outpath,"Taxa",num_sig,".pca.gradient.pdf",sep=""),24,12)
par(mfcol = c(3,6))
for(i in (-1):ncol(gmat)) 
{
par(mar=c(5,5,2,2))
#PCA
  if(i==-1){
    par(cex.axis = 2)
    par(cex.lab = 2)  
    new_df <- data.frame(li1 = df.pcoa$li[, 1], li2 = df.pcoa$li[, 2], group = group, Host = Host)
    plot(new_df$li1, new_df$li2, type = "n",
         xlim=c((min(new_df$li1)-0.1*(max(new_df$li1)-min(new_df$li1))),(max(new_df$li1)+0.1*(max(new_df$li1)-min(new_df$li1)))),
         ylim=c((min(new_df$li2)-0.1*(max(new_df$li2)-min(new_df$li2))),(max(new_df$li2)+0.1*(max(new_df$li2)-min(new_df$li2)))),
         xlab = "PC1", ylab = "PC2", main = "")
    points(new_df$li1[new_df$group == "9AM"], new_df$li2[new_df$group == "9AM"], col = "red", pch = 16, cex = 2)
    points(new_df$li1[new_df$group == "5PM"], new_df$li2[new_df$group == "5PM"], col = "blue", pch = 16, cex = 2)
    abline(v = 0)
    abline(h = 0)
    for(h in unique(new_df$Host)){
      h_sub <- new_df[new_df$Host == h,]
      lines(h_sub$li1, h_sub$li2, col = "grey")
    }
    legend("bottomleft", legend = c("9AM", "5PM"), pch = c(16, 16), col = c("red", "blue"), cex = 1.4)
  }else if(i==0){
#clinical outcome
map <- makecmap(clin)
col <- cmap(clin, map = map)
plot(Axis1,Axis2, col="white", bg=col, pch=21,cex=2, type="p", cex.lab=2, cex.axis=2,
     xlim=c((min(Axis1)-0.1*(max(Axis1)-min(Axis1))),(max(Axis1)+0.1*(max(Axis1)-min(Axis1)))), 
     ylim=c((min(Axis2)-0.1*(max(Axis2)-min(Axis2))),(max(Axis2)+0.1*(max(Axis2)-min(Axis2)))), 
     xlab="PC1", ylab="PC2",main=file.opts$clinical.type, cex.main = 2)
hkey(map, title =NA, stretch = 1.5)
mtext(paste("rho=",formatC(cor.test(Axis1,clin,method="spearman")$estimate,digits=2,format="f"),
            "\n p=",formatC(cor.test(Axis1,clin,method="spearman")$p.value,digits=2,format="f"),
            sep=""),outer=FALSE,line=-4,adj=0.9, cex=1.3)     
abline(v=0)
abline(h=0)

}else if(i>0 && cor.p.adj[i]<0.2 && abs(cor.rho[i])>cutoff){
map <- makecmap(gmat[,i])
col <- cmap(gmat[,i], map = map)
plot(Axis1,Axis2, col="white", bg=col, pch=21, type="p",cex=2, cex.lab=2, cex.axis=2,
     xlim=c((min(Axis1)-0.1*(max(Axis1)-min(Axis1))),(max(Axis1)+0.1*(max(Axis1)-min(Axis1)))), 
     ylim=c((min(Axis2)-0.1*(max(Axis2)-min(Axis2))),(max(Axis2)+0.1*(max(Axis2)-min(Axis2)))), 
     xlab="PC1", ylab="PC2",main=colnames(gmat)[i],cex.main = 2)
mtext(paste("rho=",formatC(cor.rho[i],digit=2,format="f"),"\n p.adj=",formatC(cor.p.adj[i],digit=2,format="f"),sep=""),outer=FALSE,line=-4,adj=0.9, cex=1.3)
hkey(map, title =NA, stretch = 1.5)
abline(v=0)
abline(h=0)

}
}
dev.off()
