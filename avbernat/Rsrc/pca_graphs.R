library(cowplot)    # ggplot helper functions to arrange multi-panel figures
library(FactoMineR) # multivariate methods such as principal component analysis (PCA)
library(factoextra) # PCA helper functions

PCA_graphs <- function(dataset, PCA_title){
  # cos2 and the alpha.var: alpha.var colours variables by cos2 
  # (importance of most important PC to variable), 
  # see https://personal.utdallas.edu/~herve/abdi-awPCA2010.pdf
  
  GFpca <- PCA(dataset, scale.unit = TRUE, graph = FALSE, ncp = 10)
  
  eig.val <- get_eigenvalue(GFpca)
  var.val <- GFpca$var
  # print(eig.val) #will only show in in console
  # print(var.val)
  
  scree <- fviz_eig(GFpca, addlabels = TRUE, ylim = c(0, 100))
  # print(scree)
  
  labX <- paste("PC1 (", Round(eig.val[1, 2]), "%)", sep = "")
  labY <- paste("PC1 (", Round(eig.val[2, 2]), "%)", sep = "")
  leplot <- fviz_pca_biplot(GFpca, geom.id = c("point"), 
                            geom.var = c("arrow", "text"), 
                            # alpha.var = "cos2",
                            label = "var", repel = T, 
                            col.ind = "gray", col.var = "black")
  # print(leplot)
  
  p = ggpubr::ggpar(leplot, title = PCA_title, xlab = labX, ylab = labY, 
                ggtheme = theme_classic(), font.main = c(20, "bold"), 
                font.x = 14, font.y = 14, font.tickslab = 12
  )
  
  # D = cor(dataset)
  # test <- cor.mtest(dataset)$p
  # par(mfrow=c(1,2))
  # corrplot.mixed(D,lower.col = "black", number.cex = .7, p.mat=test, sig.level=0.05)
  # corrplot.mixed(D,lower.col = "black", number.cex = .7)
  # 
  return(p)
}

Round <- function(number){
  # for plotting
  x <- round(number, 1)
  if(x%%1 == 0){
    return(paste(as.character(x), ".0", sep = ""))
  }
  else{
    return(x)
  }
}