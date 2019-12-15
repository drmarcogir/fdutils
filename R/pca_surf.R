########################################################
#'  @ CCreate PCA figure with surface for uniqueness
#'  @ Created by Marco Girardello 15/12/2019
#'  @ The function takes the following arguments:
#'  @ indf=PCA results
#'#####################################################

pca_surf<-function(insurf,inax){
  insurf  %>%
    ggplot(.)+
    geom_raster(aes(x=PC1,y= PC2,fill=fit),alpha = 0.7)+
    #geom_point(data=resdf,aes(x=PC1,y=PC2),size = 4, alpha = 0.5)+
    theme_minimal()+scale_fill_distiller(palette = "Spectral",name="Functional Uniqueness")+
    geom_segment(data = inax , aes(x = 0, y = 0, xend = (PC1*9),
    yend = (PC2*9)), arrow = arrow(length = unit(1/2, "picas")),
    color = "black")+annotate("text", x = (axes_scores$PC1*9), y = (axes_scores$PC2*9),
    label = inax$variables,colour = "black",size=5)+theme(axis.text = element_text(size=15,
    colour = "black"), axis.title = element_text(size = 15))+
    geom_vline(xintercept=c(-0,0), linetype="dotted")+
    geom_hline(yintercept=c(-0,0), linetype="dotted")->pca_plot
  return(pca_plot)
}