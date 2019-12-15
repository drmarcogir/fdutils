########################################################
#'  @ Calculate convex hull around PCA scores
#'  @ Created by Marco Girardello 15/12/2019
#'  @ The function takes the following arguments:
#'  @ indf=PCA results
#'#####################################################

conv_hull<-function(indf,resp,bufsize =  0.1){
  # fit gam
  as.formula(paste0(resp,"~s(","PC1,PC2)"))->form
  mod<-gam(form,data = resdf)
  b <- getViz(mod)
  plot(sm(b, 1)) + l_fitRaster()+ l_fitContour()->plot
  as_tibble(plot$data$fit) %>%
    dplyr::select(x,y) %>%
    rename(PC1 = x, PC2 = y)  %>%
    mutate(cat = 1:length(PC1))->preddf
  # fit lm
  as.formula(paste0(resp,"~PC1+PC2"))->form1
  mod<-lm(form1,data = resdf)
  preddf %>%
    mutate(fit = predict(mod,preddf)) ->preddf1
  # convex hull from PCA points
  pnts = st_as_sf(resdf, coords = c("PC1", "PC2"))
  pnts1 <- st_convex_hull(st_union(pnts))
  pnts1<-st_buffer(pnts1 , bufsize)
  as_Spatial(pnts1) ->pnts2
  # create raster grid
  preddf1 %>%
    dplyr::select(PC1,PC2,cat) %>%
    rasterFromXYZ()->gridr
  # final result
  tibble(cat=as.numeric(na.exclude(unlist(raster::extract(gridr,pnts2))))) %>%
    inner_join(preddf1)->finalres
  return(finalres)
}
