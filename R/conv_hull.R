########################################################
#'  @ Calculate convex hull around PCA scores
#'  @ Created by Marco Girardello 15/12/2019
#'  @ The function takes the following arguments:
#'  @ indf=PCA results
#'#####################################################

conv_hull<-function(indf,resp,bufsize =  0.1){
  # create prediction dataframe
  indf[,c("PC1","PC2")]->tmp
  raster(xmn=min(tmp$PC1),xmx=max(tmp$PC1),
         ymn=min(tmp$PC2),ymx=max(tmp$PC2),res=0.25)->tmpr
  as_tibble(coordinates(tmpr)) %>%
    mutate(cat = 1:length(x)) %>% {.->>tmp1} %>%
    rasterFromXYZ()->gridr
  # fit lm
  as.formula(paste0(resp,"~PC1+PC2"))->form1
  mod<-lm(form1,data = resdf)
  tmp1%>%
    rename(PC1 =x, PC2 =y) -> preddf
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
