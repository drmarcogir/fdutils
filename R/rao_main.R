######################################################################
#'  @ Wrapper function for calculating functional diversity using
#'  @ Rao's Q decomposition
#'  @ Created by Marco Girardello 20/11/2019
#'  @ The function takes the following arguments:
#'  @ x=matrix of abundances
#'####################################################################

raowrap<-function(x){
  # final tibble
  tibble(alphaTD=x$TD$Alpha,alphaFD=x$FD$Alpha)->df1
  return(df1)
}

dfcreate<-function(x){
  return(data.frame(x$prob,row.names=x$species))
}

rao_main<-function(x,indist){
  #pb$tick()$print()
  # sp data - check if consistent with traits
  x %>%
    dplyr::select(intersect(names(.), c(traits$Species,"x","y")))->spdat
  # number of cores to be used
  #plan(multiprocess,workers=6)
  #plan(multisession)
  spdat %>%
    # create a site list and save it as intermediate output
    mutate(siteno=paste0("s",1:length(x))) %>% {.->>coord} %>%
    # exclude x and y columns
    dplyr::select(-c(x,y)) %>%
    gather(key = "species",value="prob",-c(siteno)) %>%
    group_by(siteno) %>%
    nest()->spdat1
    spdat1 %>%
      mutate(data1 = map(data,dfcreate)) %>%
      mutate(rao_object = map(data1,~Rao(sample=.,dfunc = indist,Jost = T,dphyl=NULL, weight=F,structure=NULL))) %>%
      mutate(rao_res = map(rao_object,raowrap)) %>%
      dplyr::select(-c(data,data1)) %>%
      unnest(cols = rao_res) %>%
      # join with tibble containing coordinates
     inner_join(coord %>% dplyr::select(x,y,siteno)) ->divres
  return(divres)
}



