######################################################################
#'  @ Wrapper function for calculating functional diversity using
#'  @ fdis from FD
#'  @ Created by Marco Girardello 08/12/2019
#'  @ The function takes the following arguments:
#'  @ x=matrix of abundances
#'####################################################################

fdis_main<-function(x,intrait){
  # extract only species found in trait matrix
  x %>%
    dplyr::select(intersect(names(.), c(intrait$Species,"x","y")))->spdat
  # exclude cells with 0 species
  spdat %>%
    gather(key="species",value="pa",-c(x,y)) %>%
    group_by(x,y) %>%
    summarise(sumpa = sum(pa)) %>%
    filter(sumpa > 0) %>%
    dplyr::select(x,y) %>%
    inner_join(spdat)->spdat1
  # convert into data.frame
  spdat1 %>%
    ungroup() %>%
    dplyr::select(-c(x,y)) %>%
    as.data.frame() ->spdat2
  # dissimilarity matrix
  intrait %>%
    filter(Species %in% names(spdat2)) ->tmp
  tmp %>%
  gather(key = "trait_name", value = "trait_value",-c(Species)) %>%
    #inner_join(frisksp) %>%
    group_by(trait_name) %>%
    mutate(trait_value=as.numeric(scale(log(trait_value)))) %>%
    spread(key = "trait_name", value = "trait_value") ->tmp1
  as.matrix(tmp1[2:dim(tmp1)[2]])->traits1
  rownames(traits1)<-tmp$Species
  dist(traits1)->diss
  # calculate functional dispersion
  fdisp(d = diss , a = as.matrix(spdat2))->fdis_pa
  # combine everything together into 1 tibble
  tibble(x=spdat1$x, y = spdat1$y,fdis = fdis_pa$FDis) %>%
    #inner_join(spdat %>% dplyr::select(x,y))->fd_res1
    right_join(spdat %>% dplyr::select(x,y)) %>%
    mutate(fdis = ifelse(is.na(fdis),0,fdis))->fd_res
  # calculate species richness
  spdat %>%
    gather(key = "species",value = "pa",-c(x,y)) %>%
    #filter(pa > 0) %>%
    group_by(x,y) %>%
    summarise(rich = sum(pa)) %>%
    inner_join(fd_res)->finalres
  return(finalres)
}

