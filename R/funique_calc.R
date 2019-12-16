##########################################################################
#'  @ Calculate functional uniqueness
#'  @ Created by Marco Girardello 16/12/2019
#'  @ The function takes the following arguments:
#'  @ intrait = dataframe/tibble containing trait data
#'  @ ingeo = dataframe/tibble containing info on geographic restrictedness
#'########################################################################

funique_calc<-function(intrait,ingeo){
  # Distinctiveness
  intrait -> traits2
  reg_pool = matrix(1, ncol = dim(traits2)[1])
  colnames(reg_pool) = traits2$Species
  # distance matrix
  traits3<-as.matrix(traits2[2:11])
  row.names(traits3)<-traits2$Species
  dist_mat = compute_dist_matrix(traits3)
  # distinctivness
  reg_di = distinctiveness(reg_pool,dist_mat)
  as.data.frame(reg_di) %>%
    as_tibble() %>%
    gather(key = "species",value = "dist")->dist
  # uniqueness
  uniqueness(reg_pool,dist_mat) %>%
    as_tibble() %>%
    rename(uniqueness= Ui) %>%
    inner_join(dist) %>%
    inner_join(ingeo) %>%
    gather(key="fproperty",value="value",-c(species)) %>%
    group_by(fproperty) %>%
    mutate(value=scales::rescale(value,to = c(0, 1))) %>%
    spread(key="fproperty",value="value") %>%
    mutate(frar_dist=(dist+rest)/2,frar_uni=(uniqueness+rest)/2) %>%
    dplyr::select(species,dist,uniqueness,rest,frar_dist,frar_uni)->fdmetrics
  return(fdmetrics)
}
