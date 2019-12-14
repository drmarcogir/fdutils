#########################################################################################
#'  @ Convert raster file into tibble and name column with file name
#'  @ Created by Marco Girardello 19/11/2019
#'  @ The function takes the following arguments:
#'  @ infiles = paths to the raster files (everything together)
#'  @ binary = whether files containing binary occurrences or probabilities should be used
#'########################################################################################


get_sdm<-function(infiles,binary=NULL){
  # function for converting future rasters
  convert_rast<-function(x){
    stack(future[str_detect(future,x)])->tmpr
    names(tmpr)<-str_remove_all(str_split_fixed(names(tmpr),"deg_",n=2)[,2],"_ensemble|_TSSbin")
    rastertodf(tmpr)->tmpdf
    colnames(tmpdf)<-str_remove_all(names(tmpdf),"value.")
    list(as_tibble(tmpdf))->tmpdf1
    tibble(scenario=x,data=list(as_tibble(tmpdf)))->tmpdf1
    return(tmpdf1)
  }
  # subset binary
  if(binary==TRUE){
    infiles[stringr::str_detect(infiles,"_TSSbin")]->infiles_filt
    # present
    infiles_filt[stringr::str_detect(infiles_filt,"current")]->pres
    stack(pres)->tmp
    rastertodf(tmp)->current
    names(current)<-str_remove_all(names(current),"prob_current_|_ensemble_TSSbin|value.")
    tibble(scenario="current",data=list(as_tibble(current)))->current1
    # future scenarios
    infiles_filt[stringr::str_detect(infiles_filt,"future")]->future
    unique(str_split_fixed(str_remove_all(basename(future),"proj_future_5min_"),"deg",n=2)[,1])->scen.l
    map_dfr(scen.l,convert_rast)->future
    bind_rows(current1,future)->combined_df
    #subset probability
  } else {
    infiles[!stringr::str_detect(infiles,"_TSSbin")]->infiles_filt
    # present
    infiles_filt[stringr::str_detect(infiles_filt,"current")]->pres
    stack(pres)->tmp
    rastertodf(tmp)->current
    names(current)<-str_remove_all(names(current),"prob_current_|_ensemble|value.")
    tibble(scenario="current",data=list(as_tibble(current)))->current1
    # future scenarios
    infiles_filt[stringr::str_detect(infiles_filt,"future")]->future
    unique(str_split_fixed(str_remove_all(basename(future),"proj_future_5min_"),"deg",n=2)[,1])->scen.l
    map_dfr(scen.l,convert_rast)->future
    bind_rows(current1,future)->combined_df
  }
  return(combined_df)
}

