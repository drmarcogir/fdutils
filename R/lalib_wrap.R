spabmat[1,]$data[[1]]->x
sppamat[1,]$data[[1]]->x

function(x){
  # trait data - check if consistent with sp matrix
  traits %>%
    dplyr::filter(Species %in% names(x))->traits1
  as.matrix(traits1[2:11])->traits2
  row.names(traits2)<-traits1$Species
  # abundance
  x %>% 
  
  # presence absence
  x %>%
    dplyr::select(intersect(names(.), c(traits1$Species,"x","y"))) %>%
    mutate(siteno=paste0("s",1:length(x))) %>% {.->> spdat}%>%
    gather(key = "species",value="value",-c(siteno,x,y)) %>%
    group_by(siteno) %>%
    summarise(count=sum(value)) %>%
    filter(count > 0) %>%
    dplyr::select(siteno) %>%
    inner_join(spdat) %>%
    dplyr::select(-c(siteno)) ->spdat1
  
  as.matrix(spdat1)->spdat2
  # check before starting computation
  row.names(traits2)==names(spdat1[3:100])
  
  diss<-cluster::daisy(traits2, metric="euclidean",stand=TRUE)
  
  dbFD(x=traits2,a=spdat[3:100],stand.x=TRUE,corr="cailliez",w.abun=TRUE)->fd
  
  fdisp(d=diss,a=spdat2[,3:100])->fd
  
}