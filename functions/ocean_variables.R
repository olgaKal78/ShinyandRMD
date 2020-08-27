
##################################################################################################
#######This function aggregates variables ##################
#######from different data sources##########################
############################################################
#############################################################
##################################################################################################


#' @param sps area of interest as spatial polyon class
#' @param month
#' @param year
#' @param data_type "Bathymetry","North Atlantic Ocean Monthly Model Means","Copernicus Ocean Physics" or "Copernicus Ocean BioGeoChemistry"
#' @param parameter 
#'                     ##Specific to "Seabed Habitats Mapping around Ireland" variables##
#'  either                   
  #' "Biozone" 
  #' "Substate"
  #' "Folk_5"
  #' "EUNIS"
  #' "MSFD_BBHT"
#'                    ##Specific to "Bathymetry" variables##
#' 
  #' "elevation"  	Elevation relative to sea level
#' 
#'                    ##Specific to "North Atlantic Ocean Monthly Model Means" variables
#' 
#' either
  #' "sea_water_temperature"  	Monthly mean sea surface temperature
  #' "sea_bottom_temperature" 	Monthly mean sea bottom temperature	
  #' "sea_surface_salinity"   	Monthly mean sea surface salinity	
  #' "sea_bottom_salinity"     	Monthly mean sea bottom salinity	
  #' "sea_surface_x_velocity"	  Monthly mean surface U velocity component	
  #' "sea_surface_y_velocity" 	Monthly mean surface V velocity component	
  #' "significant_wave_height"	Monthly mean significant wave height
  #' "mean_wave_direction"    	Monthly mean mean wave direction	
  #' "mean_wave_period"       	Monthly mean absolute zero-crossing period

#'                   Specific to "Copernicus Ocean Physics" variables
#'                   
#' either
      # "thetao"   Temperature
      # "bottomT"  Sea floor potential temperature
      # "so"       Salinity 
      # "zos"      Sea surface height
      # "uo"       Eastward velocity 
      # "vo"       Northward velocity 
      # "mlotst"   Ocean mixed layer thickness defined by density 

#'                 Specific to "Copernicus Ocean BioGeoChemistry" variables
#'                 
#' either
      # "chl"	    Mass Concentration of Chlorophyll in Sea Water
      # "phyc" 	  Mole Concentration of Phytoplankton Expressed as Carbon in Sea Water
      # "o2"  	  Mole Concentration of Dissolved Oxygen in Sea Water
      # "no3"	    Mole Concentration of Nitrate in Sea Water
      # "po4"	    Mole Concentration of Phosphate in Sea Water
      # "si"	    Mole Concentration of Silicate in Sea Water
      # "fe"	    Mole Concentration of Iron in Sea Water
      # "nh4"	    Mole Concentration of Ammonium in Sea Water
      # "nppv"	  Net Primary Productivity of Carbon
      # "zeu"	    Euphotic Zone Depth

#'                 Specific to "An operational zooplankton" variables
#'                 
#' either
#'   "Acartia",
#'  "Calanus_finmarchicus
#'  "Calanus_helgolandicus"
#'  "Metridia_lucens"
#'  "Temora_longicornis"
#'  "Large_copepods"
#'  "Small_copepods"

#'  @param max_rel_error 0, 0.3 or 0.5 if data_type = An operational zooplankton"


##########################################################################################################
##########################################################################################################







ocean_variables<-function(sps,year,month,max_rel_error=NULL,
                          data_type=c("Seabed Habitats Mapping around Ireland",
                                      "Bathymetry",
                                      "North Atlantic Ocean Monthly Model Means",
                                      "Copernicus Ocean Physics",
                                      "Copernicus Ocean BioGeoChemistry",
                                      "An operational zooplankton"),
                          parameter){

###set results location##
plot.path <-"results/plots/"
data.path<-"results/data/"


#####################Reading in data ##############################

if(data_type=="Seabed Habitats Mapping around Ireland"){
  
  load("Data/sb2019.RData")
  month=NULL
  year= NULL
  
}
else if(data_type=="Bathymetry"){

  load("Data/gebco_2020.RData")
  month=NULL
  year=NULL
  parameter=NULL
  coll<-rerddap::colors$salinity
}
else if(data_type=="North Atlantic Ocean Monthly Model Means"){
  full_names<-read.csv('Data/ROMS.csv')
 load(paste0("Data/",parameter,".RData"))
 
 
}
 else if(data_type=="Copernicus Ocean Physics"){

   load("Data/copernicus_phys_monthly.RData")
   full_names<-read.csv('Data/PhysDesc.csv')
 }

 else if(data_type=="Copernicus Ocean BioGeoChemistry"){
  
   load("Data/copernicus_bio_monthly.RData")
  full_names<-read.csv('Data/BioChemDesc.csv')
 }
else if(data_type=="An operational zooplankton"){
 load("Data/Zoo.RData")
 full_names<-read.csv('Data/BioChemDesc.csv')
}



if(data_type=="Seabed Habitats Mapping around Ireland"){

  ### Conver Selected spatial polygon to class sf
  a = st_as_sf(sps)
  ###Subset Seabed habitat by  selected spatial polygon to class sf
  cc<-st_intersection(st_make_valid(sb), a)
  ###Get summary for each selected parameter  
  aKM2<-units::set_units(st_area(cc), km^2)
  cc_df <- cc %>% st_set_geometry(NULL)
  
  cc_df$area_km2<-as.numeric(aKM2)
  for(i in 1:length(parameter)){
    var<-parameter[i]
    tab<-cc_df %>%
      group_by(!!sym(var)) %>%
      dplyr::summarize(area_km2 = round(sum(area_km2),0)) %>%
      mutate(perc = paste0(round(100 * area_km2 / sum(area_km2), 1), "%"))
    
    #summary
    
    kable(tab)%>%kable_styling()%>%
      print()
    
    
    
  }
  #save subsetted shapefile
  st_write(cc,"results/data/spatial/Sea_bed_subset.shp")  
  
}
else if(data_type=="Bathymetry"){
  ####################################################################
  # ##############Spatial plot defining color scheme###################
  #raster <- raster(ncpath)
  raster=gebco_2020
  masked<-mask(x=raster,mask=sps)
  slice<-crop(x=masked,y=extent(sps))
  writeRaster(slice, filename=paste0(data.path,"spatial/",data_type,".grd"), bandorder='BIL', overwrite=TRUE)
  
  print(levelplot(slice,contour=TRUE,col.regions=coll,main="GEBCO_Bathymetry"))
  
  png(filename =paste0(plot.path,data_type,"_Spatial.png"))
  print(levelplot(slice,contour=TRUE,col.regions=coll,main="GEBCO_Bathymetry"))
  whatever <-dev.off()
   
    
}

 
else{
  
  if(data_type=="North Atlantic Ocean Monthly Model Means" & parameter %in% c("sea_surface_temperature","sea_bottom_temperature")){
    coll<-rerddap::colors$temperature
  }
  if(data_type=="North Atlantic Ocean Monthly Model Means" & parameter %in% c("sea_surface_salinity","sea_bottom_salinity")){
    coll<-rerddap::colors$salinity
  }
  if(data_type=="North Atlantic Ocean Monthly Model Means" & parameter %in% c("sea_surface_x_velocity","sea_surface_y_velocity")){
    coll<-rerddap::colors$velocity
  }
  if(data_type=="North Atlantic Ocean Monthly Model Means" & parameter %in% c("mixed_layer_depth", "significant_wave_height", "mean_wave_direction", "mean_wave_period" )){
    coll<-rerddap::colors$density
  }
  if(data_type=="Copernicus Ocean Physics" & parameter %in% c("thetao","bottomT")){
    coll<-rerddap::colors$temperature
  }
  if(data_type=="Copernicus Ocean Physics" & parameter %in% c("so")){
    coll<-rerddap::colors$salinity
  }
  if(data_type=="Copernicus Ocean Physics" & parameter %in% c("uo","vo")){
    coll<-rerddap::colors$velocity
  }
  if(data_type=="Copernicus Ocean Physics" & parameter %in% c("zos","mlotst")){
    coll<-rerddap::colors$density
  }
  
  if(data_type=="Copernicus Ocean BioGeoChemistry" & parameter %in% c("chl","phyc")){
    coll<-rerddap::colors$chlorophyll
  }
  if(data_type=="Copernicus Ocean BioGeoChemistry" & parameter %in% c("o2")){
    coll<-rerddap::colors$oxygen}
  
  if(data_type=="Copernicus Ocean BioGeoChemistry" & parameter %in% c("no3","po4","si","fe","nh4")){
    coll<-rerddap::colors$vorticity
    
  }
  if(data_type=="Copernicus Ocean BioGeoChemistry" & parameter %in% c("nppv","zeu")){
    coll<-rerddap::colors$vorticity
    
  }
  if(data_type=="An operational zooplankton" ){
    coll<-rerddap::colors$chlorophyll
  }
  year<-as.character(year)
  if(data_type=="An operational zooplankton"){
  m<-c("Q1","Q2","Q3","Q4")
  }
  else{
  m<-c("January","February","March","April","May","June","July",
       "August","September","October","November","December")}
  
  if(data_type=="North Atlantic Ocean Monthly Model Means"){
    y<-2013:2019}
  else if(data_type=="An operational zooplankton"){
    y<-1958:2013}
  else if(data_type=="Copernicus Ocean Physics"){
    y<-1993:2019 
  }
  else{y<-1992:2019}
  
  ####location matrix in rasterbrick month by year####
  lev<-matrix(nrow = length(m),ncol=length(y))
  for(i in 2:length(m)){
    lev[1,]<-seq(1,length(m)*length(y)-(length(m)-1),by=length(m))
    lev[i,]<-seq(1+(i-1),length(m)*length(y)-(length(m)-1)+(i-1),by=length(m))}
  rownames(lev)<-m
  colnames(lev)<-y
  ######Setting names for raster layers### 
  h<-list()
  for(i in 1:length(y)){
    h[[i]]<-paste0(substring(m,1,3),y[i])}
  h_names<-unlist(h) 
   
  #######################read in raster brick#################################################
  
 
  if(data_type!="An operational zooplankton"){
   
  NN<-filter(full_names,Name==parameter)[1,2]##ylab for ggplot  
  raster<-eval(sym(parameter))
  names(raster)<-h_names
  masked<-mask(x=raster,mask=sps)
  slice<-crop(x=masked[[1:(length(y)*length(m))]],y=extent(sps))
  
  }
  else{
    NN<-paste(parameter,"log abundance")
    raster<-eval(sym(paste0(parameter,"_Ab_",max_rel_error)))
    names(raster)<-h_names
    masked<-mask(x=raster,mask=sps)
    slice<-crop(x=masked,y=extent(sps)) 
  }
  
 
  
  ########################combine summurized data#########
  all_data<-list()
  data<-list()
  for(k in 1:length(month)){
    for(i in 1:length(month)){
      f<-list()
      for(j in 1:length(year)){
        d <-subset(slice,lev[month[i],year[j]])
        
        mean<-as.data.frame(cellStats(d, stat='mean', na.rm=TRUE))
        rownames(mean)<-1:dim(mean)[1]
        names(mean)<-"mean"
        sd<-as.data.frame(cellStats(d, stat='sd', na.rm=TRUE))
        rownames(sd)<-1:dim(sd)[1]
        names(sd)<-"sd"
        d1<-cbind(mean,sd)
        d1$sd_low<-d1$mean-d1$sd
        d1$sd_high<-d1$mean+d1$sd
        d1$month<-month[i]
        d1$year<-year[j]
        d1$parameter<-parameter
        d1$data_source<-data_type
        f[[j]]<-d1
      }
      data[[i]]<-f
    }
    all_data[[k]]<-ldply(data[[k]])}
  combined<-ldply(all_data)
  combined$month <- factor(combined$month, levels = month)
  ##################save to result folder############################
  write.csv(combined,paste0(data.path,parameter,".csv"),row.names=F)
  
  
  dd<-filter(combined,parameter==parameter)
 
  t<-na.omit(dd) %>%
    group_by(month) %>%
    dplyr::summarize(mm =mean(mean)) 
  ddJ<-left_join(dd,t)
  ddJ$diff<-ddJ$mm-ddJ$mean
  ddJ$cat<-NA
  ddJ$cat[which(ddJ$diff<0)]<-"Incraese"
  ddJ$cat[which(ddJ$diff>0)]<-"Decrease"
  ddJ$cat[which(ddJ$diff==0)]<-"No changes"
  ddJ$id<-as.numeric(as.factor(ddJ$year))
  
  
  
  
  ####1. Time Series Plot
  
  
  temp_plt<-ggplot(data = dd, aes(x = year, y = mean)) +
    ylab(paste("Mean",NN))+
    xlab("Year") +
    # xlim(xmin, NA) +
    geom_line(aes(group=1)) +
    geom_errorbar(aes(ymin = sd_low, ymax = sd_high), 
                  size = 0.3, width = 0.3) +
    geom_point()+
    facet_wrap( ~month, nrow = 4, scales = "fixed") +
    theme_bw() +
    theme(strip.background = element_blank())
  
  ggsave(paste0(plot.path,parameter,".png"))
  
  
  print(temp_plt)
  
  for(i in 1:length(month)){
    m<-month[i]
    
    pp<-ddJ%>%filter(month==m)%>%
      ggplot(aes(x=year,xmin = id - 0.5, 
                 xmax = id+0.5,
                 ymin = mean, ymax = mm, fill = cat)) +
      ylab(paste("Mean",NN))+
      geom_rect(col = "black") +facet_wrap(~month)+
      
      scale_fill_brewer(palette = "Set1",name="",direction = -1)
    pp<-pp+ggtitle(paste("Difference to the",length(year),"years mean(",first(year),"-",last(year),") in", m))
    ggsave(paste0(plot.path,parameter,m,"_anomalies.png"))
    print(pp)}
  
  
  ddd<-subset(slice,lev[month,year])
  
  writeRaster(ddd, filename=paste0(data.path,"spatial/",parameter,"_",data_type,".grd"), bandorder='BIL', overwrite=TRUE)
  
 ######################################################################################################################
 if(data_type!="An operational zooplankton"){  
  for(i in 1:length(month)){
    
    #####2. Violin Plot
    viol<-bwplot(ddd[[seq(i,(length(year)*length(month)),by=length(month))]],
                 ylab=paste(filter(full_names,Name==parameter)[1,2], "in",month[i]))
    print(viol)
    
    #####Save bwplot
    png(filename =paste0(plot.path,data_type,"_",parameter,month[i],"_violin.png"))
    print(bwplot(ddd[[seq(i,(length(year)*length(month)),by=length(month))]],
                 ylab=paste(filter(full_names,Name==parameter)[1,2], "in",month[i])))
    
    whatever <-dev.off()
  }
  
  
  
  
  
  for(i in 1:length(month)){
    #####3. Spatial Plot
    lev<-levelplot(ddd[[seq(i,(length(year)*length(month)),by=length(month))]],
                   contour=TRUE,col.regions=coll ,main=paste(filter(full_names,Name==parameter)[1,2], "in",month[i]))
    print(lev)
    
    ###save level plot
    png(filename =paste0(plot.path,data_type,"_",parameter,month[i],"_Spatial.png"))
    print(levelplot(ddd[[seq(i,(length(year)*length(month)),by=length(month))]],
                    contour=TRUE,col.regions=coll ,
                    main=paste(filter(full_names,Name==parameter)[1,2], "in",month[i])))
    whatever <-dev.off()}
  
  
  }  
  
}

 

}


