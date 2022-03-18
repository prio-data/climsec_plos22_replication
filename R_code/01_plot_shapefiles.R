###
### Plots of Shapefiles for Paper
###
### Authors: Claire Kelling
### Created: 12/20/2019
### Last Modified: 11/30/2021
###

#Clear the deck
rm(list=ls())

#Libraries
#devtools::install_github("dkahle/ggmap")
library(ggmap)
library(ggplot2)
library(rgdal)

#set working directory to current path
setwd(dirname(rstudioapi::getActiveDocumentContext()$path))

#plotting function
adm_plot_function2 <- function(left, right, top, bottom, country, name_of_shapefile){
  p <- ggmap(get_map(c(left = left, right = right, top = top, bottom= bottom), source = "stamen"))
  p

  adm <- readOGR(dsn="./inputs",layer=name_of_shapefile)
  adm <- spTransform(adm, "+init=epsg:4326")
  
  sp_f <- fortify(adm)
  adm_plot <- p + geom_polygon(data=sp_f,aes(long,lat, group = group), 
                               fill = NA, col = "black")
}


#################################
#### Nepal
#################################
########### ADM1
nepal_adm1_plot <- adm_plot_function2(left = 79.7, right = 88.8, top = 32.5, bottom= 25, 
                                country = "Nepal", name_of_shapefile ="Nepal_admin1" )
nepal_adm1_plot

setEPS()
postscript("./plot_nepal_adm1_b.eps")
nepal_adm1_plot
dev.off()

nepal_adm2_plot <- adm_plot_function2(left = 79.7, right = 88.8, top = 32.5, bottom= 25,
                                     country = "Nepal", name_of_shapefile ="Nepal_admin2" )
nepal_adm2_plot

setEPS()
postscript("./plot_nepal_adm2_b.eps")
nepal_adm2_plot
dev.off()

nepal_pg_plot <- adm_plot_function2(left = 79.7, right = 88.8, top = 32.5, bottom= 25,
                                     country = "Nepal", name_of_shapefile ="Nepal_pg05" )
nepal_pg_plot

setEPS()
postscript("./plot_nepal_pg05_b.eps")
nepal_pg_plot
dev.off()

nepal_pg_025_plot <- adm_plot_function2(left = 79.7, right = 88.8, top = 32.5, bottom= 25,
                                  country = "Nepal", name_of_shapefile ="Nepal_pg025" )
nepal_pg_025_plot

setEPS()
postscript("./plot_nepal_pg025_b.eps")
nepal_pg_025_plot
dev.off()


#######
####### Pakistan
#######

Pakistan_adm1_plot <- adm_plot_function2(left = 60, right = 80, top = 38, bottom= 23,
                                     country = "Pakistan", name_of_shapefile ="Pakistan_admin1" )

Pakistan_adm1_plot

setEPS()
postscript("./plot_Pakistan_adm1_b.eps")
Pakistan_adm1_plot
dev.off()

Pakistan_adm2_plot <- adm_plot_function2(left = 60, right = 80, top = 38, bottom= 23, 
                                     country = "Pakistan", name_of_shapefile ="Pakistan_admin2" )

Pakistan_adm2_plot
setEPS()
postscript("./plot_Pakistan_adm2_b.eps")
Pakistan_adm2_plot
dev.off()

Pakistan_pg_plot <- adm_plot_function2(left = 60, right = 80, top = 38, bottom= 23,
                                  country = "Pakistan", name_of_shapefile ="Pakistan_pg05" )

Pakistan_pg_plot
setEPS()
postscript("./plot_Pakistan_pg05_b.eps")
Pakistan_pg_plot
dev.off()

Pakistan_pg025_plot <- adm_plot_function2(left = 60, right = 80, top = 38, bottom= 23,
                                      country = "Pakistan", name_of_shapefile ="Pakistan_pg025" )

Pakistan_pg025_plot
setEPS()
postscript("./plot_Pakistan_pg025_b.eps")
Pakistan_pg025_plot
dev.off()

#######
####### Iraq
#######
Iraq_adm1_plot <- adm_plot_function2(left = 38, right = 49, top = 38, bottom= 28,
                                        country = "Iraq", name_of_shapefile ="Iraq_admin1" )

Iraq_adm1_plot
setEPS()
postscript("./plot_Iraq_adm1_b.eps")
Iraq_adm1_plot
dev.off()

Iraq_adm2_plot <- adm_plot_function2(left = 38, right = 49, top = 38, bottom= 28,
                                        country = "Iraq", name_of_shapefile ="Iraq_admin2" )

setEPS()
postscript("./plot_Iraq_adm2_b.eps")
Iraq_adm2_plot
dev.off()

Iraq_pg_plot <- adm_plot_function2(left = 38, right = 49, top = 38, bottom= 28,
                                     country = "Iraq", name_of_shapefile ="Iraq_pg05" )

setEPS()
postscript("./plot_Iraq_pg05_b.eps")
Iraq_pg_plot
dev.off()

Iraq_pg_025_plot <- adm_plot_function2(left = 38, right = 49, top = 38, bottom= 28, 
                                  country = "Iraq", name_of_shapefile ="Iraq_pg025" )

setEPS()
postscript("./plot_Iraq_pg025_b.eps")
Iraq_pg_025_plot
dev.off()



#######
####### Turkey
#######
Turkey_adm1_plot <- adm_plot_function2(left = 25, right = 47, top = 44, bottom= 34, 
                                    country = "Turkey", name_of_shapefile ="Turkey_admin1" )

setEPS()
postscript("./plot_Turkey_adm1_b.eps")
Turkey_adm1_plot
dev.off()

Turkey_adm2_plot <- adm_plot_function2(left = 25, right = 47, top = 44, bottom= 34, 
                                    country = "Turkey", name_of_shapefile ="Turkey_admin2" )

setEPS()
postscript("./plot_Turkey_adm2_b.eps")
Turkey_adm2_plot
dev.off()

Turkey_pg_plot <- adm_plot_function2(left = 25, right = 47, top = 44, bottom= 34,  
                                  country = "Turkey", name_of_shapefile ="Turkey_pg05" )

setEPS()
postscript("./plot_Turkey_pg05_b.eps")
Turkey_pg_plot
dev.off()

Turkey_pg_025_plot <- adm_plot_function2(left = 25, right = 47, top = 44, bottom= 34,  
                                      country = "Turkey", name_of_shapefile ="Turkey_pg025" )

setEPS()
postscript("./plot_Turkey_pg025_b.eps")
Turkey_pg_025_plot
dev.off()


#######
####### Somalia
#######
Somalia_adm1_plot <- adm_plot_function2(left = 40, right = 52, top = 12.1, bottom= -2.1, 
                                      country = "Somalia", name_of_shapefile ="Somalia_admin1" )

setEPS()
postscript("./plot_Somalia_adm1_b.eps")
Somalia_adm1_plot
dev.off()

Somalia_adm2_plot <- adm_plot_function2(left = 40, right = 52, top = 12.1, bottom= -2.1,
                                      country = "Somalia", name_of_shapefile ="Somalia_admin2" )

setEPS()
postscript("./plot_Somalia_adm2_b.eps")
Somalia_adm2_plot
dev.off()

Somalia_pg_plot <- adm_plot_function2(left = 40, right = 52, top = 12.1, bottom= -2.1,
                                    country = "Somalia", name_of_shapefile ="Somalia_pg05" )

setEPS()
postscript("./plot_Somalia_pg05_b.eps")
Somalia_pg_plot
dev.off()

Somalia_pg_025_plot <- adm_plot_function2(left = 40, right = 52, top = 12.1, bottom= -2.1,
                                        country = "Somalia", name_of_shapefile ="Somalia_pg025" )

setEPS()
postscript("./plot_Somalia_pg025_b.eps")
Somalia_pg_025_plot
dev.off()

#######
####### Colombia
#######
Colombia_adm1_plot <- adm_plot_function2(left = -80, right = -65, top = 13, bottom= -5,
                                      country = "Colombia", name_of_shapefile ="Colombia_admin1" )

setEPS()
postscript("./plot_Colombia_adm1_b.eps")
Colombia_adm1_plot
dev.off()

Colombia_adm2_plot <- adm_plot_function2(left = -80, right = -65, top = 13, bottom= -5,
                                      country = "Colombia", name_of_shapefile ="Colombia_admin2" )

setEPS()
postscript("./plot_Colombia_adm2_b.eps")
Colombia_adm2_plot
dev.off()

Colombia_pg_plot <- adm_plot_function2(left = -80, right = -65, top = 13, bottom= -5,
                                    country = "Colombia", name_of_shapefile ="Colombia_pg05" )

setEPS()
postscript("./plot_Colombia_pg05_b.eps")
Colombia_pg_plot
dev.off()

Colombia_pg_025_plot <- adm_plot_function2(left = -80, right = -65, top = 13, bottom= -5,
                                        country = "Colombia", name_of_shapefile ="Colombia_pg025" )

setEPS()
postscript("./plot_Colombia_pg025_b.eps")
Colombia_pg_025_plot
dev.off()


#######
####### Afghanistan
#######
Afghanistan_adm1_plot <- adm_plot_function2(left = 60, right = 75.5, top = 39, bottom= 28, 
                                        country = "Afghanistan", name_of_shapefile ="Afghanistan_admin1" )

setEPS()
postscript("./plot_Afghanistan_adm1_b.eps")
Afghanistan_adm1_plot
dev.off()

Afghanistan_adm2_plot <- adm_plot_function2(left = 60, right = 75.5, top = 39, bottom= 28,  
                                        country = "Afghanistan", name_of_shapefile ="Afghanistan_admin2" )

setEPS()
postscript("./plot_Afghanistan_adm2_b.eps")
Afghanistan_adm2_plot
dev.off()

Afghanistan_pg_plot <- adm_plot_function2(left = 60, right = 75.5, top = 39, bottom= 28,  
                                      country = "Afghanistan", name_of_shapefile ="Afghanistan_pg05" )

setEPS()
postscript("./plot_Afghanistan_pg05_b.eps")
Afghanistan_pg_plot
dev.off()

Afghanistan_pg_025_plot <- adm_plot_function2(left = 60, right = 75.5, top = 39, bottom= 28,  
                                          country = "Afghanistan", name_of_shapefile ="Afghanistan_pg025" )

setEPS()
postscript("./plot_Afghanistan_pg025_b.eps")
Afghanistan_pg_025_plot
dev.off()

