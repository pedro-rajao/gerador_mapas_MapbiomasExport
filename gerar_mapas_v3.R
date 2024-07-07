library(ggplot2)
library(sf)
library(dplyr)
library(terra)
library(stringr)
library(raster)
library(rgdal)
library(cowplot)
library(lattice)

# Carregar os três shapefiles
shapefile_limites2 <- readOGR(file.choose())
shapefile_restauracao <- readOGR(file.choose())
shapefile_conservacao <- readOGR(file.choose())
shapefile_restauracao_2023a <- readOGR(file.choose())


## Loading TIFFs
r2008 <- rast(file.choose())
r2009 <- rast(file.choose())
r2010 <- rast(file.choose())
r2011 <- rast(file.choose())
r2012 <- rast(file.choose())
r2014 <- rast(file.choose())
r2015 <- rast(file.choose())
r2017 <- rast(file.choose())
r2019 <- rast(file.choose())
r2021 <- rast(file.choose())

# Obter o sistema de coordenadas do raster
crs_raster <- crs(r2021)

# Reprojetar os shapefiles para a projeção do raster (UTM)
shapefile_limites2 <- spTransform(shapefile_limites2, crs_raster)
shapefile_restauracao <- spTransform(shapefile_restauracao, crs_raster)
shapefile_conservacao <- spTransform(shapefile_conservacao, crs_raster)
shapefile_restauracao_2023 <- spTransform(shapefile_restauracao_2023a, crs_raster)


# shapefile to vector
shapefile_limites <- vect(shapefile_limites2)
shapefile_linha_vermelha <- vect(shapefile_restauracao)
shapefile_linha_azul <- vect(shapefile_conservacao)
shapefile_linha_vermelha_2023 <- vect(shapefile_restauracao_2023)

View(shapefile_limites2)

####################################################
######## 
df_raster <- as.data.frame(r2021, xy=TRUE)
shapefile_limites_sf <- st_as_sf(shapefile_limites)
cores <- c("0"="white", "3" = "#006400", "9" = "#935132", "11" = "#45c2a5", "15" = '#ffd966', 
           "21" = "#fff3bf", "33" = "#0000ff")

print(
  ggplot() +
    geom_raster(aes(x = x, y = y, fill = factor(classification_2021)), data = df_raster) +
    geom_sf(data = shapefile_limites_sf, color = "#000000", fill = "transparent", lwd = 0.5) +
    scale_fill_manual(values = cores) +
    theme_void() +
    cowplot::theme_cowplot() +
    scale_y_continuous(
      breaks = seq(floor(min(df_raster$y) * 10) / 10, ceiling(max(df_raster$y) * 10) / 10, by = 0.1),
      labels = function(y) round(y, 2)
    ) +
    scale_x_continuous(
      breaks = seq(floor(min(df_raster$x) * 10) / 10, ceiling(max(df_raster$x) * 10) / 10, by = 0.1),
      labels = function(x) round(x, 2)
    ) +
    xlab("latitude (ºS)") +
    ylab("longitude (ºW)") +
    guides(fill = "none") +
    theme(axis.title = element_text(size = 10))  # Definir o tamanho da fonte do título dos eixos
)

ggsave("C:/Users/pedro.rajao/Desktop/AGEVAP/10. quantificação C-GEE PAF/1. LANDSAT-mapbiomas/rio claro/maps/2010.LULC.png", dpi = 300)



#################################################
##### maps with buffer
buffered_shapefile <- sf::st_buffer(shapefile_limites_sf, dist = 10000)  
df_raster <- as.data.frame(r2021, xy=TRUE)
sf_raster <- st_as_sf(df_raster, coords = c("x", "y"), crs = st_crs(shapefile_limites_sf))

sf_raster_clipped <- st_intersection(sf_raster, buffered_shapefile)

df_raster_clipped <- as.data.frame(sf_raster_clipped)

print(
  ggplot() +
    geom_raster(aes(x = x, y = y, fill = factor(classification_2021)), data = df_raster_buffer) +
    geom_sf(data = shapefile_limites_sf, color = "#000000", fill = "transparent", lwd = 0.5) +
    scale_fill_manual(values = cores) +
    theme_void() +
    cowplot::theme_cowplot() +
    scale_y_continuous(
      breaks = seq(floor(min(df_raster_buffer$y) * 10) / 10, ceiling(max(df_raster_buffer$y) * 10) / 10, by = 0.1),
      labels = function(y) round(y, 2)
    ) +
    scale_x_continuous(
      breaks = seq(floor(min(df_raster_buffer$x) * 10) / 10, ceiling(max(df_raster_buffer$x) * 10) / 10, by = 0.1),
      labels = function(x) round(x, 2)
    ) +
    xlab("latitude (ºS)") +
    ylab("longitude (ºW)") +
    guides(fill = "none") +
    theme(axis.title = element_text(size = 10))  # Definir o tamanho da fonte do título dos eixos
)

ggsave("C:/Users/pedro.rajao/Desktop/AGEVAP/10. quantificação C-GEE PAF/1. LANDSAT-mapbiomas/rio claro/maps/2010.LULC_buffer.png", dpi = 300)






#############################################################################
###### map generator with manual selection properties
shapefile_PROP <- shapefile_limites[shapefile_limites$NOME_PROP == "Associação Quilombola",]
shapefile_PROPver <- shapefile_linha_vermelha[shapefile_linha_vermelha$NOME_PROP == "Associação Quilombola",]
shapefile_PROPver2023 <- shapefile_linha_vermelha_2023[shapefile_linha_vermelha_2023$NOME_PROP == "Sítio Inhamoras",]
shapefile_PROPaz <- shapefile_linha_azul[shapefile_linha_azul$NOME_PROP == "Associação Quilombola",]

raster_PROP <- mask(crop(r2021, shapefile_PROPver2023), shapefile_PROPver2023)
shapefile_fator_sf <- st_as_sf(shapefile_PROP)
shapefile_fator_sfV <- st_as_sf(shapefile_PROPver)
shapefile_fator_sfA <- st_as_sf(shapefile_PROPaz)
shapefile_fator_sfV2023 <- st_as_sf(shapefile_PROPver2023)

df_raster_clipped <- as.data.frame(raster_PROP, xy=TRUE)

cores <- c("3" = "#006400", "9" = "#935132", "11" = "#45c2a5", "15" = '#ffd966', 
           "21" = "#fff3bf", "33" = "#0000ff")

print(
  ggplot() +
    geom_raster(aes(x = x, y = y, fill = factor(classification_2021)), data = df_raster_clipped) +
    scale_fill_manual(values = cores) +
    
    geom_sf(data = shapefile_fator_sfV2023, color = "red", fill = "transparent", lwd = 0.5)  +

    
    cowplot::theme_cowplot() +
    scale_y_continuous(breaks = seq(min(df_raster_clipped$y), max(df_raster_clipped$y), by = 0.1))
)


##################################################
###### map generator for automatic ALL PAF properties 
## shapefile vetorizado e raster do mapbiomas.

gerar_mapas_separados <- function(shapefile, raster) {
  # Obter os fatores únicos da variável COD_PROP
  fatores <- unique(shapefile$COD_PROP)
  
  # Definir cores para cada valor único da variável classification_2021
  cores <- c("3" = "#006400", "9" = "#935132", "11" = "#45c2a5", "15" = '#ffd966', 
             "21" = "#fff3bf", "33" = "#0000ff")
  
  # Iterar sobre cada fator e gerar mapas separados
  for (fator in fatores) {
    # Filtrar o shapefile pelo fator atual
    shapefile_fator <- shapefile[shapefile$NOME_PROP == fator, ]
    
    raster_fator <- raster::mask(raster::crop(raster, shapefile_fator), shapefile_fator)
    df_raster_fator <- as.data.frame(raster_fator, xy = TRUE)
    
    # Converter shapefile_fator para um objeto sf
    shapefile_fator_sf <- st_as_sf(shapefile_fator)
    
    p <- ggplot() +
      geom_raster(aes(x = x, y = y, fill = factor(df_raster_fator[[3]])), data = df_raster_fator) +
      geom_sf(data = shapefile_fator_sf, color = "black", fill = "transparent", size = 2)  +
      scale_fill_manual(values = cores, name = "Classes de Uso do Solo") +
      cowplot::theme_cowplot() +
      ggtitle(paste("propriedade:", fator)) +
      labs(x = "latitude", y = "longitude") +
      theme(legend.text = element_text(size = 8), 
            axis.title = element_text(size = 10),
            axis.text = element_text(size = 8),
            axis.text.x = element_text(angle = 0, hjust = 0.5),
            axis.text.y = element_text(angle = 0, hjust = 1),
            axis.ticks.length = unit(0.15, "cm")) +
      scale_x_continuous(breaks = seq(min(df_raster_fator$x), max(df_raster_fator$x), by = 0.005)) +
      scale_y_continuous(breaks = seq(min(df_raster_fator$y), max(df_raster_fator$y), by = 0.005))
    
    print(p)
  }
}

gerar_mapas_separados(shapefile_limites, r2021)


#############################################
##### map generator including shapefiles restauracao e conservacao for ALL PAF properties
### 
## COM LEGENDA DAS CLASSES
# Function to generate separate maps for each factor
gerar_mapas_separados <- function(shapefile, raster, shapefile_azul, shapefile_vermelha) {
  # Get unique values of COD_PROP
  fatores <- unique(shapefile$NOME_PROP)
  
  # Define colors for each value of classification_2021
  cores <- c("3" = "#006400", "9" = "#935132", "11" = "#45c2a5", "15" = "#ffd966", 
             "21" = "#fff3bf", "33" = "#0000ff")
  
  # Define labels for each class
  labels_classes <- c("3" = "florestal natural", "9" = "floresta plantada", "11" = "área alagada", 
                      "15" = "Pastagem", "21" = "mosaico de usos", "33" = "rios")
  
  # Iterate over each factor and generate separate maps
  for (fator in fatores) {
    # Filter shapefile by the current factor
    shapefile_fator <- shapefile[shapefile$NOME_PROP == fator, ]
    
    # Crop and mask the raster using shapefile_fator
    raster_fator <- raster::crop(raster, shapefile_fator)
    raster_fator2 <- raster::mask(raster_fator, shapefile_fator)
    df_raster_fator <- as.data.frame(raster_fator2, xy = TRUE)
    
    # Convert shapefile_fator to sf object
    shapefile_fator_sf <- st_as_sf(shapefile_fator)
    
    # Filter shapefile_azul and shapefile_vermelha by the current factor
    shapefile_azul_fator <- st_as_sf(shapefile_azul[shapefile_azul$NOME_PROP == fator, ])
    shapefile_vermelha_fator <- st_as_sf(shapefile_vermelha[shapefile_vermelha$NOME_PROP == fator, ])
    
    # Generate the map
    p <- ggplot() +
      geom_raster(aes(x = x, y = y, fill = factor(df_raster_fator[[3]])), data = df_raster_fator) +
      geom_sf(data = shapefile_fator_sf, aes(geometry = geometry), color = "black", fill = "transparent", lwd = 1.5) +
      geom_sf(data = shapefile_azul_fator, aes(geometry = geometry), color = "#32CD32", fill = "transparent", lwd = 1) +
      geom_sf(data = shapefile_vermelha_fator, aes(geometry = geometry), color = "#DC143C", fill = "transparent", lwd = 1) +
      scale_fill_manual(values = cores, labels = labels_classes, name = "classes de Uso do solo") +
      theme_cowplot() +
      ggtitle(paste(fator)) +
      labs(x = "Latitude", y = "Longitude") +
      theme(legend.text = element_text(size = 8), 
            axis.title = element_text(size = 10),
            axis.text = element_text(size = 8),
            axis.text.x = element_text(angle = 45, hjust = 1),
            axis.text.y = element_text(angle = 45, hjust = 1),
            axis.ticks.length = unit(0.15, "cm")) +
      scale_x_continuous(breaks = seq(min(df_raster_fator$x), max(df_raster_fator$x), by = 0.005),
                         labels = function(x) round(x, 2)) +
      scale_y_continuous(breaks = seq(min(df_raster_fator$y), max(df_raster_fator$y), by = 0.005),
                         labels = function(y) round(y, 2))
    
    print(p)
  }
}

gerar_mapas_separados(shapefile_limites, r2021, shapefile_linha_azul, shapefile_linha_vermelha)


###################################################
##### MAPAS SEM LEGENDA DE USO DO SOLO
# Function to generate separate and salve maps for each factor

gerar_mapas_separados <- function(shapefile, raster, shapefile_azul, shapefile_vermelha, output_dir) {
  
  # Get unique values of NOME_PROP
  fatores <- unique(shapefile$NOME_PROP)
  
  # Define colors for each value of classification_2021
  cores <- c("3" = "#006400", "9" = "#935132", "11" = "#45c2a5", "15" = "#ffd966", 
             "21" = "#fff3bf", "33" = "#0000ff")
  
  # Define labels for each class
  labels_classes <- c("3" = "florestal natural", "9" = "floresta plantada", "11" = "área alagada", 
                      "15" = "Pastagem", "21" = "mosaico de usos", "33" = "rios")
  
  # Iterate over each factor and generate separate maps
  for (fator in fatores) {
    # Filter shapefile by the current factor
    shapefile_fator <- shapefile[shapefile$NOME_PROP == fator, ]
    
    # Crop and mask the raster using shapefile_fator
    raster_fator <- raster::crop(raster, shapefile_fator)
    raster_fator2 <- raster::mask(raster_fator, shapefile_fator)
    df_raster_fator <- as.data.frame(raster_fator2, xy = TRUE)
    
    # Convert shapefile_fator to sf object
    shapefile_fator_sf <- st_as_sf(shapefile_fator)
    
    # Filter shapefile_azul and shapefile_vermelha by the current factor
    shapefile_azul_fator <- st_as_sf(shapefile_azul[shapefile_azul$NOME_PROP == fator, ])
    shapefile_vermelha_fator <- st_as_sf(shapefile_vermelha[shapefile_vermelha$NOME_PROP == fator, ])
    
    # Generate the map
    p <- ggplot() +
      geom_raster(aes(x = x, y = y, fill = factor(df_raster_fator[[3]])), data = df_raster_fator) +
      geom_sf(data = shapefile_fator_sf, aes(geometry = geometry), color = "black", fill = "transparent", lwd = 1.5) +
      geom_sf(data = shapefile_azul_fator, aes(geometry = geometry), color = "#32CD32", fill = "transparent", lwd = 1) +
      geom_sf(data = shapefile_vermelha_fator, aes(geometry = geometry), color = "#DC143C", fill = "transparent", lwd = 1) +
      scale_fill_manual(values = cores, labels = labels_classes, name = "classes de Uso do solo") +
      theme_cowplot() +
      ggtitle(paste(fator)) +
      labs(x = "latitude (º)", y = "longitude (º)") +
      theme(legend.text = element_text(size = 8), 
            axis.title = element_text(size = 10),
            axis.text = element_text(size = 12),
            axis.text.x = element_text(angle = 45, hjust = 1),
            axis.text.y = element_text(angle = 45, hjust = 1),
            axis.ticks.length = unit(0.15, "cm"),
            legend.position = "none") +
      scale_x_continuous(breaks = seq(min(df_raster_fator$x), max(df_raster_fator$x), by = 0.005),
                         labels = function(x) round(x, 2)) +
      scale_y_continuous(breaks = seq(min(df_raster_fator$y), max(df_raster_fator$y), by = 0.005),
                         labels = function(y) round(y, 2))
    
    print(p)
    
    
    # Save the plot as a high-resolution image
    output_filename <- paste(output_dir, "/", gsub("\\W+", "", fator), ".png", sep = "")
    ggsave(output_filename, p, dpi = 300)
  }
}

# Specify the output directory where the images will be saved
output_directory <- "C:/Users/pedro.rajao/Desktop/AGEVAP/10. quantificação C-GEE PAF/1. LANDSAT-mapbiomas/limite_propriedades/maps/2012"

# Call the function to generate and save the maps
gerar_mapas_separados(shapefile_limites, r2012, shapefile_linha_azul, shapefile_linha_vermelha, output_directory)



