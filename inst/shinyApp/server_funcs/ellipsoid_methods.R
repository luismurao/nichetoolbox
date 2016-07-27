observe({
  if(!is.null(data_extraction())){
    # Suggest variables to fit ellipsoid accoring to strog correlations
    if(!is.null(summs_corr_var()))
      var_suggest <- summs_corr_var()$descriptors
    else
      var_suggest <- NULL
    updateSelectInput(session,"biosEllip",
                      choices = names(data_extraction()),selected = var_suggest)
  }
})

# ---------------------------------------------------------------------
# Fit the ellispoid model
# ---------------------------------------------------------------------

# Raster layers to project the minimum volume ellipsoid

#data_ellip <- reactive({
#  if(!is.null(rasterLayers()) && input$selectM=="wWorld")
#    return(rasterLayers())
#  if(!is.null(occ_extract_from_mask()) && input$selectM=="mLayers")
#    return(occ_extract_from_mask())
#  else
#    return()
#})

# 1. Compute the minimum volume ellipsoid

mve_obj_all <- reactive({
  if(!is.null(occ_extract()) && length(input$biosEllip)>1){
    input$selectBios
    isolate({
      if(input$selectBios){
        prop_points <- as.numeric(input$prop_points)
        niche_data <- occ_extract()
        cov_centroid <- cov_center(niche_data,
                                   level=prop_points,
                                   vars=input$biosEllip)
        return(cov_centroid)
      }
    })
  }
  else
    return()

})




#mve_obj_m <- reactive({
#  if(!is.null(occ_extract_from_mask()) && !is.null(input$biosEllip)){
#    input$selectBios
#    isolate({
#      if(input$selectBios){
#        prop_points <- as.numeric(input$prop_points)
#        niche_data <- occ_extract_from_mask()
#        cov_centroid <- cov_center(niche_data,
#                                   level=prop_points,
#                                   vars=input$biosEllip)
#        return(cov_centroid)
#      }
#    })
#  }
#  else
#    return()

#})



# 2. Fit and project the model All raster area

ellip_model_all_rast <- reactive({
  if(!is.null(mve_obj_all()) && !is.null(rasterLayers())){
    input$selectBios
    isolate({
      if(input$selectBios){
        model <- ellipsoidfit(rasterLayers()[[input$biosEllip]],
                              mve_obj_all()$centroid,
                              mve_obj_all()$covariance,level = 0.95,
                              threshold = 0.001,plot = FALSE)
        return(model)
      }
    })

  }
  else
    return()

})


ellip_model_m_rast <- reactive({
  if(!is.null(mve_obj_all()) && !is.null(define_M_raster())){
    input$selectBios
    isolate({
      if(input$selectBios){
        model <- ellipsoidfit(define_M_raster()[[input$biosEllip]],
                              mve_obj_all()$centroid,
                              mve_obj_all()$covariance,level = 0.95,
                              threshold = 0.001,plot = FALSE)
        return(model)
      }
    })

  }
  else
    return()

})


ellip_model <- reactive({
  if(!is.null(ellip_model_all_rast()) && input$selectM=="wWorld")
    return(ellip_model_all_rast())
  if(!is.null(ellip_model_m_rast()) && input$selectM=="mLayers")
    return(ellip_model_m_rast())
  else
    return()
})

leaf_ellip <- reactive({

  map <- leaflet() %>%
    addTiles(
      urlTemplate = "//{s}.tiles.mapbox.com/v3/jcheng.map-5ebohr46/{z}/{x}/{y}.png",
      attribution = 'Maps by <a href="http://www.mapbox.com/">Mapbox</a>'
    )

  if(is.null(ellip_model()))
    map <- map %>%  setView(lng = 0, lat = 0, zoom = 3)
  else{
    model <- ellip_model()$suitRaster
    cbbPalette <- rev(terrain.colors(100))
    pal <- colorNumeric(cbbPalette, values(model),
                        na.color = "transparent")

    crs(model) <- "+proj=longlat +ellps=WGS84 +towgs84=0,0,0,0,0,0,0 +no_defs"
    map <- map %>% addRasterImage(model, colors = pal, opacity = 0.5) %>%
      addLegend(pal = pal, values = values(model),
                position = "topleft",labFormat = labelFormat(),
                title = "Suitability")

  }
  return(map)
})



output$ellip_map <- renderLeaflet({
  return(leaf_ellip())
})

# Download ellipsoid plot
output$EllipRasterPlot <- downloadHandler(
  filename <- "EllipsoidModelPlot.pdf",
  content <- function(file){
    pdf(file,width = 12,height = 8,pointsize = 18)
    plot(ellip_model()$suitRaster)
    dev.off()
  }
)

# Download Ellipsoid Raster

output$downEllipRas <- downloadHandler(
  filename <- "EllipsoidModelNTB.asc",
  content <- function(file){
    writeRaster(ellip_model()$suitRaster,file)
  }
)

# Download Ellipsoid distances

output$downEllipDistance <- downloadHandler(
  filename <- "EllipsoidDistancesNTB.csv",
  content <- function(file){
    ndistTable <- data.frame(ellip_model()$suits,ellip_model()$ncentedist)
    write.csv(ndistTable,file,row.names = FALSE)
  }
)

ellipsoid_plot_3d <- function(suits,data,covar,centroid,level=0.95,xlab1="",ylab1="",zlab1="Suitability"){
  dim_data <- dim(data)[2]
  if(dim_data == 3){
    data1 <- data[!is.na(suits), ]
    dfd <- dim(data1)[1] - 1
    dfn <- dim(data1)[2] - 1
    ell.radius_E <- sqrt(dfn * qf(level, dfn, dfd))
    suits2 <- suits[!is.na(suits)]
    ellips_E <- ellipsoid(center = centroid, shape = covar,
                          radius = ell.radius_E)
    if (dfd > 50000)
      np <- 50000
    else np <- dim(data1)[1]
    toSam <- sample(1:length(data1[, 1]), np)
    data1 <- data1[toSam, ]
    plot3d(data1, size = 3, col = hsv(suits2[toSam] *
                                           0.71, 0.95, 0.9))
    wire3d(ellips_E, col = 4, lit = FALSE, alpha = 0.1)
  }
  if(dim_data == 2){

    x <- seq(from = centroid[1]/1.5, to = centroid[1] * 1.25,
             length = 60)
    y <- seq(from = centroid[2]/1.5, to = centroid[2] * 1.25,
             length = 60)
    suit <- function(data,medias,covMatrix){
      a <- 1
      expo <- exp(-0.5*mahalanobis(data,medias,cov = covMatrix))
      return(a*expo)
    }
    suit1 <- function(x, y) suit(cbind(x, y), medias = centroid,
                                 covMatrix = covar)
    z <- outer(x, y, suit1)

    p1 <- persp(x, y, z, box = T, xlab = xlab1, ylab = ylab1,
                zlab = "", col = "blue", theta = 55, phi = 30, r = 40,
                d = 0.1, expand = 0.5, ticktype = "detailed", nticks = 5,
                cex.lab = 1.5, cex.axis = 1.3, cex.main = 1.5, cex.sub = 1.5)
    ranges <- t(sapply(list(x, y, z), range))
    means <- rowMeans(ranges)
    labelspace <- 0.12
    xpos <- min(x) - (diff(range(x))) * labelspace
    ypos <- min(y) - (diff(range(y))) * labelspace
    labelbot3d <- c(xpos, ypos, min(z))
    labeltop3d <- c(xpos, ypos, max(z))
    labelmid3d <- c(xpos, ypos, mean(range(z)))
    trans3dfun <- function(v) {
      trans3d(v[1], v[2], v[3], p1)
    }
    labelbot2d <- trans3dfun(labelbot3d)
    labelmid2d <- trans3dfun(labelmid3d)
    labeltop2d <- trans3dfun(labeltop3d)
    labelang <- 180/pi * atan2(labeltop2d$y - labelbot2d$y,
                               labeltop2d$x - labelbot2d$x)
    par(xpd = NA, srt = labelang)
    text(labelmid2d[1]$x, labelmid2d$y, zlab1, cex = 1.5)
  }
}


plot_ellipsoid <- reactive({
  if(!is.null(ellip_model())){
    suits <- ellip_model()$suits[,"suitability"]
    data <- ellip_model()$suits[,input$biosEllip]
    covar <- mve_obj_all()$covariance
    centroid <- mve_obj_all()$centroid
    ellipsoid_plot_3d(suits = suits,
                      data = data,covar = covar,
                      centroid = centroid,
                      xlab1 = input$biosEllip[1],
                      ylab1 = input$biosEllip[2])
  }
  else
    return()

})


output$Ellip3D <- renderRglwidget({
  dim_d <- length(input$biosEllip)
  if(!is.null(ellip_model()) && dim_d==3){
    plot_ellipsoid()
    rglwidget()
  }
  else
    return()
})


output$Ellip2D <- renderPlot({
  dim_d <- length(input$biosEllip)
  if(!is.null(ellip_model()) && dim_d==2){
    plot_ellipsoid()
  }
  else
    return()
})

# Normal Response curves

response_ell <- reactive({
  if(!is.null(ellip_model())){
    if(input$selectM=="wWorld"){
      multi.hist(occ_extract()[,input$biosEllip],
                 dcol= c("blue","red"),dlty=c("dotted", "solid"))
    }
    if(!is.null(occ_extract_from_mask()) && input$selectM=="mLayers" && !is.null(myPolygon())){
      multi.hist(occ_extract_from_mask()[,input$biosEllip],
                 dcol= c("blue","red"),dlty=c("dotted", "solid"))
    }
    else
      return()
  }

})


output$reponse_curves <- renderPlot({
  if(!is.null(response_ell()))
    response_ell()
})


# Download meta data

output$downShapMat <- downloadHandler(
  filename <- "EllipsoidMetaData.txt",
  content <- function(file){
    capture.output(mve_obj_all(),file = file)
  }
)
#output$EllipRaster <- renderPlot({
#  if(!is.null(ellip_model()))
#    plot(ellip_model()$suitRaster)
#  else{
#    messages <- "Load niche layers | extract niche data"
#    x <- -10:10
#    y <- x
#    plot(x,y,type="n", xlab="No Data", ylab="No data",cex=2)
#    text(0,0,messages,cex=3 )
#
#  }
#})
