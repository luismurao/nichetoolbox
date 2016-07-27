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

# 1. Compute the minimum volume ellipsoid


mve_obj_all <- eventReactive(input$train_ellips,{
  if(!is.null(occ_extract()) && length(input$biosEllip)>1){
    prop_points <- as.numeric(input$prop_points)
    niche_data <- occ_extract()
    cov_centroid <- cov_center(niche_data,
                               level=prop_points,
                               vars=input$biosEllip)
    return(cov_centroid)

  }
  else
    return()
})


# 2. Fit and project the model All raster area


ellip_model_all_rast <- eventReactive(input$selectBios_all,{
  if(!is.null(mve_obj_all()) && !is.null(rasterLayers()) && input$selectM == 'wWorld'){

        model <- ellipsoidfit(rasterLayers()[[input$biosEllip]],
                              mve_obj_all()$centroid,
                              mve_obj_all()$covariance,level = 0.95,
                              threshold = 0.001,plot = FALSE)
        return(model)
  }
  else
    return()
})


ellip_model_m_rast <- eventReactive(input$selectBios_m,{
  if(!is.null(mve_obj_all()) && !is.null(define_M_raster()) && input$selectM == 'mLayers'){

    model <- ellipsoidfit(define_M_raster()[[input$biosEllip]],
                          mve_obj_all()$centroid,
                          mve_obj_all()$covariance,level = 0.95,
                          threshold = 0.001,plot = FALSE)
    return(model)


  }
  else
    return()

})


#ellip_model <- reactive({
#  if(!is.null(ellip_model_all_rast()) && input$selectM=="wWorld")
#    return(ellip_model_all_rast())
#  if(!is.null(ellip_model_m_rast()) && input$selectM=="mLayers")
#    return(ellip_model_m_rast())
#  else
#    return()
#})


# Download ellipsoid plot
output$EllipRasterPlot <- downloadHandler(
  filename <- "EllipsoidModelPlot.pdf",
  content <- function(file){
    pdf(file,width = 12,height = 8,pointsize = 18)
    plot(ellip_model_all_rast()$suitRaster)
    dev.off()
  }
)

# Download Ellipsoid Raster

output$downEllipRas <- downloadHandler(
  filename <- function() paste0("EllipsoidModelNTB",input$selectM,".asc"),
  content <- function(file){
    if(!is.null(ellip_model_all_rast()) && input$selectM == "wWorld")
      writeRaster(ellip_model_all_rast()$suitRaster,file)
    if(!is.null(ellip_model_m_rast()) && input$selectM == "mLayers")
      writeRaster(ellip_model_m_rast()$suitRaster,file)
  }
)

# Download Ellipsoid distances

output$downEllipDistance <- downloadHandler(
  filename <- function() paste0("EllipsoidDistancesNTB",input$selectM,".csv"),
  content <- function(file){
    if(!is.null(ellip_model_all_rast()) && input$selectM == "wWorld"){
      ndistTable <- data.frame(ellip_model_all_rast()$suits,
                               ellip_model_all_rast()$ncentedist)
      write.csv(ndistTable,file,row.names = FALSE)
    }
    if(!is.null(ellip_model_m_rast()) && input$selectM == "mLayers"){
      ndistTable <- data.frame(ellip_model_m_rast()$suits,
                                 ellip_model_m_rast()$ncentedist)
      write.csv(ndistTable,file,row.names = FALSE)
    }

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


plot_ellipsoid_all <- reactive({
  if(!is.null(ellip_model_all_rast())){
    suits <- ellip_model_all_rast()$suits[,"suitability"]
    data <- ellip_model_all_rast()$suits[,input$biosEllip]
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

plot_ellipsoid_m <- reactive({
  if(!is.null(ellip_model_m_rast())){
    suits <- ellip_model_m_rast()$suits[,"suitability"]
    data <- ellip_model_m_rast()$suits[,input$biosEllip]
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




output$Ellip3D_all <- renderRglwidget({
  dim_d <- length(input$biosEllip)
  if(!is.null(plot_ellipsoid_all()) && dim_d==3){
    plot_ellipsoid_all()

  }
  rglwidget()
})

output$Ellip3D_m <- renderRglwidget({
  dim_d <- length(input$biosEllip)
  if(!is.null(plot_ellipsoid_m()) && dim_d==3){
    plot_ellipsoid_m()

  }
  rglwidget()
})


output$Ellip2D_all <- renderPlot({
  dim_d <- length(input$biosEllip)
  if(!is.null(plot_ellipsoid_all()) && dim_d==2){
    plot_ellipsoid_all()
  }
})


output$Ellip2D_m <- renderPlot({
  dim_d <- length(input$biosEllip)
  if(!is.null(plot_ellipsoid_m()) && dim_d==2){
    plot_ellipsoid_m()
  }
})

# Normal Response curves

response_ell_all <- reactive({
  if(!is.null(ellip_model_all_rast())){
    if(input$selectM=="wWorld"){
      multi.hist(occ_extract()[,input$biosEllip],
                 dcol= c("blue","red"),dlty=c("dotted", "solid"))
    }
    else
      return()
  }

})

# Normal Response curves

response_ell_m <- reactive({
  if(!is.null(ellip_model_m_rast())){
    if(!is.null(occ_extract_from_mask()) && input$selectM=="mLayers" && !is.null(myPolygon())){
      multi.hist(occ_extract_from_mask()[,input$biosEllip],
                 dcol= c("blue","red"),dlty=c("dotted", "solid"))
    }
    else
      return()
  }

})


output$reponse_curves_all <- renderPlot({
  if(!is.null(response_ell_all()))
    response_ell_all()
})


output$reponse_curves_m <- renderPlot({
  if(!is.null(response_ell_m()))
    response_ell_m()
})


# Download meta data

output$downShapMat <- downloadHandler(
  filename <- "EllipsoidMetaData.txt",
  content <- function(file){
    capture.output(mve_obj_all(),file = file)
  }
)
