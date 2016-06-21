options(rgl.useNULL=TRUE)
# Visualizations in niche space

niche_data <- reactive({
  if(!is.null(data_extraction()))
    return(data_extraction())
  else
    return(NULL)
})


## Enviromental niche space plot

source("scatter3d/scatterplot.R")

niche_plot <- function(data,x,y,z,gtype,ajus,ellip){

    if(gtype=="corre"){
      ifelse(!is.null(ajus),surf <- TRUE,no = surf <-FALSE)
      formula <- as.formula(paste0(y,"~",x,"+",z))
      scatter3d(formula,point.col="gray8",level = 0.95,grid = TRUE,
                xlab = x,ylab = z,zlab = y,data=data,
                surface = surf,ellipsoid = ellip,fit=ajus,
                ellipsoid.alpha = 0.22,cex = 3,col="darkgreen")

    }
    else if(gtype=="disp"){
      plot3d(data,col=rainbow(1000),cex = 3)

    }
  }


# Vew the 3D Enviromental space

output$nicho <- renderRglwidget({

  withProgress(message = 'Doing computations', value = 0, {
    gtype <- input$gtype

    x <- input$x
    y <- input$y
    z <- input$z
    ajus <- input$fit
    ellip <- input$ellip

    if(!is.null(niche_data())){
      niche_plot(data = niche_data(),x = x,y = y,z = z,
                 gtype = gtype,ajus = ajus,ellip = ellip)
      rglwidget()
    }
    else
      return(NULL)
  })

})





observe({
  if(!is.null(niche_data())){
    updateSelectInput(session,"x",choices = names(niche_data()),selected = names(niche_data())[1])
    updateSelectInput(session,"y",choices = names(niche_data()),selected =  names(niche_data())[2])
    updateSelectInput(session,"z",choices = names(niche_data()),selected =  names(niche_data())[3])
  }
})
