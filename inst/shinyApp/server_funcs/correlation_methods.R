# Correlation Analysis

#---------------------------------------------------------------------------------
# Correlation table

corr_table <- reactive({
  if(!is.null(data_extraction())){
    niche_data <- data_extraction()
    cor_table <- cor(niche_data,
                     use = "pairwise.complete.obs")
    return(cor_table)
  }
  else return(NULL)

})



output$corr_table <- renderDataTable({
  withProgress(message = 'Doing Computations', value = 0, {
    if(!is.null(corr_table())){
      niche_bivar_corr <- corr_table()
      var_names <- colnames(niche_bivar_corr)
      corTable <- cbind(var_names,niche_bivar_corr)
      return(corTable)
    }
    else{
      message <- "No niche data: extract niche values from layers!
                       (go to Niche space -> Niche data extraction)"
      df <- data.frame(NoNicheData = message)
      return(df)
    }
  })
})

output$download_cor_table <- downloadHandler(
  #filename = function() return(paste0(input$genus,"_",input$species,"M_rasters.tar")),
  filename = "ntb_correlation_table.csv",
  content = function(file) {
    if(!is.null(corr_table())){
      niche_bivar_corr <- corr_table()
      var_names <- colnames(niche_bivar_corr)
      corTable <- cbind(var_names,niche_bivar_corr)
      write.csv(corTable,file,row.names = FALSE)
    }
  }
)


#---------------------------------------------------------------------------------
# Correlation plot


corr_plot <- reactive({

  if(!is.null(corr_table())){
    # Color palette
    col1 <- colorRampPalette(
      c("#7F0000","red","#FF7F00","yellow","white",
        "cyan", "#007FFF", "blue","#00007F")
    )

    niche_bivar_corr <- corr_table()

    return(corrplot(niche_bivar_corr,
                    method="ellipse",
                    col=col1(200),order = "AOE"))
  }
  else return(NULL)


})


output$corr_plot <- renderPlot({
  #datos <- data()
  withProgress(message = 'Doing computations', value = 0, {
    if(!is.null(corr_plot()))
      corr_plot()
    else{
      message <- "No niche data: extract\nniche values from layers!\n(go to Niche space -> Niche data extraction)"
      x <- -10:10
      y <- x
      plot(x,y,type="n")
      text(0,1,message,cex=2.5)
    }
  })
})

output$download_cor_plot <- downloadHandler(
  #filename = function() return(paste0(input$genus,"_",input$species,"M_rasters.tar")),
  filename = "ntb_correlation_plot.pdf",
  content = function(file) {
    if(!is.null(corr_table())){
      pdf(file,width = 8,height = 8)
      corr_plot()
      dev.off()
    }
  }
)

