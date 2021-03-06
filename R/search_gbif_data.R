#' Function to search and download gbif data
#' @description Searchs gbif data using the function \code{\link[spocc]{occ}} from the spocc package
#' @param genus Species genus
#' @param species Species name
#' @param occlim Occurrence data search limit.
#' @param writeFile Write gibif data into a csv
#' @return Returns a data.frame with coordinate data from species
#' @export
#' @examples
#' # Species genus
#' # genus <- "ambystoma"
#' # Species name
#' # species <- "tigrinum"
#' # GBIF search
#' # ambystoma_tigrinum <- searh_gbif_data(genus,species,
#' #                                        occlim=100,
#' #                                        writeFile=FALSE)
#' # head(ambystoma_tigrinum[,1:5])
searh_gbif_data <- function(genus,species,occlim=10000,writeFile=FALSE){

  # Check if species data is on working directory
  file_name <- tolower(paste0(genus,"_",
                             species, occlim,".csv"))

  if(file_name %in% list.files())
    data_gbif <- read.csv(file_name,header=TRUE)

  # Gbif search
  else{
    df_list <- occ(query = paste(genus,species),
                   from = 'gbif',limit = occlim,
                   gbifopts = list(hasCoordinate = TRUE))
    # GBIF data
    data_gbif <- data.frame(df_list$gbif$data[[1]])
    # Remove NA data from longitude
    data_gbif <- data_gbif[!is.na(data_gbif$longitude),]
    # Remove NA data from latitude
    data_gbif <- data_gbif[!is.na(data_gbif$latitude),]

    #  Send a warning menssage if the species is not in GBIF

    if(length(data_gbif)==0L){
      data_gbif <- NULL
    }
    else if(writeFile){
      write.csv(data_gbif,file_name,row.names = FALSE)
    }
  }
  return(data_gbif)
}

