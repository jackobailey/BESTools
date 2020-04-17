#' Create User Content Data
#'
#' This function takes a copy of the full BES Internet Panel and returns a new data set indexed by respondent id that includes only a subset of variables.
#'
#' @param data Data from the BES Internet Panel, 2014-2023
#' @param vars A vector of character strings corresponding to the variable names for the given user content.
#' @param save Whether to save the data to the disk. Takes the values TRUE or FALSE.
#' @param path A file path as a character string specifying where to save the data if save == T.
#' @param file_type A character string specifying what file_type to save the data in. Takes the values "csv", "sav", or "dta". Defaults to "csv.
#' @param fuzzy If true, the function uses partial string matching. If false, it requires the complete string.
#' @return Depending on the input file_type, either a data frame or tibble of data.
#' @examples
#' dta <-
#'   read_spss("BES2019_W19_Panel_v0.2.sav") %>%
#'   create_user_data(vars = c("user1", "user2"))
#' @export


create_user_data <- function(data = NULL, vars = NULL, save = T, path = NULL, file_type = "csv", fuzzy = T){
  
  # Check if the user has added data, variable names,
  # or a file path
  
  if(is.null(data) == T){
    stop("You have not supplied any data.")
  } else if(is.null(vars) == T){
    stop("You have not supplied any variables.")
  } else if(save == T & is.null(path) == T){
    warning(paste("You have not specified a file path, defaulting to", here::here(paste0("bes_user.", file_type))))
  }
  
  
  # Run the BES panel data through select(), returning
  # the respondent ID plus any variables that match the
  # subset of variables specific to the user.
  
  if(fuzzy == T){
    
    data <- 
      data %>% 
      dplyr::select(
        id,
        matches({{vars}})
      )
    
  } else {
    
    data <- 
      data %>% 
      dplyr::select(
        id,
        {{vars}}
      )
    
  }
  
  
  # If the user wants to save the data, then we write it
  # to the file path they specify. If not, we output it
  # so that user can continue to use it within R.
  
  if(save == T){
    
    # Where the user indicates that they want to save the data
    # but no path is specified, we default to here::here().
    
    path <- ifelse(is.null(path) == T, here::here(paste0("bes_user.", file_type)), path)
    
    
    # Now, we save the data to the disk in the specified file_type
    
    if(file_type %in% "csv"){
      
      # Write CSV
      readr::write_csv(data, path = path)
      
    } else if(file_type %in% "sav"){
      
      # Write SAV
      haven::write_sav(data, path = path)
      
    } else if(file_type %in% "dta"){
      
      # Write DTA
      haven::write_dta(data, path = path)
      
    } else if(!(file_type %in% c("csv", "sav", "dta"))){
      
      # If no valid file_type isspecified, show a warning and
      # then save the data as a csv
      warning("No file file_type specified, defaulting to csv")
      write.csv(data, file = path)
      
    } 
    
    } else if (save == F){
      
      # If the user didn't want to save the data, we output
      # it to the console instead.
      
      data
    
  }
  
}
