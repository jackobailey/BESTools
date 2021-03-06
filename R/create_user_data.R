#' Create User Content Data
#'
#' This function takes a copy of the full BES Internet Panel and returns a new data set indexed by respondent id that includes only a subset of variables.
#'
#' @param data Data from the BES Internet Panel, 2014-2023
#' @param vars A vector of character strings corresponding to the variable names for the given user content.
#' @param factorise A vector of character strings corresponding to the variables that you want to convert to factor variables. Note: this does not respect partial/fuzzy matching yet.
#' @param save Whether to save the data to the disk. Takes the values TRUE or FALSE.
#' @param path A file path as a character string specifying where to save the data if save == T.
#' @param fuzzy If true, the function uses partial string matching. If false, it requires the complete string.
#' @param wave A character vector corresponding to the wave that the data come from (e.g. "W19").
#' @return Depending on the input file_type, either a data frame or tibble of data.
#' @examples
#' dta <-
#'   read_spss("BES2019_W19_Panel_v0.2.sav") %>%
#'   create_user_data(vars = c("user1", "user2"))
#' @export


create_user_data <- function(data = NULL, vars = NULL, factorise = NULL, save = T, path = NULL, fuzzy = T, wave = NULL){
  
  # Check if the user has added data, variable names,
  # or a file path
  
  if(is.null(data) == T){
    stop("You have not supplied any data.")
  } else if(is.null(vars) == T){
    stop("You have not supplied any variables.")
  } else if(save == T & is.null(path) == T){
    warning(paste("You have not specified a file path, defaulting to", here::here()))
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
  
  
  # Subset the data to include only the relevant waves
  
  if(is.null(wave) == F){
    
    data <- 
      data %>% 
      select(id,
             ends_with({{wave}}))
    
  }
  
  
  # Convert any variables to factors at the user's request
  
  if(is.null(factorise) == F){
    
    data[, factorise] <-
      data[, factorise] %>% 
      lapply(as_factor)
    
  }
  
  
  # If the user wants to save the data, then we write it
  # to the file path they specify. If not, we output it
  # so that user can continue to use it within R.
  
  if(save == T){
    
    # Where the user indicates that they want to save the data
    # but no path is specified, we default to here::here().
    
    path <- ifelse(is.null(path) == T, here::here("bes_user"), path)
    
    
    # Now, we save the data to the disk
    
    readr::write_csv(data, path = paste0(path, ".csv"))
    haven::write_sav(data, path = paste0(path, ".sav"))
    haven::write_dta(data, path = paste0(path, ".dta"))
    
    } else if (save == F){
      
      # If the user didn't want to save the data, we output
      # it to the console instead.
      
      data
    
  }
  
}