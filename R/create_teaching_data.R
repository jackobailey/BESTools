#' Create BES Teaching Data Set
#'
#' This function takes a copy of the full BES Internet Panel and returns a smaller version with only essential variables intended for teaching purposes.
#'
#' @param data Data from the BES Internet Panel, 2014-2023
#' @return Depending on the input format, either a data frame or tibble of data.
#' @examples
#' dta <-
#'   read_spss("BES2019_W19_Panel_v0.2.sav") %>%
#'   create_teaching_data()
#' @export


create_teaching_data <- function(data = NULL){
  
  # Check whether the user has added any data .
  
  if(is.null(data) == T){
    stop("You have not supplied any data.")
  }
  
  
  # Run the BES panel data through select(), returning
  # any variables that match the subset of variables we
  # want to include in the teaching data set.
  
  data <- 
    data %>% 
    dplyr::select(
      # Admin variables
      id,
      matches("^wave"),
      dateW = num_range("starttimeW", 1:100),
      
      # Weights
      num_range("wt_full_W", 1:5),
      num_range("wt_new_W", 6:100),
      wt_2010_2015 = "wt_full_W1W2W3W4W5W6",
      wt_2015_2017 = "wt_new_W7W13",
      wt_2017_2019 = "wt_new_W13_W19",
      num_range("wt_daily_W", 1:100),
      
      # Profile data
      age = Age,
      gender,
      matches("^countryW"),
      matches("^edlevelW"),
      ethnicity = any_of(c("profile_ethnicity", "p_ethnicity")),
      religion = any_of(c("profile_religion", "p_religion")),
      matches("^ns_sec_analytic"),
      hhinc = any_of(c("profile_gross_household", "p_gross_household")),
      marital = any_of(c("marital")),
      matches("^polAttentionW"),
      
      # Past voting behaviour
      voteIndy = any_of(c("profile_scotref_vote", "p_scotref")),
      turnoutIndy = any_of(c("profile_scotref_turnout")),
      voteEU = any_of(c("profile_eurefvote", "p_eurefvote")),
      turnoutEU = any_of(c("profile_eurefturnout")),
      vote05 = any_of(c("profile_past_vote_2005", "p_past_vote_2005")),
      vote10 = any_of(c("profile_past_vote_2010", "p_past_vote_2010")),
      vote15 = any_of(c("profile_past_vote_2015", "p_past_vote_2015")),
      turnout15 = any_of(c("profile_turnout_2015", "p_turnout_2015")),
      vote17 = any_of(c("profile_past_vote_2017", "p_past_vote_2017")),
      turnout17 = any_of(c("profile_turnout_2017", "p_turnout_2017")),
      vote19 = any_of(c("profile_past_vote_2019", "p_past_vote_2019")),
      turnout19 = any_of(c("voted_ge_2019", "p_turnout_2019")),
      
      # Vote and turnout
      matches("^turnoutUKGeneralW"),
      matches("^generalElectionVoteW"),
      matches("^eurefvoteW"),
      matches("^scotReferendumIntentionW"),
      
      # Political identification
      matches("^partyIdW"),
      matches("^partyIdStrengthW"),
      matches("^euIDW"),
      matches("^scotRefIDW"),
      
      # Parties
      matches("^likeConW"),
      matches("^likeLabW"),
      
      # Leaders
      matches("^likeCameronW"),
      matches("^likeMayW"),
      matches("^likeBorisW"),
      matches("^likeMilibandW"),
      matches("^likeCorbynW"),
      matches("^likeStarmerW"),
      
      # Issues/ideology/values
      matches("^small_mii_catW"),
      matches("^al_scaleW"),
      matches("^lr_scaleW"),
      matches("^britishnessW"),
      matches("^englishnessW"),
      matches("^scottishnessW"),
      matches("^welshnessW"),
      matches("^europeannessW"),
      matches("^immigSelfW"),
      matches("^immigConW"),
      matches("^immigLabW"),
      matches("^redistSelfW"),
      matches("^redistConW"),
      matches("^redistLabW"),
      matches("^enviroProtectionW"),
      matches("^cutsTooFarNHSW"),
      matches("^cutsTooFarLocalW"),
      matches("^cutsTooFarNationalW"),
      
      # Views on government
      matches("^trustMPs"),
      matches("^satDem"),
      
      # Economy
      matches("^econGenRetroW"),
      matches("^econPersonalRetroW"),
      
      # Brexit
      matches("^euPriorityBalanceW")
      
    )
  
  
  # Shorten some variable names to make them easier to use
  # in a teaching context or on small screens.
  
  names(data) <- 
    names(data) %>% 
    stringr::str_replace("wt_full_", "wt") %>% 
    stringr::str_replace("wt_new_", "wt") %>% 
    stringr::str_replace("edlevel", "edu") %>% 
    stringr::str_replace("ns_sec_analyti", "socClass") %>% 
    stringr::str_replace("polAttention", "attn") %>% 
    stringr::str_replace("turnoutUKGeneral", "turnoutGE") %>% 
    stringr::str_replace("generalElectionVote", "voteGE") %>% 
    stringr::str_replace("euRefVote", "voteEU") %>% 
    stringr::str_replace("scotReferendumIntention", "voteIndy") %>% 
    stringr::str_replace("partyId", "pid") %>% 
    stringr::str_replace("partyIdStrength", "pidStr") %>% 
    stringr::str_replace("scotRefID", "indyID") %>% 
    stringr::str_replace("likeBoris", "lokeJohnson") %>% 
    stringr::str_replace("small_mii_cat", "mii") %>% 
    stringr::str_replace("enviroProtection", "envProtect") %>% 
    stringr::str_replace("cutsTooFarNHS", "cutsNHS") %>% 
    stringr::str_replace("cutsTooFarLocal", "cutsLocal") %>% 
    stringr::str_replace("cutsTooFarNational", "cutsNat") %>% 
    stringr::str_replace("econGenRetro", "econNat") %>% 
    stringr::str_replace("econPersonalRetro", "econPers") %>% 
    stringr::str_replace("euPriorityBalance", "euBalance")

  
  # Return the teaching data set
  return(data)
  
}
