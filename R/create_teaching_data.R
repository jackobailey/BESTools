#' Create BES Teaching Data Set
#'
#' This function takes a copy of the full BES Internet Panel and returns a smaller version with only essential variables intended for teaching purposes.
#'
#' @param data A copy of the BES Internet Panel, 2014-2023
#' @return A data frame of electoral data
#' @examples
#' bes <- read_spss("BES2019_W19_Panel_v0.2.sav")
#' bes %>% create_teaching_data()
#' @export


create_teaching_data <- function(data){
  
  # Check whether the data is from the BES internet panel
  # using a simple heuristic based on presence of timing
  # variable from wave 1 and the first id number.
  
  if(!("starttimeW1" %in% names(data))){
    stop("The data are not from the BES Internal Panel.")
  } else if(data$id[1] != 16208){
    stop("The data are not from the BES Internal Panel.")
  }
  
  
  # Run the BES panel data through select(), returning
  # any variables that match the subset of variables we
  # want to include in the teaching data set.
  
  data <- 
    data %>% 
    select(
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
    str_replace("wt_full_", "wt") %>% 
    str_replace("wt_new_", "wt") %>% 
    str_replace("edlevel", "edu") %>% 
    str_replace("polAttention", "attn") %>% 
    str_replace("turnoutUKGeneral", "turnoutGE") %>% 
    str_replace("generalElectionVote", "voteGE") %>% 
    str_replace("euRefVote", "voteEU") %>% 
    str_replace("scotReferendumIntention", "voteIndy") %>% 
    str_replace("partyId", "pid") %>% 
    str_replace("partyIdStrength", "pidStr") %>% 
    str_replace("scotRefID", "indyID") %>% 
    str_replace("small_mii_cat", "mii") %>% 
    str_replace("enviroProtection", "envProtect") %>% 
    str_replace("cutsTooFarNHS", "cutsNHS") %>% 
    str_replace("cutsTooFarLocal", "cutsLocal") %>% 
    str_replace("cutsTooFarNational", "cutsNat") %>% 
    str_replace("econGenRetro", "econNat") %>% 
    str_replace("econPersonalRetro", "econPers") %>% 
    str_replace("euPriorityBalance", "euBalance")

  
  # Return the teaching dataset
  data
  
}

bes %>% create_teaching_data() %>% names()
