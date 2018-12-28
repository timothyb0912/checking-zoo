# Replication Archive for: 
# Kirkland, Patricia A. and Alexander Coppock
# Candidate Choice without Party Labels: New Insights from Conjoint Survey Experiments
# Forthcoming at Political Behavior
# Helper Functions

gen_entry <- function(est, se, p){
  entry <- paste0(format_num(est, digits = 2), " (", format_num(se, 2), ")")
  
  if(p < 0.05) {
    entry <- paste0(entry, "*")
  }
  return(entry)
}

gen_entry_vec <- Vectorize(gen_entry)

se_mean <- function(x){
  x_nona <- x[!is.na(x)]
  n <- length(x_nona)
  return(sd(x_nona)/(sqrt(n)))
}

format_num <- function(x, digits=3){
  x <- as.numeric(x)
  return(paste0(sprintf(paste0("%.", digits, "f"), x)))
}

make_attributes <- function(df){
  df <- within(df,{
               attribute = factor(attribute, 
                                  levels = c("Male", "Base category: Female", 
                                             "65", "55", "45", 
                                             "Base category: 35",
                                             
                                             "Asian", 
                                             "Black", 
                                             "Hispanic", 
                                             "Base category: White",
                                             
                                             "Attorney", 
                                             "Business Executive", 
                                             "Small Business Owner",
                                             "Police Officer", 
                                             "Electrician", 
                                             "Stay-at-Home Dad/Mom", 
                                             "Base category: Educator",
                                             
                                             "Representative in Congress",
                                             "Mayor", 
                                             "State Legislator",
                                             "City Council Member", 
                                             "School Board President", 
                                             "Base category: No Political Experience",
                                             
                                             "Republican",
                                             "Democrat",
                                             "Base category: Independent"))
               })
  return(df)
}

make_coef_group <- function(df){
  df <- within(df,{
    coef_group <- rep(NA, nrow(df))
    coef_group[grepl(pattern="Gender", x = rowname)] <- "Gender"
    coef_group[grepl(pattern="Age", x = rowname)] <- "Age"
    coef_group[grepl(pattern="Race", x = rowname)] <- "Race"
    coef_group[grepl(pattern="Job", x = rowname)] <- "Job Experience"
    coef_group[grepl(pattern="Political", x = rowname)] <- "Pol. Experience"
    coef_group[grepl(pattern="Party", x = rowname)] <- "Party"
    coef_group <- factor(coef_group, levels=c("Party", "Pol. Experience", "Job Experience", 
                                              "Race", "Age", "Gender"))  
  })
  return(df)
}


prep_for_gg <- function(fit_cl, WP=FALSE){
  df <- data.frame(ests =fit_cl[,1], ses = fit_cl[,2]) %>%
    add_rownames()
  df <- filter(df, grepl(pattern = "Gender|Age|Race|Job|Political|Party", x = df$rowname))
  df <- within(df,{
    uis <- ests + 1.96*ses
    lis <- ests - 1.96*ses
    attribute <- sub(pattern = "Gender|Age|Race|Job|Political|Party", replacement = "", x = df$rowname)
    attribute <- factor(attribute, levels = c("Male", "Base category: Female", 
                                              "65", "55", "45", 
                                              "Base category: 35",
                                              
                                              "Asian", 
                                              "Black", 
                                              "Hispanic", 
                                              "Base category: White",
                                              
                                              "Attorney", 
                                              "Business Executive", 
                                              "Small Business Owner",
                                              "Police Officer", 
                                              "Electrician", 
                                              "Stay-at-Home Dad/Mom", 
                                              "Base category: Educator",
                                              
                                              "Representative in Congress",
                                              "Mayor", 
                                              "State Legislator",
                                              "City Council Member", 
                                              "School Board President", 
                                              "Base category: No Political Experience"))
    if(WP){
      attribute <- sub(pattern = "Gender|Age|Race|Job|Political|Party", replacement = "", x = df$rowname)
      attribute <- factor(attribute, levels = c("Male", "Base category: Female", 
                                                "65", "55", "45", 
                                                "Base category: 35",
                                                
                                                "Asian", 
                                                "Black", 
                                                "Hispanic", 
                                                "Base category: White",
                                                
                                                "Attorney", 
                                                "Business Executive", 
                                                "Small Business Owner",
                                                "Police Officer", 
                                                "Electrician", 
                                                "Stay-at-Home Dad/Mom", 
                                                "Base category: Educator",
                                                
                                                "Representative in Congress",
                                                "Mayor", 
                                                "State Legislator",
                                                "City Council Member", 
                                                "School Board President", 
                                                "Base category: No Political Experience",
                                                
                                                "Republican",
                                                "Democrat",
                                                "Base category: Independent"))
    }
  })
  return(df)
}


cl   <- function(dat,fm, cluster){
  require(sandwich, quietly = TRUE)
  require(lmtest, quietly = TRUE)
  M <- length(unique(cluster))
  N <- length(cluster)
  K <- fm$rank
  dfc <- (M/(M-1))*((N-1)/(N-K))
  uj  <- apply(estfun(fm),2, function(x) tapply(x, cluster, sum));
  vcovCL <- dfc*sandwich(fm, meat=crossprod(uj)/N)
  coeftest(fm, vcovCL) }

cl_vcov   <- function(dat,fm, cluster){
  require(sandwich, quietly = TRUE)
  require(lmtest, quietly = TRUE)
  M <- length(unique(cluster))
  N <- length(cluster)
  K <- fm$rank
  dfc <- (M/(M-1))*((N-1)/(N-K))
  uj  <- apply(estfun(fm),2, function(x) tapply(x, cluster, sum));
  vcovCL <- dfc*sandwich(fm, meat=crossprod(uj)/N)
  return(vcovCL)}


