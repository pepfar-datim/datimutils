library(httptest)
play233 <- list(base_url = "https://play.dhis2.org/2.33/",
                handle = httr::handle("https://play.dhis2.org/2.33/"))
play235 <- list(base_url = "https://play.dhis2.org/2.33.5/",
                handle = httr::handle("https://play.dhis2.org/2.33.5/"))
play234 <- list(base_url = "https://play.dhis2.org/2.34/",
                handle = httr::handle("https://play.dhis2.org/2.34/"))
play2335 <- list(base_url = "https://play.dhis2.org/2.33.5/",
                handle = httr::handle("https://play.dhis2.org/2.33.5/"))
play2341 <- list(base_url = "https://play.dhis2.org/2.34.1/",
                handle = httr::handle("https://play.dhis2.org/2.34.1/"))
play2372 <- list(base_url = "https://play.dhis2.org/2.37.2/",
                 handle = httr::handle("https://play.dhis2.org/2.37.2/"))
test <- list(base_url = "https://test.datim.org/",
                 handle = httr::handle("https://test.datim.org/"))


# accounts ----------

# list out uids
uid_list <-
  list(
      uids =
         list(
           c("M9Uer9SioL7","zpgv1M2Li1Q","seh1e61fwp1","XgctRYBpSiR","TRBfaInIiOK"),
           c("M9Uer9SioL7","seh1e61fwp1","G1O0MJgw8rs","XgctRYBpSiR","TRBfaInIiOK"),
           c("seh1e61fwp1","c6hGi8GEZot","M9Uer9SioL7","gh9tn4QBbKZ","CwFniyubXbx","OoiLAfMTyMx",
             "iuD8wUFz95X","ik8m9tx6QEw","TRBfaInIiOK","SBtczqnORYA"
           ),
           c("o8ap0XE01bh", "c6hGi8GEZot", "zY2t7de7Jzz", "TRBfaInIiOK"),
           c("OoiLAfMTyMx", "cveLo35sHE9"),
           c("BQCE8Nh9TRn","M9Uer9SioL7","seh1e61fwp1","XgctRYBpSiR","TRBfaInIiOK","LLXM2rpL69u"
           ),
           c("d3E1aOVZZkZ","M9Uer9SioL7","e1HnTm7vg38","tTSFg9jRcIB","seh1e61fwp1",
             "TRBfaInIiOK","zZ711zXQxw8","c6hGi8GEZot"
           ),
           c("zZ711zXQxw8")
         ),
       streams = 
         list(
           c("ER","ESOP","HRH"),
           c("ER","ESOP","HRH"),
           c("ER","ESOP","HRH","MER","MOH","SaSR","SIMS"),
           c("ESOP","MER"),
           c("MOH"),
           c("ER","ESOP","HRH"),
           c("DHI","ER","ESOP","HRH","MCAE","MER"),
           c("DHI")
         ),
       user_type = 
         list(
           "Agency",
           "Global Agency",
           "Global",
           "Interagency",
           "MOH",
           "Partner",
           "Global Partner",
           "Unclassified User"
         )
  )
