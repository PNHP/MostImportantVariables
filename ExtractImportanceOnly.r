# Separate run through of existing RF runs to extract
# and then explore the relative importance of different layers.
#Question: are there any variables that consistently sort at the
#bottom that can/should be removed from the analyses?

library(RSQLite)
library(randomForest)

# First, change to the directory
setwd("E:/SDM/MA_Butterfly/outputs")

#get a list of what's in the directory
d <- dir(pattern = "Rdata")

#i <- 1
#loop through everything in the dir
for (i in 1:length(d)){
  #for (i in 1:46){
  fileName <- dir(pattern = "Rdata")[[i]]
  # Bring the two files into R
  load(fileName)
  impRank <- rank(-EnvVars$impVal)   #reverse order by ranking the negative
  EnvVars <- cbind(EnvVars, impRank)
  #get number of varibles used in each forest
  used <- varUsed(rf.full)
  names(used) <- names(EnvVars)

  f.importance <- data.frame( 
                            "Species" = ElementNames$SciName,
                            #"SppCode" = abbr,
                            "varCode" = rownames(EnvVars),
                            "varFullName"=EnvVars$fullName,
                            "meanDecreaseAcc" = EnvVars$impVal,
                            "impRank" = EnvVars$impRank,
                            "timesUsed" = used,
                            "dtmDate" = format(Sys.time(), "%Y %m %d"),
                            "dtmTime" = format(Sys.time(), "%X")
                            )

  #  open up a "channel" to the Access DB
  databasename <- "E:/SDM/Shared/MostImportantVariables/MostImportantVar.sqlite" 
  db <- dbConnect(SQLite(), dbname = databasename)
  #write importance values
  dbWriteTable(db, "tblImportance", f.importance, append=TRUE)
  # disconnect the SQL database
  dbDisconnect(db) #close connection
  #remind me what I just ran -- doesn't work, need print?
  ElementNames[[1]]
  #clear the decks for the next run
  rm(list=ls(all=TRUE))

#end the loop
}

