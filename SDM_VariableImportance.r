
# Separate run through of existing RF runs to extract
# and then explore the relative importance of different layers.
#Question: are there any variables that consistently sort at the
#bottom that can/should be removed from the analyses?

# originally coded by Tim Howard at some point in the past, heavily modified by Christopher Tracey in 2018
# 2019-02-19 - modify to current modeling code

library(RSQLite)
library(randomForest)

# First, change to the directory
setwd("E:/SDM/Shared/MostImportantVariables/mussels") # set to the SDM output directory
# Second, define an empty SQLlite db
databasename <- "E:/SDM/Shared/MostImportantVariables/MostImportantVar_Mussels.sqlite" # this should be an empty sqlite database, script will create and populate the table
# Third, set an output name for png file for the boxplots, this will be saved in the working directory
boxfile <- "PA_mussels" # .png will be appended by script

#get a list of what's in the directory
d <- dir(pattern="Rdata", recursive=TRUE, include.dirs=TRUE)  # modified to deal with storing Rdata in species specific subfolders

#loop through everything in the dir
for (i in 1:length(d)){
  fileName <- d[[i]]
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
  db <- dbConnect(SQLite(), dbname = databasename) 
  dbWriteTable(db, "tblImportance", f.importance, append=TRUE)   #write importance values
  dbDisconnect(db) #close connection
  #remind me what I just ran -- doesn't work, need print?
  ElementNames[[1]]
  #clear the decks for the next run
  #rm(list=ls(all=TRUE))
#end the loop
}


#get data and create box plots for variable importance across a set of models
db <- dbConnect(SQLite(), dbname = databasename)
#write importance values
SQLquery_imp <- paste("SELECT * FROM tblImportance")
importance  <- dbGetQuery(db, statement=SQLquery_imp )
dbDisconnect(db) #close connection

##means <- sqlQuery(channel = Cn.MDB.out, query = "select * from qryAverageImportance")
means <- aggregate(importance$impRank ,by=list(varFullName=importance$varFullName),FUN=mean)
means <- means[order(-means$x),]

#the order of the boxes in the boxplot is based on the order of the factors.
#change the order based on the ascending order of the means, from the means table
importance$varFullName <- factor(importance$varFullName,levels=means$varFullName)
#extract the number of models that used each variable
sampSize <- table(importance$varCode)

png(filename=paste(boxfile,"_boxplot.png",sep=""), width=10, height=10, units='in', res=600)
boxplot(impRank ~ varFullName, data=importance,
        # xlab = "Importance Rank",   
        # ylab = "Environmental Variable",
        boxfill="white", notch=FALSE,
        horizontal = TRUE,
        show.names = TRUE,      
        las=2,                  #make the category names horizontal
        par(
          mar=c(1,1,1,1), #increase margins on the left
          ps=6,                 #font point size
          pin=c(5,9)            #plot dimensions width,height (might need to resize window manually, or use (5,6)
        )
)
dev.off()

