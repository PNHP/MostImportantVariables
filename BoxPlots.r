#get data and create box plots for

library(RSQLite)
#  open up a "channel" to the Access DB
databasename <- "E:/SDM/Shared/MostImportantVariables/MostImportantVar.sqlite" 
db <- dbConnect(SQLite(), dbname = databasename)

#write importance values
###importance <- sqlQuery(channel = Cn.MDB.out, query = "SELECT * FROM tblImportance")
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

png(filename="E:/SDM/Shared/MostImportantVariables/boxplot.png", width=10, height=10, units='in', res=600)
boxplot(impRank ~ varFullName, data=importance,
        xlab = "Importance Rank", 
        #ylab = "Environmental Variable",  
        boxfill="white", notch=FALSE,
          horizontal = TRUE,
          show.names = TRUE,      #not sure I need this
          las=2,                  #make the category names horizontal
          par(
           mar=c(1,1,1,1), #increase margins on the left
           ps=8 ,                 #font point size
           pin=c(5,9)            #plot dimensions width,height (might need to resize window manually, or use (5,6)
          )#,
          #mgp = c(5,1,5)
          )
dev.off()
