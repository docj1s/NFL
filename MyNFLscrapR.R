getTeamStandings1 <- function(outputDir)
{
    require(rvest)
    require(data.table)
    require(stringr)
    url <- "https://www.espn.com/nfl/standings"
    
    webPage <- read_html(url)
    tbls <- html_nodes(webPage, 'table')
    return(tbls)
    
    finalDF <- as.data.frame(list())
    
    for(i in seq(from=1, to=length(tbls), by=4))
    {
        #df <- html_table(tbl, fill=TRUE) 
        df1 <- html_table(tbls[i+1], fill=TRUE, header=TRUE) 
        df2 <- html_table(tbls[i+3], fill=TRUE, header=TRUE) 
        
        df <- cbind(as.data.frame(df1), df2)
        names(df) <- c("Team", "Wins", "Losses", "Ties", "WinPct", "Home-Record", "Road-Record", "Div", "Conf", "PF", "PA", "Pt-Diff", "Streak")
        
        finalDF <- rbind(finalDF, df)
    }
    
    finalDF$Team <- sub("^z -|x -|\\* -", "", finalDF$Team) #remove playoff identifiers
    
    i<-1
    for(team in finalDF$Team)
    {
        #df$Team[i] <- substr(team, str_locate(team, "[A-Z][A-Z]*$")[1,1], str_locate(team, "[A-Z][A-Z]*$")[1,2])
        
        s <- regmatches(m=regexpr("^[A-Z]{1,4}", team), x=team)
        finalDF$Team[i] <- substr(s, 1, nchar(s)-1)
        
        i <- i+1
    }
    
    outputfile <- paste(outputDir, "TeamStandings.csv", sep="/")
    write.csv(finalDF, file=outputfile, row.names = FALSE)
    
}
getTeamStandings <- function(outputDir)
{
    require(rvest)
    require(data.table)
    require(stringr)
    url <- "https://www.espn.com/nfl/standings"
    #url <- "https://www.espn.com/nfl/standings/_/group/league"
    #url <- "https://sports.yahoo.com/nfl/standings/"
    
    webPage <- read_html(url)
    tbls <- html_nodes(webPage, 'table')
    
    finalDF <- as.data.frame(list())
    for(tbl in tbls)
    {
       df <- html_table(tbl, fill=TRUE) 
       #df <- html_table(tbl, fill=TRUE, header=TRUE) 
       names(df) <- c("Team", "Wins", "Losses", "Ties", "WinPct", "Home-Record", "Road-Record", "Div", "Conf", "PF", "PA", "Pt-Diff", "Streak")
       #names(df) <- c("Team", "Wins", "Losses", "Ties", "Win-Pct", "PF", "PA", "Pt-Diff", "Home-Record", "Road-Record", "Div", "Conf", "Last-5", "Streak")
       #names(df) <- c("Team", "Wins", "Losses", "Ties", "Win-Pct")
       
       i <- 1
       for(team in df$Team)
       {
           df$Team[i] <- substr(team, str_locate(team, "[A-Z][A-Z]*$")[1,1], str_locate(team, "[A-Z][A-Z]*$")[1,2])
           i <- i+1
       }
       
       if(i==1) 
       {
           next
           
       }
       finalDF <- rbind(finalDF, df)
    }
    
    outputfile <- paste(outputDir, "TeamStandings.csv", sep="/")
    write.csv(finalDF, file=outputfile, row.names = FALSE)
    
}

getTeamData <- function(outputDir)
{
    require(rvest)
    require(data.table)
    urlPath <- "http://www.espn.com/nfl/team/stats/_/type/team/name/"
    teams <- c("ari","atl","bal","buf","car","chi","cin","cle","dal","den","det","gb","hou","ind","jax","kc","lac","lar","mia","min","ne","no","nyg",
                 "nyj","oak","phi","pit","sf","sea","tb","ten", "wsh")
    
    for(team in teams)
    {
        url <- paste(urlPath, team, sep="")
        webPage <- read_html(url)
        tbls <- html_nodes(webPage, 'table')
        
       outputfile <- paste(outputDir, team, sep="")
       outputfile <- paste(outputfile, ".csv", sep="")
       
       df <- as.data.frame(list())
       for(tbl in tbls)
       {
           data <- html_table(tbl, fill=TRUE)
           tidydf <- createTidy(data)
           if(nrow(df) == 0)
           {
               df <- tidydf
           }
           else
           {
               df <- cbind(df, tidydf[2:ncol(tidydf)])
           }
           
           #print(html_table(tbl, fill=TRUE))
           #write.table(html_table(tbl, fill=TRUE), file=outputfile, append=TRUE)
           #export(data, outputfile)
           
           #fwrite(html_table(tbl, fill=TRUE), file=outputfile, append=TRUE, sep=",")
       }
       
       colnames(df)[1] <- "TEAM"
       write.csv(df, file=outputfile, row.names = FALSE)
       #return(df)
    }
}

createMatchup <-function(dir, homeTeam, roadTeam)
{
   file <- paste(homeTeam, roadTeam, sep="-")
   
   homeTeamFile <- paste("tidy", paste(homeTeam, "csv", sep="."), sep="/")
   homeTeamDF <- read.csv(paste(dir, homeTeamFile, sep="/" )) 
   
   roadTeamFile <- paste("tidy", paste(roadTeam, "csv", sep="."), sep="/")
   roadTeamDF <- read.csv(paste(dir, roadTeamFile, sep="/" )) 
   
    
}

createTidy<-function(df)
{
   #df <- as.data.frame(data)
   #names(df) <- df[2, ]
   if(nrow(df) == 5)
   {
       df <- df[-1, ]
   }
    
   names(df) <- paste(df[1, ], df[2, ], sep="-")
   df <- df[-c(1, 2), ]
   
   #remove NAs
   df <- df[colSums(!is.na(df)) > 0]
   
   df
}

createGameMatchup <- function()
{
   names <- c("teamname", "wins", "losses", "home-wins", "home-losses", "home-ties", "road-wins", "road-losses", "road-ties",
              "home-pt-diff", "road-pt-diff", "pts-forced", "pts-allowed", "off-rush", "def-rush", "off-pass", "def-pass", "3rd-downs", "3rd-downs-allowed",
              "sacks", "sacks-allowed", "turnovers", "turnovers-forced", "fg%", "punt-avg", "kickoff-return-avg", "penalty", "penalty-yds") 
}

tidyStandings <- function(dir)
{
    inFile <- paste(dir, "TeamStandings.csv", sep="/")
    df <- read.csv(inFile)
    
    for(i in 1:length(df$Home.Record))
    {
        myList <- strsplit(as.character(df$Home.Record[i]), "-")
        sVec <- unlist(myList)
        
        df$Home.Wins[i] <- sVec[1]
        df$Home.Losses[i] <- sVec[2]
        df$Home.Ties[i] <- 0
        if(length(sVec) == 3)
        {
            df$Home.Ties[3] <- 0
        }
            
    }
    
    for(i in 1:length(df$Road.Record))
    {
        myList <- strsplit(as.character(df$Road.Record[i]), "-")
        sVec <- unlist(myList)
        
        df$Road.Wins[i] <- sVec[1]
        df$Road.Losses[i] <- sVec[2]
        df$Road.Ties[i] <- 0
        if(length(sVec) == 3)
        {
            df$Road.Ties[3] <- 0
        }
    }
    
    for(i in 1:length(df$Div))
    {
        myList <- strsplit(as.character(df$Div[i]), "-")
        sVec <- unlist(myList)
        
        df$Div.Wins[i] <- sVec[1]
        df$Div.Losses[i] <- sVec[2]
        df$Div.Ties[i] <- 0
        if(length(sVec) == 3)
        {
            df$Div.Ties[3] <- 0
        }
    }
    
    for(i in 1:length(df$Conf))
    {
        myList <- strsplit(as.character(df$Conf[i]), "-")
        sVec <- unlist(myList)
        
        df$Conf.Wins[i] <- sVec[1]
        df$Conf.Losses[i] <- sVec[2]
        df$Conf.Ties[i] <- 0
        if(length(sVec) == 3)
        {
            df$Conf.Ties[3] <- 0
        }
    }
    
    ignoreNames <- c("Home.Record", "Road.Record", "Div", "Conf")
    df <- df[!names(df) %in% ignoreNames]
    
    outputfile <- paste(dir, "TeamStandingsTidy.csv", sep="/")
    write.csv(df, file=outputfile, row.names = FALSE)
}