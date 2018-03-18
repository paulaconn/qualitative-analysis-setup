library(data.table)
library(reshape2)

##################
# Functions
##################
splitByExposure <- function(data, value){
  df <- data.table(data[which(data$Exposure.to.an.individual.with.disabilities == value),])
  df <-  df[ , "TotalTag" := .N , by = c("Consensus.Tag") ]
  df <-  df[ , "TotalTagPhase" := .N , by = c("Consensus.Tag", "Phase")]
  df <- data.frame(df)
  return(df)
}

tagCtByTeams <- function(table, condition, df){
    for (i in 1:nrow(table)){
      tag <- as.character(table$tag[i])
      table[which(table$tags == tag), which(colnames(table) == condition)] <- length(
        unique(df[which(df$Consensus.Tag == tag), which(colnames(df) == "TeamID")]))
    }
    return(table)
}

totalPhase <- function(df, tag, phase, isUnique, isTeam){
  if(isTeam){
    return(length(unique(df[
      which(df$Consensus.Tag == tag & df$Phase == phase), 
      which(colnames(df) == "TeamID")])))
  }
  if (!isTeam){
    if (isUnique)
      return(length(unique(df[which(df$Consensus.Tag == tag & df$Phase == phase), 
                              which(colnames(df) == "Quote.ID")])))
    else
      return(nrow(df[which(df$Consensus.Tag == tag & df$Phase == phase), ]))    
  }
}

tagByPhase <- function(table, noExposure_df, Exposure_df, isUnique, isTeam){
  for (i in 1:nrow(table)){
    tag <- as.character(table$Tag[i])
    table[which(table$Tags == tag),]$DesignNE <- totalPhase(noExposure_df, tag, "Design", isUnique, isTeam)
    table[which(table$Tags == tag),]$DesignE <- totalPhase(Exposure_df, tag, "Design", isUnique, isTeam)
    table[which(table$Tags == tag),]$PrototypeNE <- totalPhase(noExposure_df, tag, "Prototype", isUnique, isTeam)
    table[which(table$Tags == tag),]$PrototypeE <- totalPhase(Exposure_df, tag, "Prototype", isUnique, isTeam)
    table[which(table$Tags == tag),]$FinalNE <- totalPhase(noExposure_df, tag, "Final", isUnique, isTeam)
    table[which(table$Tags == tag),]$FinalE <- totalPhase(Exposure_df, tag, "Final", isUnique, isTeam)
  }
  return(table)
}

##################
# Main Execution
##################
setwd("~/Desktop/qualitative-analysis-setup/")
allData <- read.csv(file="input/QualitativeCoding2018.csv", header = TRUE, sep = ",")
docBins <- read.csv(file="input/DocBins.csv", header = TRUE, sep = ",")
isUnique <- FALSE

# Setup data for ASSETS 19 paper
allData$Students.asked.to.OR.considered.accessibility.in.their.project <- as.character(
  allData$Students.asked.to.OR.considered.accessibility.in.their.project)
allData <- allData[which(allData$Lectures.on.accessibility == 'Y' &
                           allData$Homework.in.assistive.technology == 'N') ,]
allData["Phase"] <-  docBins$Phase
allData <- allData[which(allData$Phase!="NP"),]

# Generate unique team IDs
allData <- transform(allData, TeamID = as.numeric(interaction(Year, Instructor, 
                                                              Team.name, drop=TRUE)))
allData <- allData[with(allData, order(Year,Instructor,Team.name)),]

# Provide counts for each phase (design, prototype, final design)
allData <- data.table(allData)
allData <-  allData[ , "InstancesByTeamPhase" := .N , by = c("TeamID" , "Phase") ]
allData <-  allData[ , "TagsByTeamPhase" := .N , by = c("TeamID" , "Phase", "Consensus.Tag") ]
allData <-  allData[ , "TagRepetitionsByQuote" := .N , by = c("TeamID" , "Phase", 
                                                              "Consensus.Tag", "Quote.ID") ]
allData <-  allData[ , "QuoteRepetitions" := .N , by = c("TeamID", "Quote.ID") ]
allData <- data.frame(allData)
write.csv(allData, file="output/ASSETS2019.csv")

# Generate the exposure and no exposure groups
exposure <- splitByExposure(allData, "Y")
noExposure <- splitByExposure(allData, "N")
write.csv(exposure, file="output/exposure-ASSETS2019.csv")
write.csv(noExposure, file="output/noExposure-ASSETS2019.csv")

# Create a table for tag count summary
noExposureTags <- data.frame(table(noExposure$Consensus.Tag))
exposureTags <- data.frame(table(exposure$Consensus.Tag))
tagSummary <- cbind(noExposureTags, Exposure=exposureTags$Freq)
colnames(tagSummary) <- c("Tag", "NoExposure", "Exposure") 
rm(noExposureTags, exposureTags, docBins)

# Create a table for tag team count summary
tags <- c(as.character(unique(allData$Consensus.Tag)))
teamTagSummary <- data.frame(tags)
teamTagSummary["NoExposure"] <- NA
teamTagSummary["Exposure"] <- NA
teamTagSummary <- tagCtByTeams(teamTagSummary, "Exposure", exposure)
teamTagSummary <- tagCtByTeams(teamTagSummary, "NoExposure", noExposure)

# Create a table for tag subpopulation summary
populationNE <- data.frame(table(noExposure$Final.Population.Subcategory))
populationE <- data.frame(table(exposure$Final.Population.Subcategory))
tagPopulationSummary <- cbind(populationNE, Exposure=populationE$Freq)
colnames(tagSummary) <- c("Population", "NoExposure", "Exposure")
rm(populationNE, populationE)

# Create a table for target market summary
targetMarketNE <- data.frame(table(noExposure[which(
  noExposure$Consensus.Tag=="Target market includes a group requiring accessibility"), 
  which(colnames(noExposure)=="Final.Population.Subcategory")]))
targetMarketE <- data.frame(table(exposure[which(
  exposure$Consensus.Tag=="Target market includes a group requiring accessibility"), 
  which(colnames(exposure)=="Final.Population.Subcategory")]))
targetMarketSummary <- cbind(targetMarketNE, Exposure=targetMarketE$Freq)
colnames(tagSummary) <- c("Market", "NoExposure", "Exposure")
rm(targetMarketNE, targetMarketE)

# Calculate tables for each stage (design, prototype, final)
PhaseTagSummary <- data.frame(Tags = tags, 
                              DesignNE = rep(NA, len = length(tags)),
                              DesignE = rep(NA, len = length(tags)),
                              PrototypeNE = rep(NA, len = length(tags)),
                              PrototypeE = rep(NA, len = length(tags)),
                              FinalNE = rep(NA, len = length(tags)),
                              FinalE = rep(NA, len = length(tags)))
PhaseTeamSummary <- PhaseTagSummary
PhaseTeamSummary <- tagByPhase(PhaseTeamSummary, noExposure, exposure, isUnique, TRUE)
PhaseTagSummary <- tagByPhase(PhaseTagSummary, noExposure, exposure, isUnique, FALSE)
rm(tags)

# Provide totals
print("Number of Unique Exposure and No Exposure Teams with Accessibility Mentions:")
cat(paste0("No Exposure: ", length(unique(noExposure$TeamID)), "\t\t"))
cat(paste0("Exposure: ", length(unique(exposure$TeamID)), "\n\n"))
print("Unique Quotes for Exposure and No Exposure:")
cat(paste0("No Exposure: ", length(unique(noExposure$Quote.ID)), "\t\t"))
cat(paste0("Exposure: ", length(unique(exposure$Quote.ID)), "\n\n"))
print("Unique Quotes For Each Stage:")
cat("No Exposure: \n")
cat(sapply(unique(allData$Phase), function(x) paste0(x, ": ", length(unique(noExposure[
  which(noExposure$Phase == x), which(colnames(noExposure) == "Quote.ID")])), "\t")))
cat("\nExposure: \n")
cat(sapply(unique(allData$Phase), function(x) paste0(x, ": ", length(unique(exposure[
  which(exposure$Phase == x), which(colnames(exposure) == "Quote.ID")])), "\t")))
cat("\n\n")
print("Tags For Each Stage:")
cat("No Exposure: \n")
cat(sapply(unique(allData$Phase), function(x) paste0(x, ": ", length(noExposure[
  which(noExposure$Phase == x), which(colnames(noExposure) == "Consensus.Tag")]), "\t")))
cat("\nExposure: \n")
cat(sapply(unique(allData$Phase), function(x) paste0(x, ": ", length(exposure[
  which(exposure$Phase == x), which(colnames(exposure) == "Quote.ID")]), "\t")))


# Useful testing scripts
temp <- noExposure[which(noExposure$Phase == "Final") ,]
temp <- dcast(temp, TeamID ~ Consensus.Tag)
#print(noExposure[which(noExposure$Consensus.Tag=="Students explicitly discuss accessibility"),]$Quote)
# temp <- dcast(noExposure, TeamID ~ Consensus.Tag)