library(readxl)
library(data.table)
#library(stringr)

xlsx_file<-"S:/DATA MANAGEMENT/@Data Science/BioCelerate/SEND Harmonization/Data/metadata/CrossStudyAnalysisQuestionsforBackgroundControlDataUseCase.xlsx"

# import list of epochs and set col Manual Assigned to 'Uncertain' where not assigned
allEpochs<-data.table(read_xlsx(xlsx_file, range = "Q9 Study Phase!A61:D210"))
allEpochs[, `Manually Assigned` := ifelse(is.na(`Manually Assigned`),'Uncertain' ,`Manually Assigned`)]

# Coåy of phase detection fuctiuon from filterFindingsPhase.R
getPhase<-function(epoch) {
  if (is.na(epoch) ) {
    'Uncertain'
  } else if (grepl("pre.*(tr(ea)?t|dos|test|study|exposure)|acclimat|screen|baseline|allocat|random",
             epoch, 
             ignore.case = TRUE)) {
    'Screening' 
  } else if (grepl("recovery|post.*(tr(ea)?t|dos|test|study|exposure)", 
                   epoch,
                   ignore.case = TRUE)) {
    'Recovery' 
  } else if (  grepl("tr(ea)?t|dos|test|exposure",
                     epoch, 
                     ignore.case = TRUE)
             & !grepl("off|non|free|holiday",
                      epoch, 
                      ignore.case = TRUE)) {
    'Treatment' 
  } else {
    'Uncertain'
  }
}

# Apply detection of phase
allEpochs[, DetectedPhase := mapply(getPhase,Epoch )]

# Hit ratio - detected equal to manual assigned phase:
sum(allEpochs[`Manually Assigned` == DetectedPhase, .(Frequency)])/sum(allEpochs[, .(Frequency)])

# Show differences - detected contra manual assigned phase:
allEpochs[`Manually Assigned` != DetectedPhase]
