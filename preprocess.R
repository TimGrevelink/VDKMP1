library(data.table)
library(DT)
library(readxl)

cat("\n", file = "Quickscan VBHC.csv", append = TRUE)
data = as.data.table(read.csv("Quickscan VBHC.csv", header = T, sep = ",", check.names=FALSE, colClasses = "character"))

mappingAnswers <- as.data.table(read_xlsx("mapping.xlsx", sheet = "Blad2"))
domainQuestions <- as.data.table(read_xlsx("mapping.xlsx", sheet = "Blad1"))

for (i in mappingAnswers$Antwoord){
  replaceValue <- mappingAnswers[Antwoord == i, Score]
  data[data == i] <- as.numeric(replaceValue)
}

data = data[, Tijdstempel := as.POSIXct(Tijdstempel)]
dataT <- data.table(t(data))
dataT = dataT[, questions := colnames(data)]
colnames(dataT) <- c(data$Gebruikersnaam, "questions")
columnsToNumeric = colnames(dataT)[colnames(dataT) %in% data$Gebruikersnaam]
domainQuestions[, koppelKolom:= gsub(" ", "", Vraag, fixed = TRUE)]
dataT <- dataT[, koppelKolom:= gsub(" ", "", questions, fixed = TRUE)]
dataT = data.table(merge(dataT, domainQuestions, by.x = "koppelKolom", by.y = "koppelKolom"))[, `:=` (questions = NULL,
                                                                                                                 koppelKolom = NULL,
                                                                                                                 Vraag = NULL)]
dataT = dataT[, (columnsToNumeric) := lapply(.SD, as.numeric), .SDcols = columnsToNumeric] 

verwijdert <- nrow(domainQuestions) - nrow(dataT)
if (verwijdert > 0) {
  warning(paste0("Er zijn ", verwijdert, " vragen verwijdert, doordat hier geen score aan gekoppeld kan worden. Controleer dit bij Tim."))
}
domainQuestions = domainQuestions[, `:=` (maxScore = 5,
                                          minScore = 1)]
info <- domainQuestions[, .(maxTotaalScore = sum(maxScore),
                            minTotaalScore = sum(minScore)), by = Domein]
scores <- dataT[, lapply(.SD, sum, na.rm=TRUE) , by = Domein]

dt <- merge(scores, info, by.x = "Domein", by.y = "Domein")

cols <- c("Domein", paste0("percentage_", columnsToNumeric))
dtPerc <- dt[, c(paste0("percentage_", columnsToNumeric)) := lapply(.SD, function(x){
  (x - minTotaalScore)/ maxTotaalScore *100
}), .SDcols = columnsToNumeric][,..cols]

dtPerc = data.table(t(dtPerc))
colnames(dtPerc) <- unique(dt$Domein)
dtPerc = dtPerc[-1,][, Gebruikersnaam := columnsToNumeric]

# Tabel met percentages maken
# Tabel met std afwijkingen en andere info
# gemiddelde van de totale groep uitrekenen met std afw
