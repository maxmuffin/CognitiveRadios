
library("rjson")
library("xlsx")
# TEMPO SIMULAZIONE:  25000s
# TRANSIENTE:         3600s
# RUNS:                  20


# FUNCTIONS
medie_runs <- function(my_list){
  # mean of every run
  medie <- list()
  for (i in 1:20) {
    # print(mean(my_list[[i]]))
    medie[[length(medie)+1]] <- mean(my_list[[i]]) 
  }
  return(medie)
}

medie_dataframe <- function(nameFile, mediaRun){
  medieDT <- data.frame(mean(unlist(mediaRun, use.names = FALSE)), mediaRun)
  rownames(medieDT) <- nameFile
  colnames(medieDT) <- c("MediaMedie","M01","M02","M03","M04","M05","M06","M07","M08","M09","M10","M11","M12","M13","M14","M15","M16","M17","M18","M19","M20")
  return(medieDT)
}

save_config_xlsx<-function(pathDirectoryM, mediePrimaryQueueLenght, mediePrimaryQueueWaitingTime, medieSecondaryQueueLenght, medieSecondaryQueueWaitingTime, medieTime){
  dataConfigurazione <- data.frame()
  dataConfigurazione <- rbind(dataConfigurazione, mediePrimaryQueueLenght)
  dataConfigurazione <- rbind(dataConfigurazione, mediePrimaryQueueWaitingTime)
  dataConfigurazione <- rbind(dataConfigurazione, medieSecondaryQueueLenght)
  dataConfigurazione <- rbind(dataConfigurazione, medieSecondaryQueueWaitingTime)
  dataConfigurazione <- rbind(dataConfigurazione, medieTime)
  rownames(dataConfigurazione) <- c("PrimaryQueue - Lenght","PrimaryQueue - Waiting time","SecondaryQueue - Lenght", "SecondaryQueue - Waiting time","Time")
  
  setwd(dir = pathDirectoryM)
  outputFile <- paste0(paste0(nameFile, ".xlsx"))
  write.xlsx(dataConfigurazione, outputFile,sheetName = "punto_a_b")
  }

save_ic <- function(pathDirectoryIC, ic_primary_queue_lenght, ic_primary_queue_waiting_time, ic_secondary_queue_lenght, ic_secondary_queue_waiting_time, ic_time){
  my_index_primary_queue_lenght = do.call(rbind, ic_primary_queue_lenght)
  my_index_primary_queue_waiting_time = do.call(rbind, ic_primary_queue_waiting_time)
  my_index_secondary_queue_lenght = do.call(rbind, ic_secondary_queue_lenght)
  my_index_secondary_queue_waiting_time = do.call(rbind, ic_secondary_queue_waiting_time)
  my_index_time = do.call(rbind, ic_time)
  
  setwd(dir = pathDirectoryIC)
  outputFile <- "intervalli_di_confidenza.xlsx"
  write.xlsx(my_index_primary_queue_lenght, outputFile, sheetName = "PrimaryQueueLenght", append = TRUE)
  write.xlsx(my_index_primary_queue_waiting_time, outputFile, sheetName = "PrimaryQueueWaitingTime", append = TRUE)
  write.xlsx(my_index_secondary_queue_lenght, outputFile, sheetName = "SecondaryQueueLenght", append = TRUE)
  write.xlsx(my_index_secondary_queue_waiting_time, outputFile, sheetName = "SecondaryQueueWaitingTime", append = TRUE)
  write.xlsx(my_index_time, outputFile, sheetName = "time", append = TRUE)

}

calcolo_length_list <- function(my_list){
  # verify the length of list
  if(length(my_list) < 20){
    while (length(my_list) < 20) {
      my_list[[length(my_list)+1]] <- as.double(0)
    }
  }
  return(my_list)
}

intervalli_di_confidenza <- function(nameFile, mediaRuns){
  M <- 20
  t_95 <- 2.093
  t_90 <- 1.729
  
  x <- unlist(mediaRuns, use.names = FALSE)
  media <- mean(x)
  varianza <- var(x)
  deviazione_standard <- sqrt(varianza)
  errore_standard_media <- deviazione_standard/sqrt(M)
  
  min <- media - (t_95 * errore_standard_media)
  max <- media + (t_95 * errore_standard_media)
  dat_95 <- data.frame(min_95 = min, max_95 = max)
  
  min <- media - (t_90 * errore_standard_media)
  max <- media + (t_90 * errore_standard_media)
  dat_90 <- data.frame(min_90 = min, max_90 = max)
  
  dat <- data.frame(nameFile, dat_90, dat_95, mean = media, varianza = varianza, ds = deviazione_standard, ES_media = errore_standard_media, tS_per_ES_90 = (t_90 * errore_standard_media),tS_per_ES_95 =(t_95 * errore_standard_media))
  
  return(dat)
}


# pathDirectory: directory contenente i dati da analizzare
# pathDirectoryIC: directory dove salvare i file xlsx relativi agli intervalli di confidenza
# pathDirectoryM: directory dove salvate i file xlsx relativi alle medie 
pathDirectory <- "C:\\Users\\massi\\Desktop\\Simulazione di Sistemi\\Json\\"
pathDirectoryIC <- "C:\\Users\\massi\\Desktop\\Simulazione di Sistemi\\XLSX\\Intervalli_confidenza\\"
pathDirectoryM <- "C:\\Users\\massi\\Desktop\\Simulazione di Sistemi\\XLSX\\"
# find file .json on directory: "pathDirectory"
files <- list.files(path = pathDirectory, pattern = "\\.json")

# List IC
ic_primary_queue_lenght <- list()
ic_primary_queue_waiting_time <- list()
ic_secondary_queue_lenght <- list()
ic_secondary_queue_waiting_time <- list()
ic_time <- list()

# START
start_run <- Sys.time()

# for every file finded
for (my_file in files) {
  # read the Json File 
  #my_file <- files[1]
  my_file_name <- paste0(pathDirectory, my_file)
  print(my_file_name)
  JsonData <- fromJSON(file = my_file_name)
  
  
  # file name
  nameFile <- substr(my_file, start = 1, stop = 19)
  print(nameFile)
  
  # remove reduntant data for vectorial data
  vectorsinfo <- JsonData[[1]][["vectors"]]
  
  # remove reduntant data for scalar data
  scalarsinfo <- JsonData[[1]][["scalars"]]

  listPrimaryQueueLenght <- list()
  listPrimaryQueueWaitingTime <- list()
  listSecondaryQueueLenght <- list()
  listSecondaryQueueWaitingTime <- list()
  listTime <- list()
  
  # analysis the list for every Run: Lenght [PrimaryQueue, SecondaryQueue, time]
  for (i in 1:length(vectorsinfo)) {
    module <- vectorsinfo[[i]][["module"]]
    time <- vectorsinfo[[i]][["time"]]
    value <- vectorsinfo[[i]][["value"]]
  
    
    # add runs to the List
    if(module == "Cognitiveradios.PrimaryQueue"){
      listPrimaryQueueLenght[[length(listPrimaryQueueLenght)+1]] <- as.double(value)
    }else if(module == "Cognitiveradios.SecondaryQueue"){
      listSecondaryQueueLenght[[length(listSecondaryQueueLenght)+1]] <- as.double(value)
    }else{
      listTime[[length(listTime)+1]] <- as.double(value)
    }
  }
  
  # analysis the list for every Run: Lenght [PrimaryQueueWaitingTime, SecondaryQueueWaitingTime]
  for (k in 1:length(scalarsinfo)) {
    module2 <- scalarsinfo[[k]][["module"]]
    name <- scalarsinfo[[k]][["name"]]
    value2 <- scalarsinfo[[k]][["value"]]
    
    
    # add runs to the List
    if(module2 == "Cognitiveradios.PrimaryQueue"){
      if(name == "queueingTime:mean"){
        listPrimaryQueueWaitingTime[[length(listPrimaryQueueWaitingTime)+1]] <- as.double(value2)
      }
    }else if(module2 == "Cognitiveradios.SecondaryQueue"){
      if(name == "queueingTime:mean"){
        listSecondaryQueueWaitingTime[[length(listSecondaryQueueWaitingTime)+1]] <- as.double(value2)
      }
    }
  }

  
  # verify the length of list
  listPrimaryQueueLenght <- calcolo_length_list(listPrimaryQueueLenght)
  listSecondaryQueueLenght <- calcolo_length_list(listSecondaryQueueLenght)
  listPrimaryQueueWaitingTime <- calcolo_length_list(listPrimaryQueueWaitingTime)
  listSecondaryQueueWaitingTime <- calcolo_length_list(listSecondaryQueueWaitingTime)
  listTime <- calcolo_length_list(listTime)
  print(paste("PrimaryQueueLenght:", length(listPrimaryQueueLenght), "PrimaryQueueWT:", length(listPrimaryQueueWaitingTime), "SecondaryQueueLenght:", length(listSecondaryQueueLenght), "SecondaryQueueWT:", length(listSecondaryQueueWaitingTime), "TIME:", length(listTime), sep="   "))
  
  #################################
  # analysis list [PrimaryQueueLenght, PrimaryQueueWaitingTime, SecondaryQueueLenght, SecondaryQueueWaitingTime, time]
  
  # PRIMARY QUEUE LENGHT #
  mediaRunPrimaryQueueLenght <- medie_runs(listPrimaryQueueLenght)
  ic_primary_queue_lenght[[length(ic_primary_queue_lenght) + 1]] <- intervalli_di_confidenza(nameFile, mediaRunPrimaryQueueLenght)
  mediePrimaryQueueLenght <- medie_dataframe(nameFile, mediaRunPrimaryQueueLenght)
  # PRIMARY QUEUE WAITING TIME #
  mediaRunPrimaryQueueWaitingTime <- medie_runs(listPrimaryQueueWaitingTime)
  ic_primary_queue_waiting_time[[length(ic_primary_queue_waiting_time) + 1]] <- intervalli_di_confidenza(nameFile, mediaRunPrimaryQueueWaitingTime)
  mediePrimaryQueueWaitingTime <- medie_dataframe(nameFile, mediaRunPrimaryQueueWaitingTime)
  # SECONDARY QUEUE LENGHT#
  mediaRunSecondaryQueueLenght <- medie_runs(listSecondaryQueueLenght)
  ic_secondary_queue_lenght[[length(ic_secondary_queue_lenght) + 1]] <- intervalli_di_confidenza(nameFile, mediaRunSecondaryQueueLenght)
  medieSecondaryQueueLenght <- medie_dataframe(nameFile, mediaRunSecondaryQueueLenght)
  # SECONDARY QUEUE WAITING TIME#
  mediaRunSecondaryQueueWaitingTime <- medie_runs(listSecondaryQueueWaitingTime)
  ic_secondary_queue_waiting_time[[length(ic_secondary_queue_waiting_time) + 1]] <- intervalli_di_confidenza(nameFile, mediaRunSecondaryQueueWaitingTime)
  medieSecondaryQueuewaitingTime <- medie_dataframe(nameFile, mediaRunSecondaryQueueWaitingTime)
  # TIME #
  mediaRunTime <- medie_runs(listTime)
  ic_time[[length(ic_time) + 1]] <- intervalli_di_confidenza(nameFile, mediaRunTime)
  medieTime <- medie_dataframe(nameFile, mediaRunTime)
  
  
  # save the dataframe of the mean into xlsx file
  save_config_xlsx(pathDirectoryM, mediePrimaryQueueLenght, mediePrimaryQueueWaitingTime, medieSecondaryQueueLenght, medieSecondaryQueuewaitingTime, medieTime)
  ################################
  cat(c("MEDIE =>\t","PrimaryQueueLenght: ", mediePrimaryQueueLenght[[1]],"\tPrimaryQueueWaitingTime: ",mediePrimaryQueueWaitingTime[[1]],"\tSecondaryQueueLenght: ",medieSecondaryQueueLenght[[1]],"\tSecondaryQueueWaitingTime: ",medieSecondaryQueuewaitingTime[[1]],"\tTIME: ", medieTime[[1]],"\n"))
 
  cat('\n')# print new line
}

save_ic(pathDirectoryIC, ic_primary_queue_lenght, ic_primary_queue_waiting_time, ic_secondary_queue_lenght, ic_secondary_queue_waiting_time, ic_time)

# STOP
stop_run <- Sys.time()
cat('\n\n')
print(stop_run - start_run)