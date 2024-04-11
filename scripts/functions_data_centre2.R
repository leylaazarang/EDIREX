# Functions to be used

# Function to read data from each sheet in an excel file
# Written to be used with lapply
# I transform the output into a data.frame because of the operations that need to be done.
# @Leyla: please feel free to leave this out if you can make the operations using dplyr!

# Inputs
# x: numeric (scalar), containing the index of `all.sheets` to be used as name
#    of the sheet which will be read in
# myfile: string, the name of the excel file to be read in. It is assumed
#         that the file is stored in a subfolder called `data`
# all.sheets: a vector (character) containing the names of the sheets in the file
# Output
# A data.frame containing the data read in

read_sheet <- function(x, myfile, all.sheets){
    tb= readxl::read_excel(paste("data/", myfile, sep = ""), 
                           col_names=TRUE, sheet = all.sheets[x]) %>%
        filter(Label != "Waste") 
    mydata <- as.data.frame(tb)
    return(mydata)}




# Function to extract the data and put it into long format, per sheet
# Written to be used with lapply
# Inputs
# xi: numeric (scalar), the number of the item in the list `mydatac` to be selected;
#     typically this corresponds to the number of the sheet to be selected
# mydatac: a list, with each element corresponding to data read in from a sheet in
#          the original excel file
# Output
# A tibble with the long-format data read in
# this function is used with lapply, that returns a list of data frame 
#  corresponding to the centers.

get_data_long <- function(xi, mydatac)
{ 
    mydata <- mydatac[[ xi ]]
    # Models to be handled separately
    models.o <- c("BRC0010_BBK507", "BRC0007_BBK567")
    if(!(mymodel  %in% models.o) ){ 
        mytreat1 <- sapply(1:nrow(mydata), mysplit, char.vector = mydata$Label,
                           split.by = "---", result.sel = 2)
        mytreat1 <- str_trim(mytreat1)
        mytreat <- substr(mytreat1, start = 1, stop = 6)
        mydate1 <- str_trim(sapply(1:nrow(mydata), mysplit, char.vector = mydata$Label,
                                   split.by = ":", result.sel = 2))
        mystartd <- as.Date(sapply(1:nrow(mydata), mysplit, char.vector = mydate1,
                                   split.by = "->", result.sel = 1))
        myendd <- as.Date(sapply(1:nrow(mydata), mysplit, char.vector = mydate1,
                                 split.by = "->", result.sel = 2))
    } 
    
    if((mymodel == "BRC0010_BBK507") & (xi != 2)){ 
        mytreat1 <- sapply(1:nrow(mydata), mysplit, char.vector = mydata$Label,
                           split.by = "---", result.sel = 2)
        mytreat1 <- str_trim(mytreat1)
        mytreat <- substr(mytreat1, start = 1, stop = 3)
        mydate1 <- str_trim(sapply(1:nrow(mydata), mysplit, char.vector = mydata$Label,
                                   split.by = ":", result.sel = 2))
        mystartd <- as.Date(sapply(1:nrow(mydata), mysplit, char.vector = mydate1,
                                   split.by = "->", result.sel = 1))
        myendd <- as.Date(sapply(1:nrow(mydata), mysplit, char.vector = mydate1,
                                 split.by = "->", result.sel = 2))
        myendd2 <- as.Date(substr(mydata$`End Info orig`, start = 12, stop = 21))
        names(mydata)[names(mydata) == "End Info orig"] <- "End Info" 
        myendd[ is.na(myendd) ] <- myendd2[ is.na(myendd) ]
    } 
    
    if((mymodel == "BRC0010_BBK507") & (xi == 2)){
        # This is for VHIO, which has an empty "Label" column
        mydata$Treatment[mydata$Treatment == "T-DM1"] <- "TDM"
        mytreat <- mydata$Treatment
        mystartd <- rep(as.Date("2021-06-02"), nrow(mydata))
        myendd <- rep(as.Date("2021-07-15"), nrow(mydata))
        names(mydata)[names(mydata) == "Start date"] <- "Start Info"
        names(mydata)[names(mydata) == "End date"] <- "End Info"
        colsToLeaveOut <- c("Treatment notes2")
        mydata[, names(mydata) %in% colsToLeaveOut] <- NULL
        # Unique animal ID is under Group, not Genealogy ID
        mydata$`Genealogy ID` <- mydata$Group
    }
    
    if((mymodel == "BRC0007_BBK567") & (xi == 1)){
        # This is for NKI, which has the "Label" column formatted differently
        mytreat <- mydata$`Treatment notes`
        mytreat[mydata$`Treatment notes` == "TDM-1"] <- "TDM"
        mystartd <- rep(as.Date("2021-12-06"), nrow(mydata))
        myendd <- rep(as.Date("2022-01-17"), nrow(mydata))
    }
    
    if((mymodel == "BRC0007_BBK567") & (xi == 2)){
        # This is for VHIO, which has an empty "Label" column
        mydata$Treatment[mydata$Treatment == "T-DM1"] <- "TDM"
        mytreat <- mydata$Treatment
        mystartd <- rep(as.Date("2021-03-16"), nrow(mydata))
        myendd <- rep(as.Date("2021-07-15"), nrow(mydata))
        colsToLeaveOut <- c("Treatment notes2", "Treatment notes3")
        mydata[, names(mydata) %in% colsToLeaveOut] <- NULL
        # Unique animal ID is under Group, not Genealogy ID
        mydata$`Genealogy ID` <- mydata$Group
    }
    
    if((mymodel == "BRC0007_BBK567") & (xi > 2)){
        mytreat1 <- sapply(1:nrow(mydata), mysplit, char.vector = mydata$Label,
                           split.by = "---", result.sel = 2)
        mytreat1 <- str_trim(mytreat1)
        mytreat <- substr(mytreat1, start = 1, stop = 3)
        mydate1 <- str_trim(sapply(1:nrow(mydata), mysplit, char.vector = mydata$Label,
                                   split.by = ":", result.sel = 2))
        mystartd <- as.Date(sapply(1:nrow(mydata), mysplit, char.vector = mydate1,
                                   split.by = "->", result.sel = 1))
        myendd <- as.Date(sapply(1:nrow(mydata), mysplit, char.vector = mydate1,
                                 split.by = "->", result.sel = 2))
        
    }
    # if(mymodel == "BRC0007_BBK567_humanized"){
    #     mytreat <- mydata$Treatment
    # }
    
    # Sometimes the end date is not available from the Label column, yielding NA
    # Here we give the correct values
    myendd[is.na(myendd) & (mymodel == "CUP0002") & (xi == 3)] <- "2021-8-12"
    myendd[is.na(myendd) & (mymodel == "CUP0006") & (xi == 5)] <- "2021-07-14"
    myendd[is.na(myendd) & (mymodel == "CUP0006") & (xi == 3)] <- "2021-06-18"
    myendd[is.na(myendd) & (mymodel == "CUP0008") & (xi == 4)] <- "2021-02-24"
    myendd[is.na(myendd) & (mymodel == "CUP0008") & (xi == 5)] <- "2021-05-05"
    myendd[is.na(myendd) & (mymodel == "CUP0009") & (xi == 5)] <- "2021-04-24"
    myendd[is.na(myendd) & (mymodel == "CUP0009") & (xi == 3)] <- "2021-07-25"
    myendd[is.na(myendd) & (mymodel == "BRC0010_BBK507") & (xi == 3)] <- "2023-01-25"
    myendd[is.na(myendd) & (mymodel == "BRC0007_BBK507") & (xi == 1)] <- "2022-01-17"
    
    # mytreat1 <- sapply(1:nrow(mydata), mysplit, char.vector = mydata$Label,
    #                    split.by = "---", result.sel = 2)
    # mytreat1 <- str_trim(mytreat1)
    # mytreat <- substr(mytreat1, start = 1, stop = 6)
    # if(mymodel == "BRC0010_BBK507_latest") mytreat <- substr(mytreat1, start = 1, stop = 3)
    # mydate1 <- str_trim(sapply(1:nrow(mydata), mysplit, char.vector = mydata$Label,
    #                            split.by = ":", result.sel = 2))
    # mystartd <- as.Date(sapply(1:nrow(mydata), mysplit, char.vector = mydate1,
    #                            split.by = "->", result.sel = 1))
    # myendd <- as.Date(sapply(1:nrow(mydata), mysplit, char.vector = mydate1,
    #                          split.by = "->", result.sel = 2))
    # if(mymodel == "BRC0010_BBK507_latest") {
    #     myendd2 <- as.Date(substr(mydata$`End Info orig`, start = 12, stop = 21))
    #     names(mydata)[names(mydata) == "End Info orig"] <- "End Info" 
    # }
    # myendd[ is.na(myendd) ] <- myendd2[ is.na(myendd) ]
    # # Sometimes the end date is not available from the Label column, yielding NA
    # # Here we give the correct values
    # myendd[is.na(myendd) & (mymodel == "CUP0002") & (xi == 3)] <- "2021-8-12"
    # myendd[is.na(myendd) & (mymodel == "CUP0006") & (xi == 5)] <- "2021-07-14"
    # myendd[is.na(myendd) & (mymodel == "CUP0006") & (xi == 3)] <- "2021-06-18"
    # myendd[is.na(myendd) & (mymodel == "CUP0008") & (xi == 4)] <- "2021-02-24"
    # myendd[is.na(myendd) & (mymodel == "CUP0008") & (xi == 5)] <- "2021-05-05"
    # myendd[is.na(myendd) & (mymodel == "CUP0009") & (xi == 5)] <- "2021-04-24"
    # myendd[is.na(myendd) & (mymodel == "CUP0009") & (xi == 3)] <- "2021-07-25"
    # myendd[is.na(myendd) & (mymodel == "BRC0010_BBK507_latest") & (xi == 3)] <- "2023-01-25"
    
    mydata$`Start Info` <- mystartd
    mydata$`End Info` <- myendd
    mydata$Treatment <- mytreat
    names(mydata)[1] <- "GenealogyID"
    mydata$GenealogyID <- factor(mydata$GenealogyID) # factor needed for reshaping
    
    mydata <- mydata[, !(names(mydata) %in% c("Barcode", "Group", "Label", 
                                              "Treatment notes", "follow up time"))]
    # This should leave 4 factor/character columns at the beginning of the data,
    # and all remaining columns referring to volume measurements. These are
    # the columns containing the data and that will be used in reshaping it.
    # Therefore they all need to be of the same type (numeric). We will make sure 
    # that this is now the case.
    for(xc in 5:ncol(mydata)) mydata[, xc ] <- as.numeric(mydata[, xc ])
    
    data_long <- pivot_longer(data = mydata, 
                              cols = !c("GenealogyID", "Treatment", "Start Info", "End Info"),
                              names_to = "date", 
                              values_to = "volume", 
                              cols_vary = "slowest") %>%
                   filter(volume!=-1)
    data_long <- as.data.frame(data_long)
    data_long <- data_long[!is.na(data_long$volume), ]
    data_long$date <- as.Date(data_long$date)
    return(data_long)
}

# Function to extract only observations between the start and end of treatment
# Written to be used with lapply
get_vol_treat <- function(xi, alldata){
    data_long <- alldata[[ xi ]]
    data_long$days <- as.numeric(data_long$date - data_long$`Start Info`)
    data_long <- data_long[(data_long$days >= 0) & (data_long$date <= data_long$`End Info`), ]
    return(data_long)
}


# Function to select necessary columns for the survival analysis
# inputs
# xi: numeric (scalar), the number of the item in the list `mydatac` to be selected;
#     typically this corresponds to the number of the sheet to be selected
# mydatac: a list, with each element corresponding to data read in from a sheet in
#          the original excel file
# Output:
# surv_data: a data frame containing all the necessary variables for the survival analysis.
# written to be use with `lapply`.

get_surv_data <- function(xi, mydatac){
        mydata <- mydatac[[ xi ]]
        # Models to be handled separately
        models.o <- c("BRC0010_BBK507", "BRC0007_BBK567")
        if(!(mymodel  %in% models.o) ){ 
            mytreat1 <- sapply(1:nrow(mydata), mysplit, char.vector = mydata$Label,
                               split.by = "---", result.sel = 2)
            mytreat1 <- str_trim(mytreat1)
            mytreat <- substr(mytreat1, start = 1, stop = 6)
        } 
        
        if((mymodel == "BRC0010_BBK507") & (xi != 2)){ 
            mytreat1 <- sapply(1:nrow(mydata), mysplit, char.vector = mydata$Label,
                               split.by = "---", result.sel = 2)
            mytreat1 <- str_trim(mytreat1)
            mytreat <- substr(mytreat1, start = 1, stop = 3)
        } 
        
        if((mymodel == "BRC0010_BBK507") & (xi == 2)){
            # This is for VHIO, which has an empty "Label" column
            mydata$Treatment[mydata$Treatment == "T-DM1"] <- "TDM"
            mytreat <- mydata$Treatment
            colsToLeaveOut <- c("Treatment notes2")
            mydata[, names(mydata) %in% colsToLeaveOut] <- NULL
            # Unique animal ID is under Group, not Genealogy ID
            mydata$`Genealogy ID` <- mydata$Group
        }
        
        if((mymodel == "BRC0007_BBK567") & (xi == 1)){
            # This is for NKI, which has the "Label" column formatted differently
            mytreat <- mydata$`Treatment notes`
            mytreat[mydata$`Treatment notes` == "TDM-1"] <- "TDM"
        }
        
        if((mymodel == "BRC0007_BBK567") & (xi == 2)){
            # This is for VHIO, which has an empty "Label" column
            mydata$Treatment[mydata$Treatment == "T-DM1"] <- "TDM"
            mytreat <- mydata$Treatment
            mystartd <- rep(as.Date("2021-03-16"), nrow(mydata))
            myendd <- rep(as.Date("2021-07-15"), nrow(mydata))
            colsToLeaveOut <- c("Treatment notes2", "Treatment notes3")
            mydata[, names(mydata) %in% colsToLeaveOut] <- NULL
            # Unique animal ID is under Group, not Genealogy ID
            mydata$`Genealogy ID` <- mydata$Group
        }
        
        if((mymodel == "BRC0007_BBK567") & (xi > 2)){
            mytreat1 <- sapply(1:nrow(mydata), mysplit, char.vector = mydata$Label,
                               split.by = "---", result.sel = 2)
            mytreat1 <- str_trim(mytreat1)
            mytreat <- substr(mytreat1, start = 1, stop = 3)
        }
        mydata$Treatment <- mytreat
        names(mydata)[1] <- "GenealogyID"
        mydata$GenealogyID <- factor(mydata$GenealogyID) # factor needed for reshaping
        
       surv_data <- mydata %>%
        dplyr::select(`GenealogyID`, Barcode, Group, Treatment, `follow up time`, `event/status/sensor`)
    return(surv_data)
}







get_surv_object <- function(xi, surv_datac){
    surv_object <- Surv(surv_datac[[xi]]$`follow up time`, surv_datac[[xi]]$`event/status/sensor` == 1)
    return(surv_object)
    }



get_surv_fit <- function(xi , surv_objectc , surv_datac){
    survfit(surv_objectc[[xi]] ~ Treatment , data = surv_datac[[xi]])
}
    
# surv_object_NKI <- Surv(BC_NKI_sur$`follow up time`, BC_NKI_sur$`event/status/sensor`)
# fit_KM_NKI <- survfit(surv_object_NKI ~ Treatment, data = BC_NKI_sur)


