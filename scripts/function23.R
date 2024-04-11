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
        select(-starts_with("Treatment notes"))
    if(myfile == "BRC0023_OD-BRE-0396_MV.xlsx"){
        tb <- tb%>%
            filter(Treatment != "Waste")
    }
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
   mydata  <- data_centers[[xi]]
   names(mydata)[1] <- "GenealogyID"
   mydata$GenealogyID <- factor(mydata$GenealogyID) # factor needed for reshaping
   mydata <- mydata[, !(names(mydata) %in% c("Barcode", "Group", "Label",
                                          "follow up time",  "event/status/sensor"))]
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
if (xi == 1){
    data_long$date <- as.Date((data_long$date) , origin = "1899-12-30")
} else{
     data_long$date <- as.Date(as.numeric(data_long$date) , origin = "1899-12-30")
} 
return(data_long)
}

get_vol_treat <- function(xi, alldata){

    data_long <- alldata[[ xi ]]
    data_long$days <- as.numeric(data_long$date - as.Date(data_long$`Start Info`))
    data_long <- data_long[(data_long$days >= 0) & (data_long$date <= as.Date(data_long$`End Info`)), ]
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
    
    names(mydata)[1] <- "GenealogyID"
    mydata$GenealogyID <- factor(mydata$GenealogyID) # factor needed for reshaping
    
    surv_data <- mydata %>%
        dplyr::select(`GenealogyID`, Barcode, Group, Treatment, `follow up time`, `event/status`)
    return(surv_data)
}







get_surv_object <- function(xi, surv_datac){
    surv_object <- Surv(surv_datac[[xi]]$`follow up time`, surv_datac[[xi]]$`event/status` == 1)
    return(surv_object)
}



get_surv_fit <- function(xi , surv_objectc , surv_datac){
    survfit(surv_objectc[[xi]] ~ Treatment , data = surv_datac[[xi]])
}

# surv_object_NKI <- Surv(BC_NKI_sur$`follow up time`, BC_NKI_sur$`event/status`)
# fit_KM_NKI <- survfit(surv_object_NKI ~ Treatment, data = BC_NKI_sur)
