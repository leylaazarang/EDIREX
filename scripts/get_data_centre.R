## get_data_centre function
# To read data and pre-process, for a single animal model 
# and a single centre
# The data for multiple centres will have to subsequently be gathered
# Inputs
# mysheet: the worksheet to be read in; this typically corresponds to a centre
# myfile: the filename to be used

get_data_centre <- function(mysheet, myfile){
    
    data_centre <- readxl::read_excel(
        paste0("data/", myfile),  
        col_names= TRUE, sheet = mysheet) %>%
        mutate(Treatment = sapply(strsplit(Label, split=":"), function(x)x[1]) )  %>%
        mutate(Treatment = sapply(strsplit(Treatment, split=" --- "), function(y)y[2])) %>%
        filter(Label != "Waste") %>%
        select(-Label, -`Start Info`, -`End Info`, -`Treatment notes`) %>%
        mutate(Group=rep(myc, nrow(.))) 
    
    
    # Waste obs are ignored, those with tumors growing either too slow or too fast. 
    # These are excluded from the analysis. 
    
    # sum(is.na(colon_NKI$`Start Info`))
    
    data_centre <- data_centre %>%
        select(
            where(
                ~!all(is.na(.x))
            )
        ) # delete the columns that contain only NAs.
    
    # unique(colon_NKI$Treatment)
    # colon_NKI$`event/status/sensor`
    
    data_centre_long <- data_centre %>%
        gather(key= "time", value="tumor size", -`Genealogy ID`, -Barcode, -Group,  -Treatment, -`follow up time`, -`event/status/sensor` ) %>%
        arrange(`Genealogy ID`)
    
    data_centre_long$time <- as.Date(data_centre_long$time )
    
    data_centre_long$time <- sapply(1:length(data_centre_long$time), 
                                     function(i){
                                         length(seq.Date(from = data_centre_long$time[1],
                                                        to = data_centre_long$time[i],
                                                        by = "day"))})-1
    
    data_centre_long <- data_centre_long %>% 
        mutate(`tumor size`=as.numeric(`tumor size`)) %>%
        filter(`tumor size` !=-1)
    
    ## Standardize the tumor size 
    ID_centre <- unique(data_centre_long$`Genealogy ID`)

    # standardizing tumor size at each point 
    # the size at the start of the treatment has some negative values, 
    # and the ratio has some 0/0 and 0 values (log = -INF), 
    # also standardizing is not necessary here because I am considering random slope and random intercept than, not the mixed model with in which the  error term takes the correlation between observations into account. 
    
    
    f_centre <- function(x){ 
        data_centre_long$`tumor size`[data_centre_long$`Genealogy ID`==x]/ 
                                      data_centre_long$`tumor size`[data_centre_long$`Genealogy ID`==x][1]
    }
    data_centre_long$ST_tumorsize <- unlist(lapply(ID_centre, f_centre))
    return(data_centre = data_centre, data_centre_long = data_centre_long)
}