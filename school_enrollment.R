# Written by Christy Lam & Hana Sevcikova April 15, 2016
# This code will format College Enrollment by Student Level data downloaded from
# http://nces.ed.gov/ipeds/datacenter/Default.aspx


# identify start and end years
years <- seq(2010,2014)


# establish directories
base.dir <- "J:/Reference/CollegeFTE/2016_04_07" # the base directory where you run the script
setwd(base.dir)
directories <- paste0('ft-pt_enrollment_',years) # vector of the sub-directories


# establish data frames
names.df <- NULL
df <-NULL


# cycle through each subdirectory and format data
for (i in 1: length(years)) {
  dir <- directories [i]
  year <- years[i]
  yr <- substr(as.character(year),3,4) 
  afile <- list.files(dir, pattern="*.csv", full.names=F, recursive=FALSE) # find csv file in the subdirectory
  print (paste0("Reading this file: ",afile))
  enroll <- read.csv(file=file.path(dir, afile[1]), header=TRUE, sep=",", skip=2) # read csv file, header starts on row 3
  enroll[[7]] <- NULL # remove last blank column
  colnames(enroll) <- c("UnitId", "Institution","StudentLevel",paste0("FT",yr),paste0("PT",yr),paste0("TOT",yr)) # rename column names
  curInstitutions <- enroll[,1] # grab UnitIds in current iteration
  
  if (is.null(names.df))
    names.df <- enroll[,c("UnitId", "Institution", "StudentLevel")] # populate names.df (only in 1st iteration)
  
  existInstitutions <- names.df[,1] 
  result.from.setdiff <- setdiff(curInstitutions, existInstitutions) # find asymetrical difference between two sets of institutions
  print (result.from.setdiff) # look at the new institutions
  
  newInstitutions <- NULL
  
  if (length(result.from.setdiff)==0){ # if there aren't any new institutions
    enroll <- enroll[,-c(2)]
    print (head(enroll))
    ifelse (is.null(df),df <- enroll,df <- merge(df, enroll, by=c("UnitId","StudentLevel"))) # merge current data to existing df
  } else { # if there are new institutions
        newInstitutions <- subset(enroll, UnitId %in% result.from.setdiff) # take the new institutions' information
        print ("Finding new institutions to add to the existing list")
        names.df <- rbind(names.df, newInstitutions[,c("UnitId", "Institution", "StudentLevel")]) # add new institutions to names.df
        enroll <- enroll[,-c(2)]
        
        # Appending new institutions to df; to rbind need to create subsets 1 and 2 of df
        subset.df1 <- df[,1:2]
        subset.df2 <- df[,1:(ncol(df))]
        df <- rbind(subset.df1, newInstitutions[,c(1,3)]) # add the new institutions to subset.df1
        df <- merge(df,subset.df2, by=c("UnitId","StudentLevel"), all.x=TRUE) # merge existing years data to "df1"
        df <- merge(df, enroll, by=c("UnitId","StudentLevel")) # and merge current data to existing  data to make df complete    
  }  
}

# join data frames to include Institution name
df <- merge(df, names.df, all = TRUE)

# reorder the columns so "Institution" is the second column
df <- df[,c(1, ncol(df), 2:(ncol(df)-1))] 

# replace NA values with '-'
df[is.na(df)] <- "-"

#sort df by Institution
charInstitution <- as.character(df$Institution)
df <- df[order(charInstitution),]

# export to csv file
write.csv(df, file=paste0("CollegeEnrollment", years[1], "_",years[length(years)],"b",".csv"))

print ("Exported data to .csv")
