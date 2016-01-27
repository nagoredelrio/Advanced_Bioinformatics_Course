# Advanced Bioinformatics course

# Assignment 1

for(x in -10:10){   # Loop to create the table
  y<-2*cos(x)   # Calculate y for each x
  if(x==-10){table<-c(x,y)}   # If the table does not exist yet, create it
  else(table<-rbind(table,c(x,y)))   # If the table already exists, add to it
}
colnames(table)<-c("X","Y")

plot(table,type="l",main="y=2*cos(x)")   # Makes a line plot

# Assignment 2

library(cgdsr)   # Load the necessary library
data<-CGDS("http://www.cbioportal.org/public-portal/")   # Get data from database
cases<-getCaseLists(data,"thca_tcga")   # See cases for THCA, choose the first one
profiles<-getGeneticProfiles(data,"thca_tcga")   # See profiles for THCA, choose the 
                                                 # first one
mutation<-getProfileData(data,"BRAF",profiles[1,1],cases[1,1])   # Get data on BRAF 
                                                                 # mutations
types<-unique(mutation[,1])   # List of the kinds of mutations
for(i in 1:length(types)){   # Loop to create the table
  if(!is.na(types[i])){num<-sum(grepl(types[i],mutation[,1]))   # Get the number of 
                                                                # each mutation, the 
                                                                # if loop allows 
                                                                # avoiding NA type
    if(i==1){table<-c(as.character(types[i]),num)}   # Create the table 
    else(table<-rbind(table,c(as.character(types[i]),num)))   # Add to it
  }
}
colnames(table)<-c("Mutation","Cases")
table<-rbind(c("WT",nrow(mutation)-sum(as.integer(table[,2]))),table)   # Add the WT
# Because the NAs were removed when making the table, they have to be calculated now
# as the leftover cases.
barplot(as.integer(table[,2]),names.arg=table[,1],main="BRAF mutations in THCA",
        las=2,cex.names=0.7,ylab="Cases")   # Create the bar plot
