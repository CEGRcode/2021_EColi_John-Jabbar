library(plyr)
library(dplyr)

##manually import combined (Heatshock+mock) peaks and call it myfile_chex. 
##manually import combined Genetrack (mock+Heatshock) peaks and call it myfile_genetrack 
##Removes all duplicates including the ones having different peak length but same genomic location 
myfile_chex_deduplicated <- distinct(myfile_chex, V2, .keep_all = TRUE)
##Generate random numbers based on normal distribution for the number of rows in the fie
Random_number <-rnorm(nrow(myfile_chex_deduplicated))
##merge Random number with your file
Chexmix_file <- cbind(myfile_chex_deduplicated,Random_number)
## sort the file in descending order based on Random number
Desc_Chexmix <- arrange(Chexmix_file,desc(Random_number))
##Calculate Rank of Chexmix dataset using peak height in column 5 in a descending order
Rank<- frank(Desc_Chexmix, cols = -V5,ties.method = "first")
## merge the rank with the Chexmix File
Finalfile_Chex<-cbind(Desc_Chexmix,Rank)
##organize genetrack file to be same as chexmix files
genetrack_organize <- myfile_genetrack[,c(1,4,5,2,6,7)]
##Removes all duplicates including the ones having different peak length but same genomic location 
myfile_genetrack_deduplicated <- distinct(genetrack_organize, V4, .keep_all = TRUE)
##Generate random numbers based on normal distribution for the number of rows in the fie
Random_number <-rnorm(nrow(myfile_genetrack_deduplicated))
##merge Random number with your file
Genetrack_file <- cbind(myfile_genetrack_deduplicated,Random_number)
## sort the file in descending order based on Random number
Desc_Genetrack <- arrange(Genetrack_file,desc(Random_number))
##Calculate Rank of Genetrack dataset using peak height in column V6 in a descending order
Rank<- frank(Desc_Genetrack, cols = -V6,ties.method = "first")
## merge the rank with the Genetrack File
Finalfile_Genetrack<-cbind(Desc_Genetrack,Rank)
## renaming column names of Genetrack file
colnames(Finalfile_Genetrack)<- c('V1','V2','V3','V4','V5','V6','Random_number','Rank')
##renaming row names of Chexmix file to be ec2
to_name<- Finalfile_Genetrack[1:nrow(Finalfile_Chex),1]
Finalfile_Chex[,1] <- to_name
##Merge Chexmix and Genetrack Peaks
Combined<-rbind(Finalfile_Genetrack,Finalfile_Chex)
## sort based on Rank in ascending order
Asc_combined <- arrange(Combined,Combined$Rank)
## top500 for MeMe discovery
Top500<- Asc_combined[1:500,1:6]
## save the file
write.table(Top500, sep = "\t", file = "Mydata.txt", col.names = FALSE, row.names = FALSE)
write.table(Asc_combined[,1:6], sep = "\t", file = "Combined_Peaks.txt", col.names = FALSE, row.names = FALSE)
