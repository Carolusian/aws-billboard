library(dplyr)

# Get EC2 quotation
df_region <- read.csv('Regions.csv', stringsAsFactors = F)

df_ec2 <- read.csv('RI-Quotation-EC2.csv', stringsAsFactors = F) 
df_ec2_quot <- filter(df_ec2, Operating.System == 'Linux' & 
                        Location %in% df_region$Region.Name & 
                        LeaseContractLength == '1yr' &
                        PurchaseOption == 'Partial Upfront')

df_ec2_output <- merge(df_ec2_quot, df_region, by.x = 'Location', by.y = 'Region.Name')
write.csv(df_ec2_output, 'Consolidated-RI-Quotation-EC2.csv')

# Get RDS quotation
df_region <- read.csv('Regions.csv', stringsAsFactors = F)

df_rds <- read.csv('RI-Quotation-RDS.csv', stringsAsFactors = F)
df_rds_quot <- filter(df_rds, Database.Engine == 'PostgreSQL' &
                        Location %in% df_region$Region.Name &
                        LeaseContractLength == '1yr' &
                        PurchaseOption == 'Partial Upfront')

df_rds_output <- merge(df_rds_quot, df_region, by.x = 'Location', by.y = 'Region.Name')
write.csv(df_rds_output, 'Consolidated-RI-Quotation-RDS.csv')

# Get ElastiCache Quotation
df_region <- read.csv('Regions.csv', stringsAsFactors = F)

df_cache <- read.csv('RI-Quotation-ElastiCache.csv', stringsAsFactors = F)
df_cache_quot <- filter(df_cache, 
                        Location %in% df_region$Region.Name &
                        LeaseContractLength == '1yr')

df_cache_output <- merge(df_cache_quot, df_region, by.x = 'Location', by.y = 'Region.Name')
write.csv(df_cache_output, 'Consolidated-RI-Quotation-ElastiCache.csv')
