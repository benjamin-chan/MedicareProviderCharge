Medicare Provider Charge
========================

Last update by Benjamin Chan (<benjamin.ks.chan@gmail.com>) on `2013-05-08 22:34:50` using `R version 2.15.3 (2013-03-01)`.

Analyze CMS Medicare Provider Charge public use dataset. The data is documented and can be downloaded at the Medicare Provider Charge Data [website](http://www.cms.gov/Research-Statistics-Data-and-Systems/Statistics-Trends-and-Reports/Medicare-Provider-Charge-Data/index.html).

> As part of the Obama administration’s work to make our health care system more affordable and accountable, data are being released that show significant variation across the country and within communities in what hospitals charge for common inpatient services.

> The data provided here include hospital-specific charges for the more than 3,000 U.S. hospitals that receive Medicare Inpatient Prospective Payment System (IPPS) payments for the top 100 most frequently billed discharges, paid under Medicare based on a rate per discharge using the Medicare Severity Diagnosis Related Group (MS-DRG) for Fiscal Year (FY) 2011. These DRGs represent almost 7 million discharges or 60 percent of total Medicare IPPS discharges.

> Hospitals determine what they will charge for items and services provided to patients and these charges are the amount the hospital bills for an item or service. The Total Payment amount includes the MS-DRG amount, bill total per diem, beneficiary primary payer claim payment amount, beneficiary Part A coinsurance amount, beneficiary deductible amount, beneficiary blood deducible amount and DRG outlier amount.

> For these DRGs, average charges and average Medicare payments are calculated at the individual hospital level. Users will be able to make comparisons between the amount charged by individual hospitals within local markets, and nationwide, for services that might be furnished in connection with a particular inpatient stay.


Load the required libraries.

```r
require(RCurl, quietly = TRUE)
require(ggplot2, quietly = TRUE)
require(scales, quietly = TRUE)
require(RColorBrewer, quietly = TRUE)
```



Get data from public Dropbox file where the data was downloaded to. Read the entire dataset.

```r
url <- getURL("https://dl.dropboxusercontent.com/u/386956/Medicare_Provider_Charge_Inpatient_DRG100_FY2011.csv")
df <- read.csv(textConnection(url), header = TRUE, sep = ",")
nrow(df)
```

```
## [1] 163065
```

```r
names(df)
```

```
##  [1] "DRG.Definition"                      
##  [2] "Provider.Id"                         
##  [3] "Provider.Name"                       
##  [4] "Provider.Street.Address"             
##  [5] "Provider.City"                       
##  [6] "Provider.State"                      
##  [7] "Provider.Zip.Code"                   
##  [8] "Hospital.Referral.Region.Description"
##  [9] "Total.Discharges"                    
## [10] "Average.Covered.Charges"             
## [11] "Average.Total.Payments"
```

```r
head(df)
```

```
##                                                    DRG.Definition
## 1                        039 - EXTRACRANIAL PROCEDURES W/O CC/MCC
## 2             057 - DEGENERATIVE NERVOUS SYSTEM DISORDERS W/O MCC
## 3      064 - INTRACRANIAL HEMORRHAGE OR CEREBRAL INFARCTION W MCC
## 4       065 - INTRACRANIAL HEMORRHAGE OR CEREBRAL INFARCTION W CC
## 5 066 - INTRACRANIAL HEMORRHAGE OR CEREBRAL INFARCTION W/O CC/MCC
## 6                                        069 - TRANSIENT ISCHEMIA
##   Provider.Id                    Provider.Name Provider.Street.Address
## 1       10001 SOUTHEAST ALABAMA MEDICAL CENTER  1108 ROSS CLARK CIRCLE
## 2       10001 SOUTHEAST ALABAMA MEDICAL CENTER  1108 ROSS CLARK CIRCLE
## 3       10001 SOUTHEAST ALABAMA MEDICAL CENTER  1108 ROSS CLARK CIRCLE
## 4       10001 SOUTHEAST ALABAMA MEDICAL CENTER  1108 ROSS CLARK CIRCLE
## 5       10001 SOUTHEAST ALABAMA MEDICAL CENTER  1108 ROSS CLARK CIRCLE
## 6       10001 SOUTHEAST ALABAMA MEDICAL CENTER  1108 ROSS CLARK CIRCLE
##   Provider.City Provider.State Provider.Zip.Code
## 1        DOTHAN             AL             36301
## 2        DOTHAN             AL             36301
## 3        DOTHAN             AL             36301
## 4        DOTHAN             AL             36301
## 5        DOTHAN             AL             36301
## 6        DOTHAN             AL             36301
##   Hospital.Referral.Region.Description Total.Discharges
## 1                          AL - Dothan               91
## 2                          AL - Dothan               38
## 3                          AL - Dothan               84
## 4                          AL - Dothan              169
## 5                          AL - Dothan               33
## 6                          AL - Dothan               37
##   Average.Covered.Charges Average.Total.Payments
## 1                   32963                   5777
## 2                   20313                   4895
## 3                   38820                  10260
## 4                   27345                   6542
## 5                   17606                   4596
## 6                   20689                   4134
```

There are `163065` rows in the dataset. So let's take a sample for testing purposes.

```r
sample <- sample(seq(1:nrow(df)), 10000)
dfSample <- df[sample, ]
```

