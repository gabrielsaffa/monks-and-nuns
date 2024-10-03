################################################################################


library(ape)
library(geiger)
library(phytools)
library(rethinking)

setwd("")
ASJP_tree <- read.tree("ASJP-tree.txt") # import ASJP language tree
monk_nun <- read.csv("ea_societies2_GS.csv",header=TRUE) # import Sara's monk and nun dataset 
marriage_trans_dplace <- read.csv("Transactions at marriage prevailing type [EA006].csv") # import Transactions at marriage prevailing type [EA006] from D-PLACE
marital_comp_dplace <- read.csv("Marital composition monogamy and polygamy [EA009].csv") # import Marital composition monogamy and polygamy [EA009] from D-PLACE
inheritance_dplace <- read.csv("Inheritance rule for real property (land) [EA074].csv") # import Inheritance rule for real property (land) [EA074] from D-PLACE
class_dplace <- read.csv("Class differentiation primary [EA066].csv") # import Class differentiation: primary [EA066] from D-PLACE
region_eHRAF <- read.csv("all_eHRAF_societies.csv",header=TRUE) # geographic region


## prepare the tree
ASJP_tree$tip.label # inspect tip labels 
ASJP_tree$tip.label[ASJP_tree$tip.label=="UNAMI_UnnamedInSource"] <- "UNAMI_UNNAMEDINSOURCE" # rename to match with the data
language_ASJP <- toupper(monk_nun$language_ASJP) # change names in the data to upper case to match with names in the tree
ASJP_eHRAF_tree <- keep.tip(ASJP_tree,language_ASJP) # now we can prune the tree to match with eHRAF societies, which results in 331 societies, 21 societies less than in the data set (N=352)

## this is because some societies speak the same language or one of several dialects of the same language
## so we need to add these societies to the tree manually - as polytomies
language_ASJP[duplicated(language_ASJP)] # print the languages assigned to more than one society
which(ASJP_eHRAF_tree$tip.label=="MOORE") 
ASJP_eHRAF_tree <- bind.tip(ASJP_eHRAF_tree,tip.label="MOORE_Mossi_Ouagadougou",where=60,edge.length=1,position=1)
which(ASJP_eHRAF_tree$tip.label=="ARABIC_LIBYAN_SPOKEN") 
ASJP_eHRAF_tree <- bind.tip(ASJP_eHRAF_tree,tip.label="ARABIC_LIBYAN_SPOKEN_Sanusi",where=258,edge.length=1,position=1)
which(ASJP_eHRAF_tree$tip.label=="BLACKFOOT") 
ASJP_eHRAF_tree <- bind.tip(ASJP_eHRAF_tree,tip.label="BLACKFOOT_Kainan",where=283,edge.length=1,position=1)
ASJP_eHRAF_tree <- bind.tip(ASJP_eHRAF_tree,tip.label="BLACKFOOT_Piegan",where=283,edge.length=1,position=1)
which(ASJP_eHRAF_tree$tip.label=="CHIPPEWA") 
ASJP_eHRAF_tree <- bind.tip(ASJP_eHRAF_tree,tip.label="CHIPPEWA_Minnesota_Ojibwa",where=37,edge.length=1,position=1)
ASJP_eHRAF_tree <- bind.tip(ASJP_eHRAF_tree,tip.label="CHIPPEWA_Rainy_River",where=37,edge.length=1,position=1)
which(ASJP_eHRAF_tree$tip.label=="SAULTEAUX") 
ASJP_eHRAF_tree <- bind.tip(ASJP_eHRAF_tree,tip.label="SAULTEAUX_Bungi",where=35,edge.length=1,position=1)
which(ASJP_eHRAF_tree$tip.label=="NORTHERN_PAIUTE") 
ASJP_eHRAF_tree <- bind.tip(ASJP_eHRAF_tree,tip.label="NORTHERN_PAIUTE_Kidutokado",where=95,edge.length=1,position=1)
ASJP_eHRAF_tree <- bind.tip(ASJP_eHRAF_tree,tip.label="NORTHERN_PAIUTE_Wadatkuht",where=95,edge.length=1,position=1)
ASJP_eHRAF_tree <- bind.tip(ASJP_eHRAF_tree,tip.label="NORTHERN_PAIUTE_Kuyuidokado",where=95,edge.length=1,position=1)
which(ASJP_eHRAF_tree$tip.label=="YOKUTS_TINLINNEH") 
ASJP_eHRAF_tree <- bind.tip(ASJP_eHRAF_tree,tip.label="YOKUTS_TINLINNEH_Wukchumni",where=79,edge.length=1,position=1)
ASJP_eHRAF_tree <- bind.tip(ASJP_eHRAF_tree,tip.label="YOKUTS_TINLINNEH_Northern_Foothill_Yokuts",where=79,edge.length=1,position=1)
which(ASJP_eHRAF_tree$tip.label=="YUKI") 
ASJP_eHRAF_tree <- bind.tip(ASJP_eHRAF_tree,tip.label="YUKI_Yuki",where=238,edge.length=1,position=1)
which(ASJP_eHRAF_tree$tip.label=="MARSHALLESE") 
ASJP_eHRAF_tree <- bind.tip(ASJP_eHRAF_tree,tip.label="MARSHALLESE_Jaluit_atoll",where=327,edge.length=1,position=1)
ASJP_eHRAF_tree <- bind.tip(ASJP_eHRAF_tree,tip.label="MARSHALLESE_Majuro",where=327,edge.length=1,position=1)
which(ASJP_eHRAF_tree$tip.label=="PERSIAN") 
ASJP_eHRAF_tree <- bind.tip(ASJP_eHRAF_tree,tip.label="PERSIAN_Iranians",where=206,edge.length=1,position=1)
which(ASJP_eHRAF_tree$tip.label=="CHIRICAHUA") 
ASJP_eHRAF_tree <- bind.tip(ASJP_eHRAF_tree,tip.label="CHIRICAHUA_Mescalero",where=302,edge.length=1,position=1)
which(ASJP_eHRAF_tree$tip.label=="ITALIAN") 
ASJP_eHRAF_tree <- bind.tip(ASJP_eHRAF_tree,tip.label="ITALIAN_Italian_Canadians",where=216,edge.length=1,position=1)
which(ASJP_eHRAF_tree$tip.label=="SERBOCROATIAN") 
ASJP_eHRAF_tree <- bind.tip(ASJP_eHRAF_tree,tip.label="SERBOCROATIAN_Montenegrins",where=229,edge.length=1,position=1)
ASJP_eHRAF_tree <- bind.tip(ASJP_eHRAF_tree,tip.label="SERBOCROATIAN_Serbian_Americans",where=229,edge.length=1,position=1)
which(ASJP_eHRAF_tree$tip.label=="KALAQIN") 
ASJP_eHRAF_tree <- bind.tip(ASJP_eHRAF_tree,tip.label="KALAQIN_Xinjiang_Mongols",where=258,edge.length=1,position=1)
ASJP_eHRAF_tree <- ladderize(ASJP_eHRAF_tree) # cosmetic only
plotTree(ASJP_eHRAF_tree,lwd=1,fsize=0.1,offset=0.3) # visualize the tree
ASJP_eHRAF_tree <- write.tree(ASJP_eHRAF_tree,"ASJP_eHRAF_tree.nex") # save the tree

## add upper case language names to the data and rename multiplicated languages to match with newly created tips in the tree
monk_nun$language_ASJP_upper <- language_ASJP
monk_nun$language_ASJP_upper[191] <- "MOORE_Mossi_Ouagadougou"
monk_nun$language_ASJP_upper[155] <- "ARABIC_LIBYAN_SPOKEN_Sanusi"
monk_nun$language_ASJP_upper[44] <- "BLACKFOOT_Kainan"
monk_nun$language_ASJP_upper[45] <- "BLACKFOOT_Piegan"
monk_nun$language_ASJP_upper[216] <- "CHIPPEWA_Minnesota_Ojibwa"
monk_nun$language_ASJP_upper[217] <- "CHIPPEWA_Rainy_River"
monk_nun$language_ASJP_upper[219] <- "SAULTEAUX_Bungi"
monk_nun$language_ASJP_upper[203] <- "NORTHERN_PAIUTE_Kidutokado"
monk_nun$language_ASJP_upper[204] <- "NORTHERN_PAIUTE_Wadatkuht"
monk_nun$language_ASJP_upper[205] <- "NORTHERN_PAIUTE_Kuyuidokado"
monk_nun$language_ASJP_upper[338] <- "YOKUTS_TINLINNEH_Wukchumni"
monk_nun$language_ASJP_upper[339] <- "YOKUTS_TINLINNEH_Northern_Foothill_Yokuts"
monk_nun$language_ASJP_upper[347] <- "YUKI_Yuki"
monk_nun$language_ASJP_upper[169] <- "MARSHALLESE_Jaluit_atoll"
monk_nun$language_ASJP_upper[171] <- "MARSHALLESE_Majuro"
monk_nun$language_ASJP_upper[114] <- "PERSIAN_Iranians"
monk_nun$language_ASJP_upper[179] <- "CHIRICAHUA_Mescalero"
monk_nun$language_ASJP_upper[119] <- "ITALIAN_Italian_Canadians"
monk_nun$language_ASJP_upper[189] <- "SERBOCROATIAN_Montenegrins"
monk_nun$language_ASJP_upper[256] <- "SERBOCROATIAN_Serbian_Americans"
monk_nun$language_ASJP_upper[326] <- "KALAQIN_Xinjiang_Mongols"
name.check(ASJP_eHRAF_tree,monk_nun$language_ASJP_upper) # check if the tree and data match (should print "OK")


## prepare the variables
## data files downloaded from D-PLACE, particularly ID and coding columns that we are interested in, contain more information within cells than we need
## we'll remove this redundant information so we can match it with our eHRAF societies and re-code the values as we need
## marriage transactions
marriage_trans_dplace$id <- gsub("EA006-","",marriage_trans_dplace$id)
marriage_trans_dplace$id <- gsub("-1","",marriage_trans_dplace$id)
marriage_trans_dplace$name <- gsub("(?<=Bride-wealth)[^_]*","",marriage_trans_dplace$name,perl=TRUE)
marriage_trans_dplace$name <- gsub("(?<=Bride-service)[^_]*","",marriage_trans_dplace$name,perl=TRUE)
marriage_trans_dplace$name <- gsub("(?<=Token bride-wealth)[^_]*","",marriage_trans_dplace$name,perl=TRUE)
marriage_trans_dplace$name <- gsub("(?<=Gift exchange)[^_]*","",marriage_trans_dplace$name,perl=TRUE)
marriage_trans_dplace$name <- gsub("(?<=Woman exchange)[^_]*","",marriage_trans_dplace$name,perl=TRUE)
marriage_trans_dplace$name <- gsub("(?<=Insignificant)[^_]*","",marriage_trans_dplace$name,perl=TRUE)
marriage_trans_dplace$name <- gsub("(?<=Dowry)[^_]*","",marriage_trans_dplace$name,perl=TRUE)

## marital composition
marital_comp_dplace$id <- gsub("EA009-","",marital_comp_dplace$id)
marital_comp_dplace$id <- gsub("-1","",marital_comp_dplace$id)
marital_comp_dplace$name <- gsub("(?<=Monogamous)[^_]*","",marital_comp_dplace$name,perl=TRUE)
marital_comp_dplace$name <- gsub("(?<=Limited polygyny)[^_]*","",marital_comp_dplace$name,perl=TRUE)
marital_comp_dplace$name <- gsub("(?<=Polygyny, sororal cohabit)[^_]*","",marital_comp_dplace$name,perl=TRUE)
marital_comp_dplace$name <- gsub("(?<=Polygyny, sororal separate quarters)[^_]*","",marital_comp_dplace$name,perl=TRUE)
marital_comp_dplace$name <- gsub("(?<=Polygyny, non-sororal separate quarters)[^_]*","",marital_comp_dplace$name,perl=TRUE)
marital_comp_dplace$name <- gsub("(?<=Polygyny, non-sororal cohabit)[^_]*","",marital_comp_dplace$name,perl=TRUE)
marital_comp_dplace$name <- gsub("(?<=Polyandrous)[^_]*","",marital_comp_dplace$name,perl=TRUE)

## wealth inheritance
inheritance_dplace$id <- gsub("EA074-","",inheritance_dplace$id)
inheritance_dplace$id <- gsub("-1","",inheritance_dplace$id)
inheritance_dplace$name <- gsub("(?<=No inher. of real property)[^_]*","",inheritance_dplace$name,perl=TRUE)
inheritance_dplace$name <- gsub("(?<=Matrilineal by sister's sons)[^_]*","",inheritance_dplace$name,perl=TRUE)
inheritance_dplace$name <- gsub("(?<=Matrilineal by heirs)[^_]*","",inheritance_dplace$name,perl=TRUE)
inheritance_dplace$name <- gsub("(?<=Children, less for daughters)[^_]*","",inheritance_dplace$name,perl=TRUE)
inheritance_dplace$name <- gsub("(?<=Children)[^_]*","",inheritance_dplace$name,perl=TRUE)
inheritance_dplace$name <- gsub("(?<=Patrilineal by heirs)[^_]*","",inheritance_dplace$name,perl=TRUE)
inheritance_dplace$name <- gsub("(?<=Patrilineal by sons)[^_]*","",inheritance_dplace$name,perl=TRUE)

## class differentiation
class_dplace$id <- gsub("EA066-","",class_dplace$id)
class_dplace$id <- gsub("-1","",class_dplace$id)
class_dplace$name <- gsub("(?<=Absence of distinctions)[^_]*","",class_dplace$name,perl=TRUE)
class_dplace$name <- gsub("(?<=Wealth distinctions)[^_]*","",class_dplace$name,perl=TRUE)
class_dplace$name <- gsub("(?<=Elite stratification)[^_]*","",class_dplace$name,perl=TRUE)
class_dplace$name <- gsub("(?<=Elite stratification)[^_]*","",class_dplace$name,perl=TRUE)
class_dplace$name <- gsub("(?<=Dual stratification)[^_]*","",class_dplace$name,perl=TRUE)
class_dplace$name <- gsub("(?<=Complex stratification)[^_]*","",class_dplace$name,perl=TRUE)

## match the variables with our eHRAF societies
marriage_trans_dplace <- marriage_trans_dplace$name[match(monk_nun$soc_id,marriage_trans_dplace$id)]
marital_comp_dplace <- marital_comp_dplace$name[match(monk_nun$soc_id,marital_comp_dplace$id)]
inheritance_dplace <- inheritance_dplace$name[match(monk_nun$soc_id,inheritance_dplace$id)]
class_dplace <- class_dplace$name[match(monk_nun$soc_id,class_dplace$id)]
region <- region_eHRAF$Region[match(monk_nun$owc_id,region_eHRAF$owc_id)] 


## re-code the variables
## data for celibacy, marital composition and marriage transactions were collected by Sara (both eHRAF and D-PLACE)
## celibacy
monk_nun$sex_celibate[is.na(as.factor(monk_nun$sex_celibate))] <- "0" # change NAs to zeros (see the data description section on inferring absences!)
celibacy_cat <- ifelse(monk_nun$sex_celibate=="Monks and nuns",1,ifelse(monk_nun$sex_celibate=="Monks",2,ifelse(monk_nun$sex_celibate=="Nuns",3,0))) # celibacy categorical

## celibacy revisited
monk_nun$sex_celibate_rev[is.na(as.factor(monk_nun$sex_celibate_rev))] <- "0" # change NAs to zeros (see the data description section on inferring absences!)
celibacy_cat_rev <- ifelse(monk_nun$sex_celibate_rev=="Monks and nuns",1,ifelse(monk_nun$sex_celibate_rev=="Monks",2,ifelse(monk_nun$sex_celibate_rev=="Nuns",3,0))) # celibacy categorical


## marital composition
## fill in the missing information from eHRAF or dplace, societies with missing values are re-coded from 0 to NA
monk_nun$marital_comp[monk_nun$dplace_name=="Chugach"] <- 1
monk_nun$marital_comp[monk_nun$dplace_name=="Chekiang"] <- 2
monk_nun$marital_comp[monk_nun$dplace_name=="Ajie"] <- 1
monk_nun$marital_comp[monk_nun$eHRAF_name=="Cuban Americans"] <- NA
monk_nun$marital_comp[monk_nun$eHRAF_name=="Slovenes"] <- 2
monk_nun$marital_comp[monk_nun$eHRAF_name=="Xinjiang Uygur Autonomous Region"] <- NA
marital_comp <- as.integer(monk_nun$marital_comp)

## marital composition revisited
monk_nun$marital_comp_rev[monk_nun$dplace_name=="Chugach"] <- 1
monk_nun$marital_comp_rev[monk_nun$dplace_name=="Chekiang"] <- 2
monk_nun$marital_comp_rev[monk_nun$dplace_name=="Ajie"] <- 1
monk_nun$marital_comp_rev[monk_nun$eHRAF_name=="Cuban Americans"] <- NA
monk_nun$marital_comp_rev[monk_nun$eHRAF_name=="Slovenes"] <- 2
monk_nun$marital_comp_rev[monk_nun$eHRAF_name=="Xinjiang Uygur Autonomous Region"] <- NA
marital_comp_rev <- as.integer(monk_nun$marital_comp_rev)

## marriage transactions 
## re-code societies with missing values from 0 to NA (based on Sara's notes in her sheet) to distinguish them from those with insignificant marriage transactions (originally also 0)
monk_nun$marriage_trans[monk_nun$eHRAF_name=="Arab Americans"] <- NA
monk_nun$marriage_trans[monk_nun$eHRAF_name=="Arab Canadians"] <- NA
monk_nun$marriage_trans[monk_nun$eHRAF_name=="Cajuns"] <- NA
monk_nun$marriage_trans[monk_nun$eHRAF_name=="Cuban Americans"] <- NA
monk_nun$marriage_trans[monk_nun$eHRAF_name=="Serbian Americans"] <- 3
monk_nun$marriage_trans[monk_nun$eHRAF_name=="Xinjiang Uygur Autonomous Region"] <- NA
marriage_trans <- as.integer(monk_nun$marriage_trans) + 1

## marriage transactions revisited
monk_nun$marriage_trans_rev[monk_nun$eHRAF_name=="Arab Americans"] <- NA
monk_nun$marriage_trans_rev[monk_nun$eHRAF_name=="Arab Canadians"] <- NA
monk_nun$marriage_trans_rev[monk_nun$eHRAF_name=="Cajuns"] <- NA
monk_nun$marriage_trans_rev[monk_nun$eHRAF_name=="Cuban Americans"] <- NA
monk_nun$marriage_trans_rev[monk_nun$eHRAF_name=="Serbian Americans"] <- 3
monk_nun$marriage_trans_rev[monk_nun$eHRAF_name=="Xinjiang Uygur Autonomous Region"] <- NA
marriage_trans_rev <- as.integer(monk_nun$marriage_trans_rev) + 1


## re-code the variables
## marriage transactions dplace
marriage_trans_dplace[marriage_trans_dplace=="Bride-wealth"] <- 1
marriage_trans_dplace[marriage_trans_dplace=="Bride-service"] <- 2
marriage_trans_dplace[marriage_trans_dplace=="Token bride-wealth"] <- 3
marriage_trans_dplace[marriage_trans_dplace=="Gift exchange"] <- 4
marriage_trans_dplace[marriage_trans_dplace=="Woman exchange"] <- 5
marriage_trans_dplace[marriage_trans_dplace=="Insignificant"] <- 6
marriage_trans_dplace[marriage_trans_dplace=="Dowry"] <- 7
marriage_trans_dplace <- as.integer(marriage_trans_dplace)

## fill in missing values from ehraf and code according to dplace
marriage_trans_dplace[monk_nun$eHRAF_name=="Bahia Brazilians"] <- 7
marriage_trans_dplace[monk_nun$eHRAF_name=="Badaga"] <- 1
marriage_trans_dplace[monk_nun$eHRAF_name=="British (1485-1603)"] <- 7
marriage_trans_dplace[monk_nun$eHRAF_name=="Chinese Americans"] <- 4
marriage_trans_dplace[monk_nun$eHRAF_name=="Croats"] <- 7
marriage_trans_dplace[monk_nun$eHRAF_name=="Inner Mongolia"] <- 4
marriage_trans_dplace[monk_nun$eHRAF_name=="Italian Americans"] <- 7
marriage_trans_dplace[monk_nun$eHRAF_name=="Italian Canadians"] <- 7
marriage_trans_dplace[monk_nun$eHRAF_name=="Montenegrins"] <- 6
marriage_trans_dplace[monk_nun$eHRAF_name=="Palestinians"] <- 1
marriage_trans_dplace[monk_nun$eHRAF_name=="Serbian Americans"] <- 7
marriage_trans_dplace[monk_nun$eHRAF_name=="Slovenes"] <- 7
marriage_trans_dplace[monk_nun$eHRAF_name=="Taiwan Hokkien"] <- 4


## marital composition dplace
marital_comp_dplace[marital_comp_dplace=="Monogamous"] <- 1
marital_comp_dplace[marital_comp_dplace=="Limited polygyny"] <- 2
marital_comp_dplace[marital_comp_dplace=="Polygyny, sororal cohabit"] <- 3
marital_comp_dplace[marital_comp_dplace=="Polygyny, sororal separate quarters"] <- 4
marital_comp_dplace[marital_comp_dplace=="Polygyny, non-sororal separate quarters"] <- 5
marital_comp_dplace[marital_comp_dplace=="Polygyny, non-sororal cohabit"] <- 6
marital_comp_dplace[marital_comp_dplace=="Polyandrous"] <- 7
marital_comp_dplace <- as.integer(marital_comp_dplace)

## fill in missing values from ehraf and code according to dplace
marital_comp_dplace[monk_nun$dplace_name=="Chugach"] <- 2
marital_comp_dplace[monk_nun$dplace_name=="Ajie"] <- 2
marital_comp_dplace[monk_nun$eHRAF_name=="Bahia Brazilians"] <- 1
marital_comp_dplace[monk_nun$eHRAF_name=="Badaga"] <- 2
marital_comp_dplace[monk_nun$eHRAF_name=="British (1485-1603)"] <- 1
marital_comp_dplace[monk_nun$eHRAF_name=="Chinese Americans"] <- 1
marital_comp_dplace[monk_nun$eHRAF_name=="Croats"] <- 1
marital_comp_dplace[monk_nun$eHRAF_name=="Inner Mongolia"] <- 2
marital_comp_dplace[monk_nun$eHRAF_name=="Italian Americans"] <- 1
marital_comp_dplace[monk_nun$eHRAF_name=="Italian Canadians"] <- 1
marital_comp_dplace[monk_nun$eHRAF_name=="Montenegrins"] <- 2
marital_comp_dplace[monk_nun$eHRAF_name=="Palestinians"] <- 1
marital_comp_dplace[monk_nun$eHRAF_name=="Serbian Americans"] <- 2
marital_comp_dplace[monk_nun$eHRAF_name=="Slovenes"] <- 1
marital_comp_dplace[monk_nun$eHRAF_name=="Taiwan Hokkien"] <- 2


## wealth inheritance
inheritance_dplace[inheritance_dplace=="No inher. of real property"] <- 1
inheritance_dplace[inheritance_dplace=="Matrilineal by sister's sons"] <- 2
inheritance_dplace[inheritance_dplace=="Matrilineal by heirs"] <- 3
inheritance_dplace[inheritance_dplace=="Children, less for daughters"] <- 4
inheritance_dplace[inheritance_dplace=="Children"] <- 5
inheritance_dplace[inheritance_dplace=="Patrilineal by heirs"] <- 6
inheritance_dplace[inheritance_dplace=="Patrilineal by sons"] <- 7
inheritance_dplace <- as.integer(inheritance_dplace)

## fill in missing values from ehraf and code according to dplace
inheritance_dplace[monk_nun$dplace_name=="Alorese"] <- 5 
inheritance_dplace[monk_nun$eHRAF_name=="Badaga"] <- 7 
inheritance_dplace[monk_nun$eHRAF_name=="Bahia Brazilians"] <- 7 
inheritance_dplace[monk_nun$eHRAF_name=="Bakairi"] <- 1 
inheritance_dplace[monk_nun$eHRAF_name=="Barama River Carib"] <- 1
inheritance_dplace[monk_nun$eHRAF_name=="Bau Fijians"] <- 7
inheritance_dplace[monk_nun$eHRAF_name=="Betsileo"] <- 7
inheritance_dplace[monk_nun$eHRAF_name=="British (1485-1603)"] <- 7
inheritance_dplace[monk_nun$eHRAF_name=="Burusho"] <- 7
inheritance_dplace[monk_nun$eHRAF_name=="Cajuns"] <- 7
inheritance_dplace[monk_nun$eHRAF_name=="Cayua"] <- 1
inheritance_dplace[monk_nun$eHRAF_name=="Chachi"] <- 5
inheritance_dplace[monk_nun$eHRAF_name=="Cherokee"] <- 3
inheritance_dplace[monk_nun$eHRAF_name=="Chinookans of the Lower Columbia River"] <- 1
inheritance_dplace[monk_nun$eHRAF_name=="Creek"] <- 3
inheritance_dplace[monk_nun$eHRAF_name=="Croats"] <- 7
inheritance_dplace[monk_nun$eHRAF_name=="Cuna"] <- 5
inheritance_dplace[monk_nun$eHRAF_name=="Eyak"] <- 1
inheritance_dplace[monk_nun$eHRAF_name=="Fox"] <- 1
inheritance_dplace[monk_nun$eHRAF_name=="Garifuna"] <- 1
inheritance_dplace[monk_nun$eHRAF_name=="Goajiro"] <- 1
inheritance_dplace[monk_nun$dplace_name=="Muria Gond"] <- 1
inheritance_dplace[monk_nun$eHRAF_name=="Hidatsa"] <- 3
inheritance_dplace[monk_nun$eHRAF_name=="Huron"] <- 1
inheritance_dplace[monk_nun$dplace_name=="Afikpo"] <- 7
inheritance_dplace[monk_nun$eHRAF_name=="Inner Mongolia"] <- 7
inheritance_dplace[monk_nun$eHRAF_name=="Iran"] <- 4
inheritance_dplace[monk_nun$eHRAF_name=="Iroquois"] <- 3
inheritance_dplace[monk_nun$eHRAF_name=="Island Carib"] <- 1
inheritance_dplace[monk_nun$eHRAF_name=="Italian Americans"] <- 5
inheritance_dplace[monk_nun$eHRAF_name=="Italian Canadians"] <- 5
inheritance_dplace[monk_nun$eHRAF_name=="Kanak"] <- 1
inheritance_dplace[monk_nun$eHRAF_name=="Kapauku"] <- 7
inheritance_dplace[monk_nun$eHRAF_name=="Karaja"] <- 1
inheritance_dplace[monk_nun$eHRAF_name=="Kaska"] <- 1
inheritance_dplace[monk_nun$eHRAF_name=="Kerala"] <- 3
inheritance_dplace[monk_nun$eHRAF_name=="Lau"] <- 1
inheritance_dplace[monk_nun$eHRAF_name=="Lesu"] <- 1
inheritance_dplace[monk_nun$eHRAF_name=="Malekula"] <- 7
inheritance_dplace[monk_nun$dplace_name=="Bikinians"] <- 3
inheritance_dplace[monk_nun$eHRAF_name=="Malekula"] <- 7
inheritance_dplace[monk_nun$eHRAF_name=="Mi'kmaq"] <- 1
inheritance_dplace[monk_nun$eHRAF_name=="Miskito"] <- 1
inheritance_dplace[monk_nun$eHRAF_name=="Montenegrins"] <- 7
inheritance_dplace[monk_nun$eHRAF_name=="Natchez"] <- 1
inheritance_dplace[monk_nun$eHRAF_name=="Navaho"] <- 1
inheritance_dplace[monk_nun$eHRAF_name=="Navaho"] <- 1
inheritance_dplace[monk_nun$eHRAF_name=="Nuu-chah-nulth"] <- 1
inheritance_dplace[monk_nun$eHRAF_name=="Orokaiva"] <- 6
inheritance_dplace[monk_nun$eHRAF_name=="Palestinians"] <- 4
inheritance_dplace[monk_nun$eHRAF_name=="Samoans"] <- 6
inheritance_dplace[monk_nun$eHRAF_name=="Samoyed"] <- 1
inheritance_dplace[monk_nun$eHRAF_name=="Saramaka"] <- 3
inheritance_dplace[monk_nun$eHRAF_name=="Seminole"] <- 1
inheritance_dplace[monk_nun$eHRAF_name=="Serbs"] <- 7
inheritance_dplace[monk_nun$eHRAF_name=="Slovenes"] <- 7
inheritance_dplace[monk_nun$eHRAF_name=="Taiwan Hokkien"] <- 7
inheritance_dplace[monk_nun$eHRAF_name=="Tamil"] <- 7
inheritance_dplace[monk_nun$eHRAF_name=="Tapirape"] <- 1
inheritance_dplace[monk_nun$eHRAF_name=="Terena"] <- 1
inheritance_dplace[monk_nun$eHRAF_name=="Tinputz"] <- 1
inheritance_dplace[monk_nun$eHRAF_name=="Tlingit"] <- 1
inheritance_dplace[monk_nun$eHRAF_name=="Tucano"] <- 1
inheritance_dplace[monk_nun$eHRAF_name=="Tupinamba"] <- 1
inheritance_dplace[monk_nun$eHRAF_name=="Turkmens"] <- 7
inheritance_dplace[monk_nun$eHRAF_name=="Tzeltal"] <- 6
inheritance_dplace[monk_nun$eHRAF_name=="Ulithi"] <- 3
inheritance_dplace[monk_nun$eHRAF_name=="Uru"] <- 5
inheritance_dplace[monk_nun$eHRAF_name=="Winnebago"] <- 1
inheritance_dplace[monk_nun$eHRAF_name=="Woleai"] <- 3
inheritance_dplace[monk_nun$eHRAF_name=="Xinjiang Uygur Autonomous Region"] <- 7
inheritance_dplace[monk_nun$dplace_name=="Guaica"] <- 1
inheritance_dplace[monk_nun$dplace_name=="Shiriana"] <- 1
inheritance_dplace[monk_nun$dplace_name=="Sanema"] <- 1
inheritance_dplace[monk_nun$eHRAF_name=="Yemen"] <- 4
inheritance_dplace[monk_nun$dplace_name=="Wukchumni"] <- 1
inheritance_dplace[monk_nun$dplace_name=="Ife"] <- 6
inheritance_dplace[monk_nun$dplace_name=="Yurok"] <- 4


## re-code social stratification
class_dplace[class_dplace=="Absence of distinctions"] <- 1
class_dplace[class_dplace=="Wealth distinctions"] <- 2
class_dplace[class_dplace=="Elite stratification"] <- 3
class_dplace[class_dplace=="Dual stratification"] <- 4
class_dplace[class_dplace=="Complex stratification"] <- 5
class_dplace <- as.integer(class_dplace)

## fill in missing values from ehraf and code according to dplace
class_dplace[monk_nun$dplace_name=="Kol"] <- 2
class_dplace[monk_nun$dplace_name=="Betsileo"] <- 4
class_dplace[monk_nun$dplace_name=="Akyem"] <- 4
class_dplace[monk_nun$dplace_name=="Shona"] <- 4
class_dplace[monk_nun$dplace_name=="Sanusi"] <- 3
class_dplace[monk_nun$dplace_name=="Northern Pomo"] <- 2
class_dplace[monk_nun$dplace_name=="Tzeltal"] <- 5
class_dplace[monk_nun$dplace_name=="Tucano"] <- 1
class_dplace[monk_nun$dplace_name=="Uru"] <- 1
class_dplace[monk_nun$eHRAF_name=="Bahia Brazilians"] <- 5
class_dplace[monk_nun$eHRAF_name=="Badaga"] <- 5
class_dplace[monk_nun$eHRAF_name=="British (1485-1603)"] <- 5
class_dplace[monk_nun$eHRAF_name=="Chinese Americans"] <- 5
class_dplace[monk_nun$eHRAF_name=="Croats"] <- 5
class_dplace[monk_nun$eHRAF_name=="Inner Mongolia"] <- 4
class_dplace[monk_nun$eHRAF_name=="Italian Americans"] <- 5
class_dplace[monk_nun$eHRAF_name=="Italian Canadians"] <- 5
class_dplace[monk_nun$eHRAF_name=="Montenegrins"] <- 5
class_dplace[monk_nun$eHRAF_name=="Palestinians"] <- 5
class_dplace[monk_nun$eHRAF_name=="Serbian Americans"] <- 5
class_dplace[monk_nun$eHRAF_name=="Slovenes"] <- 5
class_dplace[monk_nun$eHRAF_name=="Taiwan Hokkien"] <- 4


## religion
religion <- as.integer(monk_nun$religion_ehraf) + 1


## finally, create a data set that we'll use for analyses
monk_nun_final <- data.frame(cbind(monk_nun$eHRAF_name,
                                   monk_nun$dplace_name,
                                   monk_nun$language_ASJP_upper,
                                   monk_nun$Lat,
                                   monk_nun$Long,
                                   celibacy_cat,
                                   celibacy_cat_rev,
                                   marital_comp,
                                   marital_comp_rev,
                                   marital_comp_dplace,
                                   inheritance_dplace,
                                   marriage_trans,
                                   marriage_trans_rev,
                                   marriage_trans_dplace,
                                   class_dplace,
                                   religion,
                                   region
)) 
colnames(monk_nun_final) <- c("eHRAF_name",
                              "dplace_name",
                              "language",
                              "latitude",
                              "longitude",
                              "celibacy_cat",
                              "celibacy_cat_rev",
                              "marital_comp",
                              "marital_comp_rev",
                              "marital_comp_dplace",
                              "inheritance_dplace",
                              "marriage_trans",
                              "marriage_trans_rev",
                              "marriage_trans_dplace",
                              "class",
                              "religion",
                              "region"
)
write.csv(monk_nun_final,file="monk_nun_final.csv",row.names=FALSE) # write the final data set


################################################################################