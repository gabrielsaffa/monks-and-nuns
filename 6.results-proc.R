################################################################################


library(rethinking)

setwd("")
cel_tr <- read.csv("cel_tr_post.csv")
cel_tr_rel <- read.csv("cel_tr_rel_post.csv")
cel_tr_eur <- read.csv("cel_tr_eur_post.csv")
cel_tr_rel_eur <- read.csv("cel_tr_rel_eur_post.csv")
cel_mar <- read.csv("cel_mar_post.csv")
cel_mar_rel <- read.csv("cel_mar_rel_post.csv")
cel_mar_eur <- read.csv("cel_mar_eur_post.csv")
cel_mar_rel_eur <- read.csv("cel_mar_rel_eur_post.csv")
cel_in <- read.csv("cel_in_post.csv")
cel_in_rel <- read.csv("cel_in_rel_post.csv")
cel_in_eur <- read.csv("cel_in_eur_post.csv")
cel_in_rel_eur <- read.csv("cel_in_rel_eur_post.csv")
monk_tr <- read.csv("monk_tr_post.csv")
monk_tr_rel <- read.csv("monk_tr_rel_post.csv")
monk_tr_eur <- read.csv("monk_tr_eur_post.csv")
monk_tr_rel_eur <- read.csv("monk_tr_rel_eur_post.csv")
monk_mar <- read.csv("monk_mar_post.csv")
monk_mar_rel <- read.csv("monk_mar_rel_post.csv")
monk_mar_eur <- read.csv("monk_mar_eur_post.csv")
monk_mar_rel_eur <- read.csv("monk_mar_rel_eur_post.csv")
monk_in <- read.csv("monk_in_post.csv")
monk_in_rel <- read.csv("monk_in_rel_post.csv")
monk_in_eur <- read.csv("monk_in_eur_post.csv")
monk_in_rel_eur <- read.csv("monk_in_rel_eur_post.csv")
nun_tr <- read.csv("nun_tr_post.csv")
nun_tr_rel <- read.csv("nun_tr_rel_post.csv")
nun_tr_eur <- read.csv("nun_tr_eur_post.csv")
nun_tr_rel_eur <- read.csv("nun_tr_rel_eur_post.csv")
nun_mar <- read.csv("nun_mar_post.csv")
nun_mar_rel <- read.csv("nun_mar_rel_post.csv")
nun_mar_eur <- read.csv("nun_mar_eur_post.csv")
nun_mar_rel_eur <- read.csv("nun_mar_rel_eur_post.csv")


## compute posterior probabilities (% PP)
## celibacy
nrow <- 2000
round((sum(cel_tr$bT>0)/nrow)*100,2) # marriage transactions: 82.65 %
round((sum(cel_mar$bM>0)/nrow)*100,2) # marital composition: 99.95 %
round((sum(cel_in$bI>0)/nrow)*100,2) # wealth inheritance: 89.15 %

## celibacy - Eurasia
round((sum(cel_tr_eur$bT>0)/nrow)*100,2) # marriage transactions: 61.20 %
round((sum(cel_mar_eur$bM>0)/nrow)*100,2) # marital composition: 94.75 %
round((sum(cel_in_eur$bI>0)/nrow)*100,2) # wealth inheritance: 84.55 %

## monks 
round((sum(monk_tr$bT>0)/nrow)*100,2) # marriage transactions: 95.70 %
round((sum(monk_mar$bM>0)/nrow)*100,2) # marital composition: 85.75 %
round((sum(monk_in$bI>0)/nrow)*100,2) # wealth inheritance: 90.35 %

## monks - Eurasia
round((sum(monk_tr_eur$bT>0)/nrow)*100,2) # marriage transactions: 86.15 %
round((sum(monk_mar_eur$bM>0)/nrow)*100,2) # marital composition: 86.45 %
round((sum(monk_in_eur$bI>0)/nrow)*100,2) # wealth inheritance: 66.80 %

## nuns 
round((sum(nun_tr$bT>0)/nrow)*100,2) # marriage transactions: 82.90 %
round((sum(nun_mar$bM>0)/nrow)*100,2) # marital composition: 88.15 %

## nuns - Eurasia
round((sum(nun_tr_eur$bT>0)/nrow)*100,2) # marriage transactions: 92.50 %
round((sum(nun_mar_eur$bM>0)/nrow)*100,2) # marital composition: 84.80 %


## religion
## marriage transactions
## celibacy
round((sum(cel_tr_rel$bT.1>0)/nrow)*100,2) # Indigenous: 12.75 %
round((sum(cel_tr_rel$bT.2>0)/nrow)*100,2) # Islam: 36.20 %
round((sum(cel_tr_rel$bT.3>0)/nrow)*100,2) # Christianity: 90.30 %
round((sum(cel_tr_rel$bT.4>0)/nrow)*100,2) # Buddhism: 54.95 %
round((sum(cel_tr_rel$bT.5>0)/nrow)*100,2) # Hinduism: 44.15 %

## celibacy - Eurasia
round((sum(cel_tr_rel_eur$bT.1>0)/nrow)*100,2) # Indigenous: 20.50 %
round((sum(cel_tr_rel_eur$bT.2>0)/nrow)*100,2) # Islam: 34.80 %
round((sum(cel_tr_rel_eur$bT.3>0)/nrow)*100,2) # Christianity: 77.45 %
round((sum(cel_tr_rel_eur$bT.4>0)/nrow)*100,2) # Buddhism: 59.55 %
round((sum(cel_tr_rel_eur$bT.5>0)/nrow)*100,2) # Hinduism: 42.90 %

## monks
round((sum(monk_tr_rel$bT.1>0)/nrow)*100,2) # Indigenous: 33.80 %
round((sum(monk_tr_rel$bT.2>0)/nrow)*100,2) # Islam: 72.20 %
round((sum(monk_tr_rel$bT.3>0)/nrow)*100,2) # Christianity: 86.75 %
round((sum(monk_tr_rel$bT.4>0)/nrow)*100,2) # Buddhism: 95.25 %
round((sum(monk_tr_rel$bT.5>0)/nrow)*100,2) # Hinduism: 76.00 %

## monks - Eurasia
round((sum(monk_tr_rel_eur$bT.1>0)/nrow)*100,2) # Indigenous: 28.70 %
round((sum(monk_tr_rel_eur$bT.2>0)/nrow)*100,2) # Islam: 72.75 %
round((sum(monk_tr_rel_eur$bT.3>0)/nrow)*100,2) # Christianity: 85.10 %
round((sum(monk_tr_rel_eur$bT.4>0)/nrow)*100,2) # Buddhism: 96.80 %
round((sum(monk_tr_rel_eur$bT.5>0)/nrow)*100,2) # Hinduism: 76.65 %

## nuns
round((sum(nun_tr_rel$bT.1>0)/nrow)*100,2) # Indigenous: 59.65 %
round((sum(nun_tr_rel$bT.2>0)/nrow)*100,2) # Islam: 50.10 %
round((sum(nun_tr_rel$bT.3>0)/nrow)*100,2) # Christianity: 76.15 %
round((sum(nun_tr_rel$bT.4>0)/nrow)*100,2) # Buddhism: 80.70 %
round((sum(nun_tr_rel$bT.5>0)/nrow)*100,2) # Hinduism: 46.75 %

## nuns - Eurasia
round((sum(nun_tr_rel_eur$bT.1>0)/nrow)*100,2) # Indigenous: 61.75 %
round((sum(nun_tr_rel_eur$bT.2>0)/nrow)*100,2) # Islam: 50.35 %
round((sum(nun_tr_rel_eur$bT.3>0)/nrow)*100,2) # Christianity: 84.95 %
round((sum(nun_tr_rel_eur$bT.4>0)/nrow)*100,2) # Buddhism: 81.95 %
round((sum(nun_tr_rel_eur$bT.5>0)/nrow)*100,2) # Hinduism: 43.90 %


## marital composition
## celibacy
round((sum(cel_mar_rel$bM.1>0)/nrow)*100,2) # Indigenous: 61.95 %
round((sum(cel_mar_rel$bM.2>0)/nrow)*100,2) # Islam: 44.25 %
round((sum(cel_mar_rel$bM.3>0)/nrow)*100,2) # Christianity: 75.80 %
round((sum(cel_mar_rel$bM.4>0)/nrow)*100,2) # Buddhism: 63.60 %
round((sum(cel_mar_rel$bM.5>0)/nrow)*100,2) # Hinduism: 45.80 %

## celibacy - Eurasia
round((sum(cel_mar_rel_eur$bM.1>0)/nrow)*100,2) # Indigenous: 25.60 %
round((sum(cel_mar_rel_eur$bM.2>0)/nrow)*100,2) # Islam: 39.75 %
round((sum(cel_mar_rel_eur$bM.3>0)/nrow)*100,2) # Christianity: 58.85 %
round((sum(cel_mar_rel_eur$bM.4>0)/nrow)*100,2) # Buddhism: 72.70 %
round((sum(cel_mar_rel_eur$bM.5>0)/nrow)*100,2) # Hinduism: 38.55 %

## monks
round((sum(monk_mar_rel$bM.1>0)/nrow)*100,2) # Indigenous: 41.05 %
round((sum(monk_mar_rel$bM.2>0)/nrow)*100,2) # Islam: 70.15 %
round((sum(monk_mar_rel$bM.3>0)/nrow)*100,2) # Christianity: 21.90 %
round((sum(monk_mar_rel$bM.4>0)/nrow)*100,2) # Buddhism: 96.20 %
round((sum(monk_mar_rel$bM.5>0)/nrow)*100,2) # Hinduism: 70.00 %

## monks - Eurasia
round((sum(monk_mar_rel_eur$bM.1>0)/nrow)*100,2) # Indigenous: 32.45 %
round((sum(monk_mar_rel_eur$bM.2>0)/nrow)*100,2) # Islam: 74.40 %
round((sum(monk_mar_rel_eur$bM.3>0)/nrow)*100,2) # Christianity: 68.50 %
round((sum(monk_mar_rel_eur$bM.4>0)/nrow)*100,2) # Buddhism: 96.25 %
round((sum(monk_mar_rel_eur$bM.5>0)/nrow)*100,2) # Hinduism: 73.80 %

## nuns
round((sum(nun_mar_rel$bM.1>0)/nrow)*100,2) # Indigenous: 61.65 %
round((sum(nun_mar_rel$bM.2>0)/nrow)*100,2) # Islam: 48.15 %
round((sum(nun_mar_rel$bM.3>0)/nrow)*100,2) # Christianity: 68.45 %
round((sum(nun_mar_rel$bM.4>0)/nrow)*100,2) # Buddhism: 63.80 %
round((sum(nun_mar_rel$bM.5>0)/nrow)*100,2) # Hinduism: 57.35 %

## nuns - Eurasia
round((sum(nun_mar_rel_eur$bM.1>0)/nrow)*100,2) # Indigenous: 55.55 %
round((sum(nun_mar_rel_eur$bM.2>0)/nrow)*100,2) # Islam: 50.35 %
round((sum(nun_mar_rel_eur$bM.3>0)/nrow)*100,2) # Christianity: 72.75 %
round((sum(nun_mar_rel_eur$bM.4>0)/nrow)*100,2) # Buddhism: 64.40 %
round((sum(nun_mar_rel_eur$bM.5>0)/nrow)*100,2) # Hinduism: 56.15 %


## wealth inheritance
## celibacy
round((sum(cel_in_rel$bI.1>0)/nrow)*100,2) # Indigenous: 44.75 %
round((sum(cel_in_rel$bI.2>0)/nrow)*100,2) # Islam: 52.05 %
round((sum(cel_in_rel$bI.3>0)/nrow)*100,2) # Christianity: 50.10 %
round((sum(cel_in_rel$bI.4>0)/nrow)*100,2) # Buddhism: 50.15 %
round((sum(cel_in_rel$bI.5>0)/nrow)*100,2) # Hinduism: 52.20 %

## celibacy - Eurasia
round((sum(cel_in_rel_eur$bI.1>0)/nrow)*100,2) # Indigenous: 49.05 %
round((sum(cel_in_rel_eur$bI.2>0)/nrow)*100,2) # Islam: 51.95 %
round((sum(cel_in_rel_eur$bI.3>0)/nrow)*100,2) # Christianity: 71.30 %
round((sum(cel_in_rel_eur$bI.4>0)/nrow)*100,2) # Buddhism: 55.35 %
round((sum(cel_in_rel_eur$bI.5>0)/nrow)*100,2) # Hinduism: 49.70 %

## monks
round((sum(monk_in_rel$bI.1>0)/nrow)*100,2) # Indigenous: 32.10 %
round((sum(monk_in_rel$bI.2>0)/nrow)*100,2) # Islam: 70.60 %
round((sum(monk_in_rel$bI.3>0)/nrow)*100,2) # Christianity: 90.80 %
round((sum(monk_in_rel$bI.4>0)/nrow)*100,2) # Buddhism: 81.70 %
round((sum(monk_in_rel$bI.5>0)/nrow)*100,2) # Hinduism: 72.75 %

## monks - Eurasia
round((sum(monk_in_rel_eur$bI.1>0)/nrow)*100,2) # Indigenous: 34.00 %
round((sum(monk_in_rel_eur$bI.2>0)/nrow)*100,2) # Islam: 63.75 %
round((sum(monk_in_rel_eur$bI.3>0)/nrow)*100,2) # Christianity: 71.80 %
round((sum(monk_in_rel_eur$bI.4>0)/nrow)*100,2) # Buddhism: 77.85 %
round((sum(monk_in_rel_eur$bI.5>0)/nrow)*100,2) # Hinduism: 69.50 %


################################################################################