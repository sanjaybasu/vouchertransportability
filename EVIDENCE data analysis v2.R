# EVIDENCE data analysis v2
# sanjay_basu@hms.harvard.edu



library(readxl)
library(readr)
library(dplyr)
library(tidyr)
library(mice)
library(tableone)
library(doBy)
setwd("~/Box/Research/Research projects/CHIVES & EVIDENCE Data for Sanjay - 1.29.2020")


demoe <- read_csv("EVIDENCE/EVIDENCE Main Project - BL & M6 - data download - 1.28.2020.csv")
democ <- read_csv("CHIVES/CHIVES Main Project - BL & M6 - data download - 1.28.2020.csv")

colnames(demoe)
colnames(democ)

demoe = demoe %>%
  mutate(pid = evidence_id) %>%
  select(pid, age_in_years, sex, education_level, race___3, ethnicity, hh_monthly_income, ppl_in_hh, fa_snap_ppt, fa_wic_ppt) %>%
  drop_na(pid) 
democ = democ %>%
  mutate(pid = chives_id) %>%
  select(pid, age_in_years, sex, education_level, race___3, ethnicity, hh_monthly_income, ppl_in_hh, fa_snap_ppt, fa_wic_ppt) %>%
  drop_na(pid)


demo = rbind(democ,demoe)


r9c = read_excel("CHIVES Food Group Diet Data - 3.5.2020.xlsx", skip = 1)
r4c = read_excel("CHIVES/CHIVES Dietary Data - 11.12.2019.xlsx")

r9c = r9c %>%
  select(-`Date of Intake`) %>%
  mutate(pid = `Participant ID`,
         time = `Project Abbreviation`) %>%
  group_by(pid,time) %>%
  summarise_each(funs(mean)) %>%
  select(-`Project Abbreviation`,-`Participant ID`) %>%
  filter(time != "M12") %>%
  mutate(loc = "SF")

r4c = r4c %>%
  select(-c(2:6, 8:12)) %>%
  mutate(pid = `Participant ID`,
         time = `Site ID`) %>%
  group_by(pid,time) %>%
  summarise_each(funs(mean)) %>%
  select(-`Site ID`,-`Participant ID`) %>%
  filter(time != "M12")

rc = full_join(r9c,r4c,by=c("pid"="pid","time"="time"))

rc$citrus_juiceC = rc$`Citrus Juice` / 2
rc$fruit_juice_nocitrusC = rc$`Fruit Juice excluding Citrus Juice` / 2
rc$citrus_fruitC = rc$`Citrus Fruit` * (3/8)
rc$fruit_nocitrusC = rc$`Fruit excluding Citrus Fruit` * (3/8)
rc$avocadoC = rc$`Avocado and Similar` / 2
rc$fried_fruitC = rc$`Fried Fruits` / 2
rc$fruit_savory_snackC = rc$`Fruit-based Savory Snack` / 8
rc$darkgreen_vegC = rc$`Dark-green Vegetables` / 2
rc$deepyellow_vegC = rc$`Deep-yellow Vegetables` / 2
rc$tomatoC = rc$Tomato * (3/8)
rc$white_potatoesC = rc$`White Potatoes` / 2
rc$fried_potatoesC = rc$`Fried Potatoes` / 2
rc$starchy_vegC = rc$`Other Starchy Vegetables` / 2
rc$legumesC = rc$`Legumes (cooked dried beans)` / 2
rc$other_vegC = rc$`Other Vegetables` / 2
rc$fried_vegC = rc$`Fried Vegetables` / 2
rc$veg_juiceC = rc$`Vegetable Juice` / 2
fruitsC = c("citrus_fruitC", "fruit_nocitrusC")
rc$fruitsC = apply(rc[, names(rc) %in% fruitsC], 1, function(x) sum(x, na.rm=TRUE))
vegC = c("darkgreen_vegC", "deepyellow_vegC", "tomatoC", "starchy_vegC")
rc$vegC = apply(rc[, names(rc) %in% vegC], 1, function(x) sum(x, na.rm=TRUE))
rc$fvC = rc$fruitsC + rc$vegC

blc = rc %>%
  filter(time=="BL") %>%
  mutate(fvCbl = fvC) %>%
  select(pid,fvCbl,loc) 
  
m6c = rc %>%
  filter(time=="M6") %>%
  mutate(fvCm6 = fvC) %>%
  select(pid,fvCm6) 

rc = full_join(blc, m6c, by=("pid")) %>%
  mutate(fvCdiff = fvCm6-fvCbl)
  
r9e = read_excel("EVIDENCE Food Group Diet Data - 3.4.2020.xlsx", skip = 1)
r4e = read_excel("EVIDENCE/EVIDENCE Diet Data - 11.25.2019.xlsx")


r9e = r9e %>%
  select(-`Date of Intake`) %>%
  mutate(pid = `Participant ID`,
         time = `Project Abbreviation`) %>%
  group_by(pid,time) %>%
  summarise_each(funs(mean)) %>%
  select(-`Project Abbreviation`,-`Participant ID`,-`Location(1SF2LA)`)

r9eloc = read_excel("EVIDENCE Food Group Diet Data - 3.4.2020.xlsx", skip = 1)

r9eloc = r9eloc %>%
  select(`Participant ID`,`Location(1SF2LA)`) %>%
  mutate(pid = `Participant ID`,
         loc = as.character(`Location(1SF2LA)`)) %>%
  select(pid,loc)
r9eloc$loc[r9eloc$loc==1]="SFev"
r9eloc$loc[r9eloc$loc==2]="LA"
r9eloc = r9eloc %>%
  distinct(pid,loc)
r9e = r9e %>%
  left_join(r9eloc, by="pid")


r4e = r4e %>%
  select(-c(2, 4:8)) %>%
  mutate(pid = `Participant ID`,
         time = `Site ID`) %>%
  group_by(pid,time) %>%
  summarise_each(funs(mean)) %>%
  select(-`Site ID`,-`Participant ID`)


re = full_join(r9e,r4e,by=c("pid"="pid","time"="time"))

re$citrus_juiceC = re$`Citrus Juice` / 2
re$fruit_juice_nocitrusC = re$`Fruit Juice excluding Citrus Juice` / 2
re$citrus_fruitC = re$`Citrus Fruit` * (3/8)
re$fruit_nocitrusC = re$`Fruit excluding Citrus Fruit` * (3/8)
re$avocadoC = re$`Avocado and Similar` / 2
re$fried_fruitC = re$`Fried Fruits` / 2
re$fruit_savory_snackC = re$`Fruit-based Savory Snack` / 8
re$darkgreen_vegC = re$`Dark-green Vegetables` / 2
re$deepyellow_vegC = re$`Deep-yellow Vegetables` / 2
re$tomatoC = re$Tomato * (3/8)
re$white_potatoesC = re$`White Potatoes` / 2
re$fried_potatoesC = re$`Fried Potatoes` / 2
re$starchy_vegC = re$`Other Starchy Vegetables` / 2
re$legumesC = re$`Legumes (cooked dried beans)` / 2
re$other_vegC = re$`Other Vegetables` / 2
re$fried_vegC = re$`Fried Vegetables` / 2
re$veg_juiceC = re$`Vegetable Juice` / 2
fruitsC = c("citrus_fruitC", "fruit_nocitrusC")
re$fruitsC = apply(re[, names(re) %in% fruitsC], 1, function(x) sum(x, na.rm=TRUE))
vegC = c("darkgreen_vegC", "deepyellow_vegC", "tomatoC", "starchy_vegC")
re$vegC = apply(re[, names(re) %in% vegC], 1, function(x) sum(x, na.rm=TRUE))
re$fvC = re$fruitsC + re$vegC

ble = re %>%
  filter(time=="BL") %>%
  mutate(fvCbl = fvC) %>%
  select(pid,fvCbl,loc) 

m6e = re %>%
  filter(time=="M6") %>%
  mutate(fvCm6 = fvC) %>%
  select(pid,fvCm6) 

re = full_join(ble, m6e, by=("pid")) %>%
  mutate(fvCdiff = fvCm6-fvCbl)

r = rbind(rc,re) 
r$loc[is.na(r$loc)]="LA"

df = demo %>%
  full_join(r, by="pid") 


varsToFactor <- c("sex","education_level","race___3","ethnicity", "fa_snap_ppt","fa_wic_ppt")
df[varsToFactor] <- lapply(df[varsToFactor], factor)

vars <- c("age_in_years", "sex", "race___3", "ethnicity", "education_level", "hh_monthly_income", "ppl_in_hh", "fa_snap_ppt","fa_wic_ppt")
tableOne <- CreateTableOne(vars = vars, strata = c("loc"), data = df)
tableOne

print(tableOne, nonnormal = c("age_in_years","hh_monthly_income","ppl_in_hh"), quote = TRUE)


table(df$loc)
summary(df$fvCm6)
summary(df$fvCbl)
summary(df$fvCdiff)

summary(df$fvCm6[df$loc=="SF"])
summary(df$fvCbl[df$loc=="SF"])
summary(df$fvCdiff[df$loc=="SF"])

summary(df$fvCm6[df$loc=="SFev"])
summary(df$fvCbl[df$loc=="SFev"])
summary(df$fvCdiff[df$loc=="SFev"])

summary(df$fvCm6[df$loc=="LA"])
summary(df$fvCbl[df$loc=="LA"])
summary(df$fvCdiff[df$loc=="LA"])

t.test(df$fvCbl,df$fvCm6, paired =T)
t.test(df$fvCbl[df$loc=="SF"],df$fvCm6[df$loc=="SF"], paired =T)
t.test(df$fvCbl[df$loc=="SFev"],df$fvCm6[df$loc=="SFev"], paired =T)
t.test(df$fvCbl[df$loc=="LA"],df$fvCm6[df$loc=="LA"], paired =T)

dfe = demoe %>%
  full_join(re, by="pid")
reg=(lm(fvCdiff~age_in_years+sex+race___3+ethnicity+loc, data=dfe))
summary(reg)
confint(reg)



# voucher redemption rate


redempc <- read_excel("CHIVES/CHIVES Redemption Tracker - ALL TO DATE 11.11.2019.xlsx")
redempc = redempc %>%
  group_by(`CHIVES ID`) %>%
  tally()

redemp <- read_excel("EVIDENCE/EVIDENCE Redemption Tracker - ALL TO DATE 2.4.2020 copy.xlsx")
redemp = redemp %>%
  group_by(evidence_id) %>%
  tally()

denomc <- read_excel("CHIVES/CHIVES # Vouchers per Ppt - 2.4.2020 copy.xlsx")
redratec = full_join(redempc,denomc,by="CHIVES ID")
redratec = redratec %>%
  mutate(redrate = n/`Total # Vouchers Received`,
         pid =  `CHIVES ID`) %>%
  select(pid, redrate)

denom <- read_excel("EVIDENCE/EVIDENCE # Vouchers per Ppt - 2.4.2020 copy.xlsx")
redrate = full_join(redemp,denom,by="evidence_id")
redrate = redrate %>%
  mutate(redrate = n/`Total # Vouchers`,
         pid =  evidence_id) %>%
  select(pid, redrate)

redrate = rbind(redrate,redratec)


dfe = dfe %>%
  full_join(redrate, by=c("pid"="pid"))

summary(dfe$redrate)
summary(dfe$redrate[df$loc=="SF"])
summary(dfe$redrate[df$loc=="SFev"])
summary(dfe$redrate[df$loc=="LA"])
summary(dfe$redrate[df$loc!="LA"])


dfe_sub  = dfe %>%
  filter(loc!="SF")
reg1=(lm(fvCdiff~age_in_years+sex+race___3+ethnicity+loc*redrate, data=dfe_sub))
summary(reg1)
confint(reg1)



# SSB consumption

ssbs = r9e %>% 
  select("time",contains("Sweetened")&(contains("Beverage")|contains("Drinks")|contains("Coffee")|contains("Tea")|contains("Water"))&(!contains("Artificially"))&(!contains("Unsweetened"))) %>%
  ungroup %>%
  mutate(ssb = rowSums(select(., -"pid", -"time")))
ssbbl = ssbs %>%
  filter(time=="BL")%>%
  mutate(ssbbl = ssb) %>%
  select(pid,ssbbl) 
ssbm6 = ssbs %>%
  filter(time=="M6")%>%
  mutate(ssbm6 = ssb) %>%
  select(pid,ssbm6) 
ssbe = full_join(ssbbl,ssbm6) %>%
  mutate(ssbdiff = ssbm6-ssbbl)

dfe = full_join(dfe,ssbe)
reg2=(lm(fvCdiff~age_in_years+sex+race___3+ethnicity+loc+ssbbl+ssbdiff, data=dfe))
summary(reg2)
confint(reg2)


summary(dfe$ssbbl[dfe$loc=="SFev"])
summary(dfe$ssbm6[dfe$loc=="SFev"])
summary(dfe$ssbdiff[dfe$loc=="SFev"])
t.test(dfe$ssbm6[dfe$loc=="SFev"],dfe$ssbbl[dfe$loc=="SFev"], paired =T)

summary(dfe$ssbbl[dfe$loc=="LA"])
summary(dfe$ssbm6[dfe$loc=="LA"])
summary(dfe$ssbdiff[dfe$loc=="LA"])
t.test(dfe$ssbm6[dfe$loc=="LA"],dfe$ssbbl[dfe$loc=="LA"], paired =T)

t.test(dfe$ssbbl[dfe$loc=="SFev"],dfe$ssbbl[dfe$loc=="LA"])
t.test(dfe$ssbm6[dfe$loc=="SFev"],dfe$ssbm6[dfe$loc=="LA"])

reg3=(lm(ssbdiff~age_in_years+sex+race___3+ethnicity+loc, data=dfe))
summary(reg3)
confint(reg3)


# HEI
##### HEI ######


##Preliminary HEI function

##Interpolation function
interp = function(x, x1, y1, x2, y2) {
  m = (y2-y1) / (x2-x1)
  b = y2-m*x2
  y = NA
  if (!is.na(x) & x<=x1) {
    y = y1
  } else if (!is.na(x) & x>x1 & x<x2) {
    y = m*x + b
  } else if (!is.na(x) & x>=x2) {
    y = y2
  }
  y
}

##HEI calculation
hei_ndsr = function(r4, r9) {
  ##Define categories of foods
  ##Adequacy
  total_fruits = c("citrus_juice", "fruit_juice_nocitrus", "citrus_fruit", "fruit_nocitrus", "avocado", "fried_fruit", "fruit_savory_snack")
  whole_fruits = c("citrus_fruit", "fruit_nocitrus", "avocado")
  total_veg = c("darkgreen_veg", "deepyellow_veg", "tomato", "white_potatoes", "fried_potatoes", "starchy_veg", "legumes", "other_veg", "fried_veg", "veg_juice")
  greens_beans = c("darkgreen_veg", "legumes")
  whole_grains = c("whole_grain_mix","whole_grain_bread","whole_grain_other","whole_grain_cracker","whole_grain_pasta","whole_grain_cereal_nosweet","whole_grain_cereal_sweet","whole_grain_cookie","whole_grain_chip","whole_grain_bar")
  dairy = c("milk_whole", "milk_reduced_fat", "milk_low_fat", "milk_nondairy", "milk_flavored_whole", "milk_flavored_reduced_fat", "milk_flavored_low_fat", "sweetened_nonfat_dry_milk", "artificially_sweetened_nonfat_dry_milk", "cheese_full_fat", "cheese_reduced_fat", "cheese_low_fat", "cheese_nondairy", "yogurt_sweetened_whole", "yogurt_sweetened_low", "yogurt_sweetened_free", "yogurt_artificially_sweetened_whole", "yogurt_artificially_sweetened_low", "yogurt_artificially_sweetened_free", "yogurt_nondairy", "frozen_dairy", "frozen_nondairy", "pudding_other", "artificially_sweetened_pudding", "dairy_sweetened_replacement", "dairy_artificially_sweetened_replacement")
  total_protein = c("legumes", "beef", "lean_beef", "veal", "lean_veal", "lamb", "lean_lamb", "fresh_pork", "lean_fresh_pork", "cured_pork", "lean_cured_pork", "game", "poultry", "lean_poultry", "fried_chicken", "fresh_fish", "lean_fish", "fried_fish", "shellfish", "fried_shellfish", "coldcuts_sausage", "lean_coldcuts_sausage", "organ_meat", "eggs", "eggs_substitute", "nuts_seeds", "nut_seed_butters", "meat_substitute")
  sea_plant_protein = c("legumes", "fresh_fish", "lean_fish", "fried_fish", "shellfish", "fried_shellfish")
  #fatty_acids #(PUFAs + MUFAs)/SFAs
  
  ##Moderation
  refined_grains = c("refined_grain_mix","refined_grain_bread","refined_grain_other","refined_grain_cracker","refined_grain_pasta","refined_grain_cereal_nosweet","refined_grain_cereal_sweet","refined_grain_cookie","refined_grain_bar","refined_grain_chip")
  #sodium #convert to g;
  #added_sugars #convert to %energy
  #saturated_fats #convert to %energy
  
  ##Look at reports 4 and 9 and pull out necessary components
  names(r4)
  r4a = r4[, names(r4) %in% c("pid", "time", "Energy (kcal)", "Total Fat (g)", "Total Carbohydrate (g)", "Total Protein (g)", "Total Saturated Fatty Acids (SFA) (g)", "Total Monounsaturated Fatty Acids (MUFA) (g)", "Total Polyunsaturated Fatty Acids (PUFA) (g)", "Added Sugars (by Total Sugars) (g)", "Sodium (mg)")]
  names(r4a) = c("ID", "date", "calories", "fat_g", "carb_g", "protein_g", "sfa_g", "mufa_g", "pufa_g", "sodium_mg", "added_sugars_g")
  
  names(r9)
  r9a = r9[, names(r9) %in% c("pid", "time", "Citrus Juice", "Fruit Juice excluding Citrus Juice", "Citrus Fruit", "Fruit excluding Citrus Fruit", "Avocado and Similar", "Fried Fruits", "Fruit-based Savory Snack", "Dark-green Vegetables", "Deep-yellow Vegetables", "Tomato", "White Potatoes", "Fried Potatoes", "Other Starchy Vegetables", "Legumes (cooked dried beans)", "Other Vegetables", "Fried Vegetables", "Vegetable Juice", "Grains, Flour and Dry Mixes - Whole Grain", "Grains, Flour and Dry Mixes - Refined Grain", "Loaf-type Bread and Plain Rolls - Whole Grain", "Loaf-type Bread and Plain Rolls - Refined Grain", "Other Breads (quick breads, corn muffins, tortillas) - Whole Grain", "Other Breads (quick breads, corn muffins, tortillas) - Refined Grain", "Crackers - Whole Grain", "Crackers - Refined Grain", "Pasta - Whole Grain", "Pasta - Refined Grain", "Ready-to-eat Cereal (not presweetened) - Whole Grain", "Ready-to-eat Cereal (not presweetened) - Refined Grain", "Ready-to-eat Cereal (presweetened) - Whole Grain", "Ready-to-eat Cereal (presweetened) - Refined Grain", "Cakes, Cookies, Pies, Pastries, Danish, Doughnuts and Cobblers - Whole Grain", "Cakes, Cookies, Pies, Pastries, Danish, Doughnuts and Cobblers - Refined Grain", "Snack Bars - Whole Grain", "Snack Bars - Refined Grain", "Snack Chips - Whole Grain", "Snack Chips - Refined Grain", "Beef", "Lean Beef", "Veal", "Lean Veal", "Lamb", "Lean Lamb", "Fresh Pork", "Lean Fresh Pork", "Cured Pork", "Lean Cured Pork", "Game", "Poultry", "Lean Poultry", "Fried Chicken - Commercial EntrÈe and Fast Food", "Fish - Fresh and Smoked", "Lean Fish - Fresh and Smoked", "Fried Fish - Commercial EntrÈe and Fast Food", "Shellfish", "Fried Shellfish - Commercial EntrÈe and Fast Food", "Cold Cuts and Sausage", "Lean Cold Cuts and Sausage", "Organ Meats", "Eggs", "Egg Substitute", "Nuts and Seeds", "Nut and Seed Butters", "Meat Alternatives", "Milk - Whole", "Milk - Reduced Fat", "Milk - Low Fat and Fat Free", "Milk - Nondairy", "Ready-to-drink Flavored Milk - Whole", "Ready-to-drink Flavored Milk - Reduced Fat", "Ready-to-drink Flavored Milk - Low Fat and Fat Free", "Sweetened Flavored Milk Beverage Powder with Non-fat Dry Milk", "Artificially Sweetened Flavored Milk Beverage Powder with Non-fat Dry Milk", "Cheese - Full Fat", "Cheese - Reduced Fat", "Cheese - Low Fat and Fat Free", "Cheese - Nondairy", "Yogurt - Sweetened Whole Milk", "Yogurt - Sweetened Low Fat", "Yogurt - Sweetened Fat Free", "Yogurt - Artificially Sweetened Whole Milk", "Yogurt - Artificially Sweetened Low Fat", "Yogurt - Artificially Sweetened Fat Free", "Yogurt - Nondairy", "Frozen Dairy Dessert", "Frozen Nondairy Dessert", "Pudding and Other Dairy Dessert", "Artificially Sweetened Pudding and Other Dairy Dessert", "Dairy-based Sweetened Meal Replacement/Supplement", "Dairy-based Artificially Sweetened Meal Replacement/Supplement")]
  names(r9a) = c("ID", "date", "citrus_juice", "fruit_juice_nocitrus", "citrus_fruit", "fruit_nocitrus", "avocado", "fried_fruit", "fruit_savory_snack", "darkgreen_veg", "deepyellow_veg", "tomato", "white_potatoes", "fried_potatoes", "starchy_veg", "legumes", "other_veg", "fried_veg", "veg_juice", "whole_grain_mix", "refined_grain_mix", "whole_grain_bread", "refined_grain_bread", "whole_grain_other", "refined_grain_other", "whole_grain_cracker", "refined_grain_cracker", "whole_grain_pasta", "refined_grain_pasta", "whole_grain_cereal_nosweet", "refined_grain_cereal_nosweet", "whole_grain_cereal_sweet", "refined_grain_cereal_sweet", "whole_grain_cookie", "refined_grain_cookie", "whole_grain_bar", "refined_grain_bar", "whole_grain_chip", "refined_grain_chip", "beef", "lean_beef", "veal", "lean_veal", "lamb", "lean_lamb", "fresh_pork", "lean_fresh_pork", "cured_pork", "lean_cured_pork", "game", "poultry", "lean_poultry", "fried_chicken", "fresh_fish", "lean_fish", "fried_fish", "shellfish", "fried_shellfish", "coldcuts_sausage", "lean_coldcuts_sausage", "organ_meat", "eggs", "eggs_substitute", "nuts_seeds", "nut_seed_butters", "meat_substitute", "milk_whole", "milk_reduced_fat", "milk_low_fat", "milk_nondairy", "milk_flavored_whole", "milk_flavored_reduced_fat", "milk_flavored_low_fat", "sweetened_nonfat_dry_milk", "artificially_sweetened_nonfat_dry_milk", "cheese_full_fat", "cheese_reduced_fat", "cheese_low_fat", "cheese_nondairy", "yogurt_sweetened_whole", "yogurt_sweetened_low", "yogurt_sweetened_free", "yogurt_artificially_sweetened_whole", "yogurt_artificially_sweetened_low", "yogurt_artificially_sweetened_free", "yogurt_nondairy", "frozen_dairy", "frozen_nondairy", "pudding_other", "artificially_sweetened_pudding", "dairy_sweetened_replacement", "dairy_artificially_sweetened_replacement")
  
  ##Merge into one file
  #r4a$unique = paste(r4a$ID, r4a$date, sep="_")
  #r9a$unique = paste(r9a$ID, r9a$date, sep="_")
  
  #names(r9a)[1:5}
  all = full_join(r4a, r9a,by=c("ID","date"))
  
  #summary(all$calories)
  
  ##Aggregate within categories
  all$total_fruits = 1000*(apply(all[, names(all) %in% total_fruits], 1, function(x) sum(x, na.rm=TRUE)) / all$calories)
  summary(all$total_fruits)
  
  all$whole_fruits = 1000*(apply(all[, names(all) %in% whole_fruits], 1, function(x) sum(x, na.rm=TRUE)) / all$calories)
  summary(all$whole_fruits)
  
  all$total_veg = 1000*(apply(all[, names(all) %in% total_veg], 1, function(x) sum(x, na.rm=TRUE)) / all$calories)
  summary(all$total_veg)
  
  all$greens_beans = 1000*(apply(all[, names(all) %in% greens_beans], 1, function(x) sum(x, na.rm=TRUE)) / all$calories)
  summary(all$greens_beans)
  
  all$whole_grains = 1000*(apply(all[, names(all) %in% whole_grains], 1, function(x) sum(x, na.rm=TRUE)) / all$calories)
  summary(all$whole_grains)
  
  all$dairy = 1000*(apply(all[, names(all) %in% dairy], 1, function(x) sum(x, na.rm=TRUE)) / all$calories)
  summary(all$dairy)
  
  all$total_protein = 1000*(apply(all[, names(all) %in% total_protein], 1, function(x) sum(x, na.rm=TRUE)) / all$calories)
  summary(all$total_protein)
  
  all$sea_plant_protein = 1000*(apply(all[, names(all) %in% sea_plant_protein], 1, function(x) sum(x, na.rm=TRUE)) / all$calories)
  summary(all$sea_plant_protein)
  
  all$fatty_acids = (all$mufa_g+all$pufa_g) / all$sfa_g
  summary(all$fatty_acids)
  
  all$refined_grains = 1000*(apply(all[, names(all) %in% refined_grains], 1, function(x) sum(x, na.rm=TRUE)) / all$calories)
  summary(all$refined_grains)
  
  all$sodium = all$sodium_mg / 1000
  summary(all$sodium)
  
  all$added_sugars = all$added_sugars_g / (all$fat_g + all$carb_g + all$protein_g)
  summary(all$added_sugars)
  
  all$saturated_fats = all$sfa_g / (all$fat_g + all$carb_g + all$protein_g)
  summary(all$saturated_fats)
  
  ##Add component scores
  ##Adequacy
  ##Total fruits: MIN (0) to ≥0.8 cup equiv. per 1,000 kcal MAX (5)
  all$ScTotalFruits = sapply(all$total_fruits, function(x) interp(x, 0, 0, 0.8, 5))
  #plot(all$total_fruits, all$ScTotalFruits)
  
  ##Whole fruits: MIN (0) to ≥0.4 cup equiv. per 1,000 kcal MAX (5)
  all$ScWholeFruits = sapply(all$whole_fruits, function(x) interp(x, 0, 0, 0.4, 5))
  #plot(all$whole_fruits, all$ScWholeFruits)
  
  ##Total veg: MIN (0) to ≥1.1 cup equiv. per 1,000 kcal MAX (5)
  all$ScTotalVeg = sapply(all$total_veg, function(x) interp(x, 0, 0, 1.1, 5))
  #plot(all$total_veg, all$ScTotalVeg)
  
  ##Greens beans: MIN (0) to ≥0.2 cup equiv. per 1,000 kcal MAX (5)
  all$ScGreensBeans = sapply(all$greens_beans, function(x) interp(x, 0, 0, 0.2, 5))
  #plot(all$greens_beans, all$ScGreensBeans)
  
  ##Whole grains: MIN (0) to ≥1.5 oz equiv. per 1,000 kcal MAX (10)
  all$ScWholeGrains = sapply(all$whole_grains, function(x) interp(x, 0, 0, 1.5, 10))
  #plot(all$whole_grains, all$ScWholeGrains)
  
  ##Dairy: MIN (0) to ≥1.3 cup equiv. per 1,000 kcal MAX (10)
  all$ScDairy = sapply(all$dairy, function(x) interp(x, 0, 0, 1.3, 10))
  #plot(all$dairy, all$ScDairy)
  
  ##Total protein: MIN (0) to ≥2.5 oz equiv. per 1,000 kcal MAX (5)
  all$ScTotalProtein = sapply(all$total_protein, function(x) interp(x, 0, 0, 2.5, 5))
  #plot(all$total_protein, all$ScTotalProtein)
  
  ##Sea food / plant protein: MIN (0) to ≥0.8 oz equiv. per 1,000 kcal MAX (5)
  all$ScSeaPlantProtein = sapply(all$sea_plant_protein, function(x) interp(x, 0, 0, 0.8, 5))
  #plot(all$sea_plant_protein, all$ScSeaPlantProtein)
  
  ##Fatty acids: ≥2.5 MAX (10) to ≤1.2 MIN (0)
  all$ScFattyAcids = sapply(all$fatty_acids, function(x) interp(x, 1.2, 0, 2.5, 10))
  #plot(all$fatty_acids, all$ScFattyAcids)
  
  ##Moderation
  ##Refined grains: ≤1.8 oz equiv. per 1,000 kcal MAX (10) to ≥4.3 oz equiv. per 1,000 kcal MIN (0)
  all$ScRefinedGrains = sapply(all$refined_grains, function(x) interp(x, 1.8, 10, 4.3, 0))
  #plot(all$refined_grains, all$ScRefinedGrains)
  
  ##Sodium: ≤1.1 gram per 1,000 kcal MAX (10) to ≥2.0 grams per 1,000 kcal MIN (0)
  all$ScSodium = sapply(all$sodium, function(x) interp(x, 1.1, 10, 2.0, 0))
  #plot(all$sodium, all$ScSodium)
  
  ##Added sugars: ≤6.5% of energy MAX (10) to ≥26% of energy MIN (0)
  all$ScAddedSugars = sapply(all$added_sugars, function(x) interp(x, 0.065, 10, 0.26, 0))
  #plot(all$added_sugars, all$ScAddedSugars)
  
  ##Saturated fats: ≤8% of energy MAX (10) to ≥16% of energy MIN (0)
  all$ScSaturatedFats = sapply(all$saturated_fats, function(x) interp(x, 0.08, 10, 0.16, 0))
  #plot(all$saturated_fats, all$ScSaturatedFats)
  
  ##Calculate HEI (and radar plot later)
  all$HEI = apply(all[, names(all) %in% c("ScTotalFruits", "ScWholeFruits", "ScTotalVeg", "ScGreensBeans", "ScWholeGrains", "ScDairy", "ScTotalProtein", "ScSeaPlantProtein", "ScFattyAcids", "ScRefinedGrains", "ScSodium", "ScAddedSugars", "ScSaturatedFats")], 1, function(x) sum(x, na.rm=TRUE))
  all2 = all[, names(all) %in% c("ID", "date", "ScTotalFruits", "ScWholeFruits", "ScTotalVeg", "ScGreensBeans", "ScWholeGrains", "ScDairy", "ScTotalProtein", "ScSeaPlantProtein", "ScFattyAcids", "ScRefinedGrains", "ScSodium", "ScAddedSugars", "ScSaturatedFats", "HEI")]
  all2
}

perID = function(dta) {
  dta2 = aggregate(dta[, 3:16], by=list(dta$ID), function(x) mean(x, na.rm=TRUE)) #change to 3:16 for HEI
  names(dta2)[1] = "ID"
  dta2
}



#### FULL SAMPLE ####

r4t = rbind(r4e,r4c)
r9t = rbind(r9e,r9c)

blh = perID(hei_ndsr(r4=r4t[r4t$time=="BL",], r9=r9t[r9t$time=="BL",]))
blh$time = "BL"
m6h = perID(hei_ndsr(r4=r4t[r4t$time=="M6",], r9=r9t[r9t$time=="M6",]))
m6h$time = "M6"

heibl = blh %>%
  mutate(heibl = HEI,
         pid = ID) %>%
  select(pid,heibl) 
heim6 = m6h %>%
  mutate(heim6 = HEI,
         pid = ID) %>%
  select(pid,heim6) 
heie = full_join(heibl,heim6) %>%
  mutate(heidiff = heim6-heibl) 

dfe = full_join(df,heie, by = c("pid"="pid"))
reg4=(lm(heidiff~age_in_years+sex+race___3+ethnicity+loc, data=dfe))
summary(reg4)
confint(reg4)

summary(dfe$heibl)
summary(dfe$heibl[dfe$loc=="SF"])
summary(dfe$heibl[dfe$loc=="SFev"])
summary(dfe$heibl[dfe$loc=="LA"])
summary(dfe$heibl[dfe$loc!="LA"])


summary(dfe$heim6)
summary(dfe$heim6[dfe$loc=="SF"])
summary(dfe$heim6[dfe$loc=="SFev"])
summary(dfe$heim6[dfe$loc=="LA"])
summary(dfe$heim6[dfe$loc!="LA"])


t.test(dfe$heibl[dfe$loc=="SF"],dfe$heim6[dfe$loc=="SF"],paired=T,na.action=na.omit)
t.test(dfe$heibl[dfe$loc=="SFev"],dfe$heim6[dfe$loc=="SFev"],paired=T,na.action=na.omit)
t.test(dfe$heibl[dfe$loc=="LA"],dfe$heim6[dfe$loc=="LA"],paired=T,na.action=na.omit)
t.test(dfe$heibl[dfe$loc!="LA"],dfe$heim6[dfe$loc!="LA"],paired=T,na.action=na.omit)

t.test(dfe$heim6,dfe$heibl,paired=T,na.action=na.omit)

#### EVIDENCE only ####

blh = perID(hei_ndsr(r4=r4e[r4e$time=="BL",], r9=r9e[r9e$time=="BL",]))
blh$time = "BL"
m6h = perID(hei_ndsr(r4=r4e[r4e$time=="M6",], r9=r9e[r9e$time=="M6",]))
m6h$time = "M6"

heibl = blh %>%
  mutate(heibl = HEI,
         pid = ID) %>%
  select(pid,heibl) 
heim6 = m6h %>%
  mutate(heim6 = HEI,
         pid = ID) %>%
  select(pid,heim6) 
heie = full_join(heibl,heim6) %>%
  mutate(heidiff = heim6-heibl) 

dfe = full_join(dfe,heie, by = c("pid"="pid"))
reg4=(lm(heidiff~age_in_years+sex+race___3+ethnicity+loc, data=dfe))
summary(reg4)
confint(reg4)

summary(dfe$heibl)
summary(dfe$heibl[dfe$loc=="SFev"])
summary(dfe$heibl[dfe$loc=="LA"])
t.test(dfe$heibl[dfe$loc=="SFev"],dfe$heim6[dfe$loc=="SFev"],paired=T,na.action=na.omit)

t.test(dfe$heibl[dfe$loc=="SFev"],dfe$heim6[dfe$loc=="SFev"],paired=T,na.action=na.omit)
t.test(dfe$heibl[dfe$loc=="LA"],dfe$heim6[dfe$loc=="LA"],paired=T,na.action=na.omit)

summary(dfe$heim6)
summary(dfe$heim6[dfe$loc=="SFev"])
summary(dfe$heim6[dfe$loc=="LA"])
t.test(dfe$heim6[dfe$loc=="SFev"],dfe$heim6[dfe$loc=="SFev"],paired=T,na.action=na.omit)
t.test(dfe$heim6[dfe$loc=="LA"],dfe$heim6[dfe$loc=="LA"],paired=T,na.action=na.omit)

t.test(dfe$heim6,dfe$heibl,paired=T,na.action=na.omit)


### Methods for Entropy Balancing

## The following list contains the objects that you will need to create if they don't already exist.
# X0 <- SFev design matrix with intercept. You can use model.matrix() to construct this object.
# X1 <- LA design matrix with intercept. You can use model.matrix() to construct this object.
# The columns in X0 and X1 must align
# Y1 <- LA F&V diff outcome.
# Z1 <- LA treatment indicator (numeric; 0,1)

library(matrixStats)

ebl = dfe %>%
  select(pid,age_in_years,sex,education_level, race___3, ethnicity, hh_monthly_income,
         ppl_in_hh, fa_snap_ppt, fa_wic_ppt, fvCbl, heibl, loc) %>%
  mutate(fv = fvCbl, hei = heibl, z = 0) %>%
  select(-fvCbl, -heibl)
em6 = dfe %>%
  select(pid,age_in_years,sex,education_level, race___3, ethnicity, hh_monthly_income,
         ppl_in_hh, fa_snap_ppt, fa_wic_ppt, fvCm6, heim6, loc) %>%
  mutate(fv = fvCm6, hei = heim6, z = 1) %>%
  select(-fvCm6, -heim6)
e = rbind(ebl,em6)

e=e[complete.cases(e),]
SFev <-  e[e$loc=="SFev",]
LA <-  e[e$loc=="LA",]
X0 <- model.matrix(~ age_in_years+sex+race___3+ethnicity+education_level+hh_monthly_income+ppl_in_hh+fa_snap_ppt+fa_wic_ppt, SFev)
X1 <- model.matrix(~ age_in_years+sex+race___3+ethnicity+education_level+hh_monthly_income+ppl_in_hh+fa_snap_ppt+fa_wic_ppt, LA)
Y1 <- LA$fv
Z1 <- LA$z

### Start Kevin's code

# Crude results
##########################################################################
### PURPOSE: Functions for Extending Inference using Entropy Balancing ###
### BY: Kevin Josey                                                    ###
##########################################################################

### Start auxillary functions ###

lagrange <- function(coefs, A, b) {
  
  temp <- sum(exp(-A %*% coefs))
  out <- temp + sum(b * coefs)
  return(out)
  
}

# Estimating equation for the SATE
esteq_sate <- function(X, Y, Z, weights, target, tau) {
  
  eq1 <- Z*weights*X - target
  eq2 <- (1 - Z)*weights*X - target
  eq3 <- weights*(Z*(Y - tau) - (1 - Z)*Y)
  
  eq <- c(eq1, eq2, eq3) 
  return(eq)
  
}

esteq_sate_mom <- function(X, Y, Z, weights, target, tau) {
  
  eq1 <- weights*(X - target)
  eq2 <- weights*(Z*(Y - tau) - (1 - Z)*Y)
  
  eq <- c(eq1, eq2) 
  return(eq)
  
}

### End auxillary functions ###

# mom == method of moments
calibrate <- function(X, Z, target, mom = FALSE, optim_ctrl = list(maxit = 500, reltol = 1e-20), ...) {
  
  if (!is.matrix(X))
    stop("X must be a matrix")
  
  if (!is.vector(target))
    stop("Z must be a vector")
  
  if (length(Z) != nrow(X))
    stop("length(Z) != nrow(X)")
  
  if (!is.vector(target))
    stop("target must be a vector")
  
  if (length(target) != ncol(X))
    stop("length(target) != ncol(X)")
  
  extraArgs <- list(...)
  
  if (length(extraArgs)) {
    
    arg <- names(formals(stats::optim))
    indx <- match(names(extraArgs), arg, nomatch = 0)
    if (any(indx == 0)) 
      stop(paste("Argument", names(extraArgs)[indx == 0], "not matched"))
    
  }
  
  fn <- match.fun(lagrange)
  n_1 <- length(Z)
  
  if (mom) {
    
    A <- cbind(X, (2*Z - 1))
    b <- c(n_1*target, 0)
    
  } else {
    
    A <- cbind(Z*X, (1 - Z)*X)
    b <- c(n_1*target, n_1*target)
    
  }
  
  # initialize coefs
  coefs_init <- rep(0, times = ncol(A))
  opt <- stats::optim(coefs_init, fn, method = "BFGS",
                      A = A, b = b, control = optim_ctrl)
  
  converged <- ifelse(opt$convergence == 0, TRUE, FALSE)
  coefs <- opt$par
  
  if (converged)
    weights <- c( exp(-A %*% coefs) )
  else
    weights <- NA
  
  out <- list(weights = weights,
              coefs = coefs,
              converged = converged,
              X = X, Z = Z, target = target,
              coefs_init = coefs_init,
              optim_ctrl = optim_ctrl,
              mom = mom)
  
  class(out) <- "calibrate"
  return(out)
  
}

# Estimation with target sample moments. Y is from the trial sample only.
estimate_sate <- function(obj, Y, ...) {
  
  if (!inherits(obj, "calibrate"))
    stop("obj must be of class \"calibrate\"")
  
  weights <- obj$weights
  coefs <- obj$coefs
  X <- obj$X
  Z <- obj$Z
  n <- length(Z)
  m <- ncol(X)
  target <- obj$target
  
  tau <- sum(weights*(2*Z - 1)*Y)/sum(Z*weights)
  conv <- TRUE
  
  if(obj$mom) {
    
    U <- matrix(0, ncol = m, nrow = m)
    v <- rep(0, times = m + 1)
    meat <- matrix(0, ncol = m + 1, nrow = m + 1)
    
    for (i in 1:n) {
      
      U[1:m,1:m] <- U[1:m,1:m] - weights[i] * (X[i,] %*% t(X[i,]))
      v[1:m] <- v[1:m] - (2*Z[i] - 1) * weights[i] * (Y[i] - Z[i]*tau) * X[i,]
      v[m + 1] <- v[m + 1] - weights[i]*Z[i]
      s <- esteq_sate_mom(X = X[i,], Y = Y[i], Z = Z[i], weights = weights[i], target = target, tau = tau)
      meat <- meat + s %*% t(s)
      
    }
    
    invbread <- matrix(0, nrow = m + 1, ncol = m + 1)
    invbread[1:m,1:m] <- U
    invbread[m + 1, ] <- v
    
    bread <- try(solve(invbread), silent = TRUE)
    
    if (inherits(bread, "try-error")) {
      
      sandie_conv <- FALSE
      warning("Sandwich estimator is singular.")
      
    } else {
      
      sandwich <- bread %*% meat %*% t(bread)
      variance <- sandwich[m + 1, m + 1]
      
    }
    
  } else {
    
    U <- matrix(0, ncol = 2*m, nrow = 2*m)
    v <- rep(0, times = 2*m + 1)
    meat <- matrix(0, ncol = 2*m + 1, nrow = 2*m + 1)
    
    for (i in 1:n) {
      
      U[1:m,1:m] <- U[1:m,1:m] - Z[i] * weights[i] * (X[i,] %*% t(X[i,]))
      U[(m + 1):(2*m),(m + 1):(2*m)] <- U[(m + 1):(2*m),(m + 1):(2*m)] - (1 - Z[i]) * weights[i] * (X[i,] %*% t(X[i,]))
      v[1:m] <- v[1:m] - Z[i] * weights[i] * (Y[i] - tau) * X[i,]
      v[(m + 1):(2*m)] <- v[(m + 1):(2*m)] + (1 - Z[i]) * weights[i] * Y[i] * X[i,]
      v[2*m + 1] <- v[2*m + 1] - weights[i]*Z[i]
      s <- esteq_sate(X = X[i,], Y = Y[i], Z = Z[i], weights = weights[i], target = target, tau = tau)
      meat <- meat + s %*% t(s)
      
    }
    
    invbread <- matrix(0, nrow = 2*m + 1, ncol = 2*m + 1)
    invbread[1:(2*m),1:(2*m)] <- U
    invbread[2*m + 1, ] <- v
    
    bread <- try(solve(invbread), silent = TRUE)
    
    if (inherits(bread, "try-error")) {
      
      sandie_conv <- FALSE
      warning("Sandwich estimator is singular.")
      
    } else {
      
      sandwich <- bread %*% meat %*% t(bread)
      variance <- sandwich[2*m + 1, 2*m + 1]
      
    }
    
  }
  
  out <- list(estimate = tau, variance = variance)
  return(out)
  
}


# Entropy balancing
tm <- colWeightedMeans(X0, w = rep(1/290,290)) # target margins# target margins
entfit <- calibrate(X = X1, Z = Z1, target = tm, mom = FALSE)

# With target sample moments

entest_sate <- try( estimate_sate(obj = entfit, Y = Y1), silent = TRUE )

entest_sate$estimate
entest_sate$variance


Y2 <- LA$hei

entest_sate2 <- try( estimate_sate(obj = entfit, Y = Y2), silent = TRUE )

entest_sate2$estimate
entest_sate2$variance

# Table 1 info

w_sd <- function(x, w, na.rm = FALSE) {
  
  if (na.rm) {
    w <- w[i <- !is.na(x)]
    x <- x[i]
  }
  
  sum.w <- sum(w)
  sum.w2 <- sum(w^2)
  mean.w <- sum(x * w) / sum(w)
  sqrt((sum.w / (sum.w^2 - sum.w2)) * sum(w * (x - mean.w)^2, na.rm = na.rm))
  
}

LA_mean <- colMeans(X1)
LA_sd <- apply(X1, 2, sd)

wts <- entfit$weights
weighted_mean <- (t(X1) %*% wts) / sum(wts)
weighted_sd <- apply(X1, 2, w_sd, w = wts)

base <- rep(1, nrow(X0)) ## there might have been an error here before. Hopefully this fixes it
SFev_mean <- (t(X0) %*% base) / sum(base)
SFev_sd <- apply(X0, 2, w_sd, w = base)

table_one <- cbind(SFev_mean, SFev_sd, LA_mean, LA_sd, weighted_mean, weighted_sd)
table_one



# which subset of features explains the F&V diff

# remove WIC
X0 <- model.matrix(~ age_in_years+sex+race___3+ethnicity+education_level+hh_monthly_income+ppl_in_hh+fa_snap_ppt, SFev)
X1 <- model.matrix(~ age_in_years+sex+race___3+ethnicity+education_level+hh_monthly_income+ppl_in_hh+fa_snap_ppt, LA)
tm <- colWeightedMeans(X0, w = rep(1/290,290)) # target margins# target margins
entfit <- calibrate(X = X1, Z = Z1, target = tm, mom = FALSE)

# With target sample moments

entest_sate <- try( estimate_sate(obj = entfit, Y = Y1), silent = TRUE )

entest_sate$estimate
entest_sate$variance

entest_sate2 <- try( estimate_sate(obj = entfit, Y = Y2), silent = TRUE )

entest_sate2$estimate
entest_sate2$variance


# and remove SNAP
X0 <- model.matrix(~ age_in_years+sex+race___3+ethnicity+education_level+hh_monthly_income+ppl_in_hh, SFev)
X1 <- model.matrix(~ age_in_years+sex+race___3+ethnicity+education_level+hh_monthly_income+ppl_in_hh, LA)
tm <- colWeightedMeans(X0, w = rep(1/290,290)) # target margins# target margins
entfit <- calibrate(X = X1, Z = Z1, target = tm, mom = FALSE)

# With target sample moments

entest_sate <- try( estimate_sate(obj = entfit, Y = Y1), silent = TRUE )

entest_sate$estimate
entest_sate$variance

entest_sate2 <- try( estimate_sate(obj = entfit, Y = Y2), silent = TRUE )

entest_sate2$estimate
entest_sate2$variance



# and remove hh size
X0 <- model.matrix(~ age_in_years+sex+race___3+ethnicity+education_level+hh_monthly_income, SFev)
X1 <- model.matrix(~ age_in_years+sex+race___3+ethnicity+education_level+hh_monthly_income, LA)
tm <- colWeightedMeans(X0, w = rep(1/290,290)) # target margins# target margins
entfit <- calibrate(X = X1, Z = Z1, target = tm, mom = FALSE)

# With target sample moments

entest_sate <- try( estimate_sate(obj = entfit, Y = Y1), silent = TRUE )

entest_sate$estimate
entest_sate$variance

entest_sate2 <- try( estimate_sate(obj = entfit, Y = Y2), silent = TRUE )

entest_sate2$estimate
entest_sate2$variance




# and remove ed

X0 <- model.matrix(~ age_in_years+sex+race___3+ethnicity+hh_monthly_income, SFev)
X1 <- model.matrix(~ age_in_years+sex+race___3+ethnicity+hh_monthly_income, LA)
tm <- colWeightedMeans(X0, w = rep(1/290,290)) # target margins# target margins
entfit <- calibrate(X = X1, Z = Z1, target = tm, mom = FALSE)

# With target sample moments

entest_sate <- try( estimate_sate(obj = entfit, Y = Y1), silent = TRUE )

entest_sate$estimate
entest_sate$variance

entest_sate2 <- try( estimate_sate(obj = entfit, Y = Y2), silent = TRUE )

entest_sate2$estimate
entest_sate2$variance



# and remove eth

X0 <- model.matrix(~ age_in_years+sex+race___3+hh_monthly_income, SFev)
X1 <- model.matrix(~ age_in_years+sex+race___3+hh_monthly_income, LA)
tm <- colWeightedMeans(X0, w = rep(1/290,290)) # target margins# target margins
entfit <- calibrate(X = X1, Z = Z1, target = tm, mom = FALSE)

# With target sample moments

entest_sate <- try( estimate_sate(obj = entfit, Y = Y1), silent = TRUE )

entest_sate$estimate
entest_sate$variance

entest_sate2 <- try( estimate_sate(obj = entfit, Y = Y2), silent = TRUE )

entest_sate2$estimate
entest_sate2$variance





# and remove race

X0 <- model.matrix(~ age_in_years+sex+hh_monthly_income, SFev)
X1 <- model.matrix(~ age_in_years+sex+hh_monthly_income, LA)
tm <- colWeightedMeans(X0, w = rep(1/290,290)) # target margins# target margins
entfit <- calibrate(X = X1, Z = Z1, target = tm, mom = FALSE)

# With target sample moments

entest_sate <- try( estimate_sate(obj = entfit, Y = Y1), silent = TRUE )

entest_sate$estimate
entest_sate$variance

entest_sate2 <- try( estimate_sate(obj = entfit, Y = Y2), silent = TRUE )

entest_sate2$estimate
entest_sate2$variance




# and remove sex

X0 <- model.matrix(~ age_in_years+hh_monthly_income, SFev)
X1 <- model.matrix(~ age_in_years+hh_monthly_income, LA)
tm <- colWeightedMeans(X0, w = rep(1/290,290)) # target margins# target margins
entfit <- calibrate(X = X1, Z = Z1, target = tm, mom = FALSE)

# With target sample moments

entest_sate <- try( estimate_sate(obj = entfit, Y = Y1), silent = TRUE )

entest_sate$estimate
entest_sate$variance

entest_sate2 <- try( estimate_sate(obj = entfit, Y = Y2), silent = TRUE )

entest_sate2$estimate
entest_sate2$variance




# and remove age

X0 <- model.matrix(~ hh_monthly_income, SFev)
X1 <- model.matrix(~ hh_monthly_income, LA)
tm <- colWeightedMeans(X0, w = rep(1/290,290)) # target margins# target margins
entfit <- calibrate(X = X1, Z = Z1, target = tm, mom = FALSE)

# With target sample moments

entest_sate <- try( estimate_sate(obj = entfit, Y = Y1), silent = TRUE )

entest_sate$estimate
entest_sate$variance

entest_sate2 <- try( estimate_sate(obj = entfit, Y = Y2), silent = TRUE )

entest_sate2$estimate
entest_sate2$variance


