library(caret)
library(ade4)
library(dplyr)
library(reshape)
library(Metrics)



md_lm = read.csv("/Users/appleowner/Documents/Kaggle /House Prices/train.csv", header = TRUE)
sapply(md_lm, class)
head(md_lm)

min(md_lm$MSSubClass)
min(md_lm$LotFrontage)
min(md_lm$LotArea)
min(md_lm$OverallQual)
min(md_lm$OverallCond)
min(md_lm$MasVnrArea)
min(md_lm$BsmtFinSF1)
min(md_lm$BsmtFinSF2)
min(md_lm$BsmtUnfSF)
min(md_lm$TotalBsmtSF)
min(md_lm$X1stFlrSF)
min(md_lm$X2ndFlrSF)
min(md_lm$LowQualFinSF)
min(md_lm$GrLivArea)
min(md_lm$BsmtFullBath)
min(md_lm$BsmtHalfBath)
min(md_lm$FullBath)
min(md_lm$HalfBath)
min(md_lm$BedroomAbvGr)
min(md_lm$WoodDeckSF)
min(md_lm$OpenPorchSF)
min(md_lm$EnclosedPorch)
min(md_lm$X3SsnPorch)
min(md_lm$ScreenPorch)
min(md_lm$PoolArea)
min(md_lm$MiscVal)
min(md_lm$MoSold)

##Dealing with missing values in continuous variables
md_lm[, "LotFrontage"][is.na(md_lm[, "LotFrontage"])] = mean(md_lm$LotFrontage, na.rm = TRUE)
md_lm[, "MasVnrArea"][is.na(md_lm[, "MasVnrArea"])] = mean(md_lm$MasVnrArea, na.rm = TRUE)

##Dealing with missing values in categorical variables
categorical_vars=c('MSZoning','Street','Alley','LotShape','LandContour','Utilities','LotConfig','LandSlope','Neighborhood','Condition1','Condition2','BldgType','HouseStyle','RoofStyle','RoofMatl','Exterior1st','Exterior2nd','MasVnrType','ExterQual','ExterCond','Foundation','BsmtQual','BsmtCond','BsmtExposure','BsmtFinType1','BsmtFinType2','Heating','HeatingQC','CentralAir','Electrical','KitchenQual','Functional','FireplaceQu','GarageType','GarageFinish','GarageQual','GarageCond','PavedDrive','PoolQC','Fence','MiscFeature','SaleType','SaleCondition')


##MSSubClass

ggplot(md_lm, aes(md_lm$LotFrontage)) + stat_bin(bins = 100, color = "black", fill = "green") + labs(x = "LotFrontage", y = "Frequency", title = "Histogram of LotFrontage") + xlim(0, 500) + scale_fill_discrete()

##MSSubClass
dev.new()
ggplot(md_lm, aes(md_lm$MSSubClass)) + stat_bin(bins = 100, color = "black", fill = "green") + labs(x = "MSSubClass", y = "Frequency", title = "Histogram of MSSubClass") + xlim(0, 500) + scale_fill_discrete()

##LotArea
dev.new()
ggplot(md_lm, aes(md_lm$LotArea)) + stat_bin(bins = 100, color = "black", fill = "green") + labs(x = "LotArea", y = "Frequency", title = "Histogram of LotArea") + xlim(0, 500) + scale_fill_discrete()

##OverallQual
dev.new()
ggplot(md_lm, aes(md_lm$OverallQual)) + stat_bin(bins = 100, color = "black", fill = "green") + labs(x = "OverallQual", y = "Frequency", title = "Histogram of OverallQual") + xlim(0, 500) + scale_fill_discrete()

##OverallCond
dev.new()
ggplot(md_lm, aes(md_lm$OverallCond)) + stat_bin(bins = 100, color = "black", fill = "green") + labs(x = "OverallCond", y = "Frequency", title = "Histogram of OverallCond") + xlim(0, 500) + scale_fill_discrete()

##MasVnrArea
dev.new()
ggplot(md_lm, aes(md_lm$MasVnrArea)) + stat_bin(bins = 100, color = "black", fill = "green") + labs(x = "MasVnrArea", y = "Frequency", title = "Histogram of MasVnrArea") + xlim(0, 500) +ylim(0, 5)+ scale_fill_discrete()

##BsmtFinSF1
dev.new()
ggplot(md_lm, aes(md_lm$BsmtFinSF1)) + stat_bin(bins = 100, color = "black", fill = "green") + labs(x = "BsmtFinSF1", y = "Frequency", title = "Histogram of BsmtFinSF1") + xlim(0, 500) +ylim(0, 5)+ scale_fill_discrete()

##BsmtFinSF2
dev.new()
ggplot(md_lm, aes(md_lm$BsmtFinSF2)) + stat_bin(bins = 100, color = "black", fill = "green") + labs(x = "BsmtFinSF2", y = "Frequency", title = "Histogram of BsmtFinSF2") + xlim(0, 500) +ylim(0, 5)+ scale_fill_discrete()

##BsmtUnfSF
dev.new()
ggplot(md_lm, aes(md_lm$BsmtUnfSF)) + stat_bin(bins = 100, color = "black", fill = "green") + labs(x = "BsmtUnfSF", y = "Frequency", title = "Histogram of BsmtUnfSF") + xlim(0, 500) + scale_fill_discrete()

##X1stFlrSF
dev.new()
ggplot(md_lm, aes(md_lm$X1stFlrSF)) + stat_bin(bins = 100, color = "black", fill = "green") + labs(x = "X1stFlrSF", y = "Frequency", title = "Histogram of X1stFlrSF") + xlim(0, 500) + scale_fill_discrete()

##X2ndFlrSF
dev.new()
ggplot(md_lm, aes(md_lm$X2ndFlrSF)) + stat_bin(bins = 100, color = "black", fill = "green") + labs(x = "X2ndFlrSF", y = "Frequency", title = "Histogram of X2ndFlrSF") + xlim(0, 500)+ ylim(0, 5) + scale_fill_discrete()

##LowQualFinSF
dev.new()
ggplot(md_lm, aes(md_lm$LowQualFinSF)) + stat_bin(bins = 100, color = "black", fill = "green") + labs(x = "LowQualFinSF", y = "Frequency", title = "Histogram of LowQualFinSF") + xlim(0, 500) + ylim(0, 5) +scale_fill_discrete()

##GrLivArea
dev.new()
ggplot(md_lm, aes(md_lm$GrLivArea)) + stat_bin(bins = 100, color = "black", fill = "green") + labs(x = "GrLivArea", y = "Frequency", title = "Histogram of GrLivArea") + xlim(0, 500) + scale_fill_discrete()

##BsmtFullBath
dev.new()
ggplot(md_lm, aes(md_lm$BsmtFullBath)) + stat_bin(bins = 100, color = "black", fill = "green") + labs(x = "BsmtFullBath", y = "Frequency", title = "Histogram of BsmtFullBath") + xlim(0, 500)+ ylim(0, 1) + scale_fill_discrete()



##relationship between dependent variable and independent variables
scatter.smooth(x = md_lm$SalePrice, y = md_lm$MSSubClass, main = "SalePrice vs MSSubClass")
dev.new()
scatter.smooth(x = md_lm$SalePrice, y = md_lm$LotFrontage, main = "SalePrice vs LotFrontage")
dev.new()
scatter.smooth(x = md_lm$SalePrice, y = md_lm$LotArea, main = "SalePrice vs LotArea")
dev.new()
scatter.smooth(x = md_lm$SalePrice, y = md_lm$OverallQual, main = "SalePrice vs OverallQual")
dev.new()
scatter.smooth(x = md_lm$SalePrice, y = md_lm$OverallCond, main = "SalePrice vs OverallCond")
dev.new()
scatter.smooth(x = md_lm$SalePrice, y = md_lm$MasVnrArea, main = "SalePrice vs MasVnrArea")
dev.new()
scatter.smooth(x = md_lm$SalePrice, y = md_lm$BsmtFinSF1, main = "SalePrice vs BsmtFinSF1")
dev.new()
scatter.smooth(x = md_lm$SalePrice, y = md_lm$BsmtFinSF2, main = "SalePrice vs BsmtFinSF2")
dev.new()
scatter.smooth(x = md_lm$SalePrice, y = md_lm$BsmtUnfSF, main = "SalePrice vs BsmtUnfSF")
dev.new()
scatter.smooth(x = md_lm$SalePrice, y = md_lm$TotalBsmtSF, main = "SalePrice vs TotalBsmtSF")
dev.new()
scatter.smooth(x = md_lm$SalePrice, y = md_lm$X1stFlrSF, main = "SalePrice vs X1stFlrSF")
dev.new()
scatter.smooth(x = md_lm$SalePrice, y = md_lm$X2ndFlrSF, main = "SalePrice vs X2ndFlrSF")
dev.new()
scatter.smooth(x = md_lm$SalePrice, y = md_lm$LowQualFinSF, main = "SalePrice vs LowQualFinSF")
dev.new()
scatter.smooth(x = md_lm$SalePrice, y = md_lm$GrLivArea, main = "SalePrice vs GrLivArea")
dev.new()
scatter.smooth(x = md_lm$SalePrice, y = md_lm$BsmtFullBath, main = "SalePrice vs BsmtFullBath")
dev.new()
scatter.smooth(x = md_lm$SalePrice, y = md_lm$BsmtHalfBath, main = "SalePrice vs BsmtHalfBath")
dev.new()
scatter.smooth(x = md_lm$SalePrice, y = md_lm$FullBath, main = "SalePrice vs FullBath")
dev.new()
scatter.smooth(x = md_lm$SalePrice, y = md_lm$HalfBath, main = "SalePrice vs HalfBath")
dev.new()
scatter.smooth(x = md_lm$SalePrice, y = md_lm$BedroomAbvGr, main = "SalePrice vs BedroomAbvGr")
dev.new()
scatter.smooth(x = md_lm$SalePrice, y = md_lm$WoodDeckSF, main = "SalePrice vs WoodDeckSF")
dev.new()
scatter.smooth(x = md_lm$SalePrice, y = md_lm$OpenPorchSF, main = "SalePrice vs OpenPorchSF")
dev.new()
scatter.smooth(x = md_lm$SalePrice, y = md_lm$EnclosedPorch, main = "SalePrice vs EnclosedPorch")
dev.new()
scatter.smooth(x = md_lm$SalePrice, y = md_lm$X3SsnPorch, main = "SalePrice vs X3SsnPorch")
dev.new()
scatter.smooth(x = md_lm$SalePrice, y = md_lm$ScreenPorch, main = "SalePrice vs ScreenPorch")
dev.new()
scatter.smooth(x = md_lm$SalePrice, y = md_lm$PoolArea, main = "SalePrice vs PoolArea")
dev.new()
scatter.smooth(x = md_lm$SalePrice, y = md_lm$MiscVal, main = "SalePrice vs MiscVal")
dev.new()
scatter.smooth(x = md_lm$SalePrice, y = md_lm$MoSold, main = "SalePrice vs MoSold")


##converting categorical variables to dummy variables
for (idx in categorical_vars)
	{
	md_dummy_var_lm = acm.disjonctif(md_lm[idx])
	md_lm[idx] = NULL
	md_lm = cbind(md_lm, md_dummy_var_lm)
	}

##take out variables that do not exist in the test data
##I found out there were not available in the test data when I went to predict the salePrice
md_lm[, c("Utilities.NoSeWa", "HouseStyle.2.5Fin", "RoofMatl.ClyTile", "RoofMatl.Membran", "RoofMatl.Metal", "RoofMatl.Roll", "Exterior1st.ImStucc",  "Exterior1st.Stone", "Exterior2nd.Other", "Heating.Floor", "Heating.OthW",  "Electrical.Mix", "GarageQual.Ex", "PoolQC.Fa", "MiscFeature.TenC", "Condition2.RRAe", "Condition2.RRAn", "Condition2.RRNn")] = NULL

##Take out the NA values in the dummy variables
md_lm[, 2:37][is.na(md_lm[, 2:37])] = 0
md_lm[, 39:272][is.na(md_lm[, 39:272])] = 0
md_lm[, 38][is.na(md_lm[, 38])] = mean(md_lm$SalePrice)



##Sampling - Train = 70% and Test = 30%
md_lm_price = md_lm$SalePrice
index = createDataPartition(y=md_lm_price, p=0.75, list=FALSE)
md_lm_train = md_lm[index,]
md_lm_test = md_lm[-index,]



##we generate interactive terms using only highly correlated variables (ie: cor > 0.5)
md_lm_train_cor = as.matrix(cor(md_lm_train))
md_lm_train_cor_melt = arrange(melt(md_lm_train_cor), -abs(value))

md_lm_train_cor_melt_1 = md_lm_train_cor_melt[c(1:298),]
md_lm_train_cor_melt_2 = md_lm_train_cor_melt[-c(1:298),]
head(md_lm_train_cor_melt_1)
head(md_lm_train_cor_melt_2)


##first linear model with no interactive and no higher order terms
min_md_lm_model = lm(SalePrice ~ 1, data = md_lm_train)
big_md_lm_model = lm(SalePrice ~ ., data = md_lm_train)
main_md_lm_model = step(min_md_lm_model, direction = "forward", big_md_lm_model)
summary(main_md_lm_model)

md_lm_pred = predict(main_md_lm_model, md_lm_test, type = "response")
md_lm_pred_output = cbind(md_lm_test, md_lm_pred)

md_lm_pred_output$md_lm_pred_new = md_lm_pred_output$md_lm_pred 
md_lm_pred_output$SalePrice_new = md_lm_pred_output$SalePrice 

md_lm_pred_output$md_lm_pred_new_log = log(md_lm_pred_output$md_lm_pred_new)
md_lm_pred_output$SalePrice_new_log = log(md_lm_pred_output$SalePrice_new)

rmse(md_lm_pred_output$md_lm_pred_new_log,md_lm_pred_output$SalePrice_new_log)


##clearly we can see that some of the dummy variables are statistically significant (Using 95% confidence interval)



##Second linear model with 2nd order interaction terms forward step wise approach
min_md_lm_model_1 = lm(SalePrice ~ 1, data = md_lm_train)

big_md_lm_model_1 = lm(SalePrice ~ . +	SaleCondition.Partial	:	SaleType.New
+	Exterior2nd.VinylSd	:	Exterior1st.VinylSd 
+	Exterior2nd.CmentBd	:	Exterior1st.CemntBd
+	Exterior2nd.MetalSd	:	Exterior1st.MetalSd 
+	RoofStyle.Hip	:	RoofStyle.Gable
+	LotShape.Reg	:	LotShape.IR1 
+	LandSlope.Mod	:	LandSlope.Gtl
+	ExterQual.TA	:	ExterQual.Gd 
+	ExterCond.TA	:	ExterCond.Gd
+	MiscFeature.Gar2	:	MiscVal
+	MiscVal	:	MiscFeature.Gar2
+	Exterior2nd.HdBoard	:	Exterior1st.HdBoard
+	GarageArea	:	GarageCars
+	Neighborhood.Somerst	:	MSZoning.FV
+	PavedDrive.Y	:	PavedDrive.N
+	Electrical.SBrkr	:	Electrical.FuseA
+	Exterior2nd.AsbShng	:	Exterior1st.AsbShng
+	TotRmsAbvGrd	:	GrLivArea
+	KitchenQual.TA	:	KitchenQual.Gd
+	X1stFlrSF	:	TotalBsmtSF
+	BldgType.1Fam	:	MSSubClass
+	HouseStyle.2Story	:	X2ndFlrSF
+	MSZoning.RM	:	MSZoning.RL
+	MasVnrType.None	:	MasVnrType.BrkFace
+	HouseStyle.1Story	:	X2ndFlrSF
+	GarageCond.TA	:	GarageQual.TA
+	Foundation.PConc	:	Foundation.CBlock
+	Exterior2nd.Stucco	:	Exterior1st.Stucco
+	SaleType.WD	:	SaleType.New
+	GarageCond.TA	:	GarageYrBlt
+	SaleCondition.Partial	:	SaleType.WD
+	BsmtQual.TA	:	BsmtQual.Gd
+	Exterior2nd.Plywood	:	Exterior1st.Plywood
+	LotConfig.Inside	:	LotConfig.Corner
+	Heating.GasW	:	Heating.GasA
+	GarageType.Detchd	:	GarageType.Attchd
+	GarageQual.TA	:	GarageYrBlt
+	BsmtFinType2.Unf	:	BsmtFinSF2
+	GrLivArea	:	X2ndFlrSF
+	BldgType.Duplex	:	KitchenAbvGr
+	MasVnrType.None	:	MasVnrArea
+	TotRmsAbvGrd	:	BedroomAbvGr
+	PoolQC.Gd	:	PoolArea
+	Exterior2nd.BrkFace	:	Exterior1st.BrkFace
+	KitchenQual.TA	:	ExterQual.TA
+	HouseStyle.2Story	:	HouseStyle.1Story
+	BldgType.TwnhsE	:	BldgType.1Fam
+	SaleCondition.Partial	:	SaleCondition.Normal
+	HeatingQC.TA	:	HeatingQC.Ex
+	GarageCond.Po	:	GarageQual.Po
+	Foundation.PConc	:	YearBuilt
+	BsmtFullBath	:	BsmtFinSF1
+	RoofMatl.CompShg	:	RoofStyle.Flat
+	ExterQual.TA	:	OverallQual
+	SaleCondition.Normal	:	SaleType.New
+	BsmtCond.TA	:	BsmtCond.Gd
+	SaleCondition.Normal	:	SaleType.WD
+	BsmtQual.TA	:	YearBuilt
+	Condition2.Norm	:	Condition2.Feedr
+	FullBath	:	GrLivArea
+	LandContour.Lvl	:	LandContour.Bnk
+	BsmtFinType1.Unf	:	BsmtFinSF1
+	KitchenQual.Gd	:	ExterQual.Gd
+	HouseStyle.2Story	:	HalfBath
+	Foundation.PConc	:	ExterQual.TA
+	ExterQual.TA	:	Foundation.PConc
+	KitchenQual.TA	:	ExterQual.Gd
+	TotRmsAbvGrd	:	X2ndFlrSF
+	GarageFinish.Unf	:	GarageType.Detchd
+	HalfBath	:	X2ndFlrSF
+	Condition1.Norm	:	Condition1.Feedr
+	BsmtQual.TA	:	Foundation.PConc
+	BsmtFinType1.Unf	:	BsmtUnfSF
+	GarageCars	:	OverallQual
+	GarageCars	:	GarageYrBlt
+	GrLivArea	:	OverallQual
+	YearRemodAdd	:	YearBuilt
+	ExterQual.TA	:	YearBuilt
+	Foundation.PConc	:	ExterQual.Gd
+	SaleCondition.Normal	:	SaleCondition.Abnorml
+	BsmtExposure.No	:	BsmtExposure.Av
+	KitchenQual.TA	:	YearRemodAdd
+	ExterCond.Ex	:	Condition2.PosA
+	Exterior2nd.AsphShn	:	Exterior1st.AsphShn
+	Condition2.PosA	:	ExterCond.Ex
+	YearBuilt	:	OverallQual
+	MasVnrType.BrkFace	:	MasVnrArea
+	Foundation.PConc	:	YearRemodAdd
+	Functional.Typ	:	Functional.Min2
+	Foundation.PConc	:	OverallQual
+	GrLivArea	:	X1stFlrSF
+	ExterQual.TA	:	YearRemodAdd
+	KitchenQual.Ex	:	ExterQual.Ex
+	Foundation.PConc	:	Exterior1st.VinylSd
+	GarageArea	:	OverallQual
+	Neighborhood.OldTown	:	MSZoning.RM
+	Foundation.PConc	:	Exterior2nd.VinylSd
+	GarageArea	:	GarageYrBlt
+	BsmtQual.Gd	:	YearBuilt
+	LandContour.Lvl	:	LandContour.HLS
+	ExterQual.Gd	:	YearBuilt
+	KitchenQual.Gd	:	ExterQual.TA
+	TotRmsAbvGrd	:	FullBath
+	Foundation.BrkTil	:	YearBuilt
+	KitchenQual.TA	:	OverallQual
+	KitchenQual.TA	:	Foundation.PConc
+	YearRemodAdd	:	OverallQual
+	FullBath	:	OverallQual
+	ExterQual.Gd	:	OverallQual
+	GarageQual.TA	:	GarageQual.Fa
+	BsmtQual.TA	:	ExterQual.TA
+	Functional.Typ	:	Functional.Min1
+	LandSlope.Sev	:	LotArea
+	GarageCars	:	YearBuilt
+	TotalBsmtSF	:	OverallQual
+	GarageFinish.Unf	:	GarageFinish.RFn
+	LandSlope.Gtl	:	LandContour.Lvl
+	KitchenQual.Gd	:	YearRemodAdd
+	HeatingQC.Ex	:	ExterQual.TA
+	BsmtQual.Ex	:	ExterQual.Ex
+	ExterQual.Gd	:	YearRemodAdd
+	HeatingQC.Ex	:	Foundation.PConc
+	BsmtCond.TA	:	BsmtCond.Fa
+	BldgType.TwnhsE	:	MSSubClass
+	Exterior2nd.CmentBd	:	Neighborhood.MeadowV
+	GarageFinish.Unf	:	YearBuilt
+	TotalBsmtSF	:	BsmtFinSF1
+	BedroomAbvGr	:	GrLivArea
+	Exterior1st.CemntBd	:	Neighborhood.MeadowV
+	Exterior1st.VinylSd	:	YearBuilt
+	KitchenQual.Ex	:	BsmtQual.Ex
+	HeatingQC.Ex	:	YearRemodAdd
+	Exterior2nd.VinylSd	:	YearBuilt
+	BsmtQual.TA	:	ExterQual.Gd
+	BsmtQual.TA	:	Foundation.CBlock
+	GarageCond.TA	:	GarageCars
+	BedroomAbvGr	:	X2ndFlrSF
+	ExterQual.Gd	:	BsmtQual.Gd, data = md_lm_train)

main_md_lm_model_1 = step(min_md_lm_model_1, direction = "forward", formula(big_md_lm_model_1))

summary(main_md_lm_model_1)

## Clearly some of the interactive terms are statisticaly significant and the our R-sqrd and adjustered R-sqrd also increased
##However, this is due to the increase in the increment of the used parameters

md_lm_pred_1 = predict(main_md_lm_model_1, md_lm_test, type = "response")
md_lm_pred_output = cbind(md_lm_test, md_lm_pred_1)
min(md_lm_pred_output$md_lm_pred_1)
mn = abs(min(md_lm_pred_1))*2
md_lm_pred_output$md_lm_pred_1_new = md_lm_pred_output$md_lm_pred_1 + mn
md_lm_pred_output$SalePrice_new = md_lm_pred_output$SalePrice + mn

md_lm_pred_output$md_lm_pred_1_new_log = log(md_lm_pred_output$md_lm_pred_1_new)
md_lm_pred_output$SalePrice_new_log = log(md_lm_pred_output$SalePrice_new)

rmse(md_lm_pred_output$md_lm_pred_1_new_log,md_lm_pred_output$SalePrice_new_log)

##For some reason when I add interactive terms to the model I get negative values in my predictions. To calculate the RMSE I needed to get ##ride of the negative value. The approach that I used seem to be influencing the RMSE to fall. I know this beacause when I increase the the multiplier from 2 to 10 the RMSE value dropped. 



##Next we try a quadratic (second order) order model with forward stepwise approach
min_md_lm_model_2 = lm(SalePrice ~ 1, data = md_lm_train)

big_md_lm_model_2 = lm(SalePrice ~ . + I(MasVnrArea^2) + I(BsmtFinSF1^2) + I(BsmtFinSF2^2) + I(BsmtUnfSF^2) + I(TotalBsmtSF^2) + I(X1stFlrSF^2) + I(GrLivArea^2) + I(GarageArea^2) + I(WoodDeckSF^2) + I(OpenPorchSF^2) + I(EnclosedPorch^2) + I(X3SsnPorch^2) + I(LotArea^2) +    MSSubClass  :     I(MasVnrArea^2), data = md_lm_train)

main_md_lm_model_2 = step(min_md_lm_model_2, direction = "forward", formula(big_md_lm_model_2))
summary(main_md_lm_model_2)


md_lm_pred_2 = predict(main_md_lm_model_2, md_lm_test, type = "response")
md_lm_pred_output_2 = cbind(md_lm_test, md_lm_pred_2)

md_lm_pred_output_2$md_lm_pred_2_log = log(md_lm_pred_output_2$md_lm_pred_2)
md_lm_pred_output_2$SalePrice_log = log(md_lm_pred_output_2$SalePrice)

rmse(md_lm_pred_output_2$md_lm_pred_2_log,md_lm_pred_output_2$SalePrice_log)




##We select those parameters that were statistically significant and rerun the model using the backward variable selection approach
min_md_lm_model_3 = lm(SalePrice ~ 1, data = md_lm_train)

big_md_lm_model_3 = lm(SalePrice ~ +	OverallQual	+	GrLivArea	+	BsmtQual.Ex
+	GarageCars	+	BsmtFinSF1	+	I(BsmtFinSF1^2)
+	KitchenQual.Ex	+	Condition2.PosN	+	TotalBsmtSF
+	BsmtExposure.No	+	SaleType.New	+	Neighborhood.NoRidge
+	LotArea	+	OverallCond	+	YearBuilt
+	BldgType.1Fam	+	BedroomAbvGr	+	Neighborhood.Crawfor
+	ExterQual.Ex	+	Neighborhood.StoneBr	+	RoofMatl.WdShngl
+	Neighborhood.NridgHt	+	PoolQC.Ex	+	Neighborhood.Somerst
+	Exterior1st.BrkFace	+	Functional.Typ	+	Condition1.Norm
+	TotRmsAbvGrd	+	LotShape.IR2	+	LowQualFinSF
+	SaleCondition.Normal	+	I(MasVnrArea^2)	+	MoSold
+	Fireplaces	+	LandContour.Lvl
+	Functional.Sev  +	I(EnclosedPorch^2)  +	EnclosedPorch
+	SaleType.Con +	I(GarageArea^2)  +	GarageType.2Types
+	GarageArea  +	ScreenPorch
+	Neighborhood.Mitchel  +	Street.Grvl  +	ExterQual.Fa
+	Exterior2nd.AsbShng  +	Neighborhood.NWAmes	+	Neighborhood.NAmes
+	`MSZoning.C (all)`  +	LandContour.HLS  +	RoofStyle.Mansard
+	Neighborhood.Edwards  +	MSZoning.RM  +	Neighborhood.Sawyer
+	LotConfig.CulDSac  +	LandSlope.Sev  +	BsmtQual.Gd
+	I(GrLivArea^2)  +	BsmtFinType1.Unf+	I(BsmtFinSF2^2)
+	BsmtExposure.Mn  +	BsmtExposure.Av  +	I(TotalBsmtSF^2), data = md_lm_train)

main_md_lm_model_3 = step(min_md_lm_model_3, direction = "forward", formula(big_md_lm_model_3))
summary(main_md_lm_model_3)


md_lm_pred_3 = predict(main_md_lm_model_3, md_lm_test, type = "response")
md_lm_pred_output_3 = cbind(md_lm_test, md_lm_pred_3)

md_lm_pred_output_3$md_lm_pred_3_log = log(md_lm_pred_output_3$md_lm_pred_3)
md_lm_pred_output_3$SalePrice_log = log(md_lm_pred_output_3$SalePrice)

rmse(md_lm_pred_output_3$md_lm_pred_3_log,md_lm_pred_output_3$SalePrice_log)
##[1] 0.1416851
##The RMSE got better. 


##With the RMSE falling after eliminating the parameters that were not statistically significant, We will again select those parameters that were statistically significant and rerun the model 
min_md_lm_model_4 = lm(SalePrice ~ 1, data = md_lm_train)

big_md_lm_model_4 = lm(SalePrice ~ +	OverallQual	+	GrLivArea	+	BsmtQual.Ex
+	GarageCars	+	BsmtFinSF1	+	I(BsmtFinSF1^2)
+	KitchenQual.Ex	+	Condition2.PosN	+	TotalBsmtSF
+	BsmtExposure.No	+	SaleType.New	+	Neighborhood.NoRidge
+	LotArea	+	OverallCond	+	YearBuilt
+	BldgType.1Fam	+	BedroomAbvGr	+	Neighborhood.Crawfor
+	ExterQual.Ex	+	Neighborhood.StoneBr	+	RoofMatl.WdShngl
+	Neighborhood.NridgHt	+	PoolQC.Ex	+	Neighborhood.Somerst
+	Exterior1st.BrkFace	+	Functional.Typ	+	Condition1.Norm
+	TotRmsAbvGrd	+	LotShape.IR2	+	LowQualFinSF
+	I(MasVnrArea^2)	+	MoSold
+	Fireplaces	+	LandContour.Lvl
+	Functional.Sev  +	I(EnclosedPorch^2)  +	EnclosedPorch
+	SaleType.Con +	I(GarageArea^2)  +	GarageType.2Types
+	GarageArea  +	ScreenPorch
+	Neighborhood.Mitchel  +	Street.Grvl  +	ExterQual.Fa
+	Exterior2nd.AsbShng  +	Neighborhood.NWAmes	+	Neighborhood.NAmes
+	`MSZoning.C (all)`  +	LandContour.HLS
+	Neighborhood.Edwards  +	MSZoning.RM  +	Neighborhood.Sawyer
+	LotConfig.CulDSac  +	LandSlope.Sev
+	I(GrLivArea^2)  +	BsmtFinType1.Unf+	I(BsmtFinSF2^2)
+	BsmtExposure.Mn  +	BsmtExposure.Av  +	I(TotalBsmtSF^2), data = md_lm_train)

main_md_lm_model_4 = step(min_md_lm_model_4, direction = "forward", formula(big_md_lm_model_4))
summary(main_md_lm_model_4)


md_lm_pred_4 = predict(main_md_lm_model_4, md_lm_test, type = "response")
md_lm_pred_output_4 = cbind(md_lm_test, md_lm_pred_4)

md_lm_pred_output_4$md_lm_pred_4_log = log(md_lm_pred_output_4$md_lm_pred_4)
md_lm_pred_output_4$SalePrice_log = log(md_lm_pred_output_4$SalePrice)

rmse(md_lm_pred_output_4$md_lm_pred_4_log,md_lm_pred_output_4$SalePrice_log)
##[1] 0.1398522









##Next we try a quadratic (third order) order model with interactive terms and forward stepwise approach
min_md_lm_model_5 = lm(SalePrice ~ 1, data = md_lm_train)

big_md_lm_model_5 = lm(SalePrice ~ . +	SaleCondition.Partial	:	SaleType.New
+	Exterior2nd.VinylSd	:	Exterior1st.VinylSd 
+	Exterior2nd.CmentBd	:	Exterior1st.CemntBd
+	Exterior2nd.MetalSd	:	Exterior1st.MetalSd 
+	RoofStyle.Hip	:	RoofStyle.Gable
+	LotShape.Reg	:	LotShape.IR1 
+	LandSlope.Mod	:	LandSlope.Gtl
+	ExterQual.TA	:	ExterQual.Gd 
+	ExterCond.TA	:	ExterCond.Gd
+	MiscFeature.Gar2	:	MiscVal
+	MiscVal	:	MiscFeature.Gar2
+	Exterior2nd.HdBoard	:	Exterior1st.HdBoard
+	GarageArea	:	GarageCars
+	Neighborhood.Somerst	:	MSZoning.FV
+	PavedDrive.Y	:	PavedDrive.N
+	Electrical.SBrkr	:	Electrical.FuseA
+	Exterior2nd.AsbShng	:	Exterior1st.AsbShng
+	TotRmsAbvGrd	:	GrLivArea
+	KitchenQual.TA	:	KitchenQual.Gd
+	X1stFlrSF	:	TotalBsmtSF
+	BldgType.1Fam	:	MSSubClass
+	HouseStyle.2Story	:	X2ndFlrSF
+	MSZoning.RM	:	MSZoning.RL
+	MasVnrType.None	:	MasVnrType.BrkFace
+	HouseStyle.1Story	:	X2ndFlrSF
+	GarageCond.TA	:	GarageQual.TA
+	Foundation.PConc	:	Foundation.CBlock
+	Exterior2nd.Stucco	:	Exterior1st.Stucco
+	SaleType.WD	:	SaleType.New
+	GarageCond.TA	:	GarageYrBlt
+	SaleCondition.Partial	:	SaleType.WD
+	BsmtQual.TA	:	BsmtQual.Gd
+	Exterior2nd.Plywood	:	Exterior1st.Plywood
+	LotConfig.Inside	:	LotConfig.Corner
+	Heating.GasW	:	Heating.GasA
+	GarageType.Detchd	:	GarageType.Attchd
+	GarageQual.TA	:	GarageYrBlt
+	BsmtFinType2.Unf	:	BsmtFinSF2
+	GrLivArea	:	X2ndFlrSF
+	BldgType.Duplex	:	KitchenAbvGr
+	MasVnrType.None	:	MasVnrArea
+	TotRmsAbvGrd	:	BedroomAbvGr
+	PoolQC.Gd	:	PoolArea
+	Exterior2nd.BrkFace	:	Exterior1st.BrkFace
+	KitchenQual.TA	:	ExterQual.TA
+	HouseStyle.2Story	:	HouseStyle.1Story
+	BldgType.TwnhsE	:	BldgType.1Fam
+	SaleCondition.Partial	:	SaleCondition.Normal
+	HeatingQC.TA	:	HeatingQC.Ex
+	GarageCond.Po	:	GarageQual.Po
+	Foundation.PConc	:	YearBuilt
+	BsmtFullBath	:	BsmtFinSF1
+	RoofMatl.CompShg	:	RoofStyle.Flat
+	ExterQual.TA	:	OverallQual
+	SaleCondition.Normal	:	SaleType.New
+	BsmtCond.TA	:	BsmtCond.Gd
+	SaleCondition.Normal	:	SaleType.WD
+	BsmtQual.TA	:	YearBuilt
+	Condition2.Norm	:	Condition2.Feedr
+	FullBath	:	GrLivArea
+	LandContour.Lvl	:	LandContour.Bnk
+	BsmtFinType1.Unf	:	BsmtFinSF1
+	KitchenQual.Gd	:	ExterQual.Gd
+	HouseStyle.2Story	:	HalfBath
+	Foundation.PConc	:	ExterQual.TA
+	ExterQual.TA	:	Foundation.PConc
+	KitchenQual.TA	:	ExterQual.Gd
+	TotRmsAbvGrd	:	X2ndFlrSF
+	GarageFinish.Unf	:	GarageType.Detchd
+	HalfBath	:	X2ndFlrSF
+	Condition1.Norm	:	Condition1.Feedr
+	BsmtQual.TA	:	Foundation.PConc
+	BsmtFinType1.Unf	:	BsmtUnfSF
+	GarageCars	:	OverallQual
+	GarageCars	:	GarageYrBlt
+	GrLivArea	:	OverallQual
+	YearRemodAdd	:	YearBuilt
+	ExterQual.TA	:	YearBuilt
+	Foundation.PConc	:	ExterQual.Gd
+	SaleCondition.Normal	:	SaleCondition.Abnorml
+	BsmtExposure.No	:	BsmtExposure.Av
+	KitchenQual.TA	:	YearRemodAdd
+	ExterCond.Ex	:	Condition2.PosA
+	Exterior2nd.AsphShn	:	Exterior1st.AsphShn
+	Condition2.PosA	:	ExterCond.Ex
+	YearBuilt	:	OverallQual
+	MasVnrType.BrkFace	:	MasVnrArea
+	Foundation.PConc	:	YearRemodAdd
+	Functional.Typ	:	Functional.Min2
+	Foundation.PConc	:	OverallQual
+	GrLivArea	:	X1stFlrSF
+	ExterQual.TA	:	YearRemodAdd
+	KitchenQual.Ex	:	ExterQual.Ex
+	Foundation.PConc	:	Exterior1st.VinylSd
+	GarageArea	:	OverallQual
+	Neighborhood.OldTown	:	MSZoning.RM
+	Foundation.PConc	:	Exterior2nd.VinylSd
+	GarageArea	:	GarageYrBlt
+	BsmtQual.Gd	:	YearBuilt
+	LandContour.Lvl	:	LandContour.HLS
+	ExterQual.Gd	:	YearBuilt
+	KitchenQual.Gd	:	ExterQual.TA
+	TotRmsAbvGrd	:	FullBath
+	Foundation.BrkTil	:	YearBuilt
+	KitchenQual.TA	:	OverallQual
+	KitchenQual.TA	:	Foundation.PConc
+	YearRemodAdd	:	OverallQual
+	FullBath	:	OverallQual
+	ExterQual.Gd	:	OverallQual
+	GarageQual.TA	:	GarageQual.Fa
+	BsmtQual.TA	:	ExterQual.TA
+	Functional.Typ	:	Functional.Min1
+	LandSlope.Sev	:	LotArea
+	GarageCars	:	YearBuilt
+	TotalBsmtSF	:	OverallQual
+	GarageFinish.Unf	:	GarageFinish.RFn
+	LandSlope.Gtl	:	LandContour.Lvl
+	KitchenQual.Gd	:	YearRemodAdd
+	HeatingQC.Ex	:	ExterQual.TA
+	BsmtQual.Ex	:	ExterQual.Ex
+	ExterQual.Gd	:	YearRemodAdd
+	HeatingQC.Ex	:	Foundation.PConc
+	BsmtCond.TA	:	BsmtCond.Fa
+	BldgType.TwnhsE	:	MSSubClass
+	Exterior2nd.CmentBd	:	Neighborhood.MeadowV
+	GarageFinish.Unf	:	YearBuilt
+	TotalBsmtSF	:	BsmtFinSF1
+	BedroomAbvGr	:	GrLivArea
+	Exterior1st.CemntBd	:	Neighborhood.MeadowV
+	Exterior1st.VinylSd	:	YearBuilt
+	KitchenQual.Ex	:	BsmtQual.Ex
+	HeatingQC.Ex	:	YearRemodAdd
+	Exterior2nd.VinylSd	:	YearBuilt
+	BsmtQual.TA	:	ExterQual.Gd
+	BsmtQual.TA	:	Foundation.CBlock
+	GarageCond.TA	:	GarageCars
+	BedroomAbvGr	:	X2ndFlrSF
+	ExterQual.Gd	:	BsmtQual.Gd + 
I(MasVnrArea^2) + I(MasVnrArea^3) + I(BsmtFinSF1^2) + I(BsmtFinSF1^3) + I(BsmtFinSF2^2) +I(BsmtFinSF2^3) + I(BsmtUnfSF^2) +I(BsmtUnfSF^3) + I(TotalBsmtSF^2) +I(TotalBsmtSF^3) + I(X1stFlrSF^2) + I(X1stFlrSF^3) + I(GrLivArea^2) +I(GrLivArea^3)+ I(GarageArea^2) + I(GarageArea^3) + I(WoodDeckSF^2) + I(WoodDeckSF^3) + I(OpenPorchSF^2) + I(OpenPorchSF^3) + I(EnclosedPorch^2) + I(EnclosedPorch^3) + I(X3SsnPorch^2) + I(X3SsnPorch^3) + I(LotArea^2) + I(LotArea^3) + MSSubClass :I(MasVnrArea^2), data = md_lm_train)

main_md_lm_model_5 = step(min_md_lm_model_5, direction = "forward", formula(big_md_lm_model_5))
summary(main_md_lm_model_5)


md_lm_pred_5 = predict(main_md_lm_model_5, md_lm_test, type = "response")
md_lm_pred_output_5 = cbind(md_lm_test, md_lm_pred_5)

md_lm_pred_output_5[] = lapply(md_lm_pred_output_5, abs)
md_lm_pred_output_5$md_lm_pred_5_log = log(md_lm_pred_output_5$md_lm_pred_5)
md_lm_pred_output_5$SalePrice_log = log(md_lm_pred_output_5$SalePrice)

rmse(md_lm_pred_output_5$md_lm_pred_5_log,md_lm_pred_output_5$SalePrice_log)









##Next we add a squareroot, and cubic root to the polynomial order model with interactive terms. we use forward stepwise approach for variable selection
min_md_lm_model_6 = lm(SalePrice ~ 1, data = md_lm_train)

big_md_lm_model_6 = lm(SalePrice ~ . +	SaleCondition.Partial	:	SaleType.New
+	Exterior2nd.VinylSd	:	Exterior1st.VinylSd 
+	Exterior2nd.CmentBd	:	Exterior1st.CemntBd
+	Exterior2nd.MetalSd	:	Exterior1st.MetalSd 
+	RoofStyle.Hip	:	RoofStyle.Gable
+	LotShape.Reg	:	LotShape.IR1 
+	LandSlope.Mod	:	LandSlope.Gtl
+	ExterQual.TA	:	ExterQual.Gd 
+	ExterCond.TA	:	ExterCond.Gd
+	MiscFeature.Gar2	:	MiscVal
+	MiscVal	:	MiscFeature.Gar2
+	Exterior2nd.HdBoard	:	Exterior1st.HdBoard
+	GarageArea	:	GarageCars
+	Neighborhood.Somerst	:	MSZoning.FV
+	PavedDrive.Y	:	PavedDrive.N
+	Electrical.SBrkr	:	Electrical.FuseA
+	Exterior2nd.AsbShng	:	Exterior1st.AsbShng
+	TotRmsAbvGrd	:	GrLivArea
+	KitchenQual.TA	:	KitchenQual.Gd
+	X1stFlrSF	:	TotalBsmtSF
+	BldgType.1Fam	:	MSSubClass
+	HouseStyle.2Story	:	X2ndFlrSF
+	MSZoning.RM	:	MSZoning.RL
+	MasVnrType.None	:	MasVnrType.BrkFace
+	HouseStyle.1Story	:	X2ndFlrSF
+	GarageCond.TA	:	GarageQual.TA
+	Foundation.PConc	:	Foundation.CBlock
+	Exterior2nd.Stucco	:	Exterior1st.Stucco
+	SaleType.WD	:	SaleType.New
+	GarageCond.TA	:	GarageYrBlt
+	SaleCondition.Partial	:	SaleType.WD
+	BsmtQual.TA	:	BsmtQual.Gd
+	Exterior2nd.Plywood	:	Exterior1st.Plywood
+	LotConfig.Inside	:	LotConfig.Corner
+	Heating.GasW	:	Heating.GasA
+	GarageType.Detchd	:	GarageType.Attchd
+	GarageQual.TA	:	GarageYrBlt
+	BsmtFinType2.Unf	:	BsmtFinSF2
+	GrLivArea	:	X2ndFlrSF
+	BldgType.Duplex	:	KitchenAbvGr
+	MasVnrType.None	:	MasVnrArea
+	TotRmsAbvGrd	:	BedroomAbvGr
+	PoolQC.Gd	:	PoolArea
+	Exterior2nd.BrkFace	:	Exterior1st.BrkFace
+	KitchenQual.TA	:	ExterQual.TA
+	HouseStyle.2Story	:	HouseStyle.1Story
+	BldgType.TwnhsE	:	BldgType.1Fam
+	SaleCondition.Partial	:	SaleCondition.Normal
+	HeatingQC.TA	:	HeatingQC.Ex
+	GarageCond.Po	:	GarageQual.Po
+	Foundation.PConc	:	YearBuilt
+	BsmtFullBath	:	BsmtFinSF1
+	RoofMatl.CompShg	:	RoofStyle.Flat
+	ExterQual.TA	:	OverallQual
+	SaleCondition.Normal	:	SaleType.New
+	BsmtCond.TA	:	BsmtCond.Gd
+	SaleCondition.Normal	:	SaleType.WD
+	BsmtQual.TA	:	YearBuilt
+	Condition2.Norm	:	Condition2.Feedr
+	FullBath	:	GrLivArea
+	LandContour.Lvl	:	LandContour.Bnk
+	BsmtFinType1.Unf	:	BsmtFinSF1
+	KitchenQual.Gd	:	ExterQual.Gd
+	HouseStyle.2Story	:	HalfBath
+	Foundation.PConc	:	ExterQual.TA
+	ExterQual.TA	:	Foundation.PConc
+	KitchenQual.TA	:	ExterQual.Gd
+	TotRmsAbvGrd	:	X2ndFlrSF
+	GarageFinish.Unf	:	GarageType.Detchd
+	HalfBath	:	X2ndFlrSF
+	Condition1.Norm	:	Condition1.Feedr
+	BsmtQual.TA	:	Foundation.PConc
+	BsmtFinType1.Unf	:	BsmtUnfSF
+	GarageCars	:	OverallQual
+	GarageCars	:	GarageYrBlt
+	GrLivArea	:	OverallQual
+	YearRemodAdd	:	YearBuilt
+	ExterQual.TA	:	YearBuilt
+	Foundation.PConc	:	ExterQual.Gd
+	SaleCondition.Normal	:	SaleCondition.Abnorml
+	BsmtExposure.No	:	BsmtExposure.Av
+	KitchenQual.TA	:	YearRemodAdd
+	ExterCond.Ex	:	Condition2.PosA
+	Exterior2nd.AsphShn	:	Exterior1st.AsphShn
+	Condition2.PosA	:	ExterCond.Ex
+	YearBuilt	:	OverallQual
+	MasVnrType.BrkFace	:	MasVnrArea
+	Foundation.PConc	:	YearRemodAdd
+	Functional.Typ	:	Functional.Min2
+	Foundation.PConc	:	OverallQual
+	GrLivArea	:	X1stFlrSF
+	ExterQual.TA	:	YearRemodAdd
+	KitchenQual.Ex	:	ExterQual.Ex
+	Foundation.PConc	:	Exterior1st.VinylSd
+	GarageArea	:	OverallQual
+	Neighborhood.OldTown	:	MSZoning.RM
+	Foundation.PConc	:	Exterior2nd.VinylSd
+	GarageArea	:	GarageYrBlt
+	BsmtQual.Gd	:	YearBuilt
+	LandContour.Lvl	:	LandContour.HLS
+	ExterQual.Gd	:	YearBuilt
+	KitchenQual.Gd	:	ExterQual.TA
+	TotRmsAbvGrd	:	FullBath
+	Foundation.BrkTil	:	YearBuilt
+	KitchenQual.TA	:	OverallQual
+	KitchenQual.TA	:	Foundation.PConc
+	YearRemodAdd	:	OverallQual
+	FullBath	:	OverallQual
+	ExterQual.Gd	:	OverallQual
+	GarageQual.TA	:	GarageQual.Fa
+	BsmtQual.TA	:	ExterQual.TA
+	Functional.Typ	:	Functional.Min1
+	LandSlope.Sev	:	LotArea
+	GarageCars	:	YearBuilt
+	TotalBsmtSF	:	OverallQual
+	GarageFinish.Unf	:	GarageFinish.RFn
+	LandSlope.Gtl	:	LandContour.Lvl
+	KitchenQual.Gd	:	YearRemodAdd
+	HeatingQC.Ex	:	ExterQual.TA
+	BsmtQual.Ex	:	ExterQual.Ex
+	ExterQual.Gd	:	YearRemodAdd
+	HeatingQC.Ex	:	Foundation.PConc
+	BsmtCond.TA	:	BsmtCond.Fa
+	BldgType.TwnhsE	:	MSSubClass
+	Exterior2nd.CmentBd	:	Neighborhood.MeadowV
+	GarageFinish.Unf	:	YearBuilt
+	TotalBsmtSF	:	BsmtFinSF1
+	BedroomAbvGr	:	GrLivArea
+	Exterior1st.CemntBd	:	Neighborhood.MeadowV
+	Exterior1st.VinylSd	:	YearBuilt
+	KitchenQual.Ex	:	BsmtQual.Ex
+	HeatingQC.Ex	:	YearRemodAdd
+	Exterior2nd.VinylSd	:	YearBuilt
+	BsmtQual.TA	:	ExterQual.Gd
+	BsmtQual.TA	:	Foundation.CBlock
+	GarageCond.TA	:	GarageCars
+	BedroomAbvGr	:	X2ndFlrSF
+	ExterQual.Gd	:	BsmtQual.Gd + 
I(MasVnrArea^2) + I(MasVnrArea^3) + I(BsmtFinSF1^2) + I(BsmtFinSF1^3) + I(BsmtFinSF2^2) +I(BsmtFinSF2^3) + I(BsmtUnfSF^2) +I(BsmtUnfSF^3) + I(TotalBsmtSF^2) +I(TotalBsmtSF^3) + I(X1stFlrSF^2) + I(X1stFlrSF^3) + I(GrLivArea^2) +I(GrLivArea^3)+ I(GarageArea^2) + I(GarageArea^3) + I(WoodDeckSF^2) + I(WoodDeckSF^3) + I(OpenPorchSF^2) + I(OpenPorchSF^3) + I(EnclosedPorch^2) + I(EnclosedPorch^3) + I(X3SsnPorch^2) + I(X3SsnPorch^3) + I(LotArea^2) + I(LotArea^3) + I(sqrt(MasVnrArea)) + I(MasVnrArea^(1/3)) + I(sqrt(BsmtFinSF1)) + I(BsmtFinSF1^(1/3)) + I(sqrt(BsmtFinSF2)) +I(BsmtFinSF2^(1/3)) + I(sqrt(BsmtUnfSF)) +I(BsmtUnfSF^(1/3)) + I(sqrt(TotalBsmtSF)) +I(TotalBsmtSF^(1/3)) + I(sqrt(X1stFlrSF)) + I(X1stFlrSF^(1/3)) + I(sqrt(GrLivArea)) +I(GrLivArea^(1/3))+ I(sqrt(GarageArea)) + I(GarageArea^(1/3)) + I(sqrt(WoodDeckSF)) + I(WoodDeckSF^(1/3)) + I(sqrt(OpenPorchSF)) + I(OpenPorchSF^(1/3)) + I(sqrt(EnclosedPorch)) + I(EnclosedPorch^(1/3)) + I(sqrt(X3SsnPorch)) + I(X3SsnPorch^(1/3)) + I(sqrt(LotArea)) + I(LotArea^(1/3)) + MSSubClass :I(MasVnrArea^2), data = md_lm_train)

main_md_lm_model_6 = step(min_md_lm_model_6, direction = "forward", formula(big_md_lm_model_6))
summary(main_md_lm_model_6)


md_lm_pred_6 = predict(main_md_lm_model_6, md_lm_test, type = "response")
md_lm_pred_output_6 = cbind(md_lm_test, md_lm_pred_6)

md_lm_pred_output_6[] = lapply(md_lm_pred_output_6, abs)
md_lm_pred_output_6$md_lm_pred_6_log = log(md_lm_pred_output_6$md_lm_pred_6)
md_lm_pred_output_6$SalePrice_log = log(md_lm_pred_output_6$SalePrice)

rmse(md_lm_pred_output_6$md_lm_pred_6_log,md_lm_pred_output_6$SalePrice_log)














tmp = lm(SalePrice ~ . + I(MasVnrArea^2) + I(MasVnrArea^3) + I(BsmtFinSF1^2) + I(BsmtFinSF1^3) + I(BsmtFinSF2^2) +I(BsmtFinSF2^3) + I(BsmtUnfSF^2) +I(BsmtUnfSF^3) + I(TotalBsmtSF^2) +I(TotalBsmtSF^3) + I(X1stFlrSF^2) + I(X1stFlrSF^3) + I(GrLivArea^2) +I(GrLivArea^3)+ I(GarageArea^2) + I(GarageArea^3) + I(WoodDeckSF^2) + I(WoodDeckSF^3) + I(OpenPorchSF^2) + I(OpenPorchSF^3) + I(EnclosedPorch^2) + I(EnclosedPorch^3) + I(X3SsnPorch^2) + I(X3SsnPorch^3) + I(LotArea^2) + I(LotArea^3) + MSSubClass :I(MasVnrArea^2), data = md_lm_train)

tmp_pred = predict(tmp, md_lm_test, type = "response")
rmse(tmp_pred,md_lm_test$SalePrice)












#########################  RANDOM FOREST  ##########################################
md_for = read.csv("/Users/appleowner/Documents/Kaggle /House Prices/train.csv", header = TRUE)
sapply(md_for, class)
head(md_for)


##Dealing with missing values in continuous variables
md_for[, "LotFrontage"][is.na(md_for[, "LotFrontage"])] = mean(md_for$LotFrontage)
md_for[, "MasVnrArea"][is.na(md_for[, "MasVnrArea"])] = mean(md_for$MasVnrArea)


##Dealing with missing values in categorical variables
categorica_vars=c('MSZoning','Street','Alley','LotShape','LandContour','Utilities','LotConfig','LandSlope','Neighborhood','Condition1','Condition2','BldgType','HouseStyle','RoofStyle','RoofMatl','Exterior1st','Exterior2nd','MasVnrType','ExterQual','ExterCond','Foundation','BsmtQual','BsmtCond','BsmtExposure','BsmtFinType1','BsmtFinType2','Heating','HeatingQC','CentralAir','Electrical','KitchenQual','Functional','FireplaceQu','GarageType','GarageFinish','GarageQual','GarageCond','PavedDrive','PoolQC','Fence','MiscFeature','SaleType','SaleCondition')

for (idx in categorical_vars){
  md_dummy_var_for = acm.disjonctif(md_for[idx])
  md_for[idx] = NULL
  md_for = cbind(md_for, md_dummy_var_for)}
  
##Now, after running the model on the training dataset and testing on the validation set, I realized some of the variables produced by the ##autoencoder did not appear in the test dataset. to solve this I could use
##levels(md_lm_train[,vars_with_miss_match_classes]) = levels(md_lm_test[,vars_with_miss_match_classes])  
##However, this did not seem to work so I went ahead and deleted those variables. 
md_for[, c("Utilities.NoSeWa", "HouseStyle.2.5Fin", "RoofMatl.ClyTile", "RoofMatl.Membran", "RoofMatl.Metal", "RoofMatl.Roll", "Exterior1st.ImStucc",  "Exterior1st.Stone", "Exterior2nd.Other", "Heating.Floor", "Heating.OthW",  "Electrical.Mix", "GarageQual.Ex", "PoolQC.Fa", "MiscFeature.TenC", "Condition2.RRAe", "Condition2.RRAn", "Condition2.RRNn")] = NULL
md_for[,'Exterior2nd.Wd Sdng'] = NULL

drops = c("Exterior1st.Wd Sdng","Exterior2nd.Wd Shng","Exterior2nd.Brk Cmn","RoofMatl.Tar&Grv", "MSZoning.C (all)")

md_for = md_for[ , !(names(md_for) %in% drops)]


md_for[, 2:37][is.na(md_for[, 2:37])] = 0
md_for[, 39:266][is.na(md_for[, 39:266])] = 0
md_for[, 38][is.na(md_for[, 38])] = mean(md_for$SalePrice)

##We split our dataset into training and validation set
md_for_price = md_for$SalePrice
index = createDataPartition(y=md_for_price, p = 0.70, list=FALSE)
md_for_train = md_for[index,]
md_for_test = md_for[-index,]


##Variable selection using the VSURF package
#install.packages("VSURF")
#library(VSURF)
#set.seed(1000, "L'Ecuyer-CMRG")
#md_for_var_sel = VSURF(SalePrice ~ ., mtry = 100, parallel = TRUE, ncore = 60, clusterType = "MPI", na.action = na.omit, data = md_for_train)
#summary(md_for_var_sel)

#plot(md_for_var_sel)
#names(md_for_var_sel) md_for_var_sel$varselect.thres         
#md_for_var_sel$varselect.interp
#md_for_var_sel$varselect.pred


##We perform prediction using the variables selected for prediction
#md_for_pred = predict(md_for_var_sel, newdata = md_for_test)

table(md_for$SalePrice)/nrow(md_for)
library(randomForest)

md_for_model = randomForest(SalePrice ~ . , data = md_for_train, importance = T)

#variable importance table
var_imp = data.frame(importance(md_for_model, type = 2))

#convert columns to row names
var_imp$Variables = row.names(var_imp)
var_imp[order(var_imp$IncNodePurity, decreasing = T),]

varImpPlot(md_for_model, sort = T, main = "Variable Importance", n.var = 10)

md_for_predict = predict(md_for_model, md_for_test)
md_for_model_output = cbind(md_for_test, md_for_predict)

md_for_model_output$log_md_for_predict = log(md_for_model_output$md_for_predict)
md_for_model_output$log_SalePrice = log(md_for_model_output$SalePrice)
rmse(md_for_model_output$log_SalePrice,md_for_model_output$log_md_for_predict)
#[1] 0.1566544



md_for_model_1 = randomForest(SalePrice ~ . + I(MasVnrArea^2) + I(BsmtFinSF1^2) + I(BsmtFinSF2^2) + I(BsmtUnfSF^2) + I(TotalBsmtSF^2) + I(X1stFlrSF^2) + I(GrLivArea^2) + I(GarageArea^2) + I(WoodDeckSF^2) + I(OpenPorchSF^2) + I(EnclosedPorch^2) + I(X3SsnPorch^2) + I(LotArea^2) +    MSSubClass  :     I(MasVnrArea^2), data = md_for_train, importance = T)

#variable importance table
var_imp_1 = data.frame(importance(md_for_model_1, type = 2))

#convert columns to row names
var_imp_1$Variables = row.names(var_imp_1)
var_imp_1[order(var_imp_1$IncNodePurity, decreasing = T),]

varImpPlot(md_for_model_1, sort = T, main = "Variable Importance", n.var = 10)

md_for_predict_1 = predict(md_for_model_1, md_for_test)
md_for_model_output_1 = cbind(md_for_test, md_for_predict_1)

md_for_model_output_1$log_md_for_predict = log(md_for_model_output_1$md_for_predict_1)
md_for_model_output_1$log_SalePrice = log(md_for_model_output_1$SalePrice)
rmse(md_for_model_output_1$log_SalePrice,md_for_model_output_1$log_md_for_predict)
#[1] 0.158991












################################### LASSO REGRESSION #####################################

library(lars)
md_lar = read.csv("/Users/appleowner/Documents/Kaggle /House Prices/train.csv", header = TRUE)
sapply(md_lar, class)
head(md_lar)


##Dealing with missing values in continuous variables
md_lar[, "LotFrontage"][is.na(md_lar[, "LotFrontage"])] = mean(md_lar$LotFrontage)
md_lar[, "MasVnrArea"][is.na(md_lar[, "MasVnrArea"])] = mean(md_lar$MasVnrArea)


##Dealing with missing values in categorical variables
categorica_vars=c('MSZoning','Street','Alley','LotShape','LandContour','Utilities','LotConfig','LandSlope','Neighborhood','Condition1','Condition2','BldgType','HouseStyle','RoofStyle','RoofMatl','Exterior1st','Exterior2nd','MasVnrType','ExterQual','ExterCond','Foundation','BsmtQual','BsmtCond','BsmtExposure','BsmtFinType1','BsmtFinType2','Heating','HeatingQC','CentralAir','Electrical','KitchenQual','Functional','FireplaceQu','GarageType','GarageFinish','GarageQual','GarageCond','PavedDrive','PoolQC','Fence','MiscFeature','SaleType','SaleCondition')

for (idx in categorical_vars){
  md_dummy_var_lar = acm.disjonctif(md_lar[idx])
  md_lar[idx] = NULL
  md_lar = cbind(md_lar, md_dummy_var_lar)}
  
##Now, after running the model on the training dataset and testing on the validation set, I realized some of the variables produced by the ##autoencoder did not appear in the test dataset. to solve this I could use
##levels(md_lm_train[,vars_with_miss_match_classes]) = levels(md_lm_test[,vars_with_miss_match_classes])  
##However, this did not seem to work so I went ahead and deleted those variables. 
md_lar[, c("Utilities.NoSeWa", "HouseStyle.2.5Fin", "RoofMatl.ClyTile", "RoofMatl.Membran", "RoofMatl.Metal", "RoofMatl.Roll", "Exterior1st.ImStucc",  "Exterior1st.Stone", "Exterior2nd.Other", "Heating.Floor", "Heating.OthW",  "Electrical.Mix", "GarageQual.Ex", "PoolQC.Fa", "MiscFeature.TenC", "Condition2.RRAe", "Condition2.RRAn", "Condition2.RRNn")] = NULL
md_lar[,'Exterior2nd.Wd Sdng'] = NULL

drops = c("Exterior1st.Wd Sdng","Exterior2nd.Wd Shng","Exterior2nd.Brk Cmn","RoofMatl.Tar&Grv", "MSZoning.C (all)")

md_lar = md_lar[ , !(names(md_for) %in% drops)]


md_lar[, 2:37][is.na(md_lar[, 2:37])] = 0
md_lar[, 39:266][is.na(md_lar[, 39:266])] = 0
md_lar[, 38][is.na(md_lar[, 38])] = mean(md_lar$SalePrice)

##We split our dataset into training and validation set
md_lar_price = md_lar$SalePrice
index = createDataPartition(y=md_lar_price, p = 0.70, list=FALSE)
md_lar_train = md_lar[index,]
md_lar_test = md_lar[-index,]

##Lasso model
lar_y = as.numeric(md_lar_train[,38])
lar_x = as.matrix(md_lar_train[,-38])

md_lar_model = lars(lar_x, lar_y, type = "lasso")
lamb_lar = c(md_lar_model$lambda, 0)
coef_lar = coef(md_lar_model)

##find the optimum lambda
library(colorspace)
colors = rainbow(265)

matplot(lamb_lar, coef_lar, xlim = c(270,-4), type = "o", pch = 20, xlab = "expression(lambda)", ylab =" expression(hat(coef_lar))", col = colors)
text(rep(0,266), coef_lar[,], colnames(lar_x), pos = 4, col = colors)
abline(v = lamb_lar[4], lty=2)
abline(h=0, lty = 2)

##Sometimes visualization gets better when you use a scaled X

coef_lar_scale = attr(md_lar_model$coef_lar, "scaled:scale")
coef_lar_rescaled = coef_lar
for(j in 1:266){
	coef_lar_rescaled[j,] = coef_lar_rescaled[j,]*coef_lar_scale
}

matplot(lamb_lar, coef_lar_rescaled, xlim = c(270,-4), type = "o", pch = 20, xlab = "expression(lambda)", ylab =" expression(hat(coef_lar))", col = colors)
text(rep(0,266), coef_lar[,], colnames(lar_x), pos = 4, col = colors)
abline(v = lamb_lar[4], lty=2)
abline(h=0, lty = 2)

best_step = md_lar_model$df[which.min(md_lar_model$Cp)]
md_lar_predict = predict.lars(md_lar_model, newx = as.matrix(md_lar_test[,-38]), s = best_step, type = "fit")
md_lar_model_output = cbind(md_lar_test, md_lar_predict)


md_lar_model_output$log_fit = log(md_lar_model_output$fit)
md_lar_model_output$log_SalePrice = log(md_lar_model_output$SalePrice)

#Test with RMSE

rmse(md_lar_model_output$log_SalePrice,md_lar_model_output$log_fit)
#[1] 0.1610994












######################################  RIDGE REGRESSION ############################################
library(glmnet)
md_rid = read.csv("/Users/appleowner/Documents/Kaggle /House Prices/train.csv", header = TRUE)
sapply(md_rid, class)
head(md_rid)


##Dealing with missing values in continuous variables
md_rid[, "LotFrontage"][is.na(md_rid[, "LotFrontage"])] = mean(md_rid$LotFrontage)
md_rid[, "MasVnrArea"][is.na(md_rid[, "MasVnrArea"])] = mean(md_rid$MasVnrArea)


##Dealing with missing values in categorical variables
categorica_vars=c('MSZoning','Street','Alley','LotShape','LandContour','Utilities','LotConfig','LandSlope','Neighborhood','Condition1','Condition2','BldgType','HouseStyle','RoofStyle','RoofMatl','Exterior1st','Exterior2nd','MasVnrType','ExterQual','ExterCond','Foundation','BsmtQual','BsmtCond','BsmtExposure','BsmtFinType1','BsmtFinType2','Heating','HeatingQC','CentralAir','Electrical','KitchenQual','Functional','FireplaceQu','GarageType','GarageFinish','GarageQual','GarageCond','PavedDrive','PoolQC','Fence','MiscFeature','SaleType','SaleCondition')

for (idx in categorical_vars){
  md_dummy_var_rid = acm.disjonctif(md_rid[idx])
  md_rid[idx] = NULL
  md_rid = cbind(md_rid, md_dummy_var_rid)}
  
##Now, after running the model on the training dataset and testing on the validation set, I realized some of the variables produced by the ##autoencoder did not appear in the test dataset. to solve this I could use
##levels(md_lm_train[,vars_with_miss_match_classes]) = levels(md_lm_test[,vars_with_miss_match_classes])  
##However, this did not seem to work so I went ahead and deleted those variables. 
md_rid[, c("Utilities.NoSeWa", "HouseStyle.2.5Fin", "RoofMatl.ClyTile", "RoofMatl.Membran", "RoofMatl.Metal", "RoofMatl.Roll", "Exterior1st.ImStucc",  "Exterior1st.Stone", "Exterior2nd.Other", "Heating.Floor", "Heating.OthW",  "Electrical.Mix", "GarageQual.Ex", "PoolQC.Fa", "MiscFeature.TenC", "Condition2.RRAe", "Condition2.RRAn", "Condition2.RRNn")] = NULL
md_rid[,'Exterior2nd.Wd Sdng'] = NULL

drops = c("Exterior1st.Wd Sdng","Exterior2nd.Wd Shng","Exterior2nd.Brk Cmn","RoofMatl.Tar&Grv", "MSZoning.C (all)")

md_rid = md_rid[ , !(names(md_for) %in% drops)]


md_rid[, 2:37][is.na(md_rid[, 2:37])] = 0
md_rid[, 39:266][is.na(md_rid[, 39:266])] = 0
md_rid[, 38][is.na(md_rid[, 38])] = mean(md_rid$SalePrice)

##We split our dataset into training and validation set
rid_x = model.matrix(SalePrice ~ ., data = md_rid)[,-1]
rid_y = as.matrix(md_rid[,38])

md_rid_price = md_rid$SalePrice
index = createDataPartition(y=md_rid_price, p = 0.70, list=FALSE)
md_rid_train = md_rid[index,]
md_rid_test = as.matrix(md_rid[-index,])


rid_y_train = as.numeric(md_rid_train[,38])
rid_x_train = as.matrix(md_rid_train[,-38])
rid_y_test = as.numeric(md_rid_test[,38])
rid_x_test = as.matrix(md_rid_test[,-38])


grid = 10^seq(10, -2, length=100)
md_rid_model = glmnet(rid_x_train, rid_y_train, alpha = 0, lambda = grid, thresh = 1e-12)

##we search for the best lambda value using the k-fold cross validation approach

set.seed(1)
cv_rid = cv.glmnet(rid_x_train, rid_y_train, alpha = 0, type.measure = "mse", nfolds = 20)
plot(cv_rid)
best_lambda = cv_rid$lambda.min
best_lambda

md_rid_predict = predict(md_rid_model, s = best_lambda, newx = rid_x_test)

rmse(log(md_rid_predict), log(rid_y_test))
##[1] 0.1527957














################################ PRINCIPAL COMPONENT REGRESSION #####################################
library(pls)
md_pcr = read.csv("/Users/appleowner/Documents/Kaggle /House Prices/train.csv", header = TRUE)
sapply(md_pcr, class)
head(md_pcr)


##Dealing with missing values in continuous variables
md_pcr[, "LotFrontage"][is.na(md_pcr[, "LotFrontage"])] = mean(md_pcr$LotFrontage)
md_pcr[, "MasVnrArea"][is.na(md_pcr[, "MasVnrArea"])] = mean(md_pcr$MasVnrArea)


##Dealing with missing values in categorical variables
categorica_vars=c('MSZoning','Street','Alley','LotShape','LandContour','Utilities','LotConfig','LandSlope','Neighborhood','Condition1','Condition2','BldgType','HouseStyle','RoofStyle','RoofMatl','Exterior1st','Exterior2nd','MasVnrType','ExterQual','ExterCond','Foundation','BsmtQual','BsmtCond','BsmtExposure','BsmtFinType1','BsmtFinType2','Heating','HeatingQC','CentralAir','Electrical','KitchenQual','Functional','FireplaceQu','GarageType','GarageFinish','GarageQual','GarageCond','PavedDrive','PoolQC','Fence','MiscFeature','SaleType','SaleCondition')

for (idx in categorical_vars){
  md_dummy_var_pcr = acm.disjonctif(md_pcr[idx])
  md_pcr[idx] = NULL
  md_pcr = cbind(md_pcr, md_dummy_var_pcr)}
  
##Now, after running the model on the training dataset and testing on the validation set, I realized some of the variables produced by the ##autoencoder did not appear in the test dataset. to solve this I could use
##levels(md_lm_train[,vars_with_miss_match_classes]) = levels(md_lm_test[,vars_with_miss_match_classes])  
##However, this did not seem to work so I went ahead and deleted those variables. 
md_pcr[, c("Utilities.NoSeWa", "HouseStyle.2.5Fin", "RoofMatl.ClyTile", "RoofMatl.Membran", "RoofMatl.Metal", "RoofMatl.Roll", "Exterior1st.ImStucc",  "Exterior1st.Stone", "Exterior2nd.Other", "Heating.Floor", "Heating.OthW",  "Electrical.Mix", "GarageQual.Ex", "PoolQC.Fa", "MiscFeature.TenC", "Condition2.RRAe", "Condition2.RRAn", "Condition2.RRNn")] = NULL
md_pcr[,'Exterior2nd.Wd Sdng'] = NULL

drops = c("Exterior1st.Wd Sdng","Exterior2nd.Wd Shng","Exterior2nd.Brk Cmn","RoofMatl.Tar&Grv", "MSZoning.C (all)")

md_pcr = md_pcr[ , !(names(md_pcr) %in% drops)]


md_pcr[, 2:37][is.na(md_pcr[, 2:37])] = 0
md_pcr[, 39:266][is.na(md_pcr[, 39:266])] = 0
md_pcr[, 38][is.na(md_pcr[, 38])] = mean(md_pcr$SalePrice)


set.seed(1)
md_pcr_price = md_pcr$SalePrice
index = createDataPartition(y=md_pcr_price, p = 0.70, list=FALSE)
md_pcr_train = md_pcr[index,]
md_pcr_test = md_pcr[-index,]



md_pcr_model = pcr(SalePrice ~ ., data = md_pcr_train, scale = TRUE, validation = "CV")
validationplot(md_pcr_model, val.type = "MSEP")

##We choose the minimum M number of components which in this case = 224
md_pcr_predict = predict(md_pcr_model, md_pcr_test, ncomp = 224)

plot(md_pcr_predict, md_pcr_test$SalePrice, xlab = "Predicted", ylab = "observed")
rmse(log(md_pcr_predict), log(md_pcr_test$SalePrice))
##[1] 0.1607436















#####################################  ELASTIC NET REGRESSION ###########################################

md_net = read.csv("/Users/appleowner/Documents/Kaggle /House Prices/train.csv", header = TRUE)
sapply(md_net, class)
head(md_net)


##Dealing with missing values in continuous variables
md_net[, "LotFrontage"][is.na(md_net[, "LotFrontage"])] = mean(md_net$LotFrontage)
md_net[, "MasVnrArea"][is.na(md_net[, "MasVnrArea"])] = mean(md_net$MasVnrArea)


##Dealing with missing values in categorical variables
categorica_vars=c('MSZoning','Street','Alley','LotShape','LandContour','Utilities','LotConfig','LandSlope','Neighborhood','Condition1','Condition2','BldgType','HouseStyle','RoofStyle','RoofMatl','Exterior1st','Exterior2nd','MasVnrType','ExterQual','ExterCond','Foundation','BsmtQual','BsmtCond','BsmtExposure','BsmtFinType1','BsmtFinType2','Heating','HeatingQC','CentralAir','Electrical','KitchenQual','Functional','FireplaceQu','GarageType','GarageFinish','GarageQual','GarageCond','PavedDrive','PoolQC','Fence','MiscFeature','SaleType','SaleCondition')

for (idx in categorical_vars){
  md_dummy_var_net = acm.disjonctif(md_net[idx])
  md_net[idx] = NULL
  md_net = cbind(md_net, md_dummy_var_net)}
  
##Now, after running the model on the training dataset and testing on the validation set, I realized some of the variables produced by the ##autoencoder did not appear in the test dataset. to solve this I could use
##levels(md_lm_train[,vars_with_miss_match_classes]) = levels(md_lm_test[,vars_with_miss_match_classes])  
##However, this did not seem to work so I went ahead and deleted those variables. 
md_net[, c("Utilities.NoSeWa", "HouseStyle.2.5Fin", "RoofMatl.ClyTile", "RoofMatl.Membran", "RoofMatl.Metal", "RoofMatl.Roll", "Exterior1st.ImStucc",  "Exterior1st.Stone", "Exterior2nd.Other", "Heating.Floor", "Heating.OthW",  "Electrical.Mix", "GarageQual.Ex", "PoolQC.Fa", "MiscFeature.TenC", "Condition2.RRAe", "Condition2.RRAn", "Condition2.RRNn")] = NULL
md_net[,'Exterior2nd.Wd Sdng'] = NULL

drops = c("Exterior1st.Wd Sdng","Exterior2nd.Wd Shng","Exterior2nd.Brk Cmn","RoofMatl.Tar&Grv", "MSZoning.C (all)")

md_net = md_net[ , !(names(md_net) %in% drops)]


md_net[, 2:37][is.na(md_net[, 2:37])] = 0
md_net[, 39:266][is.na(md_net[, 39:266])] = 0
md_net[, 38][is.na(md_net[, 38])] = mean(md_net$SalePrice)


set.seed(1)
md_net_price = md_net$SalePrice
index = createDataPartition(y=md_net_price, p = 0.70, list=FALSE)
md_net_train = md_net[index,]
md_net_test = md_net[-index,]

net_y_train = as.numeric(md_net_train[,38])
net_x_train = as.matrix(md_net_train[,-38])
net_y_test = as.numeric(md_net_test[,38])
net_x_test = as.matrix(md_net_test[,-38])

#we set up both lambda and alpha and we select the the value that minimizes cross validation error
lamb_grid_net = 10^seq(2, -2, length=100)
alpha_grid_net = seq(0, 1, length = 10)

#set up the dross validation method
trn_cntrl_net = trainControl(method = "repeatedCV", number = 10, repeats = 5)

#set up search grid for both lambda and alpha
srch_grid_net = expand.grid(.alpha = lamb_grid_net, .lambda = alpha_grid_net)

#perform cross validation forecasting Sale Price based on all features

set.seed(1)
#train function depends on caret package
require(caret)
md_net_model = train(SalePrice ~ ., data = md_net_train, method = "glmnet", tuneGrid = srch_grid_net, trControl=trn_cntrl_net, standardize=TRUE, maxit=1000000)

plot(md_net_model)

md_net_model$bestTune

md_net_model_final = md_net_model$finalModel

md_net_predict = predict(md_net_model_final, newx = net_x_test, s = md_net_model$bestTune$lambda)

plot(md_net_predict, md_net_test$SalePrice, xlab = "Predicted", ylab = "observed")
rmse(log(md_net_predict), log(md_net_test$SalePrice))
#[1] 0.1683465





md_net_model_2 = train(SalePrice ~ . , data = md_net_train, method = "glmnet", tuneGrid = srch_grid_net, trControl=trn_cntrl_net, standardize=TRUE, maxit=1000000)

plot(md_net_model_2)

md_net_model_2$bestTune

md_net_model_3 = glmnet(net_x_train, net_y_train, alpha = 0.0305, lambda = 1, thresh = 1e-12)

md_net_predict_2 = predict(md_net_model_3, newx = net_x_test, type = "link")

plot(md_net_predict_2, md_net_test$SalePrice, xlab = "Predicted", ylab = "observed")
rmse(log(md_net_predict_2), log(md_net_test$SalePrice))






###################################### xgboost  ####################################################
require(xgboost)
md_xgb = read.csv("/Users/appleowner/Documents/Kaggle /House Prices/train.csv", header = TRUE)
sapply(md_xgb, class)
head(md_xgb)


##Dealing with missing values in continuous variables
md_xgb[, "LotFrontage"][is.na(md_xgb[, "LotFrontage"])] = mean(md_xgb$LotFrontage)
md_xgb[, "MasVnrArea"][is.na(md_xgb[, "MasVnrArea"])] = mean(md_xgb$MasVnrArea)


##Dealing with missing values in categorical variables
categorica_vars=c('MSZoning','Street','Alley','LotShape','LandContour','Utilities','LotConfig','LandSlope','Neighborhood','Condition1','Condition2','BldgType','HouseStyle','RoofStyle','RoofMatl','Exterior1st','Exterior2nd','MasVnrType','ExterQual','ExterCond','Foundation','BsmtQual','BsmtCond','BsmtExposure','BsmtFinType1','BsmtFinType2','Heating','HeatingQC','CentralAir','Electrical','KitchenQual','Functional','FireplaceQu','GarageType','GarageFinish','GarageQual','GarageCond','PavedDrive','PoolQC','Fence','MiscFeature','SaleType','SaleCondition')

for (idx in categorical_vars){
  md_dummy_var_xgb = acm.disjonctif(md_xgb[idx])
  md_xgb[idx] = NULL
  md_xgb = cbind(md_xgb, md_dummy_var_xgb)}
  
##Now, after running the model on the training dataset and testing on the validation set, I realized some of the variables produced by the ##autoencoder did not appear in the test dataset. to solve this I could use
##levels(md_lm_train[,vars_with_miss_match_classes]) = levels(md_lm_test[,vars_with_miss_match_classes])  
##However, this did not seem to work so I went ahead and deleted those variables. 
md_xgb[, c("Utilities.NoSeWa", "HouseStyle.2.5Fin", "RoofMatl.ClyTile", "RoofMatl.Membran", "RoofMatl.Metal", "RoofMatl.Roll", "Exterior1st.ImStucc",  "Exterior1st.Stone", "Exterior2nd.Other", "Heating.Floor", "Heating.OthW",  "Electrical.Mix", "GarageQual.Ex", "PoolQC.Fa", "MiscFeature.TenC", "Condition2.RRAe", "Condition2.RRAn", "Condition2.RRNn")] = NULL
md_xgb[,'Exterior2nd.Wd Sdng'] = NULL

drops = c("Exterior1st.Wd Sdng","Exterior2nd.Wd Shng","Exterior2nd.Brk Cmn","RoofMatl.Tar&Grv", "MSZoning.C (all)")

md_xgb = md_xgb[ , !(names(md_xgb) %in% drops)]


md_xgb[, 2:37][is.na(md_xgb[, 2:37])] = 0
md_xgb[, 39:266][is.na(md_xgb[, 39:266])] = 0
md_xgb[, 38][is.na(md_xgb[, 38])] = mean(md_net$SalePrice)


set.seed(1)
md_xgb_price = md_xgb$SalePrice
index = createDataPartition(y=md_net_price, p = 0.70, list=FALSE)
md_xgb_train = md_xgb[index,]
md_xgb_test = md_xgb[-index,]

xgb_y_train = as.matrix(md_xgb_train[,38])
xgb_x_train = as.matrix(md_xgb_train[,-38])
xgb_y_test = as.matrix(md_xgb_test[,38])
xgb_x_test = as.matrix(md_xgb_test[,-38])

xgb_y_train = as(xgb_y_train, "sparseMatrix")
xgb_x_train = as(xgb_x_train, "sparseMatrix")
xgb_y_test = as(xgb_y_test, "sparseMatrix")
xgb_x_test = as(xgb_x_test, "sparseMatrix")

md_train_xgb = xgb.DMatrix(data = xgb_x_train, label = xgb_y_train)


##The tuning process will go through 30 iterations in search for the best parameter levels to optimize our model
All_rmse_1 = c()
Param_group_1 = c()
for (idx in 1:80){
	param_1 = list(objective = "reg:linear",
				 eval_metric = "rmse",
				 booster = "gbtree",
				 max_depth = sample(2:10, 1),
				 eta = runif(1, 0.01, 0.3),
				 gamma = runif(1, 0.0, 0.2),
				 subsample = runif(1, 0.6, 0.9),
				 colsample_bytree = runif(1, 0.5, 0.8)
				 )
	
	cv_nround_1 = 800
	cv_nfold_1 = 6
	cv_nthread_1 = 6
	
	cv_xgb_1 = xgb.cv(data = md_train_xgb, 
					params = param_1,
					nthread = cv_nthread_1,
					nfold = cv_nfold_1,
					nrounds = cv_nround_1,
					verbose = 2)
					
	min_rmse_1 = min(cv_xgb_1[,test.rmse.mean])
	All_rmse_1 = append(All_rmse_1, min_rmse_1)
	Param_group_1 = append(Param_group_1, param_1)
	#best parameter
	param_min_1 = Param_group_1[(which.min(All_rmse_1)*8+1): (which.min(All_rmse_1)*8+8)]
}

param_1


optimised_params_1 = list(objective = param_1$objective,
				       eval_metric = param_1$eval_metric,
				 	   booster = param_1$booster,
				       max_depth = param_1$max_depth,
				       eta = param_1$eta,
				       gamma = param_1$gamma,
				       subsample = param_1$subsample,
				       colsample_bytree = param_1$colsample_bytree
				       )				       
				    
md_xgb_model_1 = xgb.train(params = optimised_params_1,
						   data = md_train_xgb,
						   nrounds = 800,
						   watchlist = list(train = md_train_xgb),
						   verbose = 2,
						   print_every_n = 50,
						   nthread = 6
						   )
						   
importance_matrix_1 = xgb.importance(model = md_xgb_model_1)
print(importance_matrix_1)
xgb.plot.importance(importance_matrix = importance_matrix_1)						   

md_test_xgb	= xgb.DMatrix(data = xgb_x_test)

md_xgb_predict_1 = predict(md_xgb_model_1, md_test_xgb)
rmse(log(md_xgb_test$SalePrice), log(md_xgb_predict_1))			       
##[1] 0.1355044				       
				       
				       
				       
##We try xgb with a linear booster.
All_rmse_2 = c()
Param_group_2 = c()
for (idx in 1:80){
	param_2 = list(objective = "reg:linear",
				 eval_metric = "rmse",
				 booster = "gblinear",
				 max_depth = sample(2:10, 1),
				 eta = runif(1, 0.01, 0.3),
				 gamma = runif(1, 0.0, 0.2),
				 subsample = runif(1, 0.6, 0.9),
				 colsample_bytree = runif(1, 0.5, 0.8)
				 )
	
	cv_nround_2 = 800
	cv_nfold_2 = 6
	cv_nthread_2 = 6
	
	cv_xgb_2 = xgb.cv(data = md_train_xgb, 
					params = param_2,
					nthread = cv_nthread_2,
					nfold = cv_nfold_2,
					nrounds = cv_nround_2,
					verbose = 2)
					
	min_rmse_2 = min(cv_xgb_2[,test.rmse.mean])
	All_rmse_2 = append(All_rmse_2, min_rmse_2)
	Param_group_1 = append(Param_group_2, param_2)
	#best parameter
	param_min_2 = Param_group_2[(which.min(All_rmse_2)*8+1): (which.min(All_rmse_2)*8+8)]
}

param_2


optimised_params_2 = list(objective = param_2$objective,
				       eval_metric = param_2$eval_metric,
				 	   booster = param_2$booster,
				       max_depth = param_2$max_depth,
				       eta = param_2$eta,
				       gamma = param_2$gamma,
				       subsample = param_2$subsample,
				       colsample_bytree = param_2$colsample_bytree
				       )				       
				    
md_xgb_model_2 = xgb.train(params = optimised_params_2,
						   data = md_train_xgb,
						   nrounds = 800,
						   watchlist = list(train = md_train_xgb),
						   verbose = 2,
						   print_every_n = 50,
						   nthread = 6
						   )
						   
importance_matrix_2 = xgb.importance(model = md_xgb_model_2)
print(importance_matrix_2)
xgb.plot.importance(importance_matrix = importance_matrix_2)						   

md_test_xgb	= xgb.DMatrix(data = xgb_x_test)

md_xgb_predict_2 = predict(md_xgb_model_2, md_test_xgb)
rmse(log(md_xgb_test$SalePrice), log(md_xgb_predict_2))			       
##[1] 0.1834216	clearly the linear booster is not efficient
				       
				       
				       
##The two best models I have are the quadratic (second order) model and the xgboost with xgtree booster. 
##I will go ahead and use xgboost to predict the 				       
				       
md_test = read.csv("/Users/appleowner/Documents/Kaggle /House Prices/test.csv", header = TRUE)	
md_test_2 = md_test			       
##converting categorical variables to dummy variables
for (idx in categorical_vars)
	{
	md_dummy_var_lm = acm.disjonctif(md_test_2[idx])
	md_test_2[idx] = NULL
	md_test_2 = cbind(md_test_2, md_dummy_var_lm)
	}

##take out variables that do not exist in the test data
##I found out there were not available in the test data when I went to predict the salePrice
md_lm[, c("Utilities.NoSeWa", "HouseStyle.2.5Fin", "RoofMatl.ClyTile", "RoofMatl.Membran", "RoofMatl.Metal", "RoofMatl.Roll", "Exterior1st.ImStucc",  "Exterior1st.Stone", "Exterior2nd.Other", "Heating.Floor", "Heating.OthW",  "Electrical.Mix", "GarageQual.Ex", "PoolQC.Fa", "MiscFeature.TenC", "Condition2.RRAe", "Condition2.RRAn", "Condition2.RRNn")] = NULL

##Take out the NA values in the dummy variables
md_test_2[, 2:37][is.na(md_test_2[, 2:37])] = 0
md_test_2[, 39:272][is.na(md_test_2[, 39:272])] = 0
md_test_2[, 38][is.na(md_test_2[, 38])] = mean(md_test_2 $SalePrice)



md_test_2 = as.matrix(md_test_2[,c(1:272)])

md_test_2 = as(md_test_2, "sparseMatrix")

md_test_2_xgb	= xgb.DMatrix(data = md_test_2)				       
md_xgb_predict_2 = predict(md_xgb_model_1, md_test_2_xgb)				       
				       
				       
