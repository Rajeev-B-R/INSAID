library(data.table) # used for reading and manipulation of data
library(dplyr)      # used for data manipulation and joining
library(ggplot2)    # used for ploting
library(caret)      # used for modeling
library(corrplot)   # used for making correlation plot
library(xgboost)    # used for building XGBoost model
library(cowplot)    # used for combining multiple plots


#Link for downloading the datasets: https://datahack.analyticsvidhya.com/contest/practice-problem-big-mart-sales-iii/#ProblemStatement
train = fread("C:/Users/Rajeev/Downloads/train_v9rqX0R.csv")
test = fread("C:/Users/Rajeev/Downloads/test_AbJTz2l.csv")


#Let's check the dimensions of our data, i.e., columns and rows.
dim(train)
dim(test)


#We will take a glance over the feature names of train and test datasets.
names(train)
names(test)


#A short summary of all the features present in these dataframes.
str(train)
str(test)


test[,Item_Outlet_Sales := NA] 
combi = rbind(train, test) # combining train and test datasets 
dim(combi)


#Target variable is continuous. So, we visualise it by plotting its histogram.
ggplot(train) + 
  geom_histogram(aes(train$Item_Outlet_Sales), binwidth = 100, fill = "darkgreen") +  
  xlab("Item_Outlet_Sales")


#Histograms for visualizations of numeric independent variables. It will help us in visualizing the distributions.
p1 = ggplot(combi) + geom_histogram(aes(Item_Weight), binwidth = 0.5, fill = "blue") 
p2 = ggplot(combi) + geom_histogram(aes(Item_Visibility), binwidth = 0.005, fill = "blue") 
p3 = ggplot(combi) + geom_histogram(aes(Item_MRP), binwidth = 1, fill = "blue") 
plot_grid(p1, p2, p3, nrow = 1) # plot_grid() from cowplot package


#Gaining some insights from the categorical variables.
ggplot(combi %>% group_by(Item_Fat_Content) %>% summarise(Count = n())) +   
  geom_bar(aes(Item_Fat_Content, Count), stat = "identity", fill = "coral1")


#Making corrections and plotting the same again.
combi$Item_Fat_Content[combi$Item_Fat_Content == "LF"] = "Low Fat" 
combi$Item_Fat_Content[combi$Item_Fat_Content == "low fat"] = "Low Fat" 
combi$Item_Fat_Content[combi$Item_Fat_Content == "reg"] = "Regular" 
ggplot(combi %>% group_by(Item_Fat_Content) %>% summarise(Count = n())) +   
  geom_bar(aes(Item_Fat_Content, Count), stat = "identity", fill = "coral1")


#Let's check the other categorical variables.
# plot for Item_Type 
p4 = ggplot(combi %>% group_by(Item_Type) %>% summarise(Count = n())) +   
  geom_bar(aes(Item_Type, Count), stat = "identity", fill = "coral1") +  
  xlab("") +  
  geom_label(aes(Item_Type, Count, label = Count), vjust = 0.5) +  
  theme(axis.text.x = element_text(angle = 45, hjust = 1))+  
  ggtitle("Item_Type")
# plot for Outlet_Identifier 
p5 = ggplot(combi %>% group_by(Outlet_Identifier) %>% summarise(Count = n())) +   
  geom_bar(aes(Outlet_Identifier, Count), stat = "identity", fill = "coral1") +  
  geom_label(aes(Outlet_Identifier, Count, label = Count), vjust = 0.5) +  
  theme(axis.text.x = element_text(angle = 45, hjust = 1))
# plot for Outlet_Size 
p6 = ggplot(combi %>% group_by(Outlet_Size) %>% summarise(Count = n())) +   
  geom_bar(aes(Outlet_Size, Count), stat = "identity", fill = "coral1") +  
  geom_label(aes(Outlet_Size, Count, label = Count), vjust = 0.5) +  
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

second_row = plot_grid(p5, p6, nrow = 1) 
plot_grid(p4, second_row, ncol = 1)


#Let's check the remaining categorical variables.
# plot for Outlet_Establishment_Year 
p7 = ggplot(combi %>% group_by(Outlet_Establishment_Year) %>% summarise(Count = n())) +   
  geom_bar(aes(factor(Outlet_Establishment_Year), Count), stat = "identity", fill = "coral1") +  
  geom_label(aes(factor(Outlet_Establishment_Year), Count, label = Count), vjust = 0.5) +  
  xlab("Outlet_Establishment_Year") +  
  theme(axis.text.x = element_text(size = 8.5))
# plot for Outlet_Type 
p8 = ggplot(combi %>% group_by(Outlet_Type) %>% summarise(Count = n())) +   
  geom_bar(aes(Outlet_Type, Count), stat = "identity", fill = "coral1") +  
  geom_label(aes(factor(Outlet_Type), Count, label = Count), vjust = 0.5) +  
  theme(axis.text.x = element_text(size = 8.5))
# ploting both plots together 
plot_grid(p7, p8, ncol = 2)


#Discovering hidden relationships between independent variables and the target variable.
#Scatter plots for continuous/numeric variables and violin plots for categorical variables.
train = combi[1:nrow(train)] # extracting train data from the combined data

#Let's explore the numerical variables first.
# Item_Weight vs Item_Outlet_Sales 
p9 = ggplot(train) +      
  geom_point(aes(Item_Weight, Item_Outlet_Sales), colour = "violet", alpha = 0.3) +     
  theme(axis.title = element_text(size = 8.5))
# Item_Visibility vs Item_Outlet_Sales 
p10 = ggplot(train) +       
  geom_point(aes(Item_Visibility, Item_Outlet_Sales), colour = "violet", alpha = 0.3) +      
  theme(axis.title = element_text(size = 8.5))
# Item_MRP vs Item_Outlet_Sales 
p11 = ggplot(train) +       
  geom_point(aes(Item_MRP, Item_Outlet_Sales), colour = "violet", alpha = 0.3) +      
  theme(axis.title = element_text(size = 8.5))

second_row_2 = plot_grid(p10, p11, ncol = 2) 
plot_grid(p9, second_row_2, nrow = 2)


#checking the distribution of target variable across all the categories of each of the categorical variable.
# Item_Type vs Item_Outlet_Sales 
p12 = ggplot(train) +       
  geom_violin(aes(Item_Type, Item_Outlet_Sales), fill = "magenta") +      
  theme(axis.text.x = element_text(angle = 45, hjust = 1),            
        axis.text = element_text(size = 6),            
        axis.title = element_text(size = 8.5))
# Item_Fat_Content vs Item_Outlet_Sales 
p13 = ggplot(train) +       
  geom_violin(aes(Item_Fat_Content, Item_Outlet_Sales), fill = "magenta") +      
  theme(axis.text.x = element_text(angle = 45, hjust = 1),            
        axis.text = element_text(size = 8),            
        axis.title = element_text(size = 8.5))
# Outlet_Identifier vs Item_Outlet_Sales 
p14 = ggplot(train) +       
  geom_violin(aes(Outlet_Identifier, Item_Outlet_Sales), fill = "magenta") +      
  theme(axis.text.x = element_text(angle = 45, hjust = 1),            
        axis.text = element_text(size = 8),            
        axis.title = element_text(size = 8.5))
second_row_3 = plot_grid(p13, p14, ncol = 2) 
plot_grid(p12, second_row_3, ncol = 1)


#We saw some empty values in Outlet_Size variable. Let's check the distribution of the target variable w.r.t it.
ggplot(train) + 
  geom_violin(aes(Outlet_Size, Item_Outlet_Sales), fill = "magenta")

#Let's check the remaining variables.
p15 = ggplot(train) + 
  geom_violin(aes(Outlet_Location_Type, Item_Outlet_Sales), fill = "magenta") 
p16 = ggplot(train) + 
  geom_violin(aes(Outlet_Type, Item_Outlet_Sales), fill = "magenta") 

plot_grid(p15, p16, ncol = 1)


#Let's find missing values.
sum(is.na(combi$Item_Weight))

#we have missing values in Item_Weight and Item_Outlet_Sales. 
#Missing data in Item_Outlet_Sales can be ignored since they belong to the test dataset. 
#We'll now impute Item_Weight with mean weight based on the Item_Identifier variable.
missing_index = which(is.na(combi$Item_Weight)) 
for(i in missing_index){    
  item = combi$Item_Identifier[i]  
  combi$Item_Weight[i] = mean(combi$Item_Weight[combi$Item_Identifier == item], na.rm = T) }

#Let's check if there is still any missing data in Item_Weight.
sum(is.na(combi$Item_Weight))


#Zeroes in Item_Visibility variable can be replaced with Item_Identifier wise mean values of Item_Visibility.
ggplot(combi) + 
  geom_histogram(aes(Item_Visibility), bins = 100)

#Let's replace the zeroes.
zero_index = which(combi$Item_Visibility == 0) 
for(i in zero_index){    
  item = combi$Item_Identifier[i]  
  combi$Item_Visibility[i] = mean(combi$Item_Visibility[combi$Item_Identifier == item], na.rm = T)  }

#We'll plot the histogram of Item_Visibility again.
ggplot(combi) + 
  geom_histogram(aes(Item_Visibility), bins = 100)


#We look at the Item_Type variable and classify the categories into perishable and non_perishable making it into new features.
perishable = c("Breads", "Breakfast", "Dairy", "Fruits and Vegetables", "Meat", "Seafood")
non_perishable = c("Baking Goods", "Canned", "Frozen Foods", "Hard Drinks", "Health and Hygiene", "Household", "Soft Drinks")

# create a new feature 'Item_Type_new' 
combi[,Item_Type_new := ifelse(Item_Type %in% perishable, "perishable", ifelse(Item_Type %in% non_perishable, "non_perishable", "not_sure"))]

#Let's compare Item_Type with first 2 characters of Item_Identifier, i.e., 'DR', 'FD', and 'NC'(Drinks, Food and non-consumables).
table(combi$Item_Type, substr(combi$Item_Identifier, 1, 2))

#We create a new feature:Item_category.
combi[,Item_category := substr(combi$Item_Identifier, 1, 2)]

#We will also change values of Item_Fat_Content wherever Item_category is 'NC' because non-consumable items cannot have any fat content. 
#We will also create a couple more features - Outlet_Years and price_per_unit_wt.
combi$Item_Fat_Content[combi$Item_category == "NC"] = "Non-Edible" 

# years of operation for outlets 
combi[,Outlet_Years := 2013 - Outlet_Establishment_Year] 
combi$Outlet_Establishment_Year = as.factor(combi$Outlet_Establishment_Year) 

# Price per unit weight 
combi[,price_per_unit_wt := Item_MRP/Item_Weight]

#we saw Item_MRP was spread across in 4 chunks. 
#Now let's assign a label to each of these and use this label as a new variable.
# creating new independent variable - Item_MRP_clusters 
combi[,Item_MRP_clusters := ifelse(Item_MRP < 69, "1st",
       ifelse(Item_MRP >= 69 & Item_MRP < 136, "2nd",                                          
       ifelse(Item_MRP >= 136 & Item_MRP < 203, "3rd", "4th")))]


#Label encoding Outlet_Size and Outlet_Location_Type as these are ordinal variables.
combi[,Outlet_Size_num := ifelse(Outlet_Size == "Small", 0,                                 
       ifelse(Outlet_Size == "Medium", 1, 2))] 

combi[,Outlet_Location_Type_num := ifelse(Outlet_Location_Type == "Tier 3", 0,                                          
                                   ifelse(Outlet_Location_Type == "Tier 2", 1, 2))] 

# removing categorical variables after label encoding 
combi[, c("Outlet_Size", "Outlet_Location_Type") := NULL]

#One hot encoding for categorical variables.
ohe = dummyVars("~.", data = combi[,-c("Item_Identifier", "Outlet_Establishment_Year", "Item_Type")], fullRank = T) 
ohe_df = data.table(predict(ohe, combi[,-c("Item_Identifier", "Outlet_Establishment_Year", "Item_Type")])) 
combi = cbind(combi[,"Item_Identifier"], ohe_df)


#In our data, variables Item_Visibility and price_per_unit_wt are highly skewed. 
#So, we will treat their skewness with the help of log transformation.
combi[,Item_Visibility := log(Item_Visibility + 1)] # log + 1 to avoid division by zero 
combi[,price_per_unit_wt := log(price_per_unit_wt + 1)]

#Scaling and centering the numeric variables to make them having mean=zero, standard deviation=one and scale=(0,1).
num_vars = which(sapply(combi, is.numeric)) # index of numeric features 
num_vars_names = names(num_vars) 
combi_numeric = combi[,setdiff(num_vars_names, "Item_Outlet_Sales"), with = F] 
prep_num = preProcess(combi_numeric, method=c("center", "scale")) 
combi_numeric_norm = predict(prep_num, combi_numeric)
combi[,setdiff(num_vars_names, "Item_Outlet_Sales") := NULL] # removing numeric independent variables 
combi = cbind(combi, combi_numeric_norm)

#Splitting the combined data combi back to train and test set.
train = combi[1:nrow(train)] 
test = combi[(nrow(train) + 1):nrow(combi)] 
test[,Item_Outlet_Sales := NULL] # removing Item_Outlet_Sales as it contains only NA for test dataset

#Let's examine the correlated features of train dataset.
#It is not desirable to have correlated features if we are using linear regression.
cor_train = cor(train[,-c("Item_Identifier")]) 
corrplot(cor_train, method = "pie", type = "lower", tl.cex = 0.9)


#Building model.
linear_reg_mod = lm(Item_Outlet_Sales ~ ., data = train[,-c("Item_Identifier")])

#Prediction 
pred = predict(linear_reg_mod, test[,-c("Item_Identifier")]) 
pred


#We will build a RandomForest model with 400 trees. 
#The other tuning parameters are mtry - no. of predictor variables randomly sampled at each split, and 
#min.node.size - minimum size of terminal nodes.
#Setting this number large causes smaller trees and reduces overfitting.
set.seed(1237) 
my_control = trainControl(method="cv", number=5) # 5-fold CV 
tgrid = expand.grid(
  .mtry = c(3:10),
  .splitrule = "variance",
  .min.node.size = c(10,15,20))

rf_mod = train(x = train[, -c("Item_Identifier", "Item_Outlet_Sales")],
               y = train$Item_Outlet_Sales,
               method='ranger',
               trControl= my_control,
               tuneGrid = tgrid,
               num.trees = 400,
               importance = "permutation")

plot(rf_mod)

#Let's plot feature importance based on the RandomForest model.
plot(varImp(rf_mod))


#XGBoost
param_list = list(objective = "reg:linear",eta=0.01,gamma = 1,max_depth=6,subsample=0.8,colsample_bytree=0.5)
dtrain = xgb.DMatrix(data = as.matrix(train[,-c("Item_Identifier", "Item_Outlet_Sales")]), label= train$Item_Outlet_Sales) 
dtest = xgb.DMatrix(data = as.matrix(test[,-c("Item_Identifier")]))

#We will use cross validation for finding the optimal value of nrounds.
set.seed(112) 
xgbcv = xgb.cv(params = param_list,data = dtrain,nrounds = 1000,nfold = 5,print_every_n = 10,early_stopping_rounds = 30,maximize = F)

#As per the verbose above, we got the best validation/test score at the 429th iteration. 
#Hence, we will use nrounds = 429 for building the XGBoost model.
xgb_model = xgb.train(data = dtrain, params = param_list, nrounds = 429)

#Variable importance
var_imp = xgb.importance(feature_names = setdiff(names(train), c("Item_Identifier", "Item_Outlet_Sales")),model = xgb_model) 
xgb.plot.importance(var_imp)