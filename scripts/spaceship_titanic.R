


# https://www.r-bloggers.com/2023/01/imputation-in-r-top-3-ways-for-imputing-missing-data/#:~:text=We%E2%80%99ll%20now%20explore%20a%20suite%20of%20basic%20techniques%20for%20imputation
# https://www.r-bloggers.com/2016/04/missing-value-treatment/#:~:text=Prediction%20is%20most%20advanced%20method%20to%20impute%20your%20missing%20values
# https://www.r-bloggers.com/2015/10/imputing-missing-data-with-r-mice-package/



# basic tree to get idea of predictors
tr <- d %>%
  filter(dat == "train")

mdl_part <- rpart(as.factor(Transported) ~ . -dat -PassengerId -Name
                  -Cabin,
                  data = tr)
mdl_part
rpart.plot(mdl_part)
fancyRpartPlot(mdl_part)


# -----------------------------------------------------------------------------
# univ eda
# -----------------------------------------------------------------------------

#$ CryoSleep   : chr  "False" "False" "False" "False" ...
d %>%
  filter(dat == "train") %>%
  group_by(CryoSleep) %>%
  summarise(n = n()) %>%
  mutate(p = n/sum(n))

#$ VIP         : chr  "False" "False" "True" "False" ...
d %>%
  filter(dat == "train") %>%
  group_by(VIP) %>%
  summarise(n = n()) %>%
  mutate(p = n/sum(n))

(t <- table(d$VIP, d$Transported))
(p <- prop.table(t, 1))

#$ Age         : num  39 24 58 33 16 44 26 28 35 14 ...

#$ RoomService : num  0 109 43 0 303 0 42 0 0 0 ...
#$ FoodCourt   : num  0 9 3576 1283 70 ...
#$ ShoppingMall: num  0 25 0 371 151 0 3 0 17 0 ...
#$ Spa         : num  0 549 6715 3329 565 ...
#$ VRDeck      : num  0 44 49 193 2 0 0 0 0 0 ...



#$ PassengerId : chr  "0001_01" "0002_01" "0003_01" "0003_02" ...
#$ HomePlanet  : chr  "Europa" "Earth" "Europa" "Europa" ...
#$ Cabin       : chr  "B/0/P" "F/0/S" "A/0/S" "A/0/S" ...
#$ Destination : chr  "TRAPPIST-1e" "TRAPPIST-1e" "TRAPPIST-1e" "TRAPPIST-1e" ...
#$ Name        : chr  "Maham Ofracculy" "Juanna Vines" "Altark Susent" "Solam Susent" ...

#$ Transported : chr  "False" "True" "False" "False" ...


# hacer func que da vars con NA
# add ISNA flag
# add flag for missing values
  # read each var y ver que genero
  # total $, passenger id get
  # cabin get

# caret rf
# caret boost
# caret svm
# caret knn
# caret mn




 


# -----------------------------------------------------------------------------
# sandbox to delete
# -----------------------------------------------------------------------------


x <- d[,c("Age", "RoomService")]

d2 <- impute.knn(as.maxtrix(x))

m <- as.matrix(x)


d2 <- impute(m,
             lmFun = "lassoR",
             cFun = "rpartC")


train[is.na(train$Age),]$Age <- 27
train[is.na(train$RoomService),]$RoomService <- 222
train[is.na(train$FoodCourt),]$FoodCourt <- 452




# Print column names with their column numbers
for (i in seq_along(colnames(d))) {
  cat(i, colnames(d)[i], "\n")
}

featurePlot(x = train[,c(6,8,9)], 
            y = as.factor(train$Transported), 
            plot = "pairs",
            auto.key = list(columns = 1)
            )

            
featurePlot(x = train[,c(6,8,9)], 
            y = train$Transported, 
            plot = "box", 
            ## Pass in options to bwplot() 
            scales = list(y = list(relation="free"),
                          x = list(rot = 90)
            ))
            
            #layout = c(3,1))
            
            auto.key = list(columns = 2))


featurePlot(x = iris[, 1:4], 
            y = iris$Species, 
            plot = "pairs",
            ## Add a key at the top
            auto.key = list(columns = 3))

featurePlot(x = iris[, 1:4], 
            y = iris$Species, 
            plot = "box", 
            ## Pass in options to bwplot() 
            scales = list(y = list(relation="free"),
                          x = list(rot = 90)),  
            layout = c(4,1 ), 
            auto.key = list(columns = 2))

train2 <- train %>%
  filter(RoomService > 0) %>%
  filter(RoomService < 1000)

featurePlot(x = train2[, 8:9], 
            y = as.factor(train2$Transported), 
            plot = "box", 
            ## Pass in options to bwplot() 
            scales = list(y = list(relation="free"),
                          x = list(rot = 90)),  
            layout = c(2,1 ), 
            auto.key = list(columns = 2))




