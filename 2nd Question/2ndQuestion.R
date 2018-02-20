setwd("F:/ML/competitions/scg 17/fraud detection/")
train=read.csv("train.csv")
E=train
test=read.csv("test.csv")
F=test

na=vector(length = ncol(data))
for(i in 1:ncol(data))
{
  na[i]=length(which(as.character(data[,i])==""))
}
print(na)
#print(colnames(E))

un=vector(length = ncol(E))
for(i in 1:ncol(E))
{
  un[i]=length(unique(E[,i]))
}
print(un)

'No na values in the data'

id = train[,1]
train = train[,-1]
test_id = as.character(test[,1])
test = test[,-1]
cl = as.numeric(train[,ncol(train)])
train = train[,-ncol(train)]

nr=nrow(train)
data=rbind(train,test)

for(i in 1:7)
{
  data[,i] = data[,i]/max(data[,i])
}

for(i in 8:ncol(data))
{
  print(i)
  if(i == 13)
  {
    w = which(as.character(data[,i])=="")
    if(length(w) > 0)
    {
      u = unique(data[,i])
      u_count = vector(length = length(u))
      print(length(u))
      for(j in 1:length(u_count))
      {
        u_count[j] = which(data[,i] == u[j])
      }
      print(u[which(u_count == max(u_count))])
      data[w,i] = u[which(u_count == max(u_count))]
    }
  }
}
data = data[,-13]

for(i in 1:7)
{
  data[,1] = as.numeric(data[,1])
}

for (i in 8:ncol(data))
{
  data[,i] = as.factor(data[,i])
}

new_train = data[1:nr,]
new_test = data[(nr+1):nrow(data),]

write.csv(new_train ,'new_train.csv',row.names = FALSE)
write.csv(new_test ,'new_test.csv',row.names = FALSE)
write.csv(cl, 'train_class.csv',row.names = FALSE)

library(xgboost)

data=data.matrix(data)
feat_mat=data[1:nr,]
feat_mat_test=data[(nr+1):nrow(data),]

dtrain <- xgb.DMatrix(as.matrix(feat_mat),label = cl)
dtest <- xgb.DMatrix(as.matrix(feat_mat_test))


xgb_params = list(
  seed = 0,
  colsample_bytree = 0.7,
  subsample = 0.7,
  eta = 0.075,
  objective = 'reg:linear',
  max_depth = 6,
  num_parallel_tree = 1,
  min_child_weight = 1,
  base_score = 7,
  gamma = 2
)

res = xgb.cv(xgb_params,
             dtrain,
             nrounds=200,
             nfold=4,
             early_stopping_rounds=25,
             print_every_n = 10,
             verbose= 1,
             eval_metric='auc'
             )

best_nrounds = res$best_iteration

gbdt = xgb.train(xgb_params, dtrain, best_nrounds)

pred=predict(gbdt,dtest)
head(pred)

m=matrix(ncol = 2,nrow = nrow(test))
colnames(m)=c('transaction_id','target')
m[,1]=test_id
m[,2]=pred

write.csv(m,"na_filled_depth6_119_gamma0.csv",row.names = FALSE)
