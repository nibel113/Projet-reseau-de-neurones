library(tfruns)


runs <- tuning_run("Deep_4hidden_tuning.R", sample = 0.01, 
                   runs_dir = "Deep4_tuning",
                   flags = list(
                     dropout1 = c(0.01, 0.1, 0.05),
                     #dropout2 = c(0.01, 0.1, 0.05),
                     #dropout3 = c(0.01, 0.1, 0.05),
                     #dropout4 = c(0.01, 0.1, 0.05),
                     optimizer= c('rmsprop'),
                     hidden1=c(10,30,50),
                     hidden2=c(10,30,50),
                     hidden3=c(10,30,50),
                     hidden4=c(10,30,50),
                     batch=c(5000,10000),
                     act=c("relu"),
                     epochs=500,
                     lr_annealing=c(0.1,0.05)))
