library(tfruns)


runs <- tuning_run("Shallow_tuning.R", sample = 0.2, 
                   runs_dir = "shallow_tuning",
                   flags = list(
                     dropout1 = c(0.01, 0.1, 0.05),
                     optimizer= c('rmsprop', 'adam'),
                     hidden1=c(10,20,30,40,50),
                     batch=c(1000,5000,10000),
                     act=c("relu"),
                     epochs=10)
                   )
