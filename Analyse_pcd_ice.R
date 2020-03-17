#Analyse pcd et ice shallow
library("iml")
source("best_shallow.R")
view(ls_runs(runs_dir = "shallow_tuning_poisdeviance"))
view_run(run_dir = "shallow_tuning_poisdeviance/2020-03-17T16-17-51Z"	)

predictor <- Predictor$new(model=model,data=XtestNN,y=YtestNN)

imp <- FeatureImp$new(predictor,loss = "mse")

plot(imp)
