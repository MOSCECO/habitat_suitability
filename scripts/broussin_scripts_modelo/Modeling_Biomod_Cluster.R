library(biomod2)
library(rpart)

setwd("/mnt/beegfs/egoberville")

modele = c("R", "CH")
pa = c(5000, 10000, 30000, 50000)

for (i in 1:2){
  
  for (j in 1:4) {
    mat.mod <- read.csv(paste0("Mat_mod_G_morhua_",pa[j],"_",modele[i],".csv"))
    
    
    pa.tab <- data.frame(PA1 = c(rep(T,163176), rep(T,pa[j]), rep(F,(nrow(mat.mod)-pa[j]-163176))),
                         PA2 = c(rep(T,163176), rep(F,pa[j]), rep(T,pa[j]), rep(F,nrow(mat.mod)-pa[j]*2-163176)),
                         PA3 = c(rep(T,163176), rep(F,pa[j]*2), rep(T,pa[j]),rep(F,nrow(mat.mod)-pa[j]*3-163176)),
                         PA4 = c(rep(T,163176), rep(F,pa[j]*3), rep(T,nrow(mat.mod)-pa[j]*3-163176)))
    
    myRespName = paste0("Gadus.morhua_", pa[j], modele[i])
    resp.data <- c(rep(1,163176), rep(NA, nrow(mat.mod) - 163176))
    
    
    bmData <- BIOMOD_FormatingData(resp.var= resp.data,
                                   expl.var= mat.mod[,2:6],
                                   resp.xy = mat.mod[,1:2], 
                                   resp.name = myRespName,
                                   PA.strategy = "user.defined",
                                   PA.table = pa.tab)
    
    # ===========================================================================================================
    # 2. Defining Models Options using default options.
    myBiomodOption <- BIOMOD_ModelingOptions(RF=list(ntree=1000))
    
    # ===========================================================================================================
    # 3. Computing the models
    mySDMModel <- BIOMOD_Modeling(data=bmData,
                                  models = c('RF'), # c('GLM', 'GBM', 'GAM', 'CTA', 'ANN', 'SRE', 'FDA', 'MARS', 'RF') 
                                  models.options = myBiomodOption,
                                  NbRunEval = 5, # Number of Evaluation run
                                  do.full.models = FALSE, # if true, models calibrated and evaluated with the whole dataset are done
                                  DataSplit = 80, # % of data used to calibrate the models, the remaining part will be used for testing
                                  VarImport = 3,# Number of permutation to estimate variable importance
                                  models.eval.meth = c('KAPPA','TSS','ROC'), # names of evaluation metrics
                                  SaveObj = TRUE, # keep all results and outputs on hard drive or not 
                                  modeling.id = paste(myRespName,"FirstModeling",sep=""))
    
    # Models evaluations
    Eval <- as.data.frame(get_evaluations(mySDMModel))
    Eval <- as.data.frame(t(Eval)) #Invertir lignes et colonnes
    write.csv(Eval, paste0("Eval_",pa[j],"_",modele[i],".csv"))
    
    # Relative importance of the explanatory variables
    Variables <- as.data.frame(get_variables_importance(mySDMModel))
    write.csv(Variables, paste0("Poids_des_Vars_",pa[j],"_",modele[i],".csv"))
    
  }
}

