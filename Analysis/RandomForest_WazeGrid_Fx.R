# Random Forest functions for Waze
# Adapted from SDI_Waze repository
require(pROC)
######################################################################
# do.rf function for running random forest models in parallel ----

# Arguments:
# train.dat - Data frame containing all the predictors and the response variable

# omits - Vector of column names in the data to omit from the model. Used to generate the model formula, if the formula is not otherwise provided in the `formula` argument.

# response.var - Vector of the data to use as the response variable, e.g. count for a given alert_type

# model.no - character value to keep track of model number used; cannot be left blank

# formula - If provided, use this formula instead of the automatically generated one for the random forest model

# test.dat - If provided, this will be used to test the random forest model. If not provided, a split of the training data will be used according to the test.split argument. Both test.dat and test.split cannot be provided. 

# pred.dat - If provided, generates predicted values for a new set of data (i.e., 2020 Waze counts for covid_waze project)

# test.split - if test.dat is not specified, use this to randomly split the training data into two portions by row, with this split value being used as the test proportion of the data.

# rf.inputs - list of arguments to pass to randomForest

# thin.dat - value from 0 to 1 for proportion of the training and test data to use in fitting the model. E.g. thin.dat = 0.2, use only 20% of the training and test data; useful for testing new features. 

# s3_save - flag for saving to an S3 bucket. Set to FALSE to run on local machines outside of SDC

do.rf <- function(train.dat, omits, response.var = "count", model.no,
                  test.dat = NULL, 
                  pred.dat = NULL,
                  test.split = .30,
                  split.by = NULL,
                  thin.dat = NULL,
                  cutoff = c(0.8, 0.2),
                  rf.inputs = list(ntree.use = 500, avail.cores = 4, mtry = NULL, maxnodes = NULL, nodesize = 5),
                  s3_save = FALSE){
  
  if(!is.null(test.dat) & !missing(test.split)) stop("Specify either test.dat or test.split, but not both")

  class(train.dat) <- "data.frame"

  fitvars <- names(train.dat)[is.na(match(names(train.dat), omits))]
  
  # Remove any rows with NA in predictors or response. NA in count means no Waze data are expected
  cc <- complete.cases(train.dat[,fitvars])
  train.dat <- train.dat[cc,]
  
  # Remove response.var from fitvars
  fitvars <- fitvars[-grep(response.var, fitvars)]
  
  # Provide mtry if null
  if(is.null(rf.inputs$mtry)){
    mtry.use = if (!is.factor(response.var)) max(floor(length(fitvars)/3), 1) else floor(sqrt(length(fitvars)))
  } else {mtry.use = rf.inputs$mtry}
  
  # Thin data sets if thin.dat provided
  if(!is.null(thin.dat)) {
    train.dat <- train.dat[sample(1:nrow(train.dat), size = nrow(train.dat)*thin.dat),]
    if(!is.null(test.dat)){
      test.dat <- test.dat[sample(1:nrow(test.dat), size = nrow(test.dat)*thin.dat),]
    }
  }
  
  # 70:30 split or Separate training and test data
  # Adding options to split by day or by week. Assumes column hextime is available and is a character vector in POSIX format
  if(is.null(test.dat)){
    if(is.null(split.by)){
      trainrows <- sort(sample(1:nrow(train.dat), size = nrow(train.dat)*(1-test.split), replace = F))
    } # end if is.null split.by else
    
    testrows <- (1:nrow(train.dat))[!1:nrow(train.dat) %in% trainrows]
    rundat = train.dat[trainrows,]
    test.dat.use = train.dat[testrows,]
  } else {
    class(test.dat) <- "data.frame"
    rundat = train.dat
    
    # Remove any rows with NA in predictors from test.dat
    cc <- complete.cases(test.dat[,fitvars])
    test.dat <- test.dat[cc,]
    
    test.dat.use = test.dat
    comb.dat <- rbind(train.dat, test.dat)
  }
  
  
  # Start RF in parallel
  starttime = Sys.time()
  
  # make a cluster of all available cores
  cl <- makeCluster(rf.inputs$avail.cores, useXDR = F) 
  registerDoParallel(cl)
  
  rf.out <- foreach(ntree = rep(rf.inputs$ntree.use/rf.inputs$avail.cores, rf.inputs$avail.cores),
                  .combine = randomForest::combine, .multicombine=T, .packages = 'randomForest') %dopar% 
    randomForest(x = rundat[,fitvars], y = rundat[,response.var], 
                 ntree = ntree, mtry = mtry.use, 
                 maxnodes = rf.inputs$maxnodes, nodesize = rf.inputs$nodesize,
                 keep.forest = T)
  
  stopCluster(cl); rm(cl); gc(verbose = F) # Stop the cluster immediately after finished the RF
  
  timediff = Sys.time() - starttime
  cat(round(timediff,2), attr(timediff, "unit"), "to fit model", model.no, "\n")
  # End RF in parallel

  # Begin if factor response variable
  # ! Note: this is left over from SDI Waze. Not updated for covid_waze since we are using numeric response
  if(class(rundat[,response.var])=="factor"){
    rf.pred <- predict(rf.out, test.dat.use[fitvars], cutoff = cutoff)
    rf.prob <- predict(rf.out, test.dat.use[fitvars], type = "prob", cutoff = cutoff)

  predtab <- table(test.dat.use[,response.var], rf.pred)
  
  reference.vec <- test.dat.use[,response.var]
  levels(reference.vec) = c("NoCrash", "Crash")
  levels(rf.pred) = c("NoCrash","Crash")
  
  reference.vec <-as.factor(as.character(reference.vec))
  rf.pred <- as.factor(as.character(rf.pred))
  
  (predtab <- table(rf.pred, reference.vec, 
                    dnn = c("Predicted","Observed"))) 
  bin.mod.diagnostics(predtab)
  
  # pROC::roc - response, predictor
  model_auc <- pROC::auc(test.dat.use[,response.var], rf.prob[,colnames(rf.prob)=="1"])

  pdf(file = paste0("AUC_", model.no, ".pdf"), width = 6, height = 6)
  plot(pROC::roc(test.dat.use[,response.var], rf.prob[,colnames(rf.prob)=="1"]),
       main = paste0("Model ", model.no),
       grid=c(0.1, 0.2),
       ylim = c(0, 1), xlim = c(1, 0))
  legend("bottomright", legend = round(model_auc, 4), title = "AUC", inset = 0.25)
  
  # dev.print(device = jpeg, file = paste0("AUC_", model.no, ".jpg"), width = 500, height = 500)
  dev.off()

  out.df <- data.frame(test.dat.use[, c("GRID_ID", "Year", "day", "hour", response.var)], rf.pred, rf.prob)
  out.df$day <- as.numeric(out.df$day)
  names(out.df)[5:8] <- c("Obs", "Pred", "Prob.Noncrash", "Prob.Crash")
  out.df = data.frame(out.df,
                      TN = out.df$Obs == 0 &  out.df$Pred == "NoCrash",
                      FP = out.df$Obs == 0 &  out.df$Pred == "Crash",
                      FN = out.df$Obs == 1 &  out.df$Pred == "NoCrash",
                      TP = out.df$Obs == 1 &  out.df$Pred == "Crash")
  
  
  
  } # end if factor response variable
  
  # Begin if continuous response variable  
  if(class(rundat[,response.var])=="numeric"){
    
    # use test.dat.use for predictions if no pred.dat provided
    if(is.null(pred.dat)){
      pred.dat = test.dat.use
    }
    rf.pred <- predict(rf.out, pred.dat[fitvars])

    out.df <- data.frame(pred.dat, pred_count = rf.pred)
    
  } # end if continuous response variable
  
  write.csv(out.df,
            file = file.path(output.loc, paste(model.no, "RandomForest_pred.csv", sep = "_")),
            row.names = F)
  
  savelist = c("rf.out", "rf.pred", "out.df") 
  if(is.null(test.dat)) savelist = c(savelist, "testrows", "trainrows")
  if(!is.null(thin.dat)) savelist = c(savelist, "test.dat.use")
  
  fn = paste(model.no, "RandomForest_Output.RData", sep= "_")
  
  save(list = savelist, file = file.path(output.loc, fn))
  
  # Copy to S3
  if(s3_save){
    system(paste("aws s3 cp",
                 file.path(output.loc, fn),
                 file.path(teambucket, state, "RandomForest_Output", fn)))
  }
  
  # Output is list of three elements: Nobs data frame, predtab table, binary model diagnotics table, and mean squared error
  if(class(rundat[,response.var])=="factor"){
  outlist =  list(Nobs, predtab, diag = bin.mod.diagnostics(predtab), 
         mse = mean(as.numeric(as.character(test.dat.use[,response.var])) - 
                      as.numeric(rf.prob[,"1"]))^2,
         runtime = timediff,
         auc = as.numeric(model_auc) # do not save complete output
    ) 
  }    
  if(class(rundat[,response.var])=="numeric"){
  outlist =  list(Nobs_fit = nrow(rundat), Nobs_test = nrow(test.dat.use), 
         mse = mean(as.numeric(as.character(test.dat.use[,response.var])) - 
                      as.numeric(rf.prob))^2,
         runtime = timediff
    )
  }
  outlist
} # end do.rf function



reassess.rf <- function(train.dat, omits, response.var = "MatchEDT_buffer_Acc", model.no,
                        test.dat = NULL,
                        rf.inputs = list(ntree.use = 500, avail.cores = 4, mtry = NULL, maxnodes = NULL, nodesize = 5),
                        cutoff = c(0.8, 0.2)){

  class(train.dat) <- "data.frame"

  # Load fitted model
  cat("Loading", model.no, "\n")
  s3load(object = file.path(outputdir, paste("Model", model.no, "RandomForest_Output.RData", sep= "_")),
         bucket = waze.bucket)  
  
  fitvars <- names(train.dat)[is.na(match(names(train.dat), omits))]
  
  # 70:30 split or Separate training and test data
  if(is.null(test.dat)){
    rundat = train.dat[trainrows,]
    test.dat.use = train.dat[testrows,]
  } else {
    class(test.dat) <- "data.frame"
    rundat = train.dat
    test.dat.use = test.dat
    comb.dat <- rbind(train.dat, test.dat)
  }
  
  Nobs <- data.frame(nrow(rundat),
                     sum(as.numeric(as.character(rundat[,response.var])) == 0),
                     sum(as.numeric(as.character(rundat[,response.var])) > 0),
                     length(rundat$nWazeAccident[train.dat$nWazeAccident>0]) )
  
  colnames(Nobs) = c("N", "No EDT", "EDT present", "Waze accident present")
  
  # Begin if factor response variable  
  if(class(rundat[,response.var])=="factor"){
    rf.pred <- predict(rf.out, test.dat.use[fitvars], cutoff = cutoff)
    rf.prob <- predict(rf.out, test.dat.use[fitvars], type = "prob", cutoff = cutoff)
    
    predtab <- table(test.dat.use[,response.var], rf.pred)
    
    reference.vec <- test.dat.use[,response.var]
    levels(reference.vec) = c("NoCrash", "Crash")
    levels(rf.pred) = c("NoCrash","Crash")
    
    reference.vec <-as.factor(as.character(reference.vec))
    rf.pred <-as.factor(as.character(rf.pred))
    
    (predtab <- table(rf.pred, reference.vec, 
                      dnn = c("Predicted","Observed"))) 
    bin.mod.diagnostics(predtab)
    
    # pROC::roc - response, predictor
    model_auc <- pROC::auc(test.dat.use[,response.var], rf.prob[,colnames(rf.prob)=="1"])
    
    pdf(file = paste0("AUC_", model.no, ".pdf"), width = 6, height = 6)
    plot(pROC::roc(test.dat.use[,response.var], rf.prob[,colnames(rf.prob)=="1"]),
         main = paste0("Model ", model.no),
         grid=c(0.1, 0.2),
         ylim = c(0, 1), xlim = c(1, 0))
    legend("bottomright", legend = round(model_auc, 4), title = "AUC", inset = 0.25)
    
    #dev.print(device = jpeg, file = paste0("AUC_", model.no, ".jpg"), width = 500, height = 500)
    dev.off()
    
    out.df <- data.frame(test.dat.use[, c("GRID_ID", "day", "hour", response.var)], rf.pred, rf.prob)
    out.df$day <- as.numeric(out.df$day)
    names(out.df)[4:7] <- c("Obs", "Pred", "Prob.Noncrash", "Prob.Crash")
    out.df = data.frame(out.df,
                        TN = out.df$Obs == 0 &  out.df$Pred == "NoCrash",
                        FP = out.df$Obs == 0 &  out.df$Pred == "Crash",
                        FN = out.df$Obs == 1 &  out.df$Pred == "NoCrash",
                        TP = out.df$Obs == 1 &  out.df$Pred == "Crash")
    
    } # end if factor response variable
  
  # Begin if continuous response variable  
  if(class(rundat[,response.var])=="numeric"){
    rf.prob <- predict(rf.out, test.dat.use[fitvars])
    
    rf.pred <- cut(rf.prob, breaks = c(-100, cutoff[2], 100), include.lowest = T, labels = c("NoCrash","Crash"))
    
    out.df <- data.frame(test.dat.use[, c("GRID_ID", "day", "hour", response.var)], rf.pred, rf.prob)
    out.df$day <- as.numeric(out.df$day)
    names(out.df)[4:6] <- c("Obs", "Pred", "Prob.Crash")
    out.df = data.frame(out.df,
                        TN = out.df$Obs == 0 &  out.df$Pred == "NoCrash",
                        FP = out.df$Obs == 0 &  out.df$Pred == "Crash",
                        FN = out.df$Obs == 1 &  out.df$Pred == "NoCrash",
                        TP = out.df$Obs == 1 &  out.df$Pred == "Crash")
    
    
  } # end if continuous response variable
  
  write.csv(out.df,
            file = paste(model.no, "RandomForest_pred.csv", sep = "_"),
            row.names = F)
  
  savelist = c("rf.out", "rf.pred", "rf.prob", "out.df") 
  if(is.null(test.dat)) savelist = c(savelist, "testrows", "trainrows")

  s3save(list = savelist,
         object = file.path(outputdir, paste("Model", model.no, "RandomForest_Output.RData", sep= "_")),
         bucket = waze.bucket)
  
  # Output is list of three elements: Nobs data frame, predtab table, binary model diagnotics table, and mean squared error
  if(class(rundat[,response.var])=="factor"){
    outlist =  list(Nobs, predtab, diag = bin.mod.diagnostics(predtab), 
                    mse = mean(as.numeric(as.character(test.dat.use[,response.var])) - 
                                 as.numeric(rf.prob[,"1"]))^2,
                    auc = as.numeric(model_auc) # do not save complete output
    ) 
  }    
  if(class(rundat[,response.var])=="numeric"){
    outlist =  list(Nobs, 
                    mse = mean(as.numeric(as.character(test.dat.use[,response.var])) - 
                                 as.numeric(rf.prob))^2
                    )
  }
  outlist
} # end reassss.rf function

