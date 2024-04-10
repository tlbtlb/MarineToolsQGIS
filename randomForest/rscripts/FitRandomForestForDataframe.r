# FitRandomForestForDataframe.r
#
# Copyright (C) 2012 Jason J. Roberts
#
# This program is free software; you can redistribute it and/or
# modify it under the terms of the GNU General Public License
# as published by the Free Software Foundation; either version 2
# of the License, or (at your option) any later version.
#
# This program is distributed in the hope that it will be useful,
# but WITHOUT ANY WARRANTY; without even the implied warranty of
# MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
# GNU General Public License (available in the file LICENSE.TXT)
# for more details.
#
# You should have received a copy of the GNU General Public License
# along with this program; if not, write to the Free Software
# Foundation, Inc., 51 Franklin Street, Fifth Floor, Boston, MA  02110-1301, USA.

FitRandomForestForDataframe <- function(f, trainingData, ntree, mtry, rPackage, outputModelFile, replace=FALSE, cfMaxSurrogate=NULL, seed=NULL, importance=TRUE,
                                        useScaledImportance=FALSE, useConditionalImportance=FALSE, xVar=NULL, yVar=NULL, zVar=NULL, mVar=NULL, coordinateSystem=NULL, 
                                        writeSummaryFile=TRUE, writeImportancePlot=TRUE, writePartialDependencePlots=TRUE, plotFileFormat="png", res=1000.0, width=3000.0,
                                        height=3000.0, pointSize=10.0, bg="white")
{

    library(stats)
    library(rPackage, character.only=TRUE)
    
    digits <- options()$digits
    options(digits=3)
    
    tryCatch({
        timeCancelLastChecked <- CheckForCancel()
        
        # If the model formula includes factors, we must explicitly specify
        # their levels. If we do not, prediction will fail with "Type of
        # predictors in new data do not match that of the training data." (for
        # the randomForest package) or "Levels in factors of new data do not
        # match original data" (for the party package) when the new data do not
        # have all of the levels represented, as often happens when predicting
        # rasters one line at a time.

        modelMetadataTemp <- GetModelMetadata(f, rPackage, trainingData, NA, NA, NA, NA, NA)
        for (term in c(modelMetadataTemp$predictorExprs, modelMetadataTemp$offsetExprs))
            if (term %in% names(modelMetadataTemp$factorLevels))
                f <- formula(paste(f[2], "~", gsub(term, sprintf("factor(%s, levels=c(%s))", term, paste(sapply(modelMetadataTemp$factorLevels[[term]], function(s) sprintf('"%s"',s)), sep="", collapse=",")), f[3], fixed=TRUE)))
        # If the caller specified a seed, initialize the random number generator.
        
        if (!is.null(seed))
            set.seed(seed)

        # Fit the model using the package the caller requested.

        message(sprintf("Fitting the random forest using the %s package.", rPackage))
        
        if (rPackage == "randomForest")
        {
            #print(paste("Rows ",nrow(trainingData)))
            #print(paste("Cols ",ncol(trainingData)))
            trainingData <- na.omit(trainingData)
            #for (variable in names(trainingData))
            #{
            #    print(paste(variable," ",class(trainingData[[variable]])))
            #}
            #print(paste("Rows ",nrow(trainingData)))
            #print(paste("Cols ",ncol(trainingData)))
            ntrees = as.integer(ntree)
            attach(trainingData)                    # Must use this in order for expressions like factor(x) in the formula to work
            tryCatch({
                if (!is.null(mtry))
                    #model <- randomForest(f, ntree=ntree, mtry=mtry, replace=replace, importance=importance)
                    values <- eval(parse(text=sprintf("model <- randomForest(%s, ntree=ntrees, mtry=mtry, replace=replace, importance=importance)", f)))$y
               else
                    #model <- randomForest(Presence2 ~ AgeMap_Ma_ + KEnergy_ma + Productivi, ntree=ntrees, replace=replace, importance=importance)
                    values <- eval(parse(text=sprintf("model <- randomForest(%s, ntree=ntrees, replace=replace, importance=importance)", f)))$y
                    # Note: This eval(parse(text="...")) stuff is necessary due to a limitation of the randomForest function (classic problem related to how R does lazy evaluation)
            }, finally=detach(trainingData))
        }
        else
        {
            if (is.null(mtry))      # cforest_unbiased already defaults to 5, but we hardcode it here so when we print mtry later, we get 5 rather than NULL
                mtry <- 5
            if (!is.null(cfMaxSurrogate))
                model <- cforest(f, data=trainingData, controls=cforest_unbiased(ntree=ntree, mtry=mtry, maxsurrogate=cfMaxSurrogate))
            else
                model <- cforest(f, data=na.omit(trainingData), controls=cforest_unbiased(ntree=ntree, mtry=mtry))
        }
        timeCancelLastChecked <- CheckForCancel(timeCancelLastChecked)
        # Calculate variable importance, if requested by the caller.
        
        if (importance)
        {
            message("Estimating predictor variable importance.")
            if (rPackage == "randomForest")
            {
                imp <- importance(model, scale=useScaledImportance)
                imp <- imp[order(imp[,ncol(imp)-1], decreasing=TRUE),]
            }
            else
            {
                imp <- varimp(model, conditional=useConditionalImportance)
                imp <- as.matrix(imp[order(imp, decreasing=TRUE)])
                colnames(imp) <- c("Importance")
            }
        }
        
        timeCancelLastChecked <- CheckForCancel(timeCancelLastChecked)
                
        # Generate a summary of the model, including variable importance if requested.
        
        modelSummary <- paste("\n",
                              "MODEL SUMMARY:\n",
                              "==============\n",
                              paste(capture.output(print(model)), sep="", collapse="\n"), "\n",
                              sep="")
                              
        if (!is.null(seed))
            modelSummary <- paste(modelSummary,
                                  sprintf("Random number generator seed: %i\n", seed),
                                  sep="")
                              
        if (rPackage == "party")
            modelSummary <- paste(modelSummary,
                                  sprintf("Number of variables tried at each split: %i\n", mtry),
                                  "\n",
                                  sep="")
    
        if (importance)
            if (rPackage == "randomForest")
                modelSummary <- paste(modelSummary,
                                      "\n",
                                      "Estimated predictor variable importance:\n",
                                      "\n",
                                      paste(capture.output(print(imp)), sep="", collapse="\n"), "\n",
                                      sep="")
            else
                modelSummary <- paste(modelSummary,
                                      sprintf("Estimated predictor variable importance (conditional = %s):\n", useConditionalImportance),
                                      "\n",
                                      paste(capture.output(print(imp)), sep="", collapse="\n"), "\n",
                                      sep="")
                                  
        # Replace the randomForest "Call" portion of the summary with the formula, etc.
        
        if (rPackage == "randomForest")
        {
            modelSummary <- sub("formula = f, ", paste("formula = ", paste(as.character(f[2]), as.character(f[1]), as.character(f[3])), ",\n              ", sep=""), modelSummary)
            modelSummary <- sub(" +ntree = ntree", sprintf(" ntree = %i", ntrees), modelSummary)
            modelSummary <- sub(" +replace = replace", sprintf(" replace = %s", replace), modelSummary)
            modelSummary <- sub(" +importance = importance)", sprintf(" importance = %s)\n", importance), modelSummary)
        }
    
        # Print the summary.
        writeLines(strsplit(modelSummary, "\n")[[1]])
        message("")
    
        # Write the output file.
        modelMetadata <- GetModelMetadata(f, rPackage, trainingData, xVar, yVar, zVar, mVar, coordinateSystem, model)
        save(model, trainingData, modelMetadata, file=outputModelFile, compress=FALSE)
        
        timeCancelLastChecked <- CheckForCancel(timeCancelLastChecked)
    
        # Strip the extension, if one exists, off the output file path.
        
        outputFilePrefix <- file.path(dirname(outputModelFile), strsplit(basename(outputModelFile), ".", fixed=TRUE)[[1]])[1]
    
        # Write the summary file, if requested.
        
        if (writeSummaryFile)
        {
            f <- file(sprintf("%s_summary.txt", outputFilePrefix), "wt")
            tryCatch(writeLines(modelSummary, f), finally=close(f))
        }

        # Write the importance plot, if requested.
        
        if (rPackage == "randomForest" && importance && writeImportancePlot)
        {
            message("Plotting predictor variable importance.")

            if (model$type == "regression")
                title = "Variable Importance"
            else
                title = "Variable Importance for All Classes"
            
            if (plotFileFormat == "emf")
                win.metafile(sprintf("%s_importance.emf", outputFilePrefix), width=width*2, height=height*2, pointsize=pointSize)
            else
                png(sprintf("%s_importance.png", outputFilePrefix), res=res, width=width*2, height=height*2, pointsize=pointSize, bg=bg)

            varImpPlot(model, type=1, scale=useScaledImportance, main=title)
            graphics.off()
            timeCancelLastChecked <- CheckForCancel(timeCancelLastChecked)
            
            # If this is a classification model, create plots for each class.
            
            if (model$type == "classification")
                for (i in 1:length(model$classes))
                {
                    c = model$classes[i]
            
                    if (plotFileFormat == "emf")
                        win.metafile(sprintf("%s_importance_class_%s.emf", outputFilePrefix, c), width=width*2, height=height*2, pointsize=pointSize)
                    else
                        png(sprintf("%s_importance_class_%s.png", outputFilePrefix, c), res=res, width=width*2, height=height*2, pointsize=pointSize, bg=bg)
        
                    varImpPlot(model, class=c, type=1, scale=useScaledImportance, main=sprintf("Variable Importance for Class %s", c))
                    graphics.off()
                    timeCancelLastChecked <- CheckForCancel(timeCancelLastChecked)
                }
        }
        
        # Write the partial dependence plots, if requested.
        writePartialDependencePlots = FALSE # TLB 28/11/23 Not working so skipping plotting
        if (rPackage == "randomForest" && writePartialDependencePlots)
        {
            # First determine the minimum and maximum value of the y axis among all plots.
            
            message("Determining y-axis range for partial dependence plots (this can take a long time).")
            
            minY = NA
            maxY = NA

            for (i in 1:length(modelMetadata$predictorVars))
            {
                term <- modelMetadata$predictorVars[i]
                #print(paste("term Firsttime = ",term," length ",length(modelMetadata$predictorVars)))
                
                if (model$type == "regression")
                {
                    values <- eval(parse(text=sprintf("partialPlot(model, na.omit(trainingData), %s, plot=FALSE)", term)))$y          # Note: This eval(parse(text="...")) stuff is necessary due to a limitation of the partialPlot function (classic problem related to how R does lazy evaluation)
                    #partialPlot(model, na.omit(trainingData), term,  plot=FALSE)
                    #print(paste("First min Y = ",minY," maxY = ",maxY))
                    minY <- min(na.omit(c(minY, values)))
                    maxY <- max(na.omit(c(maxY, values)))
                }
                else
                    for (j in 1:length(model$classes))
                    {
                        values <- eval(parse(text=sprintf("partialPlot(model, na.omit(trainingData), %s, which.class=model$classes[j], plot=FALSE)", term)))$y      # See above for explanation of eval(parse(...))
                        minY <- min(na.omit(c(minY, values)))
                        maxY <- max(na.omit(c(maxY, values)))
                    }
                timeCancelLastChecked <- CheckForCancel(timeCancelLastChecked)
            }
            
            # Now create the plots using that max y value.

            for (i in 1:length(modelMetadata$predictorVars))
            {
                term <- modelMetadata$predictorVars[i]
                # print(paste("term = ",term))
            
                message(sprintf("Plotting partial dependence of %s.", term))
                
                if (model$type == "regression")
                {
                    if (plotFileFormat == "emf")
                        win.metafile(sprintf("%s_pd_%s.emf", outputFilePrefix, term), width=width*2, height=height*2, pointsize=pointSize)
                    else
                        png(sprintf("%s_pd_%s.png", outputFilePrefix, term), res=res, width=width*2, height=height*2, pointsize=pointSize, bg=bg)
                    #print(paste("Second min Y = ",minY," maxY = ",maxY))
                    eval(parse(text=sprintf("partialPlot(model, na.omit(trainingData), %s, xlab=\"%s\", ylim=c(%f, %f))", term, term, minY, maxY)))     # See above for explanation of eval(parse(...))
                    graphics.off()
                }
                else
                    for (j in 1:length(model$classes))
                    {
                        if (plotFileFormat == "emf")
                            win.metafile(sprintf("%s_pd_class_%s_%s.emf", outputFilePrefix, model$classes[j], term), width=width*2, height=height*2, pointsize=pointSize)
                        else
                            png(sprintf("%s_pd_class_%s_%s.png", outputFilePrefix, model$classes[j], term), res=res, width=width*2, height=height*2, pointsize=pointSize, bg=bg)

                        eval(parse(text=sprintf("partialPlot(model, na.omit(trainingData), %s, which.class=model$classes[j], ylim=c(%f, %f))", term, minY, maxY)))     # See above for explanation of eval(parse(...))
                        graphics.off()
                    }
                timeCancelLastChecked <- CheckForCancel(timeCancelLastChecked)
            }
        }
        
    # Ensure that all graphics devices are turned off before we exit,
    # so that no files are left open.
    
    }, finally={
        options(digits=digits)
        graphics.off() 
    })

    # Return successfully.
    
    if (rPackage == "randomForest")
        return(list(model$classes, modelMetadata$predictorVars))
        
    return(list(numeric(), numeric()))
}
