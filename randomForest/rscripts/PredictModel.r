# PredictLMForArcGISRasters.r
#
# Copyright (C) 2008 Jason J. Roberts and Ben D. Best
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

ckappa <- function (r) 
{
    r <- na.omit(r)
    r1 <- r[, 1]
    r2 <- r[, 2]
    n1 <- as.character(r1)
    n2 <- as.character(r2)
    lev <- levels(as.factor(c(n1, n2)))
    p <- length(lev)
    tab <- matrix(nrow = p, ncol = p)
    dimnames(tab) <- list(levels(as.factor(c(n1, n2))), levels(as.factor(c(n1, 
        n2))))
    dim1 <- dimnames(tab)[[1]]
    dim2 <- dimnames(tab)[[2]]
    tabi <- table(n1, n2)
    dimi1 <- dimnames(tabi)[[1]]
    dimi2 <- dimnames(tabi)[[2]]
    for (i in 1:p) for (j in 1:p) {
        if ((sum(dim1[i] == dimi1) == 1) & (sum(dim2[j] == dimi2) == 
            1)) 
            tab[i, j] <- tabi[dim1[i], dim2[j]]
        else tab[i, j] <- 0
    }
    tsum <- sum(tab)
    ttab <- tab/tsum
    tm1 <- apply(ttab, 1, sum)
    tm2 <- apply(ttab, 2, sum)
    agreeP <- sum(diag(ttab))
    chanceP <- sum(tm1 * tm2)
    kappa2 <- (agreeP - chanceP)/(1 - chanceP)
    result <- list(table = tab, kappa = kappa2)
    result
}


PredictModel <- function(model, modelMetadata, cutoff, ignoreOutOfRangeValues, returnPredictedErrors, predictorValues, warnedAboutMissingLevels, allValuesForPredictorAreNA, allValuesAreNA)
{
    # If the caller requested that we ignore out of range values, iterate
    # through the model terms and evaluate each one against the predictorValues.
    # Then, for all results that are out of range, set the predictors that
    # produced them to NA. For continuous terms (data class "numeric"), "out of
    # range" means the value of the term obtained from the predictorValues is 
    # less than the minimum or greater than the maximum of the value of the term
    # obtained from the training data. For categorical terms (data class
    # "factor" or "logical"), "out of range" means the value of the term (which
    # is a factor level) obtained from the predictorValues is not one of those
    # (factor levels) obtained from the training data.
    #
    # Also determine which records have NA for one or more prediction terms.
    
    anyAreNA <- logical(nrow(predictorValues))             # For each record, TRUE if any term is NA
    allAreNA <- logical(nrow(predictorValues))             # For each record, TRUE if all terms are NA
    
    termExpressions <- c(modelMetadata$predictorExprs, modelMetadata$offsetExprs)
    termDataClasses <- c(modelMetadata$predictorDataClasses, modelMetadata$offsetDataClasses)
    
    if (length(termExpressions) > 0)
    {
        allAreNA <- allAreNA | TRUE

        for (i in 1:length(termExpressions))
        {
            termDataClass <- termDataClasses[i]
            termExpression <- termExpressions[i]
            parsedTermExpression <- parse(text=termExpression)
            result <- eval(parsedTermExpression, predictorValues)
            
            if (termDataClass %in% c("numeric", "nmatrix.1"))       # nmatrix.1 is the class of object returned by the lo() smoother in the gam package (not the mgcv package) when just one variable is smoothed 
            {
                if (ignoreOutOfRangeValues)
                    for (predictor in all.vars(parsedTermExpression))
                        predictorValues[[predictor]][result < modelMetadata$minNumericValues[[termExpression]] | result > modelMetadata$maxNumericValues[[termExpression]]] <- NA
            }

            else if (termDataClass == "nmatrix.2")
            {
                # nmatrix.2 is the class of object returned by the lo() smoother in
                # the gam package (not the mgcv package) when two variables are
                # smoothed. In this case, do nothing. We do not fully support lo()
                # with more than one variable but will do the best we can to allow
                # predictions.
            }

            else if (termDataClass == "factor")
            {
                isMissingLevel <- !(levels(result) %in% modelMetadata$factorLevels[[termExpression]])
                for (predictor in all.vars(parsedTermExpression))
                    predictorValues[[predictor]][isMissingLevel] <- NA
                if (any(isMissingLevel) && !(termExpression %in% names(warnedAboutMissingLevels)))
                {
                    warning(paste("In the data used for this prediction, there are values for the categorical model term", termExpression, "that were not present in the data used to fit the model. No predictions can be made for these values."), call.=FALSE)
                    warnedAboutMissingLevels[[termExpression]] <- TRUE
                }
            }

            else if (termDataClass == "logical")
            {
                isMissingLevel <- !(result %in% modelMetadata$logicalValues[[termExpression]])
                for (predictor in all.vars(parsedTermExpression))
                    predictorValues[[predictor]][isMissingLevel] <- NA
                if (any(isMissingLevel) && !(termExpression %in% names(warnedAboutMissingLevels)))
                {
                    warning(paste("In the data used for this prediction, there are values for the categorical model term", termExpression, "that were not present in the data used to fit the model. No predictions can be made for these values."), call.=FALSE)
                    warnedAboutMissingLevels[[termExpression]] <- TRUE
                }
            }

            else
                stop(sprintf("The model formula term \"%s\" has an unsupported data class \"%s\". Please contact the MGET development team for assistance.", termExpression, termDataClass), call.=FALSE)

            # Determine which records have NA for this term.

            if (termDataClass != "nmatrix.2")
                isNA <- is.na(result)
            else
                isNA <- apply(is.na(result), 1, any)

            # Keep track of whether all of the values for this term are NA for
            # the entire prediction, which may span multiple calls to us.

            allValuesForPredictorAreNA[[termExpression]] <- allValuesForPredictorAreNA[[termExpression]] && all(isNA)

            # For each record, we need to keep track of whether any or all of the
            # terms are NA. Incorporate this term into that calculation.

            anyAreNA <- anyAreNA | isNA
            allAreNA <- allAreNA & isNA
        }
    }
    
    # Do the prediction. The prediction code varies according to the kind of
    # model that was fitted. First handle rpart models.
    
    predictedError <- NULL

    if (!is.null(modelMetadata$rPackage) && modelMetadata$rPackage == "rpart")
    {
        # Do the prediction. For "class" models (i.e. classification trees) that
        # have a binary response: predict the probability of the second class;
        # if a cutoff was provided, apply it and return the original class
        # levels, otherwise return the probability.

        if (model$method == "class")
        {
            if (length(modelMetadata$factorLevels[[modelMetadata$responseExpr]]) == 2)
            {
                predictedResponse <- as.vector(suppressWarnings(predict(model, newdata=predictorValues, type="prob")[,2]))
                if (!is.null(cutoff))
                    predictedResponse <- modelMetadata$factorLevels[[modelMetadata$responseExpr]][as.integer(predictedResponse >= cutoff) + 1]
            }
            else
                predictedResponse <- as.vector(suppressWarnings(predict(model, newdata=predictorValues, type="class")))
        }
        
        # Otherwise (it is a regression model), predict the response values.
        
        else
        {
            predictedResponse <- as.vector(suppressWarnings(predict(model, newdata=predictorValues, type="vector")))

            # For Poisson models, rpart predicts a rate. If an offset was
            # specified when the model was fitted, convert the rate back to a
            # count, for consistency with GLM and GAM.
            
            if (tolower(model$method) == "poisson" && length(modelMetadata$offsetVars) == 1)
                predictedResponse <- predictedResponse * predictorValues[modelMetadata$offsetVars[1]][[1]]
        }

        # The rpart documentation implies that rpart can do predictions if
        # *some* of the predictors are NA but not if *all* of the predictors are
        # NA. We therefore expect that rows that have NA for all predictors
        # would result in a predicted value of NA. But rpart does not do that.
        #
        # So: Set all rows having NA for all predictors to NA.

        predictedResponse[allAreNA] <- NA

        # If the caller requested that out of range values be ignored, set rows
        # that had any NAs to NA.
        
        if (ignoreOutOfRangeValues)
            predictedResponse[anyAreNA] <- NA
    }
    
    # Handle random forest models fitted with randomForest or party.
    
    else if (!is.null(modelMetadata$rPackage) && modelMetadata$rPackage %in% c("randomForest", "party"))
    {
        # For binary classification forests, get the predicted probability of
        # the second class. If a cutoff was provided, apply it and return the
        # original class levels, otherwise return the probability.
        
        if (modelMetadata$isBinaryClassification)
        {
            if (modelMetadata$rPackage == "randomForest")
                predictedResponse <- as.vector(suppressWarnings(predict(model, newdata=predictorValues, type="prob")[,2]))
            else    # modelMetadata$rPackage == "party"
                predictedResponse <- as.vector(sapply(suppressWarnings(predict(model, newdata=predictorValues, type="prob")), "[", i = 2))

            if (!is.null(cutoff))
                predictedResponse <- modelMetadata$factorLevels[[modelMetadata$responseExpr]][as.integer(predictedResponse >= cutoff) + 1]
        }

        # For other models (regression forests or classification forests with
        # more than two classes), just get the predicted response.

        else
            predictedResponse <- as.vector(suppressWarnings(predict(model, newdata=predictorValues, type="response")))

        # If the caller requested that out of range values be ignored, set rows
        # that had any NAs to NA.
        
        if (ignoreOutOfRangeValues)
            predictedResponse[anyAreNA] <- NA
    }

    # Handle GLM and GAM.

    else
    {
        # The predict.gam function from the R gam library will fail if we pass
        # in NA for covariates that were fitted with the loess smoother (lo). To
        # work around this, set the predictors to dummy values if they are NA.
        # For the dummy values, use the minimum values of the predictors used to
        # fit the model. (If we pass in values outside the range of values used
        # to fit the model, loess will report a warning on a per-record basis,
        # drowning the user in warnings.) These dummy predictor values will
        # result in bogus predictions of the response variable. Later, we will
        # convert these back to NA.
        
        for (variable in names(model$data))
            if (variable != modelMetadata$responseVar)
                if (is.factor(model$data[[variable]]))
                    predictorValues[[variable]][anyAreNA] <- modelMetadata$factorLevels[[variable]][1]
                else if (is.logical(model$data[[variable]]))
                    predictorValues[[variable]][anyAreNA] <- modelMetadata$logicalValues[[variable]][1]
                else
                    predictorValues[[variable]][anyAreNA] <- modelMetadata$minNumericValues[[variable]]
            
        # Do the prediction.

        predictions <- suppressWarnings(predict(model, newdata=predictorValues, type="response", se.fit=returnPredictedErrors))

        # If the caller requested predicted errors, extract both the predicted
        # response and the errors. Otherwise just extract the response.

        if (returnPredictedErrors)
        {
            predictedResponse <- as.vector(predictions$fit)
            predictedError <- as.vector(predictions$se.fit)
        }
        else
            predictedResponse <- as.vector(predictions)
            
        # Set rows that had NA for one or more predictor variables back to NA.
        # This will overwrite bogus predictions that resulted from dummy values.
	
        predictedResponse[anyAreNA] <- NA
        if (returnPredictedErrors)
            predictedError[anyAreNA] <- NA             

        # If the caller provided a cutoff, classify the response.

        if (!is.null(cutoff))
            if (modelMetadata$responseDataClass == "factor")
                predictedResponse <- modelMetadata$factorLevels[[modelMetadata$responseExpr]][as.integer(predictedResponse >= cutoff) + 1]
            else if (modelMetadata$responseDataClass == "integer")
                predictedResponse <- as.integer(predictedResponse >= cutoff)
            else
                predictedResponse <- as.numeric(as.integer(predictedResponse >= cutoff))
    }

    # Keep track of whether the predicted values were NA for the entire
    # prediction, which may span multiple calls to us.

    allValuesAreNA <- allValuesAreNA && all(is.na(predictedResponse))
    
    # Return the predicted response and error.
    
    return(list(predictedResponse, predictedError, warnedAboutMissingLevels, allValuesForPredictorAreNA, allValuesAreNA))
}


PrintPredictionWarnings <- function(rPackage, allPredictors, allValuesForPredictorAreNA, allValuesAreNA)
{
    # For non-rpart models, warn the user about any predictors were NA for all
    # records or cells.

    if (is.null(rPackage) || rPackage != "rpart")
    {
        naPredictors <- character()
        for (predictor in allPredictors)
            if (allValuesForPredictorAreNA[[predictor]])
                naPredictors <- append(naPredictors, predictor, length(naPredictors))

        if (length(naPredictors) > 0)
            if (length(naPredictors) == 1)
                warning(paste("All of the values of the term", naPredictors[1], "were missing (represented by NA in the R programming language). This usually happens when predictor values are missing (they are NULL or NoData) or not within the range used to fit the model. A response can only be predicted when none of the terms are missing. As a result, all of the predicted values will be set to NULL (if you are predicting values from a table) or NoData (if you are predicting values from rasters)."), call.=FALSE)
            else
                warning(paste("All of the values of the following terms missing (NA in the R programming language): ", paste(naPredictors, collapse=", "), ". This usually happens when predictor values are missing (they are NULL or NoData) or not within the range used to fit the model. A response can only be predicted when none of the terms are missing. As a result, all of the predicted values will be set to NULL (if you are predicting values from a table) or NoData (if you are predicting values from rasters).", sep=""), call.=FALSE)

        # Warn the user if no predictions could be performed (even though the
        # predictors each had data for at least one record or cell).

        else if (allValuesAreNA)
            warning("For each prediction that was done, the value of at least one term was missing (represented by NA in the R programming language). Because of this, NULL (if you are predicting values from a table) or NoData (if you are predicting values from rasters) will be stored for all predictions. This usually happens when predictor values are missing (they are NULL or NoData) or not within the range used to fit the model. If this result is unexpected, check the predictor values you provided as input and make sure they are correct.", call.=FALSE)
    }

    # For rpart models, only warn the user if all predicted values were NA.

    else if (allValuesAreNA)
        warning("All of the predictions resulted in R \"NA\" values and will be stored as NULL (if you are predicting values from a table) or NoData (if you are predicting values from rasters). This usually happens when predictor values are missing (they are NULL or NoData) or not within the range used to fit the model. If this result is unexpected, check the predictor values you provided as input and make sure they are correct.", call.=FALSE)
}


PredictModelForArcGISRasters <- function(model, modelMetadata, rastersForPredictors, constantsForPredictors, outputResponseFile, outputErrorFile=NULL, ignoreOutOfRangeValues=FALSE, cutoff=NULL)
{
    library(stats)
    
    # Verify that the caller provided all of the necessary predictors. Also
    # determine which predictors must be coerced to factor levels or logical
    # values from whatever was read from the rasters or provided as constant
    # values. (Rasters cannot store strings, so we provide a workaround that
    # allows integers to be used instead.)

    if (is.null(rastersForPredictors) || length(rastersForPredictors) <= 0)
        stop("You must specify at least one predictor raster.", call.=FALSE)
        
    integerFactorPredictors <- character(0)
    nonIntegerFactorPredictors <- character(0)
    logicalPredictors <- character(0)
    
    for (predictor in c(modelMetadata$predictorVars, modelMetadata$offsetVars))
    {
        if (!(predictor %in% labels(rastersForPredictors)) && (!is.null(constantsForPredictors) && !(predictor %in% labels(constantsForPredictors))))
            stop(sprintf("The model included a predictor named \"%s\" but you did not specify a raster or constant value for it. Please do that and try again.", predictor), call.=FALSE)
            
        if (predictor %in% names(modelMetadata$factorLevels))
            if (length(grep("[^0123456789]", modelMetadata$factorLevels[[predictor]])) == 0)
                integerFactorPredictors <- c(integerFactorPredictors, predictor)
            else
            {
                nonIntegerFactorPredictors <- c(nonIntegerFactorPredictors, predictor)
                if (predictor %in% labels(rastersForPredictors))
                    warning(sprintf("In this model, %s is a categorical variable with character string values but a raster was provided for the prediction. Rasters cannot store character strings. It will be assumed that that the raster contains integer codes corresponding to the categorical values, as follows: %s",
                                    predictor,
                                    paste(sprintf("%i: %s", 1:length(modelMetadata$factorLevels[[predictor]]), modelMetadata$factorLevels[[predictor]]), sep="", collapse=", ")), call.=FALSE)
            }
            
        else if (predictor %in% names(modelMetadata$logicalValues))
            logicalPredictors <- c(logicalPredictors, predictor)
    }
    
    # Load the rgdal library, which we use to read the predictor rasters.

    library(rgdal)
    
    # Obtain the characteristics of the predictor rasters. For safety, verify
    # that they all have the same dimensions. The Python function that generated
    # them should guarantee this, but it is cheap to check them here.
        
    rasterInfo <- open.SpatialGDAL(rastersForPredictors[1], silent=TRUE)
    tryCatch(
    {
        if (length(rastersForPredictors) > 1)
            for (i in 2:length(rastersForPredictors))
            {
                rasterInfo2 <- open.SpatialGDAL(rastersForPredictors[i], silent=TRUE)
                tryCatch(
                {
                    if (any(rasterInfo2@grid@cells.dim != rasterInfo@grid@cells.dim))
                        stop("All of the predictor rasters must have the same number of rows and columns.", call.=FALSE)
                }, finally=close(rasterInfo2))
            }

        rasterCols <- as.integer(round(rasterInfo@grid@cells.dim[1]))
        rasterRows <- as.integer(round(rasterInfo@grid@cells.dim[2]))
        rasterXLLCorner <- rasterInfo@bbox["x","min"]
        rasterYLLCorner <- rasterInfo@bbox["y","min"]
        rasterCellSize <- rasterInfo@grid@cellsize[1]
    }, finally=close(rasterInfo))
    
    # Determine the data type and NoData value of the output rasters.
    # For classification models, use the most compact integer data type
    # and corresponding NoData value. Otherwise (for regression models),
    # 32-bit floating point as the data type and
    # -3.4028235e+038 as the NoData value (the 32-bit IEEE-754
    # floating-point negative number that is farthest from 0;
    # traditionally used by ArcGIS for NoData).

    if (modelMetadata$isBinaryClassification && !is.null(cutoff) || modelMetadata$isNonBinaryClassification)
    {
        # If the response is a factor and we can convert all of the levels to
        # integers, then we will use them.
        
        if (modelMetadata$responseDataClass == "factor")
        {
            convertLevelsToIntegers <- FALSE

            if (length(grep("[^0123456789]", modelMetadata$factorLevels[[modelMetadata$responseExpr]])) == 0)
            {
                convertLevelsToIntegers <- TRUE
                integerLevels <- as.integer(modelMetadata$factorLevels[[modelMetadata$responseExpr]])
                minResponseValue <- min(integerLevels)
                maxResponseValue <- max(integerLevels)
            }
            
            # We cannot convert all of the factor levels to integers. If it is a
            # binary model, we will just use values 0 and 1.
            
            else if (modelMetadata$isBinaryClassification)
            {
                minResponseValue <- as.integer(0)
                maxResponseValue <- as.integer(1)
                warning(sprintf("In this model, the response is a categorical variable with character string values. Rasters cannot store character strings. The output raster will receive integer codes corresponding to the categorical values, as follows: %s",
                                paste(sprintf("%i: %s", 0:1, modelMetadata$factorLevels[[modelMetadata$responseExpr]]), sep="", collapse=", ")), call.=FALSE)
            }
            
            # If it is not a binary model (it has more than two classes), use
            # 1 ... number of levels.
            
            else
            {
                minResponseValue <- as.integer(1)
                maxResponseValue <- as.integer(length(modelMetadata$factorLevels[[modelMetadata$responseExpr]]))
                warning(sprintf("In this model, the response is a categorical variable with character string values. Rasters cannot store character strings. The output raster will receive integer codes corresponding to the categorical values, as follows: %s",
                                paste(sprintf("%i: %s", 1:length(modelMetadata$factorLevels[[modelMetadata$responseExpr]]), modelMetadata$factorLevels[[modelMetadata$responseExpr]]), sep="", collapse=", ")), call.=FALSE)
            }
        }
        
        # Otherwise (the response is not a factor), the only way we can be here
        # if it is a binary classification.

        else if (modelMetadata$responseDataClass %in% c("logical", "numeric"))
        {
            if (!modelMetadata$isBinaryClassification)
                stop("Programming error in this tool. modelMetadata$responseDataClass is either logical or numeric, but this is not a binary classification. Please contact the MGET development team for assistance..", call.=FALSE)
                
            minResponseValue <- as.integer(0)
            maxResponseValue <- as.integer(1)
        }

        else
            stop(sprintf("Programming error in this tool. The response variable of this model uses an unsupported data class. Please contact the MGET development team for assistance."), call.=FALSE)
            
        # Now determine the data type and noDataValue from the minimum and
        # maximum values.

        if (minResponseValue > -128 && maxResponseValue <= 127)
        {
            dataType <- "signedint"
            nbytes <- 1
            noDataValue <- as.integer(-128)
        }
        else if (minResponseValue >= -128 && maxResponseValue < 127)
        {
            dataType <- "signedint"
            nbytes <- 1
            noDataValue <- as.integer(127)
        }
        else if (minResponseValue > 0 && maxResponseValue <= 255)
        {
            dataType <- "unsignedint"
            nbytes <- 1
            noDataValue <- as.integer(0)
        }
        else if (minResponseValue >= 0 && maxResponseValue < 255)
        {
            dataType <- "unsignedint"
            nbytes <- 1
            noDataValue <- as.integer(255)
        }
        else if (minResponseValue > -32768 && maxResponseValue <= 32767)
        {
            dataType <- "signedint"
            nbytes <- 2
            noDataValue <- as.integer(-32768)
        }
        else if (minResponseValue >= -32768 && maxResponseValue < 32767)
        {
            dataType <- "signedint"
            nbytes <- 2
            noDataValue <- as.integer(32767)
        }
        else if (minResponseValue > 0 && maxResponseValue <= 65535)
        {
            dataType <- "unsignedint"
            nbytes <- 2
            noDataValue <- as.integer(0)
        }
        else if (minResponseValue >= 0 && maxResponseValue < 65535)
        {
            dataType <- "unsignedint"
            nbytes <- 2
            noDataValue <- as.integer(65535)
        }
        else if (minResponseValue > -2147483648 && maxResponseValue <= 2147483647)
        {
            dataType <- "signedint"
            nbytes <- 4
            noDataValue <- as.integer(-2147483648)
        }
        else if (minResponseValue >= -2147483648 && maxResponseValue < 2147483647)
        {
            dataType <- "signedint"
            nbytes <- 4
            noDataValue <- as.integer(2147483647)
        }
        else if (minResponseValue > 0)
        {
            dataType <- "unsignedint"
            nbytes <- 4
            noDataValue <- as.integer(0)
        }
        else
        {
            dataType <- "unsignedint"
            nbytes <- 4
            noDataValue <- as.integer(4294967296)
        }
    }
    else
    {
        if (modelMetadata$isBinaryClassification && is.null(cutoff))
            warning("This appears to be a binary classification model but no cutoff value was provided. The output will be a continuous floating-point value ranging from 0 to 1. To obtain a binary integer value, please provide a cutoff and try again.", call.=FALSE)

        dataType <- "float"
        nbytes <- 4
        noDataValue <- -3.4028235e+038
    }

    # Create the .hdr files that describe the binaries we will write.

    header <- file(paste(outputResponseFile, ".hdr", sep=""), "wb")
    tryCatch(
    {
        cat("ncols ", rasterCols, "\r\n", sep="", file=header)
        cat("nrows ", rasterRows, "\r\n", sep="", file=header)
        cat("cellsize ", formatC(rasterCellSize, format="g", digits=15, width=-1), "\r\n", sep="", file=header)
        cat("xllcorner ", formatC(rasterXLLCorner, format="g", digits=15, width=-1), "\r\n", sep="", file=header)
        cat("yllcorner ", formatC(rasterYLLCorner, format="g", digits=15, width=-1), "\r\n", sep="", file=header)
        if (dataType == "float")
            cat("nodata_value ", formatC(noDataValue, format="g", digits=8, width=-1), "\r\n", sep="", file=header)
        else
            cat("nodata_value ", noDataValue, "\r\n", sep="", file=header)
        cat("nbits ", as.integer(nbytes * 8), "\r\n", sep="", file=header)
        cat("pixeltype ", dataType, "\r\n", sep="", file=header)
        if (.Platform$endian == "little")
            cat("byteorder lsbfirst\r\n", file=header)
        else
            cat("byteorder msbfirst\r\n", file=header)
    }, finally=close(header))

    if (!is.null(outputErrorFile))
    {
        header <- file(paste(outputErrorFile, ".hdr", sep=""), "wb")
        tryCatch(
        {
            cat("ncols ", rasterCols, "\r\n", sep="", file=header)
            cat("nrows ", rasterRows, "\r\n", sep="", file=header)
            cat("cellsize ", formatC(rasterCellSize, format="g", digits=15, width=-1), "\r\n", sep="", file=header)
            cat("xllcorner ", formatC(rasterXLLCorner, format="g", digits=15, width=-1), "\r\n", sep="", file=header)
            cat("yllcorner ", formatC(rasterYLLCorner, format="g", digits=15, width=-1), "\r\n", sep="", file=header)
            cat("nodata_value ", formatC(noDataValue, format="g", digits=8, width=-1), "\r\n", sep="", file=header)
            cat("nbits 32\r\n", file=header)
            cat("pixeltype float\r\n", file=header)
            if (.Platform$endian == "little")
                cat("byteorder lsbfirst\r\n", file=header)
            else
                cat("byteorder msbfirst\r\n", file=header)
        }, finally=close(header))
    }

    # Open the output binaries.

    responseFile <- file(paste(outputResponseFile, ".bil", sep=""), "wb")
    tryCatch(
    {
        if (!is.null(outputErrorFile))
            errorFile <- file(paste(outputErrorFile, ".bil", sep=""), "wb")
        tryCatch(
        {
            # Open each predictor raster as a GDAL dataset.

            datasets <- character()
            tryCatch(
            {
                datasets <- list()
                for (predictor in labels(rastersForPredictors))
                    datasets[[predictor]] <- GDAL.open(rastersForPredictors[[predictor]])

                # keep track of various conditions in which we want to warn the
                # user.
                    
                warnedAboutMissingLevels <- list()

                allValuesForPredictorAreNA <- list()
                for (predictor in c(modelMetadata$predictorExprs, modelMetadata$offsetExprs))
                    allValuesForPredictorAreNA[[predictor]] <- TRUE

                allValuesAreNA <- TRUE

                # For each row of the predictor rasters, do a prediction and
                # write the results to the output files. We do this one row at a
                # time to minimize the amount of memory required to run the
                # tool. This is slower than reading everything into memory and
                # doing the prediction all in one shot (1.5x slower in my
                # limited tests) but it is well worth it, since doing it in one
                # shot can require too much memory.

                timeCancelLastChecked <- CheckForCancel()

                for (row in 1:rasterRows)
                {
                    timeCancelLastChecked <- CheckForCancel(timeCancelLastChecked)
                    
                    # If we have processed 1% of the rows, estimate
                    # the time required to process the rest. If it
                    # exceeds 1 minute, report a warning message to
                    # alert the user that he will have to wait a
                    # while.

                    if (row <= 2)
                        started = Sys.time()
                    else if (row == round(rasterRows / 100) + 1 && as.numeric((Sys.time() - started) * 99, units="mins") > 1)
                        warning(sprintf("This prediction is estimated to require %0.1f minutes to complete.", as.numeric((Sys.time() - started), units="mins") * 99), call.=FALSE)
                    
                    # Read this row from each predictor raster.

                    predictorValues <- list()
                    for (predictor in labels(rastersForPredictors))
                    {
                        predictorValues[[predictor]] <- getRasterData(datasets[[predictor]], offset=c(row-1,0), region.dim=c(1,rasterCols), as.is=TRUE)
                        
                        # If this predictor is a factor with integer levels,
                        # convert the values we read from the raster to
                        # character vectors.
                        
                        if (predictor %in% integerFactorPredictors)
                            predictorValues[[predictor]] <- as.character(as.integer(predictorValues[[predictor]]))
                            
                        # If this predictor is a factor with non-integer levels
                        # (i.e. character levels), use the values we read from
                        # the rasters as indexes into the factor levels.
                        
                        else if (predictor %in% nonIntegerFactorPredictors)
                        {
                            indices <- predictorValues[[predictor]]
                            indices[!(predictorValues[[predictor]] %in% 1:length(modelMetadata$factorLevels[[predictor]]))] <- NA
                            predictorValues[[predictor]] <- modelMetadata$factorLevels[[predictor]][indices]
                            predictorValues[[predictor]][predictorValues[[predictor]] %in% 1:length(modelMetadata$factorLevels[[predictor]])] <- "MGET_UNKNOWN_LEVEL"
                        }
                        
                        # If this predictor is a logical, convert the values we
                        # read from the raster to character vectors.
                        
                        else if (predictor %in% names(modelMetadata$logicalValues))
                            predictorValues[[predictor]] <- as.logical(predictorValues[[predictor]])
                    }
                        
                    # For predictors that have constant values, allocate vectors
                    # that repeat those values the appropriate number of times.
                    
                    if (!is.null(constantsForPredictors))
                        for (predictor in labels(constantsForPredictors))
                            predictorValues[[predictor]] <- rep(constantsForPredictors[[predictor]], length(predictorValues[[labels(rastersForPredictors)[1]]]))

                    # Do the prediction.

                    result <- PredictModel(model, modelMetadata, cutoff, ignoreOutOfRangeValues, !is.null(outputErrorFile), as.data.frame(predictorValues, stringsAsFactors=FALSE), warnedAboutMissingLevels, allValuesForPredictorAreNA, allValuesAreNA)

                    predictedResponse <- result[[1]]
                    predictedError <- result[[2]]
                    warnedAboutMissingLevels <- result[[3]]
                    allValuesForPredictorAreNA <- result[[4]]
                    allValuesAreNA <- result[[5]]
                    
                    # If this is a classification model, convert the response to
                    # integers. For response variables that were originally
                    # integers, convert to the original integers. For binary
                    # classifications, regardless of the data type of the
                    # response variable, convert to 0 or 1. For response
                    # variables that were originally strings, convert to factor
                    # level indices numbered 1 to length(levels()).
                    
                    if (modelMetadata$isBinaryClassification && !is.null(cutoff) || modelMetadata$isNonBinaryClassification)
                        if (modelMetadata$responseDataClass == "factor")
                            if (convertLevelsToIntegers)
                                predictedResponse <- as.integer(predictedResponse)          # If the response variable was originally integers, convert from character representations of those integers back to the actual integers
                            else if (modelMetadata$isBinaryClassification)
                                predictedResponse <- as.integer(match(predictedResponse, modelMetadata$factorLevels[[modelMetadata$responseExpr]]) - 1)         # For binary, we want 0 and 1 instead of 1 and 2, so must subtract 1
                            else
                                predictedResponse <- as.integer(match(predictedResponse, modelMetadata$factorLevels[[modelMetadata$responseExpr]]))             # For non-binary (i.e. three or more levels), we want 1..n
                        else
                            predictedResponse <- as.integer(predictedResponse)          # The only way to get here is for binary classifications of continuous responses (e.g. binomial GLM, GAM); convert 0..1 response to 0 or 1
                    
                    # Convert NAs to NoData values.
                    
                    predictedResponse[is.na(predictedResponse)] <- noDataValue
                    if (!is.null(outputErrorFile))
                        predictedError[is.na(predictedError)] <- noDataValue

                    # Write the row to the binary files.

                    writeBin(predictedResponse, responseFile, size=nbytes)
                    if (!is.null(outputErrorFile))
                        writeBin(predictedError, errorFile, size=4)
                }
            }, finally=for (ds in datasets) GDAL.close(ds))
        }, finally=if (!is.null(outputErrorFile)) close(errorFile))
    }, finally=close(responseFile))

    # Warn the user about any predictors were NoData for all cells, etc.
    
    PrintPredictionWarnings(modelMetadata$rPackage, c(modelMetadata$predictorExprs, modelMetadata$offsetExprs), allValuesForPredictorAreNA, allValuesAreNA)
}


PredictModelForDataframe <- function(model, modelMetadata, trainingData, newData=NULL, ignoreOutOfRangeValues=FALSE, cutoff=NULL, outputPlotFile=NULL, measure1="tpr", measure2="fpr", colorize=TRUE, outputSummaryFile=NULL, res=1000.0, width=3000.0, height=3000.0, pointSize=10.0, bg="white")
{
    library(stats)
    
    # Verify that the caller provided all of the necessary predictors.
    
    useTrainingData <- is.null(newData)
    
    if (useTrainingData)
        newData <- trainingData
    else
        for (predictor in c(modelMetadata$predictorVars, modelMetadata$offsetVars))
            {
            if (!(predictor %in% labels(newData)[[2]]))
                stop(sprintf("The model included a variable named \"%s\" but newData does not contain a column for it. Please add that column and try again.", predictor), call.=FALSE)
            }
    # Do the prediction. For binary models, force the probability to be returned
    # by passing NULL for the cutoff parameter to PredictModel(), regardless of
    # what the caller passed to us for cutoff. We will apply the cutoff ourself
    # later.

    warnedAboutMissingLevels <- list()

    allValuesForPredictorAreNA <- list()
    for (predictor in c(modelMetadata$predictorExprs, modelMetadata$offsetExprs))
    {
        #print(predictor)
        allValuesForPredictorAreNA[[predictor]] <- TRUE
    }
    allValuesAreNA <- TRUE
    #print(newData)
    result <- PredictModel(model, modelMetadata, NULL, ignoreOutOfRangeValues, FALSE, newData, warnedAboutMissingLevels, allValuesForPredictorAreNA, allValuesAreNA)

    predictedResponse <- result[[1]]
    predictedError <- result[[2]]
    warnedAboutMissingLevels <- result[[3]]
    allValuesForPredictorAreNA <- result[[4]]
    allValuesAreNA <- result[[5]]
 
    # Warn the user about any predictors were NoData for all cells, etc.
 
    PrintPredictionWarnings(modelMetadata$rPackage, c(modelMetadata$predictorExprs, modelMetadata$offsetExprs), allValuesForPredictorAreNA, allValuesAreNA)
   
    # If the data include the response variable, compute summary statistics.

    if (!allValuesAreNA && modelMetadata$responseVar %in% labels(newData)[[2]])
    {
        # Only compute statistics if there are any actual values.
        
        response <- data.frame(actual=eval(parse(text=modelMetadata$responseExpr), newData), predicted=predictedResponse)
        if (length(modelMetadata$offsetVars) == 1)
            response["offset"] <- newData[modelMetadata$offsetVars[1]][[1]]
        
        if (length(response$actual) > 0 && !all(is.na(response$actual)))
        {
            response <- na.omit(response)

            if (useTrainingData)
                modelSummary <- paste("MODEL PERFORMANCE SUMMARY:\n",
                                      "==========================\n",
                                      "\n",
                                      "Statistics calculated from the training data.\n",
                                      sep="")
            else
                modelSummary <- paste("MODEL PERFORMANCE SUMMARY:\n",
                                      "==========================\n",
                                      sep="")

            # For binary classification models, compute summary statistics using
            # ROCR.
            
            if (modelMetadata$isBinaryClassification)
            {
                # If the actual response (used to fit the model) is a factor (it
                # will be for rpart and random forest models, but not for binomial
                # GLMs and GAMs), convert it to numeric binary values.

                if (modelMetadata$responseDataClass == "factor")
                    response$actual <- as.integer(response$actual) - 1

                # If the caller did not provide a cutoff, calculate one now using
                # the ROCR package.
                
                library(ROCR)
         
                cutoffAutomaticallySelected <- FALSE
                pred <- prediction(response$predicted, response$actual)
                
                if (is.null(cutoff))
                {
                    perf <- performance(pred, 'sens', 'spec')
                    bestCutoff <- which.max(mapply(sum, unlist(perf@y.values), unlist(perf@x.values), -1))
                    cutoff <- pred@cutoffs[[1]][bestCutoff]
                    cutoffAutomaticallySelected <- TRUE
                }

                # Calculate performance statistics that do not require a cutoff,
                # such as AUC. Do them in a try, so that the whole function
                # does not fail for certain input data. The only case I know of is
                # the prbe calculation can fail with:
                #
                #     Error in .performance.precision.recall.break.even.point(predictions = c(1L, : Not enough distinct predictions to compute precision/recall intersections.
                
                auc <- NA
                mxe <- NA
                prbe <- NA
                rmse <- NA

                try({ auc <- performance(pred, "auc")@y.values[[1]] }, silent=TRUE)
                try({ mxe <- performance(pred, "mxe")@y.values[[1]] }, silent=TRUE)
                try({ prbe <- unlist(performance(pred, "prbe")@y.values) }, silent=TRUE)
                try({ rmse <- performance(pred, "rmse")@y.values[[1]] }, silent=TRUE)

                if (length(prbe) > 0)
                    prbe <- prbe[length(prbe)]
                else
                    prbe <- NA

                modelSummary <- paste(modelSummary,
                                      "\n",
                                      sprintf("Area under the ROC curve (auc)           = %.3f\n", auc),
                                      sprintf("Mean cross-entropy (mxe)                 = %.3f\n", mxe),
                                      sprintf("Precision-recall break-even point (prbe) = %.3f\n", prbe),
                                      sprintf("Root-mean square error (rmse)            = %.3f\n", rmse),
                                      sep="")

                if (cutoff >= 0)
                {
                    tn = length(which((response$predicted < cutoff) & (response$actual == 0)))
                    fn = length(which((response$predicted < cutoff) & (response$actual != 0)))
                    tp = length(which((response$predicted >= cutoff) & (response$actual == 1)))
                    fp = length(which((response$predicted >= cutoff) & (response$actual != 1)))

                    if (cutoffAutomaticallySelected)
                        modelSummary <- paste(modelSummary,
                                              "\n",
                                              sprintf("Cutoff selected by maximizing the Youden index = %.3f\n", cutoff),
                                              sep="")
                    else
                        modelSummary <- paste(modelSummary,
                                              "\n",
                                              sprintf("User-specified cutoff = %.3f\n", cutoff),
                                              sep="")
                                              
                    modelSummary <- paste(modelSummary,
                                          "\n",
                                          "Confusion matrix for that cutoff:\n",
                                          "\n",
                                          sep="")

                    if (modelMetadata$responseDataClass == "factor")
                    {
                        classLengths <- nchar(modelMetadata$factorLevels[[modelMetadata$responseExpr]])
                        columnWidth <- c(7 + rev(classLengths), 6)
                        modelSummary <- paste(modelSummary,
                                              sprintf("          %-*s  Actual %s  Actual %s  Total\n", max(classLengths), " ", modelMetadata$factorLevels[[modelMetadata$responseExpr]][2], modelMetadata$factorLevels[[modelMetadata$responseExpr]][1]),
                                              sprintf("Predicted %-*s  %*i  %*i %*i\n", max(classLengths), modelMetadata$factorLevels[[modelMetadata$responseExpr]][2], columnWidth[1], tp, columnWidth[2], fp, columnWidth[3], tp+fp),
                                              sprintf("Predicted %-*s  %*i  %*i %*i\n", max(classLengths), modelMetadata$factorLevels[[modelMetadata$responseExpr]][1], columnWidth[1], fn, columnWidth[2], tn, columnWidth[3], tn+fn),
                                              sprintf("Total     %-*s  %*i  %*i %*i\n", max(classLengths), " ", columnWidth[1], tp+fn, columnWidth[2], fp+tn, columnWidth[3], tp+fn+fp+tn),
                                              sep="")
                    }
                    else
                        modelSummary <- paste(modelSummary,
                                              "             Actual 1  Actual 0     Total\n",
                                              sprintf("Predicted 1 %9i %9i %9i\n", tp, fp, tp+fp),
                                              sprintf("Predicted 0 %9i %9i %9i\n", fn, tn, tn+fn),
                                              sprintf("      Total %9i %9i %9i\n", tp+fn, fp+tn, tp+fn+fp+tn),
                                              sep="")
                                          
                    tn <- as.double(tn)
                    fn <- as.double(fn)
                    tp <- as.double(tp)
                    fp <- as.double(fp)
                    acc <- (tp+tn)/(tp+fp+tn+fn)
                    
                    response$predicted <- as.integer(response$predicted >= cutoff)      # For calculating kappa

                    modelSummary <- paste(modelSummary,
                                          "\n",
                                          "Model performance statistics for that cutoff:\n",
                                          "\n",
                                          sprintf("Accuracy (acc)                                = %.3f\n", acc),
                                          sprintf("Error rate (err)                              = %.3f\n", (fp+fn)/(tp+fp+tn+fn)),
                                          sprintf("Rate of positive predictions (rpp)            = %.3f\n", (tp+fp)/(tp+fp+tn+fn)),
                                          sprintf("Rate of negative predictions (rnp)            = %.3f\n", (tn+fn)/(tp+fp+tn+fn)),
                                          "\n",
                                          sprintf("True positive rate (tpr, or sensitivity)      = %.3f\n", tp/(tp+fn)),
                                          sprintf("False positive rate (fpr, or fallout)         = %.3f\n", fp/(fp+tn)),
                                          sprintf("True negative rate (tnr, or specificity)      = %.3f\n", tn/(fp+tn)),
                                          sprintf("False negative rate (fnr, or miss)            = %.3f\n", fn/(tp+fn)),
                                          "\n",
                                          sprintf("Positive prediction value (ppv, or precision) = %.3f\n", tp/(tp+fp)),
                                          sprintf("Negative prediction value (npv)               = %.3f\n", tn/(tn+fn)),
                                          sprintf("Prediction-conditioned fallout (pcfall)       = %.3f\n", fp/(tp+fp)),
                                          sprintf("Prediction-conditioned miss (pcmiss)          = %.3f\n", fn/(tn+fn)),
                                          "\n",
                                          sprintf("Matthews correlation coefficient (mcc)        = %.3f\n", (tp*tn - fp*fn)/sqrt((tp+fn)*(fp+tn)*(tp+fp)*(fn+tn))),
                                          sprintf("Odds ratio (odds)                             = %.3f\n", (tp*tn)/(fn*fp)),
                                          sprintf("SAR                                           = %.3f\n", (acc + auc + rmse)/3),
                                          "\n",
                                          sprintf("Cohen's kappa (K)                             = %.3f\n", ckappa(response)$kappa),
                                          sep="")
                }
                else
                    modelSummary <- paste(modelSummary,
                                          "\n",
                                          "No cutoff was provided or estimated; confusion matrix and related summary statistics cannot be calculated.\n",
                                          sep="")
                
                # Create the performance plot.
                
                if (!is.null(outputPlotFile))
                {
                    perf <- performance(pred, measure1, measure2)
                    
                    if (tolower(substring(outputPlotFile, nchar(outputPlotFile)-3, nchar(outputPlotFile))) == ".emf")
                        win.metafile(outputPlotFile, width=width, height=height, pointsize=pointSize)
                    else
                        png(outputPlotFile, res=res, width=width, height=height, pointsize=pointSize, bg=bg)
        
                    tryCatch({
                        if (measure1 == "tpr" && measure2 == "fpr")     # Handle ROC plots specially
                        {
                            plot(perf, colorize=colorize, lwd=5)
                            abline(0, 1, lty=2, lwd=0.5, col="grey25")
                            if (cutoff >= 0)
                            {
                                tpr = tp/(tp+fn)
                                fpr = fp/(fp+tn)
                                if (colorize)
                                    points(x=fpr, y=tpr, cex=1.5, lwd=2)
                                else
                                    points(x=fpr, y=tpr, pch=21, cex=1.5, lwd=2, bg=bg)
                                text(x=fpr+0.03, y=tpr-0.03, labels=sprintf("Cutoff = %.3f", cutoff), adj=c(0,1))
                            }
                        }
                        else
                            plot(perf, colorize=colorize && measure2 != "cutoff")
                    }, finally={ graphics.off() })
                }
            }

            # For other classification models, compute summary statistics using
            # caret.
            
            else if (modelMetadata$isNonBinaryClassification)
            {
                library(caret)
                
                factorLevels <- unique(c(as.vector(response$predicted), as.vector(response$actual)))
                
                modelSummary <- paste(modelSummary,
                                     "\n",
                                     paste(capture.output(print(confusionMatrix(factor(as.vector(response$predicted), levels=factorLevels), factor(as.vector(response$actual), levels=factorLevels), dnn=c("Predicted", "Actual")))), sep="", collapse="\n"), "\n",
                                     sep="")
            }
            
            # For regression models, compute % variance explained (a.k.a. Efron's
            # pseudo R-squared).
            
            else
            {
                GetRegressionStats <- function (actual, predicted, responseVar)
                {
                    mae <- mean(abs(actual - predicted))
                    mse <- mean((actual - predicted)**2)
                    rmse <- sqrt(mse)
                    nrmse <- rmse / (max(actual) - min(actual)) * 100

                    r <- cor(actual, predicted, method="pearson")
                    tau <- cor(actual, predicted, method="kendall")
                    rho <- cor(actual, predicted, method="spearman")

                    rsqCorCoef <- (cor(predicted, actual))**2
                    rsqExplVar2 <- var(predicted)/var(actual)
                    rsqRandomForest <- 1 - mse/var(actual)

                    modelSummary <- paste("\n",
                                          sprintf("Mean Absolute Error (MAE)                  = %f\n", mae),
                                          sprintf("Mean Squared Error (MSE)                   = %f\n", mse),
                                          sprintf("Root Mean Squared Error (RMSE)             = %f\n", rmse),
                                          sprintf("Normalized Root Mean Squared Error (NRMSE) = %.2f%%\n", nrmse),
                                          "\n",
                                          sprintf("NRMSE calculated as RMSE / (max(%s) - min(%s)) * 100\n", responseVar, responseVar),
                                          "\n",
                                          sprintf("Pearson's r    = %.3f\n", r),
                                          sprintf("Kendall's tau  = %.3f\n", tau),
                                          sprintf("Spearman's rho = %.3f\n", rho),
                                          "\n",
                                          sprintf("R-squared (as squared Pearson's r)               = %.3f\n", rsqCorCoef),
                                          sprintf("R-squared (as explained variance/total variance) = %.3f\n", rsqExplVar2),
                                          sprintf("R-squared (as 1 - MSE/total variance)            = %.3f\n", rsqRandomForest),
                                          sep="")
                }
                
                modelSummary <- paste(modelSummary,
                                      "\n",
                                      sprintf("Number of records = %i\n", length(response$actual)),
                                      sep="")
                                      
                if ((modelMetadata$rPackage == "rpart" && tolower(model$method) == "poisson" || (is.null(modelMetadata$rPackage) || modelMetadata$rPackage %in% c("mgcv", "gam")) && (model$family$family %in% c("poisson", "quasipoisson") || length(grep("^Negative Binomial", model$family$family)) == 1 || length(grep("^Tweedie", model$family$family)) == 1)) && length(modelMetadata$offsetVars) == 1)
                {
                    modelSummary <- paste(modelSummary,
                                          "\n",
                                          sprintf("Summary statistics for the predicted response as a count (%s):\n", modelMetadata$responseVar),
                                          GetRegressionStats(response$actual, response$predicted, modelMetadata$responseVar),
                                          "\n",
                                          sprintf("Summary statistics for the predicted response as a rate (%s/%s):\n", modelMetadata$responseVar, modelMetadata$offsetVars[1]),
                                          GetRegressionStats(response$actual / response$offset, response$predicted / response$offset, sprintf("%s/%s", modelMetadata$responseVar, modelMetadata$offsetVars[1])),
                                          sep="")
                }
                else
                    modelSummary <- paste(modelSummary, GetRegressionStats(response$actual, response$predicted, modelMetadata$responseVar))
            }
            
            # Print the summary statistics.

            message("")
            writeLines(strsplit(modelSummary, "\n")[[1]])
            message("")
            
            # If the caller provided a summary file path, write the summary 
            # statistics to the file.
            
            if (!is.null(outputSummaryFile))
            {
                f <- file(outputSummaryFile, "wt")
                tryCatch(writeLines(modelSummary, f), finally=close(f))
            }
        }
        else
        {
            warning("There are no actual values of the response variable available. Summary statistics cannot be calculated.", call.=FALSE)
            if (!is.null(outputSummaryFile))
            {
                f <- file(outputSummaryFile, "wt")
                tryCatch(writeLines(c("There were no actual values of the response variable available. Summary statistics could not be calculated."), f), finally=close(f))
            }
        }
    }
    else if (allValuesAreNA && modelMetadata$responseVar %in% labels(newData)[[2]] && !is.null(outputSummaryFile))
    {
        f <- file(outputSummaryFile, "wt")
        tryCatch(writeLines(c("All of the predictions resulted in R \"NA\" values. Summary statistics could not be calculated."), f), finally=close(f))
    }
    
    # If this is a binary classification and a cutoff was provided or
    # calculated, classify the predicted continuous response into a binary
    # result.

    if (modelMetadata$isBinaryClassification && !is.null(cutoff) && cutoff >= 0)
    {
        predictedResponse <- as.integer(predictedResponse >= cutoff)

        if (modelMetadata$responseDataClass == "factor")
            predictedResponse <- as.vector(modelMetadata$factorLevels[[modelMetadata$responseExpr]][predictedResponse + 1])
    }
    
    # Return successfully.
    
    return(list(predictedResponse, cutoff))
}