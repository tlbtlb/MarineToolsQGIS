# Utils.r
#
# Copyright (C) 2011 Jason J. Roberts
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

CheckForCancel <- function(timeLastChecked=NULL)
{
    # If timeLastChecked is NULL, we are being called for the first
    # time. If there happens to be a file named
    # "GeoEcoCancelRequested_" followed by the process ID that was
    # somehow left over from a previous process, delete it so we don't
    # misinterpret it as an immediate cancel request. (This should
    # never happen, but we do it anyway for safety.)

    if (is.null(timeLastChecked))
    {
        tempDir <- Sys.getenv("TEMP")
        if (tempDir == "")
            tempDir <- Sys.getenv("TMP")

        if (tempDir != "" && file.exists(file.path(tempDir, sprintf("GeoEcoCancelReqested_%i", Sys.getpid()))))
            unlink(file.path(tempDir, sprintf("GeoEcoCancelReqested_%i", Sys.getpid())))
    }
    
    # If five seconds or more have elapsed since we last checked for a
    # cancel request, check for one now.
    
    if (is.null(timeLastChecked) || proc.time()[3] >= timeLastChecked + 5)
    {
        # Write a message with a special string. When the GeoEco
        # Python function that handles messages recieves this string,
        # it will check for a cancel request (and not log the
        # message).

        message("CHECK_FOR_CANCEL_REQUEST")

        # Look in the TEMP directory file a file named
        # "GeoEcoCancelRequested_" followed by the process ID. If we
        # find it, call stop with a message indicating we were
        # cancelled.

        tempDir <- Sys.getenv("TEMP")
        if (tempDir == "")
            tempDir <- Sys.getenv("TMP")

        if (tempDir != "" && file.exists(file.path(tempDir, sprintf("GeoEcoCancelReqested_%i", Sys.getpid()))))
        {
            unlink(file.path(tempDir, sprintf("GeoEcoCancelReqested_%i", Sys.getpid())))
            stop("Cancel requested", call.=FALSE)
        }

        # We were not cancelled. Return the current time.
        
        return(proc.time()[3])
    }

    # Less than five seconds have elapsed since we last checked for a
    # cancel request. Just return the time we last checked.
    
    return(timeLastChecked)
}


GetModelMetadata <- function(formulaOrTerms, rPackage, trainingData, xVar, yVar, zVar, mVar, coordinateSystem, model=NULL)
{
    # Initialize the list we will populate and return.

    modelMetadata <- list()
    
    modelMetadata$rPackage <- rPackage
    modelMetadata$xVar <- xVar
    modelMetadata$yVar <- yVar
    modelMetadata$zVar <- zVar
    modelMetadata$mVar <- mVar
    modelMetadata$coordinateSystem <- coordinateSystem
    
    # Build a model.frame from the formulaOrTerms and trainingData. From this,
    # we can extract all the relevant metadata we'll need later.

    modelFrame <- model.frame(formulaOrTerms, trainingData)
    modelTerms <- attr(modelFrame, "terms")
    dataClasses <- attr(modelTerms, "dataClasses")

    # Extract the response term and its characteristics.
    
    if (attr(modelTerms, "response") < 1)
        stop("The model formula must include a response term.", call.=FALSE)

    modelMetadata$responseExpr <- names(dataClasses[attr(modelTerms, "response")])
    modelMetadata$responseDataClass <- as.character(dataClasses[attr(modelTerms, "response")])
    modelMetadata$responseVar <- all.vars(parse(text=modelMetadata$responseExpr))

    if (rPackage == "rpart" && model$method == "poisson" && modelMetadata$responseDataClass == "nmatrix.2" && length(modelMetadata$responseVar) == 2)       # Special case for rpart poisson models specified cbind(A, B) ~ ...
    {
        modelMetadata$offsetExprs <- modelMetadata$responseVar[1]
        modelMetadata$offsetDataClasses <- "numeric"
        modelMetadata$offsetVars <- modelMetadata$responseVar[1]

        modelMetadata$responseExpr <- modelMetadata$responseVar[2]
        modelMetadata$responseDataClass <- "numeric"
        modelMetadata$responseVar <- modelMetadata$responseVar[2]
    }
    else if (length(modelMetadata$responseVar) != 1)
        stop("The response term of the model formula must reference exactly one variable. Response terms that reference more than one variable are not allowed.", call.=FALSE)
        
    modelMetadata$responseVarClass <- class(trainingData[[modelMetadata$responseVar]])

    # Extract the offset terms and their characteristics.
    
    if (!is.null(attr(modelTerms, "offset")))
    {
        modelMetadata$offsetExprs <- names(dataClasses[attr(modelTerms, "offset")])
        modelMetadata$offsetDataClasses <- as.character(dataClasses[attr(modelTerms, "offset")])
        modelMetadata$offsetVars <- all.vars(parse(text=modelMetadata$offsetExprs))
    }

    # Extract the predictor terms and their characteristics.
    
    allPredictors <- dataClasses[-c(attr(modelTerms, "response"), attr(modelTerms, "offset"))]
    modelMetadata$predictorExprs <- names(allPredictors)
    modelMetadata$predictorDataClasses <- as.character(allPredictors)
    modelMetadata$predictorVars <- all.vars(parse(text=modelMetadata$predictorExprs))

    # For all numeric variables used in the formula, and the formula terms
    # themselves, extract their min and max values; for factors, their levels;
    # for logicals, their unique values.

    modelMetadata$minNumericValues <- list()
    modelMetadata$maxNumericValues <- list()
    modelMetadata$factorLevels <- list()
    modelMetadata$logicalValues <- list()
    
    # Tim Added
    #print("Tim added")
    #for (variable in names(trainingData))
    #{
    #    print(paste("A ",variable," ",class(trainingData[[variable]])))
    #}

    for (variable in names(trainingData))
        if (class(trainingData[[variable]]) %in% c("numeric", "integer", "character"))
        {
            modelMetadata$minNumericValues[[variable]] <- min(trainingData[[variable]])
            modelMetadata$maxNumericValues[[variable]] <- max(trainingData[[variable]])
        }
        else if (class(trainingData[[variable]]) == "factor")
            modelMetadata$factorLevels[[variable]] <- levels(trainingData[[variable]])
        else if (class(trainingData[[variable]]) == "logical")
            modelMetadata$logicalValues[[variable]] <- unique(trainingData[[variable]])
        else 
            #stop(sprintf("The variable \"%s\" in the model formula has an unsupported data class \"%s\". Please contact the MGET development team for assistance.", variable, class(trainingData[[variable]])), call.=FALSE)

    for (i in 1:length(dataClasses))
    {
        # Tim Added
        #b= paste("dataClasses =",length(dataClasses))
        #print(b)
        termDataClass <- dataClasses[i]
        termExpression <- names(dataClasses[i])
        
        if (termDataClass %in% c("numeric", "nmatrix.1"))       # nmatrix.1 is the class of object returned by the lo() smoother in the gam package (not the mgcv package) when just one variable is smoothed 
        {
            modelMetadata$minNumericValues[[termExpression]] <- min(modelFrame[[termExpression]])
            modelMetadata$maxNumericValues[[termExpression]] <- max(modelFrame[[termExpression]])
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
            modelMetadata$factorLevels[[termExpression]] <- levels(modelFrame[[termExpression]])
        else if (termDataClass == "logical")
            modelMetadata$logicalValues[[termExpression]] <- unique(modelFrame[[termExpression]])
        else
            stop(sprintf("The model formula term \"%s\" has an unsupported data class \"%s\". Please contact the MGET development team for assistance.", termExpression, termDataClass), call.=FALSE)
    }
    
    # Do the same thing 
    
    # For the caller's convenience, create a vector listing all model variables.

    modelMetadata$allVars <- c(modelMetadata$responseVar, modelMetadata$offsetVars, modelMetadata$predictorVars)
    
    # Determine what kind of model it is.
    
    if (!is.null(model))
    {
        modelMetadata$isBinaryClassification <- (!is.null(rPackage) && rPackage %in% c("mgcv", "gam") || is.null(rPackage)) && model$family$family %in% c("binomial", "quasibinomial") || 
                                                !is.null(rPackage) && modelMetadata$responseDataClass == "factor" && length(modelMetadata$factorLevels[[modelMetadata$responseExpr]]) == 2 &&
                                                    (rPackage == "rpart" && model$method == "class" ||
                                                     rPackage == "randomForest" && model$type == "classification" ||
                                                     rPackage == "party" && (model@responses@is_nominal[1] || model@responses@is_ordinal[1]))
        
        modelMetadata$isNonBinaryClassification <- !modelMetadata$isBinaryClassification && !is.null(rPackage) && 
                                                   (rPackage == "rpart" && model$method == "class" ||
                                                    rPackage == "randomForest" && model$type == "classification" ||
                                                    rPackage == "party" && (model@responses@is_nominal[1] || model@responses@is_ordinal[1]))
    }

    # Return successfully.
    
    return(modelMetadata)
}
