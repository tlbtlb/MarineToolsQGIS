# -*- coding: utf-8 -*-
"""
/***************************************************************************
 randomForest
                                 A QGIS plugin
 Random Forest Prediction
 Generated by Plugin Builder: http://g-sherman.github.io/Qgis-Plugin-Builder/
                              -------------------
        begin                : 2023-08-01
        git sha              : $Format:%H$
        copyright            : (C) 2023 by Annika Catulli
        email                : a
 ***************************************************************************/

/***************************************************************************
 *                                                                         *
 *   This program is free software; you can redistribute it and/or modify  *
 *   it under the terms of the GNU General Public License as published by  *
 *   the Free Software Foundation; either version 2 of the License, or     *
 *   (at your option) any later version.                                   *
 *                                                                         *
 ***************************************************************************/
"""
from qgis.PyQt.QtCore import QSettings, QTranslator, QCoreApplication
from qgis.PyQt.QtGui import QIcon
from qgis.PyQt.QtWidgets import QAction,QFileDialog, QMessageBox
from qgis.core import QgsProject 

# Import the code for the dialog
from .randomForest_dialog import randomForestDialog
import os.path

from qgis.core import Qgis,QgsMessageLog
from qgis.gui import QgsMessageBar

from qgis.core import QgsProcessing
from qgis.core import QgsProcessingAlgorithm
from qgis.core import QgsProcessingMultiStepFeedback
from qgis.core import QgsProcessingParameterRasterLayer
from qgis.core import QgsProcessingParameterRasterDestination
from qgis.core import QgsProcessingParameterDefinition
from qgis.core import QgsVectorLayer
from qgis.core import (QgsSymbol,QgsSimpleFillSymbolLayer,QgsRendererCategory,QgsCategorizedSymbolRenderer)
from qgis.core import QgsRasterLayer
from qgis.core import QgsRasterBandStats

from qgis.core import QgsCoordinateReferenceSystem
from qgis.gui import QgsExtentGroupBox

import processing
import sys
import traceback
import os,glob
import random

class randomForest:
    """QGIS Plugin Implementation."""

    def select_input_file1(self): 
        filename, _filter = QFileDialog.getOpenFileName(selfMT.dlg, "Select input point file","", '*.shp') 
        selfMT.dlg.presenceFile.setText(filename) 
        #autofill
        autofill = filename[:-4]+"_prediction.img"
        selfMT.dlg.predictionOutput.setText(autofill)
        if os.path.exists(autofill):
            selfMT.dlg.exists1.setText("Existing file will be overwritten")
        else:
            selfMT.dlg.exists1.setText("")
        layer = QgsVectorLayer(filename, "ogr")
        fields = layer.fields()
        selfMT.dlg.comboBox.clear() 
        # Populate the comboBox with names of all the loaded fields   
        selfMT.dlg.comboBox.addItems([field.name() for field in fields]) 
                   
    def listInput(self): 
        items, _filter = QFileDialog.getOpenFileNames(selfMT.dlg, "Select predicting raster files (img & tif)","", '*.img *.tif') 
        selfMT.dlg.listWidget.addItems(items) 
        # set the extent limits to the first file's extent
        if len(items) > 0:
            firstLayer = selfMT.dlg.listWidget.item(0).text()
            layer = QgsRasterLayer(firstLayer)
            crs = QgsCoordinateReferenceSystem("EPSG:4326")
            exas = selfMT.dlg.extentBox.outputExtent()
            rExtXMin = exas.xMinimum()
            rExtXMax = exas.xMaximum()
            rExtYMin = exas.yMinimum()
            rExtYMax = exas.yMaximum()
            #if rExtXMin == 0.0 and rExtXMax == 0.0 and rExtYMin == 0.0 and rExtYMax == 0.0 :
            selfMT.dlg.extentBox.setOriginalExtent(layer.extent(),crs)
            selfMT.dlg.extentBox.setCurrentExtent(layer.extent(),crs)
            selfMT.dlg.extentBox.setOutputExtentFromCurrent()         
        presenceFile = selfMT.dlg.presenceFile.text()
        layer = QgsVectorLayer(presenceFile, "ogr")
        fields = layer.fields()
        selectedFieldIndex = selfMT.dlg.comboBox.currentIndex()
        presenceField = str(fields[selectedFieldIndex].name())
        predictionOutput = selfMT.dlg.predictionOutput.text()
        autofill = predictionOutput[:-4] + "_" + presenceField +".img"
        selfMT.dlg.predictionOutput.setText(autofill)
        if os.path.exists(autofill):
            selfMT.dlg.exists1.setText("Existing file will be overwritten")
        else:
            selfMT.dlg.exists1.setText("")
        
    def select_output_file(self): 
        filename, _filter = QFileDialog.getSaveFileName(selfMT.dlg, "Select output prediction file","", '*.img *.tif') 
        selfMT.dlg.predictionOutput.setText(filename) 
        if os.path.exists(filename):
            selfMT.dlg.exists1.setText("Existing file will be overwritten")
        else:
            selfMT.dlg.exists1.setText("")

    def clear_items(self):
        clear = []
        selfMT.dlg.listWidget.clear() 
           
    def help(self): 
        import webbrowser
        import marinetools
        MThelp = os.path.dirname(marinetools.__file__) + "\\randomForest\\randomForest.pdf"
        webbrowser.open(MThelp)  

    def Rhelp(self): 
        import webbrowser
        import marinetools
        MThelp = os.path.dirname(marinetools.__file__) + "\\randomForest\\Rprogramming.pdf"
        webbrowser.open(MThelp)  

    def run(self):
        # Create the dialog with elements (after translation) and keep reference
        # Only create GUI ONCE in callback, so that it will only load when the plugin is started
        import random

        plugin_dir = os.path.dirname(__file__).replace('\\','\\\\')
        try:
            RscriptsOrig = QSettings().value('/Processing/Configuration/R_SCRIPTS_FOLDER')
            RscriptsNew = plugin_dir.replace('\\\\','/') + '/rscripts'
            if RscriptsOrig.find(RscriptsNew) == -1:
                QSettings().setValue( '/Processing/Configuration/R_SCRIPTS_FOLDER',RscriptsOrig+";"+RscriptsNew)
                QSettings().sync()
                QMessageBox.information(None, "Information:", "This tool requires certain R scripts to be available.\nThey have been installed but\nPlease restart QGIS")
                return
        except:
                QMessageBox.information(None, "Information:", "This tool requires the R software to be available.\nInstructions will be shown and\nthen restart QGIS")
                randomForest.Rhelp(self)
                return

        global selfMT
        alphabet = '1234567890abcdefghijklmnopqrstuvwxyz'
        rand = alphabet[random.randint(1,25)] + alphabet[random.randint(1,35)] + alphabet[random.randint(1,35)] + alphabet[random.randint(1,35)]

        if self.first_start == True:
            self.first_start = False
            self.dlg = randomForestDialog()
            selfMT = self
            self.dlg.listInputButton.clicked.connect(randomForest.listInput) 
            self.dlg.InFileRaster.clicked.connect(randomForest.select_input_file1) 
            self.dlg.OutFileRaster.clicked.connect(randomForest.select_output_file) 
            self.dlg.Clear.clicked.connect(randomForest.clear_items) 
            self.dlg.helpButton.clicked.connect(randomForest.help) 
            self.dlg.Rhelp.clicked.connect(randomForest.Rhelp) 

        # show the dialog
        self.dlg.show()
        # Run the dialog event loop
        result = self.dlg.exec_()
        
        # See if OK was pressed
        if result:
            presenceFile = self.dlg.presenceFile.text()
            predictionOutput = self.dlg.predictionOutput.text()
            if os.path.exists(predictionOutput):
                os.remove(predictionOutput)
            
            #Make the temporary directory
            TempDir = str(os.path.dirname(presenceFile)) + "/tempMT"
            if not os.path.exists(TempDir):
                os.mkdir(TempDir)
            
            layer = QgsVectorLayer(presenceFile, "ogr")
            fields = layer.fields()
            nrows = layer.featureCount()
            selectedFieldIndex = self.dlg.comboBox.currentIndex()
            presenceField = str(fields[selectedFieldIndex].name())
            
            #Take inputs 
            chosenClass = self.dlg.nameInput.text()
            if chosenClass == "":
                chosenClass = " = \'Crust\'"
                
            testPercentage = float(self.dlg.percentInput.text())
            if testPercentage == "":
                testPercentage = 25.0
            
            rasterListCount = self.dlg.listWidget.count()
            rasterList = []
            items = []
            for x in range(rasterListCount):
                items.append(self.dlg.listWidget.item(x).text())
            rasterList = items
            
            resOut = self.dlg.resBox.text()
            if resOut == "":
                resOut = "0.2"
            cutOff = self.dlg.cutOff.text()
            if cutOff == "":
                cutOff = "0.5"
            deleteIntermediate = self.dlg.delInter.isChecked()

            #Get the extent as text
            exas = self.dlg.extentBox.outputExtent()

            rExtXMin = exas.xMinimum()
            rExtXMax = exas.xMaximum()
            rExtYMin = exas.yMinimum()
            rExtYMax = exas.yMaximum()
            if rExtXMin != 0.0 or rExtXMax != 0.0 or rExtYMin != 0.0 or rExtYMax != 0.0 :
                exasAll = str(rExtXMin) + "," +str(rExtXMax) + "," +str(rExtYMin) + "," +str(rExtYMax)
                self.iface.messageBar().pushMessage("", "Reduced area to " + exasAll, level=Qgis.Info)
            else:
                exasAll = "-180.0, 180.0 ,-90.0, 90.0"
                self.iface.messageBar().pushMessage("", "Using the global area", level=Qgis.Info)
                
            BC_test = str(os.path.dirname(presenceFile)) + "/RF_Model.Rdata"
            if os.path.exists(BC_test):
                os.remove(BC_test)
                
            BC_summary = str(os.path.dirname(presenceFile)) + "/RF_Model_summary.txt"
            #if os.path.exists(BC_summary):
                #os.remove(BC_summary)
                
            BC_Importance = str(os.path.dirname(presenceFile)) + "/RF_Model_importance.png"
            if os.path.exists(BC_Importance):
                os.remove(BC_Importance)
            
            #Determine if all of the points will be used 
            useAllPoints = self.dlg.useAllBox.isChecked()
            if useAllPoints:
                self.iface.messageBar().pushMessage("", "Using all the points ", level=Qgis.Info)
            else:
                self.iface.messageBar().pushMessage("", "Using a subset of the points, matching the area to be predicted", level=Qgis.Info)
            
            self.iface.messageBar().pushMessage("", "Chosen " + str(presenceField) +  str(chosenClass), level=Qgis.Info)
            
            
            tableNames = []
            #loop through the input rasters and add them to the list if they are rasters and CLIP
            rasters = []
            for x in range(rasterListCount):
                inRS = self.dlg.listWidget.item(x).text()
                #print(inRS)
                raster = QgsRasterLayer(inRS)
                result = processing.run("gdal:cliprasterbyextent", {'INPUT':raster,'PROJWIN':exasAll,'OVERCRS':False,'NODATA':None,'OPTIONS':'','DATA_TYPE':0,'EXTRA':'','OUTPUT':'TEMPORARY_OUTPUT'})
                raster = result['OUTPUT']
                baseName = os.path.basename(inRS)
                name = baseName.split('.')[0]
                tableNames.append(name + "_" + str(x))    
            
            #If not all of the points are to be used, the presence file is clipped according to the extent of the subset
            #Presence_shp = TempDir + u"/Presence_shp.shp"
            if not useAllPoints:
                extent = QgsRasterLayer(raster).extent()
                result = processing.run("gdal:clipvectorbyextent", {'INPUT':presenceFile,'EXTENT':extent,'OPTIONS':'','OUTPUT':'TEMPORARY_OUTPUT'})
                Presence_shp2 = result['OUTPUT']
                processing.run("native:fieldcalculator", {'INPUT':Presence_shp2,'FIELD_NAME':'o','FIELD_TYPE':0,'FIELD_LENGTH':0,'FIELD_PRECISION':0,'FORMULA':'1','OUTPUT':'TEMPORARY_OUTPUT'})
            else:
                Presence_shp2 = presenceFile
               
     
            # Use whole file or Chosen Class for Presence Data
            if chosenClass == "": # make up some absence data !!!!!! NOT recommended NOT tested
                # Process: Get Count for absence
                Row_Count = Presence_shp.featureCount()
           
                # Make buffer around the features
                result = processing.run("native:buffer", {'INPUT':Presence_CopyFeatures_shp,'DISTANCE':Distance_value_or_field,'SEGMENTS':5,'END_CAP_STYLE':0,'JOIN_STYLE':0, \
                                                          'MITER_LIMIT':2,'DISSOLVE':True,'OUTPUT':'TEMPORARY_OUTPUT'})
                Presence2Buffer_shp = result['OUTPUT']
            
                #Get the absence area by removing the presence area from the coverage file
                result = processing.run("native:difference", {'INPUT':inRS,'OVERLAY':Presence2Buffer_shp,'OUTPUT':'TEMPORARY_OUTPUT','GRID_SIZE':None})
                Derived_absence_area_shp = result['OUTPUT']
                
                #Create Random Points for absence
                result = processing.run("qgis:randompointsinsidepolygons", {'INPUT':Derived_absence_area_shp,'STRATEGY':0,'VALUE':Row_Count,'MIN_DISTANCE':resOut,'OUTPUT':'TEMPORARY_OUTPUT'})
                Derived_Absence = result['OUTPUT']
                
                # Set Presence2 to 1 using field manager to indicate absence
                result = processing.run("native:fieldcalculator", {'INPUT':Derived_absence_area_shp,'FIELD_NAME':'Presence2','FIELD_TYPE':1,'FIELD_LENGTH':0,'FIELD_PRECISION':0, \
                                                                   'FORMULA':'1','OUTPUT':'TEMPORARY_OUTPUT'})
                Absence_CopyFeatures_shp = result['OUTPUT']
            
            else:
                
                whereClause = '"' + str(presenceField) + '"' + str(chosenClass) 
                #print(whereClause)
                result = processing.run("native:extractbyexpression", {'INPUT':Presence_shp2,'EXPRESSION':whereClause,'OUTPUT':'TEMPORARY_OUTPUT','FAIL_OUTPUT':'TEMPORARY_OUTPUT'})
                Presence_CopyFeatures_shp = result['OUTPUT']
                Absence_CopyFeatures_shp = result['FAIL_OUTPUT']
            
            # Set Presence2 to 2 using fieldcalculator 
            result = processing.run("native:fieldcalculator", {'INPUT':Presence_CopyFeatures_shp,'FIELD_NAME':'Presence2','FIELD_TYPE':1,'FIELD_LENGTH':0,'FIELD_PRECISION':0, \
                                                               'FORMULA':'2','OUTPUT':'TEMPORARY_OUTPUT'})
            Presence_CopyFeatures3_shp = result['OUTPUT']
            # Set Presence2 to 0 using fieldcalculator 
            result = processing.run("native:fieldcalculator", {'INPUT':Absence_CopyFeatures_shp,'FIELD_NAME':'Presence2','FIELD_TYPE':1,'FIELD_LENGTH':0,'FIELD_PRECISION':0, \
                                                               'FORMULA':'0','OUTPUT':'TEMPORARY_OUTPUT'})
            Absence_CopyFeatures3_shp = result['OUTPUT']

            # Merge the layers together
            infiles = [Absence_CopyFeatures3_shp, Presence_CopyFeatures3_shp]
            result = processing.run("native:mergevectorlayers", {'LAYERS':infiles,'CRS':None,'OUTPUT':'TEMPORARY_OUTPUT'})
            Presence_absence_shp = result['OUTPUT']
            
            # Retain only the main attributes Class and Presence2 value
            result = processing.run("native:retainfields", {'INPUT':Presence_absence_shp,'FIELDS':[str(presenceField),'Presence2'],'OUTPUT':'TEMPORARY_OUTPUT'})
            TestGroup1 = result['OUTPUT']
            tempForExtract = TestGroup1
            
            #Extract the raster values from each of the layers in the layer list
            for x in range(rasterListCount-1):
                inRS = self.dlg.listWidget.item(x).text()
                column = str(os.path.basename(inRS)[:8]) + str('{:02d}'.format(x))
                result = processing.run("native:rastersampling", {'INPUT':tempForExtract,'RASTERCOPY': inRS,'COLUMN_PREFIX':column,'OUTPUT':'TEMPORARY_OUTPUT'})
                tempForExtract = result['OUTPUT']
            #Presence_absence_values = tempForExtract
            # Add last column and make into an output file called Presence_absence_values
            inRS = self.dlg.listWidget.item(rasterListCount-1).text()
            column = str(os.path.basename(inRS)[:8]) + str('{:02d}'.format(x+1))

            Presence_absence_values = TempDir + "/Presence_absence_values"+rand+".shp"
            result = processing.run("native:rastersampling", {'INPUT':tempForExtract,'RASTERCOPY': inRS,'COLUMN_PREFIX':column,'OUTPUT':Presence_absence_values})
            layer1 = QgsVectorLayer(Presence_absence_values, "ogr")

            tempForExtract = Presence_absence_values
            #Remove any rows with Nullvalues from each of the columns list
            for x in range(rasterListCount-1):
                inRS = self.dlg.listWidget.item(x).text()
                column = str(os.path.basename(inRS)[:8]) + str('{:02d}'.format(x))
                result = processing.run("native:extractbyattribute", {'INPUT':tempForExtract,'FIELD':column,'OPERATOR':9,'VALUE':'','OUTPUT':'TEMPORARY_OUTPUT'})
                tempForExtract = result['OUTPUT']
            #Presence_absence_values = tempForExtract
            # Add last column and make into an output file called Presence_absence_values
            inRS = self.dlg.listWidget.item(rasterListCount-1).text()
            column = str(os.path.basename(inRS)[:8]) + str('{:02d}'.format(x+1))
            result = processing.run("native:extractbyattribute", {'INPUT':tempForExtract,'FIELD':column,'OPERATOR':9,'VALUE':'','OUTPUT':'TEMPORARY_OUTPUT'})
            Presence_absence_values2 = result['OUTPUT']
            if deleteIntermediate:
                QgsProject.instance().addMapLayer(layer1, False)
                QgsProject.instance().removeMapLayer(layer1.id())
                remove_temp_files(Presence_absence_values)
 
            # Process: Randomly Split Table Into Training and Test Records 
            result = processing.run("native:fieldcalculator", {'INPUT':Presence_absence_values2,'FIELD_NAME':'ran','FIELD_TYPE':0,'FIELD_LENGTH':4,'FIELD_PRECISION':4,'FORMULA':' randf(0,1)','OUTPUT':'TEMPORARY_OUTPUT'})
            Random_values = result['OUTPUT']
            result = processing.run("native:orderbyexpression", {'INPUT':Random_values,'EXPRESSION':'"ran"','ASCENDING':True,'NULLS_FIRST':False,'OUTPUT':'TEMPORARY_OUTPUT'})
            OrderedRandom_values = result['OUTPUT']
            NumberedRandom_values = TempDir + "/NumberedRandom_values"+rand+".shp"
            result = processing.run("native:fieldcalculator", {'INPUT':OrderedRandom_values,'FIELD_NAME':'linenum2','FIELD_TYPE':0,'FIELD_LENGTH':0,'FIELD_PRECISION':0,'FORMULA':' $id ','OUTPUT':NumberedRandom_values})
            layer2 = QgsVectorLayer(NumberedRandom_values, "ogr")
            nrows = layer2.featureCount()

            Presence_PA_Select0_shp = TempDir + "/Presence_PA_Select0"+rand+".shp"
            expression = "linenum2 > " + str(float(testPercentage)*nrows/100)
            result = processing.run("native:extractbyexpression", {'INPUT':NumberedRandom_values,'EXPRESSION':expression,'OUTPUT':Presence_PA_Select0_shp})
            expression = "linenum2 <= " + str(float(testPercentage)*nrows/100)
            result = processing.run("native:extractbyexpression", {'INPUT':NumberedRandom_values,'EXPRESSION':expression,'OUTPUT':'TEMPORARY_OUTPUT'})
            ModelTestData1 = result['OUTPUT']
            if deleteIntermediate:
                QgsProject.instance().addMapLayer(layer2, False)
                QgsProject.instance().removeMapLayer(layer2.id())
                remove_temp_files(NumberedRandom_values)

            # Presence_PA_Select0_shp - Training Data for model 
            # ModelTestData1 - Model test data 

            
            # Create Model
            InFormula = u"Presence2 ~ " 
            for x in range(rasterListCount):
                inRS = self.dlg.listWidget.item(x).text()
                column = str(os.path.basename(inRS)[:8]) + str('{:02d}'.format(x))
                if x > 0:
                    InFormula = InFormula + " + " 
                InFormula = InFormula + column
  
            self.iface.messageBar().pushMessage("","Starting Model Creation Random Forest", level=Qgis.Info)
            print("Starting Model Creation")
            trainingData = str(Presence_PA_Select0_shp)
            Result = processing.run("r:rfmakemodel", {'Layer':trainingData,'BC_test':str(BC_test),'InFormula':str(InFormula),'ntree':'500','rscriptsSource':str(plugin_dir)})
            self.iface.messageBar().pushMessage("","Finished Model Creation Random Forest", level=Qgis.Info)
            print("Finished Model Creation")

            try:
                with open(BC_summary) as f:
                    lines = f.read()
                    print(lines)
                f.close()
            except:
                print("Summary file missing? - deleted? - not created?")

            # Process: Resample
            inRS = self.dlg.listWidget.item(0).text()
            result = processing.run("gdal:warpreproject", {'INPUT':inRS,'SOURCE_CRS':None,'TARGET_CRS':None,'RESAMPLING':0,'NODATA':None,'TARGET_RESOLUTION':resOut, \
                                                           'OPTIONS':'','DATA_TYPE':0,'TARGET_EXTENT':None,'TARGET_EXTENT_CRS':None,'MULTITHREADING':False,'EXTRA':'','OUTPUT':'TEMPORARY_OUTPUT'})
            AreaBathy_Subset0_Resam_tif = result['OUTPUT']
            # Turn the raster layer into a point layer
            result = processing.run("native:pixelstopoints", {'INPUT_RASTER':AreaBathy_Subset0_Resam_tif,'RASTER_BAND':1,'FIELD_NAME':'VALUE','OUTPUT':'TEMPORARY_OUTPUT'})
            RasterValuePoints1 = result['OUTPUT']
                                                  
            OutPoints3 = TempDir + "/OutPoints3"+rand+".shp"
            #Extract the raster values from each of the layers in the layer list for the resampled raster points
            for x in range(rasterListCount-1):
                inRS = self.dlg.listWidget.item(x).text()
                column = str(os.path.basename(inRS)[:8]) + str('{:02d}'.format(x))
                result = processing.run("native:rastersampling", {'INPUT':RasterValuePoints1,'RASTERCOPY': inRS,'COLUMN_PREFIX':column,'OUTPUT':'TEMPORARY_OUTPUT'})
                RasterValuePoints1 = result['OUTPUT']
            inRS = self.dlg.listWidget.item(rasterListCount-1).text()
            column = str(os.path.basename(inRS)[:8]) + str('{:02d}'.format(x+1))
            result = processing.run("native:rastersampling", {'INPUT':RasterValuePoints1,'RASTERCOPY': inRS,'COLUMN_PREFIX':column,'OUTPUT':OutPoints3})
            layer3 = QgsVectorLayer(OutPoints3, "ogr")

            RasterValuePoints3 = TempDir + "/RasterValuePoints3"+rand+".shp"       
            RasterValuePoints1 = OutPoints3
            for x in range(rasterListCount-1):
                inRS = self.dlg.listWidget.item(x).text()
                column = str(os.path.basename(inRS)[:8]) + str('{:02d}'.format(x))
                result = processing.run("native:extractbyattribute", {'INPUT':RasterValuePoints1,'FIELD':column,'OPERATOR':9,'VALUE':'','OUTPUT':'TEMPORARY_OUTPUT'})
                RasterValuePoints1 = result['OUTPUT']
            inRS = self.dlg.listWidget.item(rasterListCount-1).text()
            column = str(os.path.basename(inRS)[:8]) + str('{:02d}'.format(x+1))
            result = processing.run("native:extractbyattribute", {'INPUT':RasterValuePoints1,'FIELD':column,'OPERATOR':9,'VALUE':'','OUTPUT':RasterValuePoints3})
            if deleteIntermediate:
                QgsProject.instance().addMapLayer(layer3, False)
                QgsProject.instance().removeMapLayer(layer3.id())
                remove_temp_files(OutPoints3)
            
            OutPoints = TempDir + "/RasterRFOutPoints"+rand+".shp"
            # Process: Predict Random Forest From Table 
            self.iface.messageBar().pushMessage("","Starting Prediction", level=Qgis.Info)
            Result2 = processing.run("r:rfpredict", {'BC_test':str(BC_test),'Points':str(RasterValuePoints3), 'OutPoints':str(OutPoints), 'cutoff':0.5, 'rscriptsSource':str(plugin_dir) })
            self.iface.messageBar().pushMessage("","Finished Prediction", level=Qgis.Info)

            #TODO: Check if this Scatterplot command works
            '''
            self.iface.messageBar().pushMessage("","Starting Scatterplot", level=Qgis.Info)
            liststring = [ [tableNames[1],"d*1"] ]
            #Append the names of each band to a string
            for name in tableNames:
                liststring.append([name,"d*1"])
            print(liststring)

            GeoEco.Statistics.Exploratory.RExploratoryPlots.ScatterplotMatrixForArcGISTable(Presence_PA_Select0_shp, liststring, None, "Histogram", "Smooth", "Correlation", \
            None, None, None, Scatter_Presence_pdf)
            self.iface.messageBar().pushMessage("","Finished Scatterplot", level=Qgis.Info)
            '''
            
            # Get a layer of all the points where the test data is 1
            # ModelTestData1 - Model test data - TestData = 1
            
            # Process: Point to Raster
            BC_RF_P1_tif    = TempDir + "\\BC_RF_P1_tif"+rand+".tif"
            result = processing.run("gdal:rasterize", {'INPUT':OutPoints,'FIELD':'RF_predict','BURN':None,'USE_Z':False,'UNITS':1,'WIDTH':resOut,'HEIGHT':resOut,'EXTENT':exas, \
                                                       'NODATA':-1.0,'OPTIONS':'','DATA_TYPE':5,'INIT':None,'INVERT':False,'EXTRA':'','OUTPUT':BC_RF_P1_tif})
            expr = '"' + str(BC_RF_P1_tif) + '@1" /2.0'
            processing.run("qgis:rastercalculator", {'EXPRESSION':expr,'LAYERS':str(BC_RF_P1_tif),'CELLSIZE':0,'EXTENT':None,'CRS':None,'OUTPUT':str(predictionOutput)})
            
            #Extract the values of the raster layer at the specified points 
            result = processing.run("native:rastersampling", {'INPUT':ModelTestData1,'RASTERCOPY':BC_RF_P1_tif, \
                                                              'COLUMN_PREFIX':'RF_pred','OUTPUT':'TEMPORARY_OUTPUT'})
            RFtmp_prediction_test = result['OUTPUT']
            
            # Confusion Matrices and Statistics

            # Calculation for confusion matrix if 50% chosen (0.5)
            # Presence2  RF_pred1   Output
            #    0        0-1        0
            #    0        1-2        1
            #    2        0-1        2
            #    2        1-2        3
            #  cutOff value 0.5 gives 1 as boundary  add 1-cutoffx2
            boundaryCutoff = 1.0 - (float(cutOff)*2.0)

            formulaExpression = '"Presence2" + round(("RF_pred1" +' + str(boundaryCutoff) + ')/2)'
            result = processing.run("native:fieldcalculator", {'INPUT':RFtmp_prediction_test,'FIELD_NAME':'Prediction', \
                                                               'FIELD_TYPE':0,'FIELD_LENGTH':0,'FIELD_PRECISION':0, \
                                                               'FORMULA':formulaExpression,'OUTPUT':'TEMPORARY_OUTPUT'})
            RFtmp_prediction_test2 = result['OUTPUT']
            
            listClassesCsv = TempDir + "\\listClasses"+rand+".csv"

            #Create a CSV containing each value and how often it occurs
            processing.run("qgis:statisticsbycategories", {'INPUT':RFtmp_prediction_test2,'VALUES_FIELD_NAME':'','CATEGORIES_FIELD_NAME':["Prediction"],'OUTPUT':listClassesCsv})
          
            #TODO: Check
            NoPresNoPred = 1
            NoPresYePred = 1
            YePresNoPred = 1
            YePresYePred = 1
            with open(listClassesCsv) as f:
                lines = f.read().splitlines()
                classVar = []
                for vars in lines:
                    if str(vars).split(",")[0] != "Prediction" and str(vars).split(",")[0] != "":
                        if int(float(str(vars).split(",")[0])) == 0:
                            NoPresNoPred = int(float(str(vars).split(",")[1].replace('"','')))
                        if int(float(str(vars).split(",")[0])) == 1:
                            NoPresYePred = int(float(str(vars).split(",")[1].replace('"','')))
                        if int(float(str(vars).split(",")[0])) == 2:
                            YePresNoPred = int(float(str(vars).split(",")[1].replace('"','')))
                        if int(float(str(vars).split(",")[0])) == 3:
                            YePresYePred = int(float(str(vars).split(",")[1].replace('"','')))
            f.close()
            #Output the messages 
            with open(BC_summary, "a+") as f:
                f.write("Presence Absence File used = " + os.path.basename(presenceFile) + "\n")
                f.write("Working in directory       = " + os.path.dirname(presenceFile) + "\n")
                f.write("Using presence identity      " + whereClause + "\n")
                f.write("Number of samples used     = " + str(nrows) + "\n")
                Div = round(float(testPercentage)*nrows/100)
                f.write("A random selection of      = " + str(nrows - (round(float(testPercentage)*nrows/100))) + " samples for model creation" + "\n")
                f.write("The remainder of samples   = " + str(round(float(testPercentage)*nrows/100)) + " for model testing (" + str(testPercentage) + " %)" + "\n")
                f.write("Input files used:" + "\n")
                for x in range(rasterListCount):
                    inRS = self.dlg.listWidget.item(x).text()
                    column = str(os.path.basename(inRS)[:8]) + str('{:02d}'.format(x))
                    f.write("                           = " + str(os.path.basename(inRS)) + "  " + column + "\n")
                f.write("Area extent predicted      = " + str(exasAll) + "\n")
                if useAllPoints:
                    f.write("but used all the input samples" + "\n")
                f.write("Output resolution of grid  = " + resOut + "\n")
                f.write(" " + "\n")
                f.write("Confusion Matrix (based on "+ str(round(float(cutOff)*100.0)) + "% value presence absence)" + "\n")
                f.write("                      Presence  Absence" + "\n")
                f.write("  Predicted Presence     "+ str(YePresYePred) + "       " + str(NoPresYePred) + "\n")
                f.write("  Predicted Absence      "+ str(YePresNoPred) + "       " + str(NoPresNoPred) + "\n")
                f.write(" " + "\n")

                print(" ")
                print("Confusion Matrix (based on "+ str(round(float(cutOff)*100.0)) + "% value presence absence)")
                print("                      Presence  Absence")
                print("  Predicted Presence     "+ str(YePresYePred) + "       " + str(NoPresYePred))
                print("  Predicted Absence      "+ str(YePresNoPred) + "       " + str(NoPresNoPred))
                print(" ")
               
                #TODO: Test this, syntax looks fine omission and comission
                pec = NoPresYePred /float(YePresYePred + NoPresYePred)
                peo = YePresNoPred /float(YePresYePred + YePresNoPred)
                ppa = YePresYePred /float(YePresYePred + YePresNoPred)
                pua = YePresYePred /float(YePresYePred + NoPresYePred)
                aec = YePresNoPred /float(YePresNoPred + NoPresNoPred)
                aeo = NoPresYePred /float(NoPresYePred + NoPresNoPred)
                apa = NoPresNoPred /float(NoPresYePred + NoPresNoPred)
                aua = NoPresNoPred /float(YePresNoPred + NoPresNoPred)
                
                f.write("                      Errors of    Errors of    Producer     User" + "\n")
                f.write("                      Commission   Omission     Accuracy     Accuracy" + "\n")
                f.write("  Predicted Presence   " + str(pec)[:4] + "         " + str(peo)[:4]+ "         " + str(ppa)[:4]+ "         " + str(pua)[:4] + "\n")
                f.write("  Predicted Absence    " + str(aec)[:4] + "         " + str(aeo)[:4]+ "         " + str(apa)[:4]+ "         " + str(aua)[:4] + "\n")
                f.write(" " + "\n")
                               
                print("                      Errors of    Errors of    Producer     User")
                print("                      Commission   Omission     Accuracy     Accuracy")
                print("  Predicted Presence   " + str(pec)[:4] + "         " + str(peo)[:4]+ "         " + str(ppa)[:4]+ "         " + str(pua)[:4])
                print("  Predicted Absence    " + str(aec)[:4] + "         " + str(aeo)[:4]+ "         " + str(apa)[:4]+ "         " + str(aua)[:4])
                print(" ")
                               
                sum = float(YePresYePred + NoPresYePred + YePresNoPred + NoPresNoPred)
                p0 = (YePresYePred + NoPresNoPred) / sum
                pe = (((YePresYePred + NoPresYePred) / sum)) * (((YePresYePred + YePresNoPred) / sum)) + \
                     (((YePresNoPred + NoPresNoPred) / sum)) * (((NoPresYePred + NoPresNoPred) / sum))
                kappa = (p0-pe)/(1.0-pe)
                f.write("  Overall Accuracy       " + str(p0*100.0)[:6] + " %" + "\n")
                f.write("  Cohen's Kappa          " + str(kappa*100.0)[:6] + " %" + "\n")
                print("  Overall Accuracy       " + str(p0*100.0)[:6] + " %")
                print("  Cohen's Kappa          " + str(kappa*100.0)[:6] + " %")
                
                # Kappa values
                # 0.01 - 0.20 as none to slight
                # 0.21 - 0.40 as fair
                # 0.41 - 0.60 as moderate
                # 0.61 - 0.80 as substantial
                # 0.81 - 1.00 as almost perfect agreement
            
            f.close()

            fname = os.path.dirname(str(predictionOutput))
            vlayer = QgsRasterLayer(str(predictionOutput), str(predictionOutput[len(fname)+1:]))
            QgsProject.instance().addMapLayer(vlayer)

            if deleteIntermediate:
                print('Deleting Files')
                remove_temp_files(BC_RF_P1_tif)
                remove_temp_files(listClassesCsv)
                remove_temp_files(OutPoints) # RasterRFOutPoints
                remove_temp_files(Presence_PA_Select0_shp)
                remove_temp_files(RasterValuePoints3)
                try:
                    os.rmdir(TempDir)
                except:
                    print("Temporary directory is not empty to delete")
                
            pass

def remove_temp_files(tempfilename):
    try:
        for f in glob.glob(str(tempfilename[:-4]) + "*"):
            os.remove(f)
    except:
        print(tempfilename + " not removed")

    return
def remove_real_files(tempfilename):
    layer2 = QgsVectorLayer(tempfilename, "ogr")
    QgsProject.instance().addMapLayer(layer2, False)
    QgsProject.instance().removeMapLayer(layer2.id())
    for f in glob.glob(str(tempfilename[:-4]) + "*"):
        os.remove(f)

    return

             