# -*- coding: utf-8 -*-
"""
/***************************************************************************
 surveyLines
                                 A QGIS plugin
 Draws survey lines and points delineated by a polygon
 Generated by Plugin Builder: http://g-sherman.github.io/Qgis-Plugin-Builder/
                              -------------------
        begin                : 2023-07-25
        git sha              : $Format:%H$
        copyright            : (C) 2023 by a
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
from qgis.PyQt.QtWidgets import QAction,QFileDialog
from qgis.core import QgsProject


# Initialize Qt resources from file resources.py
#from .resources import *
# Import the code for the dialog
from .SurveyLines_dialog import SurveyLinesDialog
import os.path

from qgis.PyQt.QtCore import QSettings, QTranslator, QCoreApplication
from qgis.PyQt.QtGui import QIcon

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
from qgis.core import QgsProperty
from qgis.core import QgsCoordinateReferenceSystem

import processing
import sys
import traceback
import os
import random

class SurveyLines:
    """QGIS Plugin Implementation."""

    
    def select_input_file1(self):
        filename, _filter = QFileDialog.getOpenFileName(selfMT.dlg, "Input Polygon area file","", '*.shp')
        selfMT.dlg.comboBox.clear() 
        selfMT.dlg.comboBox.insertItem(0,filename)
        selfMT.dlg.comboBox.setCurrentIndex(0)
        #autofill
        lineEdit_2 = selfMT.dlg.lineEdit_2.text()
        if lineEdit_2 == "":
            lineEdit_2 = "0"
        lineEdit_3 = selfMT.dlg.lineEdit_3.text()
        if lineEdit_3 == "":
            lineEdit_3 = "100"
        if selfMT.dlg.reverseBox.isChecked():
            rb="_Rev"
        else:
            rb=""
        if selfMT.dlg.farEndBox.isChecked():
            fb="_Far"
        else:
            fb=""
        lineEdit_3 = selfMT.dlg.lineEdit_3.text()
        if lineEdit_3 == "":
            lineEdit_3 = "100"
        autoPoly = filename[:-4]+"_wayPoints_"+lineEdit_2+"_"+lineEdit_3++fb+rb+".shp"
        selfMT.dlg.lineEdit_4.setText(autoPoly)
        if os.path.exists(autoPoly):
            selfMT.dlg.exists1.setText("Existing file will be overwritten")
        else:
            selfMT.dlg.exists1.setText("")
        autoPoly = filename[:-4]+"_surveyLines_"+lineEdit_2+"_"+lineEdit_3+fb+rb+".shp"
        selfMT.dlg.lineEdit_5.setText(autoPoly)
        if os.path.exists(autoPoly):
            selfMT.dlg.exists2.setText("Existing file will be overwritten")
        else:
            selfMT.dlg.exists2.setText("")
      
    def select_input_file2(self):
        filename, _filter = QFileDialog.getOpenFileName(selfMT.dlg, "Select Output Waypoints shapefile ","", '*.shp')
        selfMT.dlg.lineEdit_4.setText(filename)
        if os.path.exists(filename):
            selfMT.dlg.exists1.setText("Existing file will be overwritten")
        else:
            selfMT.dlg.exists1.setText("")
            
    def select_input_file3(self):
        filename, _filter = QFileDialog.getOpenFileName(selfMT.dlg, "Select Output Survey Lines shapefile","", '*.shp')
        selfMT.dlg.lineEdit_5.setText(filename)
        if os.path.exists(filename):
            selfMT.dlg.exists2.setText("Existing file will be overwritten")
        else:
            selfMT.dlg.exists2.setText("")

    def indexChanged(self): 
        selectedLayerIndex = selfMT.dlg.comboBox.currentIndex()
        currentText = selfMT.dlg.comboBox.currentText()
        layers = QgsProject.instance().mapLayers().values()
        a=0
        filename="NULL"
        for layer in (layer1 for layer1 in layers if str(layer1.type())== "0" or str(layer1.type())== "LayerType.Vector"):
            if a == selectedLayerIndex:
                filename = str(layer.source())
            a=a+1
        filename1= selfMT.dlg.lineEdit_4.text()[0:len(currentText[:-4])]
        if filename1[0:3] == "_wa" or currentText[:-4] == filename1[0:len(currentText[:-4])]:
            filename = currentText
        #autofill
        lineEdit_2 = selfMT.dlg.lineEdit_2.text()
        if lineEdit_2 == "":
            lineEdit_2 = "0"
        lineEdit_3 = selfMT.dlg.lineEdit_3.text()
        if lineEdit_3 == "":
            lineEdit_3 = "100"
        if selfMT.dlg.reverseBox.isChecked():
            rb="_Rev"
        else:
            rb=""
        if selfMT.dlg.farEndBox.isChecked():
            fb="_Far"
        else:
            fb=""
        autoPoly = filename[:-4]+"_wayPoints_"+lineEdit_2+"_"+lineEdit_3+fb+rb+".shp"
        selfMT.dlg.lineEdit_4.setText(autoPoly)
        if os.path.exists(autoPoly):
            selfMT.dlg.exists1.setText("Existing file will be overwritten")
        else:
            selfMT.dlg.exists1.setText("")
        autoPoly = filename[:-4]+"_surveyLines_"+lineEdit_2+"_"+lineEdit_3+fb+rb+".shp"
        selfMT.dlg.lineEdit_5.setText(autoPoly)
        if os.path.exists(autoPoly):
            selfMT.dlg.exists2.setText("Existing file will be overwritten")
        else:
            selfMT.dlg.exists2.setText("")

    def help(self): 
        import webbrowser
        import marinetools
        MThelp = os.path.dirname(marinetools.__file__) + "\\surveylines\\SurveyLines.pdf"
        webbrowser.open(MThelp)  
        
    def run(self):
        """Run method that performs all the real work"""
        global selfMT

        # Create the dialog with elements (after translation) and keep reference
        # Only create GUI ONCE in callback, so that it will only load when the plugin is started
        if self.first_start == True:
            self.first_start = False
            self.dlg = SurveyLinesDialog()
            selfMT = self
            self.dlg.pushButton_1.clicked.connect(SurveyLines.select_input_file1)
            self.dlg.pushButton_2.clicked.connect(SurveyLines.select_input_file2)
            self.dlg.pushButton_3.clicked.connect(SurveyLines.select_input_file3)
            self.dlg.lineEdit_2.textChanged.connect(SurveyLines.indexChanged) 
            self.dlg.lineEdit_3.textChanged.connect(SurveyLines.indexChanged)
            self.dlg.farEndBox.stateChanged.connect(SurveyLines.indexChanged)
            self.dlg.reverseBox.stateChanged.connect(SurveyLines.indexChanged)
            self.dlg.helpButton.clicked.connect(SurveyLines.help) 
            self.dlg.comboBox.currentIndexChanged.connect(SurveyLines.indexChanged)
          
        # Fetch the currently loaded layers
        layers = QgsProject.instance().mapLayers().values()
        self.dlg.comboBox.clear() 
        # Populate the comboBox with names of all the raster loaded layers  (type="1")
        self.dlg.comboBox.addItems([layer.name() for layer in layers if str(layer.type())== "0" or str(layer.type())== "LayerType.Vector"])
        SurveyLines.indexChanged(self) 
            
        # show the dialog
        self.dlg.show()
        # Run the dialog event loop
        result = self.dlg.exec_()
        
        # See if OK was pressed
        if result:
            # Do something useful here - delete the line containing pass and
            # substitute with your code.
            selectedLayerIndex = self.dlg.comboBox.currentIndex()
            currentText = selfMT.dlg.comboBox.currentText()
            layers = QgsProject.instance().mapLayers().values()
            a=0
            for layer in (layer1 for layer1 in layers if str(layer1.type())== "0" or str(layer1.type())== "LayerType.Vector"):
                if a == selectedLayerIndex:
                    filename = str(layer.source())
                a=a+1
            if currentText not in filename:
                filename = currentText

            surveyedPolygon = filename 
           
            azimuth = self.dlg.lineEdit_2.text()  
            spacing = self.dlg.lineEdit_3.text()          
            outputWayPoints = self.dlg.lineEdit_4.text() 
            outputSurveyLines = self.dlg.lineEdit_5.text()
            if os.path.exists(outputWayPoints):
                os.remove(outputWayPoints)
            if os.path.exists(outputSurveyLines):
                os.remove(outputSurveyLines)
                      
            #make shapes not convex
            if not self.dlg.concaveBox.isChecked():
                result = processing.run("qgis:minimumboundinggeometry", {'INPUT':surveyedPolygon,'FIELD':'','TYPE':3,'OUTPUT':'TEMPORARY_OUTPUT'})
                layer = result['OUTPUT']
            else:
                #reference the original file, will be copied to another layer anyway
                layer  = surveyedPolygon
                
            if spacing == "":
                spacing = "100"
                
            """# Calculate buffer width
            if not self.dlg.outsideBox.isChecked():
                outputValue = 1
            else:
                outputValue = 0.01
            buffer = str(spacing/2.0) + " * " + str(outputValue)
            #Make buffers around the polygon
                      
            result = processing.run("native:fieldcalculator", {'INPUT':layer,'FIELD_NAME':'Bearing','FIELD_TYPE':0,'FIELD_LENGTH':0,'FIELD_PRECISION':0,'FORMULA':str(outputValue) +'* "perimeter"/100 ','OUTPUT':'TEMPORARY_OUTPUT'})
            bufferRes = result['OUTPUT']
            result = processing.run("native:buffer", {'INPUT':bufferRes,'DISTANCE': QgsProperty.fromExpression('"perimeter"'),'SEGMENTS':5,'END_CAP_STYLE':0,'JOIN_STYLE':0,'MITER_LIMIT':2,'DISSOLVE':False,'OUTPUT':'TEMPORARY_OUTPUT'})
            bufferLayer = result['OUTPUT']
            """
            if self.dlg.outsideBox.isChecked():
                buffer = 0.01
            else:
                buffer = float(spacing)/(-2.0)
            print(buffer)
            #Make buffers around the polygon
            result = processing.run("native:buffer", {'INPUT':layer,'DISTANCE': buffer,'SEGMENTS':5,'END_CAP_STYLE':0,'JOIN_STYLE':0,'MITER_LIMIT':2,'DISSOLVE':False,'OUTPUT':'TEMPORARY_OUTPUT'})
            bufferLayer = result['OUTPUT']

            result = processing.run("qgis:exportaddgeometrycolumns", {'INPUT':bufferLayer,'CALC_METHOD':1,'OUTPUT':'TEMPORARY_OUTPUT'})
            geomAdded = result['OUTPUT']
            #Place a centroid in the shape
            result = processing.run("native:centroids", {'INPUT':geomAdded,'ALL_PARTS':False,'OUTPUT':'TEMPORARY_OUTPUT'})
            centroidLayer = result['OUTPUT']
          
            #Replace null values
            if azimuth == "":
                azimuth = 0
            
            #Calculate the bearing field and add it to the centroid
            if not self.dlg.farEndBox.isChecked():
                bearing = float(azimuth)
            else:
                bearing = float(azimuth) + 180
                
            result = processing.run("native:fieldcalculator", {'INPUT':centroidLayer,'FIELD_NAME':'Bearing','FIELD_TYPE':0,'FIELD_LENGTH':0,'FIELD_PRECISION':0,'FORMULA':bearing,'OUTPUT':'TEMPORARY_OUTPUT'})
            calcBearingLayer = result['OUTPUT']
            
            #add a perpend field value
            result = processing.run("native:fieldcalculator", {'INPUT':calcBearingLayer,'FIELD_NAME':'Perpend','FIELD_TYPE':0,'FIELD_LENGTH':0,'FIELD_PRECISION':0,'FORMULA': '"Bearing" - 135.0','OUTPUT':'TEMPORARY_OUTPUT'})
            calcPerpLayer = result['OUTPUT']
            
            #make the projected line at the correct angle and distance
            result = processing.run("native:projectpointcartesian", {'INPUT':calcPerpLayer,'BEARING':QgsProperty.fromExpression('"Perpend"'),'DISTANCE':QgsProperty.fromExpression('"perimeter"'),'OUTPUT':'TEMPORARY_OUTPUT'})             
            projectedPoint = result['OUTPUT']
            result = processing.run("native:shortestline", {'SOURCE': calcPerpLayer,'DESTINATION':projectedPoint,'METHOD':1,'NEIGHBORS':1,'DISTANCE':None,'OUTPUT':'TEMPORARY_OUTPUT'})
            tempLine1 = result['OUTPUT']
            
            #Find center of the created line
            result = processing.run("native:centroids", {'INPUT':tempLine1,'ALL_PARTS':False,'OUTPUT':'TEMPORARY_OUTPUT'})
            tempLine1Centroid = result['OUTPUT']
            #set the perpendicular value to the bearing 
            result = processing.run("native:fieldcalculator", {'INPUT':tempLine1Centroid,'FIELD_NAME':'Perpend','FIELD_TYPE':0,'FIELD_LENGTH':0,'FIELD_PRECISION':0,'FORMULA':' "Bearing" ','OUTPUT':'TEMPORARY_OUTPUT'})
            temp1CentPerp = result['OUTPUT']
            
            if not self.dlg.farEndBox.isChecked():
                outputValue = float(azimuth) + 90
            else:
                outputValue = float(azimuth) - 90
                
            result = processing.run("native:fieldcalculator", {'INPUT':temp1CentPerp,'FIELD_NAME':'Perpend','FIELD_TYPE':0,'FIELD_LENGTH':0,'FIELD_PRECISION':0,'FORMULA':outputValue,'OUTPUT':'TEMPORARY_OUTPUT'})
            temp1CentPerp2 = result['OUTPUT']    
                
            #make the projected line at the correct angle and distance
            result = processing.run("native:projectpointcartesian", {'INPUT':temp1CentPerp2,'BEARING':QgsProperty.fromExpression('"Perpend"'),'DISTANCE':QgsProperty.fromExpression('"perimeter"'),'OUTPUT':'TEMPORARY_OUTPUT'})             
            tempPoint2 = result['OUTPUT']
            result = processing.run("native:shortestline", {'SOURCE': temp1CentPerp2,'DESTINATION':tempPoint2,'METHOD':1,'NEIGHBORS':1,'DISTANCE':None,'OUTPUT':'TEMPORARY_OUTPUT'})
            tempLine2Int = result['OUTPUT']
          
           
            # The formula for the distances isnt variable by the box yet. 
            result = processing.run("native:fieldcalculator", {'INPUT':tempLine2Int,'FIELD_NAME':'lineDist','FIELD_TYPE':0,'FIELD_LENGTH':0,'FIELD_PRECISION':0,'FORMULA':spacing,'OUTPUT':'TEMPORARY_OUTPUT'})
            tempLine2 = result['OUTPUT']   
            result = processing.run("native:pointsalonglines", {'INPUT':tempLine2,'DISTANCE':QgsProperty.fromExpression('lineDist'),'START_OFFSET':0,'END_OFFSET':0,'OUTPUT':'TEMPORARY_OUTPUT'})
            pointLayer1 = result['OUTPUT']
            result = processing.run("native:fieldcalculator", {'INPUT':pointLayer1,'FIELD_NAME':'Perpend','FIELD_TYPE':0,'FIELD_LENGTH':0,'FIELD_PRECISION':0,'FORMULA':'"Perpend"-90','OUTPUT':'TEMPORARY_OUTPUT'})
            pointLayer1Perp = result['OUTPUT']
            
           
            result = processing.run("native:projectpointcartesian", {'INPUT':pointLayer1Perp,'BEARING':QgsProperty.fromExpression('"Perpend"'),'DISTANCE':QgsProperty.fromExpression('"perimeter"'),'OUTPUT':'TEMPORARY_OUTPUT'})
            pointLayer2 = result['OUTPUT']
            result = processing.run("native:shortestline", {'SOURCE':pointLayer1Perp,'DESTINATION':pointLayer2,'METHOD':0,'NEIGHBORS':1,'DISTANCE':None,'OUTPUT':'TEMPORARY_OUTPUT'})
            endingLines = result['OUTPUT']
          
            result = processing.run("native:clip", {'INPUT':endingLines,'OVERLAY':bufferLayer,'OUTPUT':'TEMPORARY_OUTPUT'})
            clipResult = result['OUTPUT']
            #get the points from the vertices
            result = processing.run("native:extractvertices", {'INPUT':clipResult,'OUTPUT':'TEMPORARY_OUTPUT'})
            vertices= result['OUTPUT']
            result = processing.run("native:fieldcalculator", {'INPUT':vertices,'FIELD_NAME':'Ident','FIELD_TYPE':1,'FIELD_LENGTH':0,'FIELD_PRECISION':0,'FORMULA':'@id','OUTPUT':'TEMPORARY_OUTPUT'})
            resultLayer = result['OUTPUT']
            
            result = processing.run("native:addxyfields", {'INPUT':resultLayer,'CRS':QgsCoordinateReferenceSystem('EPSG:4326'),'PREFIX':'','OUTPUT':'TEMPORARY_OUTPUT'})
            addedResult = result['OUTPUT']
            result = processing.run("native:fieldcalculator", {'INPUT':addedResult,'FIELD_NAME':'LONG_DEG','FIELD_TYPE':0,'FIELD_LENGTH':0,'FIELD_PRECISION':0,'FORMULA':'floor( "x" )','OUTPUT':'TEMPORARY_OUTPUT'})
            longDegResult = result['OUTPUT']
            result = processing.run("native:fieldcalculator", {'INPUT':longDegResult,'FIELD_NAME':'LONG_MIN','FIELD_TYPE':0,'FIELD_LENGTH':6,'FIELD_PRECISION':3,'FORMULA':' abs( "LONG_DEG" - "x" )*60','OUTPUT':'TEMPORARY_OUTPUT'})
            longMinResult = result['OUTPUT']
            result = processing.run("native:fieldcalculator", {'INPUT':longMinResult,'FIELD_NAME':'LAT_DEG','FIELD_TYPE':0,'FIELD_LENGTH':0,'FIELD_PRECISION':0,'FORMULA':' floor(  "y" )','OUTPUT':'TEMPORARY_OUTPUT'})
            latDegResult = result['OUTPUT']
            result = processing.run("native:fieldcalculator", {'INPUT':latDegResult,'FIELD_NAME':'LAT_MIN','FIELD_TYPE':0,'FIELD_LENGTH':6,'FIELD_PRECISION':3,'FORMULA':' abs( "LAT_DEG" - "y" )*60','OUTPUT':'TEMPORARY_OUTPUT'})
            latMinResult =  result['OUTPUT']
            
            result = processing.run("native:fieldcalculator", {'INPUT':latMinResult,'FIELD_NAME':'tempOrdering','FIELD_TYPE':1,'FIELD_LENGTH':0,'FIELD_PRECISION':0,'FORMULA':'CASE\r\nWHEN "Ident"%4 =0 THEN "Ident"-1\r\nWHEN "Ident"%4 =3 THEN "Ident"+1\r\nELSE "Ident"\r\nEND','OUTPUT':'TEMPORARY_OUTPUT'})
            swapped = result['OUTPUT']
    
            if self.dlg.reverseBox.isChecked():
                result = processing.run("native:fieldcalculator", {'INPUT':swapped,'FIELD_NAME':'order','FIELD_TYPE':1,'FIELD_LENGTH':0,'FIELD_PRECISION':0,'FORMULA':'1000 - "tempOrdering"','OUTPUT':'TEMPORARY_OUTPUT'})
                swapped = result['OUTPUT']
            else:
                result = processing.run("native:fieldcalculator", {'INPUT':swapped,'FIELD_NAME':'order','FIELD_TYPE':1,'FIELD_LENGTH':0,'FIELD_PRECISION':0,'FORMULA':'"tempOrdering"','OUTPUT':'TEMPORARY_OUTPUT'})
                swapped = result['OUTPUT']
                
            processing.run("native:retainfields", {'INPUT':swapped,'FIELDS':['y','LONG_DEG','LONG_MIN','LAT_DEG','x','LAT_MIN','order'],'OUTPUT':outputWayPoints})
            
            processing.run("native:pointstopath", {'INPUT':outputWayPoints,'CLOSE_PATH':False,'ORDER_EXPRESSION':'order','NATURAL_SORT':False,'GROUP_EXPRESSION':'','OUTPUT':outputSurveyLines})

            fname = os.path.dirname(str(outputSurveyLines))
            vlayer = QgsVectorLayer(str(outputSurveyLines), str(outputSurveyLines[len(fname)+1:]), "ogr")
            QgsProject.instance().addMapLayer(vlayer)
            
            fname = os.path.dirname(str(outputWayPoints))
            vlayer = QgsVectorLayer(str(outputWayPoints), str(outputWayPoints[len(fname)+1:]), "ogr")
            QgsProject.instance().addMapLayer(vlayer)
      