# -*- coding: utf-8 -*-
"""
/***************************************************************************
 RemoveValues
                                 A QGIS plugin
 This plugin removes specified values from a raster image
 Generated by Plugin Builder: http://g-sherman.github.io/Qgis-Plugin-Builder/
                              -------------------
        begin                : 2023-07-27
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
from qgis.PyQt.QtWidgets import QAction,QFileDialog,QMessageBox 
from qgis.core import QgsProject 

from PyQt5.QtGui import *

# Import the code for the dialog
from .RemoveValues_dialog import RemoveValuesDialog
import os.path

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
from qgis.core import QgsColorRampShader
from qgis.core import QgsRasterFileWriter
from qgis.core import QgsRasterPipe

from qgis.analysis import QgsRasterCalculator, QgsRasterCalculatorEntry

import processing
import sys
import traceback
import os
import random

class LoadingScreenDlg:
    """Loading screen animation."""
    from qgis.PyQt.QtWidgets import QDialog, QLabel 
    from qgis.PyQt.QtGui import QMovie, QPalette, QColor

    def __init__(self, gif_path):
        self.dlg = self.QDialog()
        self.dlg.setWindowTitle("Please Wait")
        self.dlg.setWindowModality(False)
        self.dlg.setFixedSize(200, 100)
        pal = self.QPalette()
        role = self.QPalette.Background
        pal.setColor(role, self.QColor(255, 255, 255))
        self.dlg.setPalette(pal)
        self.label_animation = self.QLabel(self.dlg)
        self.movie = self.QMovie(gif_path)
        self.label_animation.setMovie(self.movie)

    def start_animation(self):
        self.movie.start()
        self.dlg.show()
        return

    def stop_animation(self):
        self.movie.stop()
        self.dlg.done(0)       

class RemoveValues:
    """QGIS Plugin Implementation."""

    def select_input_file1(self): 
        filename, _filter = QFileDialog.getOpenFileName(selfMT.dlg, "Select input raster file","", '*.img *.tif') 
        selfMT.dlg.inputCombo.clear() 
        selfMT.dlg.inputCombo.insertItem(0,filename)
        selfMT.dlg.inputCombo.setCurrentIndex(0)
        #autofill
        autoPoly = filename[:-4]+"_out.img"
        selfMT.dlg.outputRaster.setText(autoPoly)
        if os.path.exists(autoPoly):
            selfMT.dlg.exists1.setText("Existing file will be overwritten")
        else:
            selfMT.dlg.exists1.setText("")
      
    def select_input_file2(self): 
        filename, _filter = QFileDialog.getSaveFileName(selfMT.dlg, "Select output raster file","", '*.img *.tif') 
        selfMT.dlg.outputRaster.setText(filename) 
        if os.path.exists(filename):
            selfMT.dlg.exists1.setText("Existing file will be overwritten")
        else:
            selfMT.dlg.exists1.setText("")
        
    def indexChanged(self): 
        selectedLayerIndex = selfMT.dlg.inputCombo.currentIndex()
        currentText = selfMT.dlg.inputCombo.currentText()
        layers = QgsProject.instance().mapLayers().values()
        a=0
        filename="NULL"
        for layer in (layer1 for layer1 in layers if str(layer1.type())== "1" or str(layer1.type())== "LayerType.Raster"):
            if a == selectedLayerIndex:
                filename = str(layer.source())
            a=a+1
        filename1= selfMT.dlg.outputRaster.text()[0:len(currentText[:-4])]
        if filename1[0:3] == "_ou" or currentText[:-4] == filename1[0:len(currentText[:-4])]:
            filename = currentText
        #autofill
        autoPoly = filename[:-4]+"_out.img"
        selfMT.dlg.outputRaster.setText(autoPoly)
        if os.path.exists(autoPoly):
            selfMT.dlg.exists1.setText("Existing file will be overwritten")
        else:
            selfMT.dlg.exists1.setText("")
      
    def help(self): 
        import webbrowser
        import marinetools
        MThelp = os.path.dirname(marinetools.__file__) + "\\removevalues\\RemoveValues.pdf"
        webbrowser.open(MThelp)  

    def removeOutliers(inFile, outFile):
        """ Removes values outside of certain range"""
        
        high = selfMT.dlg.highestInput1.text()
        low = selfMT.dlg.lowestInput1.text()
        if high == "":
            high = "9999"
        if low == "":
            low = "-9999"

        #take the file and make it usable by raster calculator
        tempName = os.path.split(inFile)[1]
        name = tempName.split('.')[0]
        layerRef = name + '@1'
        
        #Form the expression
        bool = '("'+ layerRef + '"> ' + low + ') AND("'+ layerRef + '"  < ' + high + ')'
        expr = '(1/' + bool + ') * "'+ layerRef + '"'
        #print(bool)
        #print(expr)
        #Calculate the new raster
        infile = QgsRasterLayer(inFile)
        crs = infile.crs()
        extent = infile.extent()
        processing.run("qgis:rastercalculator", {'EXPRESSION':expr,'LAYERS':[inFile],'CELLSIZE':None,'EXTENT':extent,'CRS':None,'OUTPUT':outFile})
        
    def removeSingle(inFile, outFile):
        """Removes a single value and replaces it with null"""
    
        value = selfMT.dlg.singleInput1.text()
        if value == "":
            value = "0"
  
        #take the file and make it usable by raster calculator
        tempName = os.path.split(inFile)[1]
        name = tempName.split('.')[0]
        layerRef = name + '@1'
        
        #Form the expression
        bool = '("'+ layerRef + '" != ' + value + ')'
        expr = '(0/' + bool + ') + "'+ layerRef + '"'
        
        #print(bool)
        #print(expr)
        #Calculate the new raster 
        infile = QgsRasterLayer(inFile)
        crs = infile.crs()
        extent = infile.extent()
        processing.run("qgis:rastercalculator", {'EXPRESSION':expr,'LAYERS':[inFile],'CELLSIZE':None,'EXTENT':extent,'CRS':crs,'OUTPUT':outFile})

    def removeRange(inFile, outFile):
        #Turn a range of values into null values
        start = selfMT.dlg.startInput1.text()
        end = selfMT.dlg.endInput1.text()
        if start == "":
            start = "-9999"
        if end == "":
            end = "9999"

        #take the file and make it usable by raster calculator
        tempName = os.path.split(inFile)[1]
        name = tempName.split('.')[0]
        layerRef = name + '@1'

        #Form the expression
        bool = '("'+ layerRef + '"<= ' + start + ') OR ("'+ layerRef + '">= ' + end + ')'
        expr = '(0/(' + bool + ')) + "'+ layerRef + '"'
        #print(bool)
        #print(expr)
         #Calculate the new raster 
        infile = QgsRasterLayer(inFile)
        crs = infile.crs()
        extent = infile.extent()
        processing.run("qgis:rastercalculator", {'EXPRESSION':expr,'LAYERS':[inFile],'CELLSIZE':None,'EXTENT':extent,'CRS':crs,'OUTPUT':outFile})
    
    def run(self):
        """Run method that performs all the real work"""
        global selfMT

        # Create the dialog with elements (after translation) and keep reference
        # Only create GUI ONCE in callback, so that it will only load when the plugin is started

        if self.first_start == True:
            self.first_start = False
            self.dlg = RemoveValuesDialog()
            selfMT = self
            self.dlg.inButton.clicked.connect(RemoveValues.select_input_file1) 
            self.dlg.outButton.clicked.connect(RemoveValues.select_input_file2) 
            self.dlg.helpButton.clicked.connect(RemoveValues.help) 
            self.dlg.inputCombo.currentIndexChanged.connect(RemoveValues.indexChanged)

        layers = QgsProject.instance().mapLayers().values()
        self.dlg.inputCombo.clear() 
        self.dlg.inputCombo.addItems([layer.name() for layer in layers if str(layer.type())== "1" or str(layer.type())== "LayerType.Raster"])
        RemoveValues.indexChanged(self) 
          # show the dialog
        self.dlg.show()
        # Run the dialog event loop
        result = self.dlg.exec_()
        
        # See if OK was pressed
        if result:

            selectedLayerIndex = self.dlg.inputCombo.currentIndex()
            currentText = selfMT.dlg.inputCombo.currentText()
            layers = QgsProject.instance().mapLayers().values()
            a=0
            for layer in (layer1 for layer1 in layers if str(layer1.type())== "1" or str(layer1.type())== "LayerType.Raster"):
                if a == selectedLayerIndex:
                    filename = str(layer.source())
                a=a+1
            if currentText not in filename:
                filename = currentText

            inFile = filename
            outFile = self.dlg.outputRaster.text()

            plugin_dir = os.path.dirname(__file__)
            gif_path = os.path.join(plugin_dir, "loading.gif")
            self.loading_screen = LoadingScreenDlg(gif_path)  # init loading dlg
            self.loading_screen.start_animation()  # start loading dlg

            if os.path.exists(outFile):
                os.remove(outFile)
            
            if self.dlg.nullButton.isChecked():
                #Fill the null values if it is a single value
                fillValue = self.dlg.nullInput1.text()
                if fillValue == "":
                    fillValue = "0"
                processing.run("native:fillnodata", {'INPUT':inFile,'BAND':1,'FILL_VALUE':fillValue,'OUTPUT':outFile})
            elif self.dlg.outLiersButton.isChecked():
                RemoveValues.removeOutliers(inFile, outFile)
            elif self.dlg.singleButton.isChecked():
                RemoveValues.removeSingle(inFile, outFile)
            elif self.dlg.rangeButton.isChecked():
                RemoveValues.removeRange(inFile, outFile)

            self.loading_screen.stop_animation()
                
            if os.path.exists(outFile):
                fname = os.path.dirname(str(outFile))
                vlayer = QgsRasterLayer(str(outFile), str(outFile[len(fname)+1:]))
                QgsProject.instance().addMapLayer(vlayer)
