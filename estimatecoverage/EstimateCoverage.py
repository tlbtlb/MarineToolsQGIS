# -*- coding: utf-8 -*-
"""
/***************************************************************************
 EstimateCoverage
                                 A QGIS plugin
 Estimate the coverage of a beam 
 Generated by Plugin Builder: http://g-sherman.github.io/Qgis-Plugin-Builder/
                              -------------------
        begin                : 2023-07-24
        git sha              : $Format:%H$
        copyright            : (C) 2023 by Annika
        email                : abc1g22@soton.ac.uk
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
from qgis.PyQt.QtWidgets import QAction,QFileDialog # added
from qgis.core import QgsProject # added

# Import the code for the dialog
from .EstimateCoverage_dialog import EstimateCoverageDialog
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

import processing
import sys
import traceback
import os


class EstimateCoverage:
    """QGIS Plugin Implementation."""

    def select_input_file1(self): # added
        filename, _filter = QFileDialog.getOpenFileName(selfMT.dlg, "Select input raster background bathymetry","", '*.img *.tif') # added
        selfMT.dlg.lineEdit_1.setText(filename) # added
      
    def select_input_file2(self): # added
        filename, _filter = QFileDialog.getOpenFileName(selfMT.dlg, "Select input line of track","", '*.shp') # added
        selfMT.dlg.lineEdit_2.setText(filename) # added
        #autofill
        autoPoly = filename[:-4]+"_coverage.shp"
        selfMT.dlg.lineEdit_4.setText(autoPoly)
        if os.path.exists(autoPoly):
            selfMT.dlg.exists1.setText("Existing file will be overwritten")
        else:
            selfMT.dlg.exists1.setText("")
        
    def select_input_file4(self): # added
        filename, _filter = QFileDialog.getSaveFileName(selfMT.dlg, "Define output coveragLete polygon","", '*.shp') # added
        selfMT.dlg.lineEdit_4.setText(filename) # added
        if os.path.exists(filename):
            selfMT.dlg.exists1.setText("Existing file will be overwritten")
        else:
            selfMT.dlg.exists1.setText("")
 
    def help(self): 
        import webbrowser
        import marinetools
        MThelp = os.path.dirname(marinetools.__file__) + "\\estimatecoverage\\EstimateCoverage.pdf"
        webbrowser.open(MThelp)  
        
    def run(self):
        """Run method that performs all the real work"""
        global selfMT

        # Create the dialog with elements (after translation) and keep reference
        # Only create GUI ONCE in callback, so that it will only load when the plugin is started
        if self.first_start == True:
            self.first_start = False
            self.dlg = EstimateCoverageDialog()
            selfMT = self
            self.dlg.pushButton_1.clicked.connect(EstimateCoverage.select_input_file1) # added
            self.dlg.pushButton_2.clicked.connect(EstimateCoverage.select_input_file2) # added
            self.dlg.pushButton_4.clicked.connect(EstimateCoverage.select_input_file4) # added
            self.dlg.helpButton.clicked.connect(EstimateCoverage.help) 

        # Fetch the currently loaded layers
        layers = QgsProject.instance().layerTreeRoot().children() # added
            
        # show the dialog
        self.dlg.show()
        # Run the dialog event loop
        result = self.dlg.exec_()
        # See if OK was pressed
        if result:
            # Do something useful here - delete the line containing pass and
            # substitute with your code.
            
            inputRaster = self.dlg.lineEdit_1.text()  
            tracklines = self.dlg.lineEdit_2.text()  
            swathAng = self.dlg.lineEdit_3.text()            
            outStencil = self.dlg.lineEdit_4.text()
            if os.path.exists(outStencil):
                os.remove(outStencil)
                
            if swathAng.isnumeric():
                swathAng = int(swathAng)
            else:
                swathAng = 140
            
            newdir = str(os.path.dirname(tracklines) + "/tempMT")
            if not os.path.exists(newdir):
                os.mkdir(newdir)
             
            import random
            alphabet = 'ZYXWVUTSRQPONMLKJIHGFEDCBA0987654321'
            rand = alphabet[random.randint(1,25)] + alphabet[random.randint(1,35)] + alphabet[random.randint(1,35)] + alphabet[random.randint(1,35)]
            
            tempfile3 = newdir + "/tempoutputpoints"+rand+".shp"

            stats = processing.run("native:rasterlayerstatistics", {'INPUT':str(inputRaster),'BAND':1})
            InputMean = stats["MEAN"]
            InputStd = stats["STD_DEV"]
            pointsGap = abs(InputMean)- abs(InputStd)
            #Generate points along lines
            result = processing.run("native:pointsalonglines", {'INPUT':tracklines,'DISTANCE':pointsGap,'START_OFFSET':0,'END_OFFSET':0,'OUTPUT':'TEMPORARY_OUTPUT'})
            tempfile2 = result['OUTPUT']
            
            #result = processing.run("native:rastersampling", {'INPUT':tempfile2,'RASTERCOPY':inputRaster,'COLUMN_PREFIX':'rasterValues','OUTPUT':'TEMPORARY_OUTPUT'})
            #tempfile3 = result['OUTPUT']
            processing.run("native:rastersampling", {'INPUT':tempfile2,'RASTERCOPY':inputRaster,'COLUMN_PREFIX':'rasterValues','OUTPUT':tempfile3})

            #calculate coverage width
            import math 
            tanTheta = math.tan((math.pi/180)*float(swathAng/2))
            coverageFormula = ' -("rasterValu") * ' + str(tanTheta) 
            
            result = processing.run("native:fieldcalculator", {'INPUT':tempfile3,'FIELD_NAME':'coverage','FIELD_TYPE':0,
            'FIELD_LENGTH':0,'FIELD_PRECISION':0,'FORMULA':coverageFormula,'OUTPUT':'TEMPORARY_OUTPUT'})
            tempfile4 = result['OUTPUT']

            processing.run("native:buffer", {'INPUT':tempfile4,'DISTANCE':QgsProperty.fromExpression('"coverage"'),'SEGMENTS':5,'END_CAP_STYLE':0,'JOIN_STYLE':0,'MITER_LIMIT':2,
            'DISSOLVE':True,'OUTPUT':outStencil})
            
            fname = os.path.dirname(str(outStencil))
            vlayer = QgsVectorLayer(str(outStencil), str(outStencil[len(fname)+1:]), "ogr")
            QgsProject.instance().addMapLayer(vlayer)
      
            try:
                for f in glob.glob(str(tempfile3[:-4]) + "*"):
                    os.remove(f)
            except:
                print(tempfile3 + " not removed")
              
            
           
