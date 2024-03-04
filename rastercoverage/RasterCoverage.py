# -*- coding: utf-8 -*-
"""
/***************************************************************************
 RasterCoverage
                                 A QGIS plugin
 Makes polygon of area of raster data
 Generated by Plugin Builder: http://g-sherman.github.io/Qgis-Plugin-Builder/
                              -------------------
        begin                : 2023-09-04
        git sha              : $Format:%H$
        copyright            : (C) 2023 by Tim Le Bas
        email                : tlb@noc.ac.uk
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
from qgis.PyQt.QtWidgets import QAction
from qgis.PyQt.QtWidgets import QAction,QFileDialog 
from qgis.core import QgsProject 

# Initialize Qt resources from file resources.py
# Import the code for the dialog
from .RasterCoverage_dialog import RasterCoverageDialog
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
import processing

class RasterCoverage:
    """QGIS Plugin Implementation."""

    def select_input_file1(self): 
        filename, _filter = QFileDialog.getOpenFileName(selfMT.dlg, "Select input bathymetry file ","", '*.img *.tif') 
        selfMT.dlg.lineEdit_1.setText(filename) 
        autoPoly = filename[:-4]+"_coverage.shp"
        selfMT.dlg.lineEdit_4.setText(autoPoly)
        if os.path.exists(autoPoly):
            selfMT.dlg.exists1.setText("Existing file will be overwritten")
        else:
            selfMT.dlg.exists1.setText("")

    def select_input_file4(self): 
        filename, _filter = QFileDialog.getSaveFileName(selfMT.dlg, "Select output contours shapefile","", '*.shp') 
        selfMT.dlg.lineEdit_4.setText(filename) 
        if os.path.exists(filename):
            selfMT.dlg.exists1.setText("Existing file will be overwritten")
        else:
            selfMT.dlg.exists1.setText("")

    def help(self): 
        import webbrowser
        import marinetools
        MThelp = os.path.dirname(marinetools.__file__) + "\\rastercoverage\\RasterCoverage.pdf"
        webbrowser.open(MThelp)  
        
    def run(self):
        import random
        global selfMT
        """Run method that performs all the real work"""

        # Create the dialog with elements (after translation) and keep reference
        # Only create GUI ONCE in callback, so that it will only load when the plugin is started
        if self.first_start == True:
            self.first_start = False
            self.dlg = RasterCoverageDialog()
            selfMT = self
            self.dlg.pushButton_1.clicked.connect(RasterCoverage.select_input_file1) 
            self.dlg.pushButton_4.clicked.connect(RasterCoverage.select_input_file4) 
            self.dlg.helpButton.clicked.connect(RasterCoverage.help) 
           
        # Fetch the currently loaded layers
        layers = QgsProject.instance().layerTreeRoot().children() 
        
        # show the dialog
        self.dlg.show()
        # Run the dialog event loop
        result = self.dlg.exec_()
        
        # See if OK was pressed

        if result:
            import glob
            filename1 = self.dlg.lineEdit_1.text()  
            filename4 = self.dlg.lineEdit_4.text()  
            if os.path.exists(filename4):
                os.remove(filename4)
             
            result = processing.run("gdal:polygonize", {'INPUT':filename1,'BAND':1,'FIELD':'DN','EIGHT_CONNECTEDNESS':False,'EXTRA':'','OUTPUT':'TEMPORARY_OUTPUT'})
            polyCov = result['OUTPUT']
            processing.run("native:aggregate", {'INPUT':polyCov,'GROUP_BY':'NULL','AGGREGATES':[{'aggregate': 'sum','delimiter': ',','input': '"fid"','length': 0,'name': 'fid','precision': 0,'sub_type': 0,'type': 4,'type_name': 'int8'},{'aggregate': 'sum','delimiter': ',','input': '"DN"','length': 0,'name': 'DN','precision': 0,'sub_type': 0,'type': 2,'type_name': 'integer'}],'OUTPUT':filename4})
            
            fname = os.path.dirname(str(filename4))
            vlayer = QgsVectorLayer(str(filename4), str(filename4[len(fname)+1:]), "ogr")
            QgsProject.instance().addMapLayer(vlayer)

            pass

