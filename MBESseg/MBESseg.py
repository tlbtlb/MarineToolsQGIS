# -*- coding: utf-8 -*-
"""
/***************************************************************************
 MBESseg
                                 A QGIS plugin
 Multibeam Object Based Image Analysis -making polygon interpretation
 Generated by Plugin Builder: http://g-sherman.github.io/Qgis-Plugin-Builder/
                              -------------------
        begin                : 2024-02-02
        git sha              : $Format:%H$
        copyright            : (C) 2024 by National Oceanography Centre, UK.
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
from qgis.PyQt.QtWidgets import QAction,QFileDialog,QMessageBox 
from qgis.core import QgsProject 


# Import the code for the dialog
from .MBESseg_dialog import MBESsegDialog
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

class MBESseg:
    """QGIS Plugin Implementation."""

    def select_output_file(self): 
        filename, _filter = QFileDialog.getSaveFileName(selfMT.dlg, "Select output polygon file ","", '*.shp') 
        selfMT.dlg.outputPolys.setText(filename) 
        if os.path.exists(filename):
            selfMT.dlg.exists1.setText("Existing file will be overwritten")
        else:
            selfMT.dlg.exists1.setText("")
         
    def select_inputBathy_file(self): 
        filename, _filter = QFileDialog.getOpenFileName(selfMT.dlg, "Select input Bathymetry file ","", '*.img *.tif') 
        selfMT.dlg.mapFileBathy.setText(filename) 
        #autofill
        Clusters = selfMT.dlg.Clusters.text()
        if Clusters == "":
            Clusters = "10"
        MinimumSize = selfMT.dlg.MinimumSize.text()
        if MinimumSize == "":
            MinimumSize = "30"
        autoPoly = filename[:-4]+"_MBESseg_"+Clusters+"_"+MinimumSize+".shp"
        selfMT.dlg.outputPolys.setText(autoPoly)
        selfMT.dlg.mapFileBathyCombo.clear() 
        if os.path.exists(autoPoly):
            selfMT.dlg.exists1.setText("Existing file will be overwritten")
        else:
            selfMT.dlg.exists1.setText("")

    def select_inputBacks_file(self): 
        selfMT.dlg.mapFileBacks.clear()
        filename, _filter = QFileDialog.getOpenFileName(selfMT.dlg, "Select input Backscatter file ","", '*.img *.tif') 
        selfMT.dlg.mapFileBacks.setText(filename) 
        selfMT.dlg.mapFileBacksCombo.clear() 
        
    def updateName(self): 
        filename = selfMT.dlg.mapFileBathy.text()
        #autofill
        Clusters = selfMT.dlg.Clusters.text()
        if Clusters == "":
            Clusters = "10"
        MinimumSize = selfMT.dlg.MinimumSize.text()
        if MinimumSize == "":
            MinimumSize = "30"
        autoPoly = filename[:-4]+"_MBESseg_"+Clusters+"_"+MinimumSize+".shp"
        selfMT.dlg.outputPolys.setText(autoPoly)
        if os.path.exists(autoPoly):
            selfMT.dlg.exists1.setText("Existing file will be overwritten")
        else:
            selfMT.dlg.exists1.setText("")
        
    def help(self): 
        import webbrowser
        import marinetools
        MThelp = os.path.dirname(marinetools.__file__) + "\\MBESseg\\MBESseg.pdf"
        webbrowser.open(MThelp)  
 
    def otbInstall(self):
        import webbrowser
        import marinetools
        MThelp = os.path.dirname(marinetools.__file__) + "\\obia\\OTBinstall.pdf"
        webbrowser.open(MThelp)
        
    def run(self):
        """Run method that performs all the real work"""
        import tempfile,glob,os
        import random
        import shutil
        import math
        from marinetools.MBESseg.MBESseg_dialog import MBESsegDialog
        global selfMT

        # Create the dialog with elements (after translation) and keep reference
        # Only create GUI ONCE in callback, so that it will only load when the plugin is started
        if self.first_start == True:
            self.first_start = False
            self.dlg = MBESsegDialog()
            selfMT = self
            self.dlg.outputFilePolys.clicked.connect(MBESseg.select_output_file) 
            self.dlg.inputFileBacks.clicked.connect(MBESseg.select_inputBacks_file) 
            self.dlg.inputFileBathy.clicked.connect(MBESseg.select_inputBathy_file)
            self.dlg.Clusters.textChanged.connect(MBESseg.updateName) 
            self.dlg.MinimumSize.textChanged.connect(MBESseg.updateName) 
            self.dlg.helpButton.clicked.connect(MBESseg.help) 

        # Fetch the currently loaded layers
        layers = QgsProject.instance().layerTreeRoot().children() 
        # Clear the contents of the comboBox from previous runs
        # self.dlg = MBES_SegmentationDialog()
        self.dlg.mapFileBathyCombo.clear() 
        # Populate the comboBox with names of all the loaded layer   
        self.dlg.mapFileBathyCombo.addItems([layer.name() for layer in layers]) 
        # Clear the contents of the comboBox from previous runs
        self.dlg.mapFileBacksCombo.clear() 
        # Populate the comboBox with names of all the loaded layer   
        self.dlg.mapFileBacksCombo.addItems([layer.name() for layer in layers]) 
        
        # show the dialog
        self.dlg.show()
        # Run the dialog event loop
        result = self.dlg.exec_()
        
        # See if OK was pressed

        if result:
            OutputFilename = self.dlg.outputPolys.text()
            if OutputFilename == "":
                self.iface.messageBar().pushMessage("\nNo outputfile was given")
                return
            if os.path.exists(OutputFilename):
                os.remove(OutputFilename)
            InputBathyLine = self.dlg.mapFileBathy.text()
            InputBacksLine = self.dlg.mapFileBacks.text()
            if str(InputBathyLine) == "":
                selectedLayerIndex = self.dlg.mapFileBathyCombo.currentIndex()
                InputBathy = layers[selectedLayerIndex].layer()
            else:
                InputBathy = QgsRasterLayer(InputBathyLine,"Bathymetry") # change to Layer
                
            if str(InputBacksLine) == "":
                selectedLayerIndex = self.dlg.mapFileBacksCombo.currentIndex()
                InputBacks = layers[selectedLayerIndex].layer()
            else:
                InputBacks = QgsRasterLayer(InputBacksLine,"Backscatter") # change to Layer
          
            clusters = self.dlg.Clusters.text()
            MinSize = self.dlg.MinimumSize.text()
            if clusters == "":
                clusters = "10"
            if MinSize == "":
                MinSize = "30"
                
            try:
                processing.run("otb:ImageEnvelope", {'in':InputBathy,'out':'TEMPORARY_OUTPUT','sr':0,'elev.dem':'','elev.geoid':'','elev.default':0,'proj':''})
            except:
                QMessageBox.information(None, "Information:", "This tool requires (OrfeoToolBox (OTB) to be installed\nDownload from https://www.orfeo-toolbox.org/download/\n and install locally.")
                MBESseg.otbInstall(self)
                return
            
            newdir = str(os.path.dirname(OutputFilename) + "/tempMT")
            if not os.path.exists(newdir):
                os.mkdir(newdir)
            alphabet = '1234567890abcdefghijklmnopqrstuvwxyz'
            rand = alphabet[random.randint(1,25)] + alphabet[random.randint(1,35)] + alphabet[random.randint(1,35)] + alphabet[random.randint(1,35)]
            temp1 =str(os.path.dirname(OutputFilename) + "/tempMT/OTBkmeans.tif")
            temp2 =str(os.path.dirname(OutputFilename) + "/tempMT/OTBkmeansCentroids.dat")
            temp3 =str(os.path.dirname(OutputFilename) + "/tempMT/RAsieve.tif")
            temp4 =str(os.path.dirname(OutputFilename) + "/tempMT/VCpolygons"+rand+".shp")
            temp5 =str(os.path.dirname(OutputFilename) + "/tempMT/VGaggregate"+rand+".shp")
            temp5a =str(os.path.dirname(OutputFilename) + "/tempMT/VGaggregateA"+rand+".shp")
            temp5b =str(os.path.dirname(OutputFilename) + "/tempMT/VGaggregateB"+rand+".shp")
            temp5c =str(os.path.dirname(OutputFilename) + "/tempMT/VGaggregateC"+rand+".shp")
            temp6 =str(os.path.dirname(OutputFilename) + "/tempMT/GRAslope"+rand+".tif")
            temp7 =str(os.path.dirname(OutputFilename) + "/tempMT/GRAroughness"+rand+".tif")
            temp6a =str(os.path.dirname(OutputFilename) + "/tempMT/GRAslopeA.tif")
            temp7a =str(os.path.dirname(OutputFilename) + "/tempMT/GRAroughnessA.tif")
            temp8 =str(os.path.dirname(OutputFilename) + "/tempMT/GRMbuildvirtualraster.vrt")
            temp9 =str(os.path.dirname(OutputFilename) + "/tempMT/INBackscatterA.tif")

            print("Inputs")
            self.iface.messageBar().pushMessage("\nNumber of clusters = " + str(clusters)+"\n" +
                                                "Minimum size       = " + str(MinSize)+"\n" +
                                                "Output will be     " + str(OutputFilename))
            print(str(InputBathy))
            print(str(InputBacks))
            print(str(clusters))
            print(str(MinSize))
            print(str(OutputFilename))
            
            processing.run("native:slope", {'INPUT':InputBathy,'Z_FACTOR':1,'OUTPUT':temp6})
            processing.run("gdal:roughness", {'INPUT':InputBathy,'BAND':1,'COMPUTE_EDGES':False,'OPTIONS':'','OUTPUT':temp7})

            slopelayer = QgsRasterLayer(temp6,"slope")
            slopeStats = slopelayer.dataProvider().bandStatistics(1,QgsRasterBandStats.All)
            slopeMin = slopeStats.minimumValue
            slopeMax = slopeStats.maximumValue
            roughnesslayer = QgsRasterLayer(temp7,"roughness")
            roughnessStats = roughnesslayer.dataProvider().bandStatistics(1,QgsRasterBandStats.All)
            roughnessMin = roughnessStats.minimumValue
            roughnessMax = roughnessStats.maximumValue
            BackscatterStats = InputBacks.dataProvider().bandStatistics(1,QgsRasterBandStats.All)
            BackscatterMin = BackscatterStats.minimumValue
            BackscatterMax = BackscatterStats.maximumValue

            processing.run("grass7:r.rescale.eq", {'input':InputBacks,'from':[BackscatterMin,BackscatterMax],'to':[0,254],'output':temp9,'GRASS_REGION_PARAMETER':None,'GRASS_REGION_CELLSIZE_PARAMETER':0,'GRASS_RASTER_FORMAT_OPT':'','GRASS_RASTER_FORMAT_META':''})
            processing.run("grass7:r.rescale.eq", {'input':temp6,'from':[0.0,slopeMax],'to':[0,254],'output':temp6a,'GRASS_REGION_PARAMETER':None,'GRASS_REGION_CELLSIZE_PARAMETER':0,'GRASS_RASTER_FORMAT_OPT':'','GRASS_RASTER_FORMAT_META':''})
            processing.run("grass7:r.rescale.eq", {'input':temp7,'from':[roughnessMin,roughnessMax],'to':[0,254],'output':temp7a,'GRASS_REGION_PARAMETER':None,'GRASS_REGION_CELLSIZE_PARAMETER':0,'GRASS_RASTER_FORMAT_OPT':'','GRASS_RASTER_FORMAT_META':''})

            #self.iface.messageBar().pushMessage("Now Layering")
            processing.run("gdal:buildvirtualraster", {'INPUT':[temp6a,temp7a,temp9],'RESOLUTION':0,'SEPARATE':True,'PROJ_DIFFERENCE':False,'ADD_ALPHA':False,'ASSIGN_CRS':None,'RESAMPLING':0,'SRC_NODATA':'','EXTRA':'','OUTPUT':temp8})
            processing.run("otb:KMeansClassification", {'in':temp8,'out':temp1,'nc':clusters,'ts':100,'maxit':1000,'centroids.in':'','centroids.out':temp2,'sampler':'periodic','sampler.periodic.jitter':0,'vm':None,'nodatalabel':0,'cleanup':True,'rand':0,'outputpixeltype':5})
            remove_tif_files(temp6a)
            remove_tif_files(temp7a)
            
            power = math.log(int(MinSize),2)+1.0 # Increments of power of two sizes
            first = temp1
            for x in range(1,int(power)):
                next=str(os.path.dirname(OutputFilename) + "/RAsieve"+str(x)+".tif")
                Size = 2**x
                processing.run("gdal:sieve", {'INPUT':first,'THRESHOLD':Size,'EIGHT_CONNECTEDNESS':False,'NO_MASK':False,'MASK_LAYER':None,'EXTRA':'','OUTPUT':next})
                remove_tif_files(first)
                first = next
            processing.run("gdal:sieve", {'INPUT':first,'THRESHOLD':MinSize,'EIGHT_CONNECTEDNESS':False,'NO_MASK':False,'MASK_LAYER':None,'EXTRA':'','OUTPUT':temp3})
            remove_tif_files(first)

            processing.run("native:pixelstopolygons", {'INPUT_RASTER':temp3,'RASTER_BAND':1,'FIELD_NAME':'VALUE','OUTPUT':temp4})
            #self.iface.messageBar().pushMessage("Make polygons")
            processing.run("native:aggregate", {'INPUT':temp4,'GROUP_BY':'"VALUE"','AGGREGATES':[{'aggregate': 'mean','delimiter': ',','input': '"VALUE"','length': 20,'name': 'VALUE','precision': 8,'sub_type': 0,'type': 6,'type_name': 'double precision'}],'OUTPUT':temp5})
            processing.run("native:zonalstatisticsfb", {'INPUT':temp5,'INPUT_RASTER':InputBacks,'RASTER_BAND':1,'COLUMN_PREFIX':'backs_','STATISTICS':[2,4],'OUTPUT':temp5a})
            processing.run("native:zonalstatisticsfb", {'INPUT':temp5a,'INPUT_RASTER':temp6,'RASTER_BAND':1,'COLUMN_PREFIX':'slope_','STATISTICS':[2,4],'OUTPUT':temp5b})
            processing.run("native:zonalstatisticsfb", {'INPUT':temp5b,'INPUT_RASTER':temp7,'RASTER_BAND':1,'COLUMN_PREFIX':'rough_','STATISTICS':[2,4],'OUTPUT':temp5c})
            processing.run("native:multiparttosingleparts", {'INPUT':temp5c,'OUTPUT':OutputFilename})
            
            fname = os.path.dirname(str(OutputFilename))
            vlayer = QgsVectorLayer(str(OutputFilename), str(OutputFilename[len(fname)+1:]), "ogr")
            QgsProject.instance().addMapLayer(vlayer)
            colour_polygons_random(vlayer)
            
            QgsProject.instance().addMapLayer(slopelayer, False)
            QgsProject.instance().removeMapLayer(slopelayer.id())
            os.remove(temp6)
            QgsProject.instance().addMapLayer(roughnesslayer, False)
            QgsProject.instance().removeMapLayer(roughnesslayer.id())
            os.remove(temp7)
            remove_tif_files(temp3)
            remove_tif_files(temp9)
            remove_tif_files(temp1)
            os.remove(temp8)
            os.remove(temp2)
            
            remove_shp_files(temp4)
            remove_shp_files(temp5)
            remove_shp_files(temp5a)
            remove_shp_files(temp5b)
            remove_shp_files(temp5c)
            #shutil.rmtree(str(os.path.dirname(OutputFilename) + "/tempMT"))

            return

def remove_tif_files(tempfilename):
            import glob,os
            templayer = QgsRasterLayer(tempfilename,"templayer")
            QgsProject.instance().addMapLayer(templayer, False)
            QgsProject.instance().removeMapLayer(templayer.id())
            for f in glob.glob(tempfilename.replace(".tif",".*")):
                os.remove(f)
            return
        
def remove_shp_files(tempfilename):  # not working yet
            import glob,os
            from osgeo import ogr
            try:
                templayer = QgsVectorLayer(tempfilename,"templayer")
                QgsProject.instance().addMapLayer(templayer, True)
                QgsProject.instance().takeMapLayer(templayer)
                QgsProject.instance().removeMapLayer(templayer)
                #for f in glob.glob(tempfilename.replace(".shp",".*")):
                #    print (str(f))
                #    os.remove(f)
                
                #QgsProject.instance().reloadAllLayers()  # removes 2 of the 4 files
                #driver = ogr.GetDriverByName("ESRI Shapefile")
                #if os.path.exists(tempfilename):
                #     driver.DeleteDataSource(tempfilename)
            except:
                print(tempfilename," not deleted")

            return
        
def colour_polygons_random(layer):
    # provide file name index and field's unique values
    from random import randrange

    fni = layer.fields().indexFromName('VALUE')
    unique_values = layer.uniqueValues(fni)

    # fill categories
    categories = []
    for unique_value in unique_values:
        # initialize the default symbol for this geometry type
        symbol = QgsSymbol.defaultSymbol(layer.geometryType())
        # configure a symbol layer
        layer_style = {}
        layer_style['color'] = '%d, %d, %d' % (randrange(0, 256), randrange(0, 256), randrange(0, 256))
        layer_style['outline'] = '#000000'
        symbol_layer = QgsSimpleFillSymbolLayer.create(layer_style)
        # replace default symbol layer with the configured one
        if symbol_layer is not None:
            symbol.changeSymbolLayer(0, symbol_layer)
        # create renderer object
        category = QgsRendererCategory(unique_value, symbol, str(unique_value))
        # entry for the list of category items
        categories.append(category)
    # create renderer object
    renderer = QgsCategorizedSymbolRenderer('VALUE', categories)

    # assign the created renderer to the layer
    if renderer is not None:
        layer.setRenderer(renderer)

    layer.triggerRepaint()
    pass

