# -*- coding: utf-8 -*-
"""
/***************************************************************************
 OBIA
 A QGIS plugin
 Object Based Image Analysis
 Generated by Plugin Builder: http://g-sherman.github.io/Qgis-Plugin-Builder/
                              -------------------
        begin                : 2022-10-10
        git sha              : $Format:%H$
        copyright            : (C) 2022 by Tim Le Bas
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
from .OBIA_dialog import OBIADialog
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

import processing
import os
import os.path
import subprocess

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

class OBIA:
    """QGIS Plugin Implementation."""
    def select_input_file1(self): 
        filename, _filter = QFileDialog.getOpenFileName(selfMT.dlg, "Select input raster file 1","", '*.img *.tif') 
        selfMT.dlg.comboBox1.clear() 
        selfMT.dlg.comboBox1.insertItem(0,filename)
        selfMT.dlg.comboBox1.setCurrentIndex(0)
        #autofill
        Clusters = selfMT.dlg.Clusters.text()
        if Clusters == "":
            Clusters = "10"
        MinimumSize = selfMT.dlg.MinimumSize.text()
        if MinimumSize == "":
            MinimumSize = "30"
        autoPoly = filename[:-4]+"_OBIA_"+Clusters+"_"+MinimumSize+".shp"
        selfMT.dlg.lineEdit.setText(autoPoly)
        if os.path.exists(autoPoly):
            selfMT.dlg.exists1.setText("Existing file will be overwritten")
        else:
            selfMT.dlg.exists1.setText("")
        
    def select_input_file2(self): 
        filename, _filter = QFileDialog.getOpenFileName(selfMT.dlg, "Select input raster file 2","", '*.img *.tif') 
        selfMT.dlg.comboBox2.clear() 
        selfMT.dlg.comboBox2.insertItem(0,filename)
        selfMT.dlg.comboBox2.setCurrentIndex(0)
    def select_input_file3(self): 
        filename, _filter = QFileDialog.getOpenFileName(selfMT.dlg, "Select input raster file 3","", '*.img *.tif') 
        selfMT.dlg.comboBox3.clear() 
        selfMT.dlg.comboBox3.insertItem(0,filename)
        selfMT.dlg.comboBox3.setCurrentIndex(0)
    def select_input_file4(self): 
        filename, _filter = QFileDialog.getOpenFileName(selfMT.dlg, "Select input raster file 4","", '*.img *.tif') 
        selfMT.dlg.comboBox4.clear() 
        selfMT.dlg.comboBox4.insertItem(0,filename)
        selfMT.dlg.comboBox4.setCurrentIndex(0)
    def select_input_file5(self): 
        filename, _filter = QFileDialog.getOpenFileName(selfMT.dlg, "Select input raster file 5","", '*.img *.tif') 
        selfMT.dlg.comboBox5.clear() 
        selfMT.dlg.comboBox5.insertItem(0,filename)
        selfMT.dlg.comboBox5.setCurrentIndex(0)
    def select_output_file(self): 
        filename, _filter = QFileDialog.getSaveFileName(selfMT.dlg, "Select output file ","", '*.shp') 
        selfMT.dlg.lineEdit.setText(filename) 
        if os.path.exists(filename):
            selfMT.dlg.exists1.setText("Existing file will be overwritten")
        else:
            selfMT.dlg.exists1.setText("")

    def indexChanged(self): 
        selectedLayerIndex = selfMT.dlg.comboBox1.currentIndex()
        currentText = selfMT.dlg.comboBox1.currentText()
        layers = QgsProject.instance().mapLayers().values()
        a=0
        filename="NULL"
        #for layer in layers if str(layer.type())== "1":
        for layer in (layer1 for layer1 in layers if str(layer1.type())== "1" or str(layer1.type())== "LayerType.Raster"):
            if a == selectedLayerIndex:
                filename = str(layer.source())
            a=a+1
        filename1= selfMT.dlg.lineEdit.text()[0:len(currentText[:-4])]
        if filename1[0:3] == "_OB" or currentText[:-4] == filename1[0:len(currentText[:-4])]:
            filename = currentText
        #autofill
        Clusters = selfMT.dlg.Clusters.text()
        if Clusters == "":
            Clusters = "10"
        MinimumSize = selfMT.dlg.MinimumSize.text()
        if MinimumSize == "":
            MinimumSize = "30"
        autoPoly = filename[:-4]+"_OBIA_"+Clusters+"_"+MinimumSize+".shp"
        selfMT.dlg.lineEdit.setText(autoPoly)
        if os.path.exists(autoPoly):
            selfMT.dlg.exists1.setText("Existing file will be overwritten")
        else:
            selfMT.dlg.exists1.setText("")
                    
    def help(self): 
        import webbrowser
        import marinetools
        MThelp = os.path.dirname(marinetools.__file__) + "\\obia\\OBIA.pdf"
        webbrowser.open(MThelp)
        
    def otbInstall(self):
        import webbrowser
        import marinetools
        MThelp = os.path.dirname(marinetools.__file__) + "\\obia\\OTBinstall.pdf"
        webbrowser.open(MThelp)
        
    def run(self):
        import random
        import marinetools
        from marinetools.obia.OBIA_dialog import OBIADialog
        global selfMT
        """Run method that performs all the real work"""

        # Create the dialog with elements (after translation) and keep reference
        # Only create GUI ONCE in callback, so that it will only load when the plugin is started
        if self.first_start == True:
            self.first_start = False
            self.dlg = OBIADialog()
            selfMT = self
            self.dlg.pushButton_1.clicked.connect(OBIA.select_input_file1) 
            self.dlg.pushButton_2.clicked.connect(OBIA.select_input_file2) 
            self.dlg.pushButton_3.clicked.connect(OBIA.select_input_file3) 
            self.dlg.pushButton_4.clicked.connect(OBIA.select_input_file4) 
            self.dlg.pushButton_5.clicked.connect(OBIA.select_input_file5) 
            self.dlg.pushButton.clicked.connect(OBIA.select_output_file) 
            self.dlg.Clusters.textChanged.connect(OBIA.indexChanged) 
            self.dlg.MinimumSize.textChanged.connect(OBIA.indexChanged) 
            self.dlg.helpButton.clicked.connect(OBIA.help) 
            self.dlg.comboBox1.currentIndexChanged.connect(OBIA.indexChanged)
            
        # Fetch the currently loaded layers
        layers = QgsProject.instance().mapLayers().values()

        self.dlg.comboBox1.clear() 
        self.dlg.comboBox1.addItems([layer.name() for layer in layers if str(layer.type())== "1" or str(layer.type())== "LayerType.Raster"])
        self.dlg.comboBox2.clear() 
        self.dlg.comboBox2.addItems([layer.name() for layer in layers if str(layer.type())== "1" or str(layer.type())== "LayerType.Raster"])
        self.dlg.comboBox3.clear() 
        self.dlg.comboBox3.addItems([layer.name() for layer in layers if str(layer.type())== "1" or str(layer.type())== "LayerType.Raster"])
        self.dlg.comboBox3.insertItem(0,"")
        max = selfMT.dlg.comboBox3.setCurrentIndex(0)
        self.dlg.comboBox4.clear() 
        self.dlg.comboBox4.addItems([layer.name() for layer in layers if str(layer.type())== "1" or str(layer.type())== "LayerType.Raster"])
        self.dlg.comboBox4.insertItem(0,"")
        max = selfMT.dlg.comboBox4.setCurrentIndex(0)
        self.dlg.comboBox5.clear() 
        self.dlg.comboBox5.addItems([layer.name() for layer in layers if str(layer.type())== "1" or str(layer.type())== "LayerType.Raster"])
        self.dlg.comboBox5.insertItem(0,"")
        max = selfMT.dlg.comboBox5.setCurrentIndex(0)
        OBIA.indexChanged(self) 
        
        # show the dialog
        self.dlg.show()
        # Run the dialog event loop
        result = self.dlg.exec_()
        
        # See if OK was pressed

        if result:
            import glob
            selectedLayerIndex = self.dlg.comboBox1.currentIndex()
            currentText = selfMT.dlg.comboBox1.currentText()
            layers = QgsProject.instance().mapLayers().values()
            a=0
            for layer in (layer1 for layer1 in layers if str(layer1.type())== "1" or str(layer1.type())== "LayerType.Raster"):
                if a == selectedLayerIndex:
                    filename = str(layer.source())
                a=a+1
            if currentText not in filename:
                filename = currentText
            filename1a = filename
            
            selectedLayerIndex = self.dlg.comboBox2.currentIndex()
            currentText = selfMT.dlg.comboBox2.currentText()
            layers = QgsProject.instance().mapLayers().values()
            a=0
            for layer in (layer1 for layer1 in layers if str(layer1.type())== "1" or str(layer1.type())== "LayerType.Raster"):
                if a == selectedLayerIndex:
                    filename = str(layer.source())
                a=a+1
            if currentText not in filename:
                filename = currentText
            filename2a = filename

            selectedLayerIndex = self.dlg.comboBox3.currentIndex()
            currentText = selfMT.dlg.comboBox3.currentText()
            layers = QgsProject.instance().mapLayers().values()
            a=1
            for layer in (layer1 for layer1 in layers if str(layer1.type())== "1" or str(layer1.type())== "LayerType.Raster"):
                if a == selectedLayerIndex:
                    filename = str(layer.source())
                a=a+1
            if currentText not in filename or currentText == "":
                filename = currentText
            filename3a = filename

            selectedLayerIndex = self.dlg.comboBox4.currentIndex()
            currentText = selfMT.dlg.comboBox4.currentText()
            layers = QgsProject.instance().mapLayers().values()
            a=1
            for layer in (layer1 for layer1 in layers if str(layer1.type())== "1" or str(layer1.type())== "LayerType.Raster"):
                if a == selectedLayerIndex:
                    filename = str(layer.source())
                a=a+1
            if currentText not in filename or currentText == "":
                filename = currentText                
            filename4a = filename

            selectedLayerIndex = self.dlg.comboBox5.currentIndex()
            currentText = selfMT.dlg.comboBox5.currentText()
            layers = QgsProject.instance().mapLayers().values()
            a=1
            #for layer in layers:
            for layer in (layer1 for layer1 in layers if str(layer1.type())== "1" or str(layer1.type())== "LayerType.Raster"):
                if a == selectedLayerIndex:
                    filename = str(layer.source())
                a=a+1
            if currentText not in filename or currentText == "":
                filename = currentText
            filename5a = filename
            
            clusters = self.dlg.Clusters.text()
            MinSize = self.dlg.MinimumSize.text()
            if clusters == "":
                clusters = "10"
            if MinSize == "":
                MinSize = "30"
            if filename2a == "" and filename3a == "" and filename4a == "" and filename5a == "":
                QMessageBox.information(None, "Information:", "Must have at least two input files please.")
                return
            try:
                processing.run("otb:ImageEnvelope", {'in':filename1a,'out':'TEMPORARY_OUTPUT','sr':0,'elev.dem':'','elev.geoid':'','elev.default':0,'proj':''})
            except:
                QMessageBox.information(None, "Information:", "This tool requires (OrfeoToolBox (OTB) to be installed\nDownload from https://www.orfeo-toolbox.org/download/\n and install locally.")
                OBIA.otbInstall(self)
                return
            print("Input filenames:")
            print(filename1a)
            print(filename2a)
            if filename3a != "":
                print(filename3a)
            if filename4a != "":
                print(filename4a)
            if filename5a != "":
                print(filename5a)
                
            plugin_dir = os.path.dirname(__file__)
            gif_path = os.path.join(plugin_dir, "loading.gif")
            self.loading_screen = LoadingScreenDlg(gif_path)  # init loading dlg
            self.loading_screen.start_animation()  # start loading dlg

            OutputFilename = self.dlg.lineEdit.text()
            if os.path.exists(OutputFilename):
                os.remove(OutputFilename)
            newdir = str(os.path.dirname(OutputFilename) + "/tempMT")
            if not os.path.exists(newdir):
                os.mkdir(newdir)
            
            alphabet = '1234567890abcdefghijklmnopqrstuvwxyz'
            rand = alphabet[random.randint(1,25)] + alphabet[random.randint(1,35)] + alphabet[random.randint(1,35)] + alphabet[random.randint(1,35)]
            filename1 = ""
            filename2 = ""
            filename3 = ""
            filename4 = ""
            filename5 = ""
            
            filename1 = newdir + "/tmpFile1_"+rand+".img"
            file1layer = QgsRasterLayer(filename1a,"file1")
            file1Stats = file1layer.dataProvider().bandStatistics(1,QgsRasterBandStats.All)
            file1Min = file1Stats.minimumValue
            file1Max = file1Stats.maximumValue
            processing.run("grass7:r.rescale.eq", {'input':filename1a,'from':[file1Min,file1Max],'to':[0,254],'output':filename1,'GRASS_REGION_PARAMETER':None,'GRASS_REGION_CELLSIZE_PARAMETER':0,'GRASS_RASTER_FORMAT_OPT':'','GRASS_RASTER_FORMAT_META':''})

            filename2 = newdir + "/tmpFile2_"+rand+".img"
            file2layer = QgsRasterLayer(filename2a,"file2")
            file2Stats = file2layer.dataProvider().bandStatistics(1,QgsRasterBandStats.All)
            file2Min = file2Stats.minimumValue
            file2Max = file2Stats.maximumValue
            processing.run("grass7:r.rescale.eq", {'input':filename2a,'from':[file2Min,file2Max],'to':[0,254],'output':filename2,'GRASS_REGION_PARAMETER':None,'GRASS_REGION_CELLSIZE_PARAMETER':0,'GRASS_RASTER_FORMAT_OPT':'','GRASS_RASTER_FORMAT_META':''})

            if filename3a != "":
                filename3 = newdir + "/tmpFile3_"+rand+".img"
                file3layer = QgsRasterLayer(filename3a,"file3")
                file3Stats = file3layer.dataProvider().bandStatistics(1,QgsRasterBandStats.All)
                file3Min = file3Stats.minimumValue
                file3Max = file3Stats.maximumValue
                processing.run("grass7:r.rescale.eq", {'input':filename3a,'from':[file3Min,file3Max],'to':[0,254],'output':filename3,'GRASS_REGION_PARAMETER':None,'GRASS_REGION_CELLSIZE_PARAMETER':0,'GRASS_RASTER_FORMAT_OPT':'','GRASS_RASTER_FORMAT_META':''})
            else: # make a 0 file (or null)
                filename3 = newdir + "/tmpFile3"+rand+".img"
                processing.run("gdal:rastercalculator", {'INPUT_A':filename1a,'BAND_A':1,'FORMULA':'A*(0.0)','OUTPUT':filename3})

            if filename4a != "":
                filename4 = newdir + "/tmpFile4_"+rand+".img"
                file4layer = QgsRasterLayer(filename4a,"file4")
                file4Stats = file4layer.dataProvider().bandStatistics(1,QgsRasterBandStats.All)
                file4Min = file4Stats.minimumValue
                file4Max = file4Stats.maximumValue
                processing.run("grass7:r.rescale.eq", {'input':filename4a,'from':[file4Min,file4Max],'to':[0,254],'output':filename4,'GRASS_REGION_PARAMETER':None,'GRASS_REGION_CELLSIZE_PARAMETER':0,'GRASS_RASTER_FORMAT_OPT':'','GRASS_RASTER_FORMAT_META':''})
          
            if filename5a != "":
                filename5 = newdir + "/tmpFile5_"+rand+".img"
                file5layer = QgsRasterLayer(filename5a,"file5")
                file5Stats = file5layer.dataProvider().bandStatistics(1,QgsRasterBandStats.All)
                file5Min = file5Stats.minimumValue
                file5Max = file5Stats.maximumValue
                processing.run("grass7:r.rescale.eq", {'input':filename5a,'from':[file5Min,file5Max],'to':[0,254],'output':filename5,'GRASS_REGION_PARAMETER':None,'GRASS_REGION_CELLSIZE_PARAMETER':0,'GRASS_RASTER_FORMAT_OPT':'','GRASS_RASTER_FORMAT_META':''})
            InputRas = newdir + "/GRMbuildvirtualraster"+rand+".vrt"
            if filename5 != "":
                processing.run("gdal:buildvirtualraster", {'INPUT':[filename1,filename2,filename3,filename4,filename5],'RESOLUTION':0,'SEPARATE':True,'PROJ_DIFFERENCE':False,'ADD_ALPHA':False,'ASSIGN_CRS':None,'RESAMPLING':0,'SRC_NODATA':'','EXTRA':'','OUTPUT':InputRas})
            if filename4 != "" and filename5 == "":
                processing.run("gdal:buildvirtualraster", {'INPUT':[filename1,filename2,filename3,filename4],'RESOLUTION':0,'SEPARATE':True,'PROJ_DIFFERENCE':False,'ADD_ALPHA':False,'ASSIGN_CRS':None,'RESAMPLING':0,'SRC_NODATA':'','EXTRA':'','OUTPUT':InputRas})
            if filename3 != "" and filename4 == "" and filename5 == "":
                processing.run("gdal:buildvirtualraster", {'INPUT':[filename1,filename2,filename3],'RESOLUTION':0,'SEPARATE':True,'PROJ_DIFFERENCE':False,'ADD_ALPHA':False,'ASSIGN_CRS':None,'RESAMPLING':0,'SRC_NODATA':'','EXTRA':'','OUTPUT':InputRas})

            temp1 =newdir + "/OTBkmeans"+rand+".img"
            temp2 =newdir + "/Gtranslate"+rand+".img"
            temp3 =newdir + "/RAsieve"+rand+".tif"
            temp4 =newdir + "/VCpolygons"+rand+".shp"
            temp5 =newdir + "/VGaggregate"+rand+".shp"
            temp5a =newdir + "/VGaggregateA"+rand+".shp"
            temp5b =newdir + "/VGaggregateB"+rand+".shp"
            temp5c =newdir + "/VGaggregateC"+rand+".shp"
            temp5d =newdir + "/VGaggregateD"+rand+".shp"
            temp5e =newdir + "/VGaggregateE"+rand+".shp"
            
            print ("Picking", str(clusters), "clusters (classes)  ")
            
            processing.run("otb:KMeansClassification", {'in':InputRas,'out':temp1,'nc':clusters,'ts':100,'maxit':1000,'centroids.in':'','centroids.out':'','sampler':'periodic','sampler.periodic.jitter':0,'vm':None,'nodatalabel':0,'cleanup':True,'rand':0,'outputpixeltype':5})

            """print(os.path.dirname(__file__))
            segdir = os.path.dirname(__file__) + "/segmentation"
            segdir2 = segdir.replace("\\","/")
            segexe = segdir2 + "/ShepherdSeg.exe"
            segmentClumps2 = temp1.replace("\\","/") #  r"\tmpMT\tmpoutputSegments.img"
            tmpPath2 = newdir.replace("\\","/")
            processing.run("gdal:translate", {'INPUT':InputRas,'TARGET_CRS':None,'NODATA':None,'COPY_SUBDATASETS':False,'OPTIONS':'','EXTRA':'','DATA_TYPE':0,'OUTPUT':temp2})
            fullcomm = segexe +" "+ temp2 +" "+ segmentClumps2 +" "+ tmpPath2 +" "+ str(clusters) +" "+ str(MinSize)
            print("cd "+ segdir)
            print(fullcomm)
            
            #subprocess.call(r"cmd /c cd " + segdir + " & " + str(fullcomm) + " > "+ newdir + "/temp.txt")
            subprocess.call(r"cmd /c " + str(fullcomm) )
            """






            print ("Removing classes with less than", str(MinSize),"pixels")
            
            import math
            power = math.log(int(MinSize),2)+1.0 # Increments of power of two sizes
            first = temp1
            #print("power ",str(power))
            for x in range(1,int(power)):
                next=newdir + "/RAsieve"+str(x)+".tif"
                Size = 2**x
                #print("Size ",str(Size))
                processing.run("gdal:sieve", {'INPUT':first,'THRESHOLD':Size,'EIGHT_CONNECTEDNESS':False,'NO_MASK':False,'MASK_LAYER':None,'EXTRA':'','OUTPUT':next})
                first = next
            processing.run("gdal:sieve", {'INPUT':first,'THRESHOLD':MinSize,'EIGHT_CONNECTEDNESS':False,'NO_MASK':False,'MASK_LAYER':None,'EXTRA':'','OUTPUT':temp3})
            processing.run("native:pixelstopolygons", {'INPUT_RASTER':temp3,'RASTER_BAND':1,'FIELD_NAME':'VALUE','OUTPUT':temp4})
            print ("Making polygons")
            processing.run("native:aggregate", {'INPUT':temp4,'GROUP_BY':'"VALUE"','AGGREGATES':[{'aggregate': 'mean','delimiter': ',','input': '"VALUE"','length': 20,'name': 'VALUE','precision': 8,'sub_type': 0,'type': 6,'type_name': 'double precision'}],'OUTPUT':temp5})

            if filename4 == "" and filename5 == "": # 2 or 3 inputs
                #result = processing.run("native:zonalstatisticsfb", {'INPUT':temp5,'INPUT_RASTER':filename1,'RASTER_BAND':1,'COLUMN_PREFIX':'file1_','STATISTICS':[2,4],'OUTPUT':'TEMPORARY_OUTPUT'})
                #temp5a = result['OUTPUT']
                #result = processing.run("native:zonalstatisticsfb", {'INPUT':temp5a,'INPUT_RASTER':filename2,'RASTER_BAND':1,'COLUMN_PREFIX':'file2_','STATISTICS':[2,4],'OUTPUT':'TEMPORARY_OUTPUT'})
                #temp5b = result['OUTPUT']
                #result = processing.run("native:zonalstatisticsfb", {'INPUT':temp5b,'INPUT_RASTER':filename3,'RASTER_BAND':1,'COLUMN_PREFIX':'file3_','STATISTICS':[2,4],'OUTPUT':'TEMPORARY_OUTPUT'})
                #temp5e = result['OUTPUT']
                result = processing.run("native:zonalstatisticsfb", {'INPUT':temp5,'INPUT_RASTER':filename1,'RASTER_BAND':1,'COLUMN_PREFIX':'file1_','STATISTICS':[2,4],'OUTPUT':temp5a})
                result = processing.run("native:zonalstatisticsfb", {'INPUT':temp5a,'INPUT_RASTER':filename2,'RASTER_BAND':1,'COLUMN_PREFIX':'file2_','STATISTICS':[2,4],'OUTPUT':temp5b})
                result = processing.run("native:zonalstatisticsfb", {'INPUT':temp5b,'INPUT_RASTER':filename3,'RASTER_BAND':1,'COLUMN_PREFIX':'file3_','STATISTICS':[2,4],'OUTPUT':temp5e})
            if filename4 != "" and filename5 == "":  # 4 inputs
                processing.run("native:zonalstatisticsfb", {'INPUT':temp5,'INPUT_RASTER':filename1,'RASTER_BAND':1,'COLUMN_PREFIX':'file1_','STATISTICS':[2,4],'OUTPUT':temp5a})
                processing.run("native:zonalstatisticsfb", {'INPUT':temp5a,'INPUT_RASTER':filename2,'RASTER_BAND':1,'COLUMN_PREFIX':'file2_','STATISTICS':[2,4],'OUTPUT':temp5b})
                processing.run("native:zonalstatisticsfb", {'INPUT':temp5b,'INPUT_RASTER':filename3,'RASTER_BAND':1,'COLUMN_PREFIX':'file3_','STATISTICS':[2,4],'OUTPUT':temp5c})
                processing.run("native:zonalstatisticsfb", {'INPUT':temp5c,'INPUT_RASTER':filename4,'RASTER_BAND':1,'COLUMN_PREFIX':'file4_','STATISTICS':[2,4],'OUTPUT':temp5e})
            if filename5 != "":   # 5 inputs
                processing.run("native:zonalstatisticsfb", {'INPUT':temp5,'INPUT_RASTER':filename1,'RASTER_BAND':1,'COLUMN_PREFIX':'file1_','STATISTICS':[2,4],'OUTPUT':temp5a})
                processing.run("native:zonalstatisticsfb", {'INPUT':temp5a,'INPUT_RASTER':filename2,'RASTER_BAND':1,'COLUMN_PREFIX':'file2_','STATISTICS':[2,4],'OUTPUT':temp5b})
                processing.run("native:zonalstatisticsfb", {'INPUT':temp5b,'INPUT_RASTER':filename3,'RASTER_BAND':1,'COLUMN_PREFIX':'file3_','STATISTICS':[2,4],'OUTPUT':temp5c})
                processing.run("native:zonalstatisticsfb", {'INPUT':temp5c,'INPUT_RASTER':filename4,'RASTER_BAND':1,'COLUMN_PREFIX':'file4_','STATISTICS':[2,4],'OUTPUT':temp5d})
                processing.run("native:zonalstatisticsfb", {'INPUT':temp5d,'INPUT_RASTER':filename5,'RASTER_BAND':1,'COLUMN_PREFIX':'file5_','STATISTICS':[2,4],'OUTPUT':temp5e})

            self.loading_screen.stop_animation()

            print ("Output file = ",OutputFilename)
            processing.run("native:multiparttosingleparts", {'INPUT':temp5e,'OUTPUT':OutputFilename})
            
            for f in glob.glob(temp1.replace(".tif",".*")):
                os.remove(f)
            for f in glob.glob(newdir + "/RAsieve*"):
                os.remove(f)
            #remove_temp_files(InputRas) #
            #remove_temp_files(filename1) #
            #remove_temp_files(filename2) #
            #remove_temp_files(filename3) #
            #if filename4 != "":
                #remove_temp_files(filename4) #
            #if filename5 != "":
                #remove_temp_files(filename5) #
            #remove_temp_files(temp4) #
            #remove_temp_files(temp5) #
            #remove_temp_files(temp5a) #
            #remove_temp_files(temp5b) #
            #remove_temp_files(temp5c) #
            #remove_temp_files(temp5d) #
            #remove_temp_files(temp5e) #
            fname = os.path.dirname(str(OutputFilename))
            vlayer = QgsVectorLayer(str(OutputFilename), str(OutputFilename[len(fname)+1:]), "ogr")
            QgsProject.instance().addMapLayer(vlayer)
            colour_polygons_random(vlayer)

            pass
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

def remove_temp_files(tempfilename):
    try:
        for f in glob.glob(str(tempfilename[:-4]) + "*"):
            os.remove(f)
    except:
        print(tempfilename + " not removed")

    return
