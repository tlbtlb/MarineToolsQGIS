#!/usr/bin/env python
# -*- coding: utf-8 -*-
# --------------------------------------------------------
#    taken from hcmgis (c) 2018 by Quach Dong Thang
#
#    modified             : 2024 Tim Le Bas 
#    email                :  tlb@noc.ac.uk
# --------------------------------------------------------
# To make the <name>_form.py file from the ui file:
# Open “OSGeo4W Shell” window

# cd C:/Users/tlb/Documents/Software/QGIS_plugins/marinetools/CSV2Grid 
# python -m PyQt5.uic.pyuic -x CSV2Grid_form.ui -o CSV2Grid_form.py

"""
/***************************************************************************
 *                                                                         *
 *   This program is free software; you can redistribute it and/or modify  *
 *   it under the terms of the GNU General Public License v3.0.            *
 *                                                                         *
 ***************************************************************************/
 """

import csv
import math
import os.path
import operator
import sys
from urllib.request import urlopen
import json
import processing
import glob
import time
from .resources import *

from qgis.core import *
from PyQt5 import QtCore, QtGui, QtWidgets
from PyQt5.QtCore import *
from PyQt5.QtGui import *
from PyQt5.QtWidgets import *
from owslib.wfs import WebFeatureService
from qgis.gui import QgsMessageBar
import qgis.utils
from glob import glob
import urllib, re, ssl
from time import sleep
from xml.etree.ElementTree import XML, fromstring
import webbrowser
from PyQt5.QtWidgets import QApplication
from PyQt5.QtWidgets import QApplication, QWidget, QPushButton, QMessageBox
try:
    from qgis.gui import QgsGenericProjectionSelector
except ImportError:
    from qgis.gui import QgsProjectionSelectionDialog

from .CSV2Grid_form import *
global selectedcrsdef
global selectedcrsdef_2
selectedcrsdef = ""
selectedcrsdef_2 = ""
# ------------------------------------------------------------------------------
#    csv2grid_dialog - base class for hcmgis dialogs containing utility functions
# ------------------------------------------------------------------------------
class csv2grid_dialog(QtWidgets.QDialog):
    def __init__(self, iface):
        QtWidgets.QDialog.__init__(self)
        self.iface = iface
        return

    def csv2grid_read_csv_header(self, input_csv_name):
        # This may take awhile with large CSV files
        input_csv = QgsVectorLayer(input_csv_name)

        field_names = []

        if (not input_csv) or (input_csv.featureCount() <= 0) or (len(input_csv.fields()) <= 0):
            return field_names

        for field in input_csv.fields():
            field_names.append(field.name())

        return field_names


    def csv2grid_set_status_bar(self, status_bar, status_lable):
        status_bar.setMinimum(0)
        status_bar.setMaximum(100)
        status_bar.setValue(0)
        status_bar.setFormat("Ready")
        self.status_bar = status_bar
        self.status_lable = status_lable

    def csv2grid_status_callback(self, percent_complete, lable):
        try:
            self.status_lable.setText(lable)
            message = str(int(percent_complete)) + "%"
            self.status_bar.setFormat(message)

            if percent_complete < 0:
                self.status_bar.setValue(0)
            elif percent_complete > 100:
                self.status_bar.setValue(100)
            else:
                self.status_bar.setValue(percent_complete)

            self.iface.statusBarIface().showMessage(message)

            # print("status_callback(" + message + ")")
        except:
            print(message)

        # add handling of "Close" button
        return 0


class CSV2Grid_dialog(csv2grid_dialog, Ui_CSV2Grid_form):
    def __init__(self, iface):
        csv2grid_dialog.__init__(self, iface)
        self.setupUi(self)
        self.BtnApplyClose.button(QtWidgets.QDialogButtonBox.Close).setAutoDefault(True)
        self.csv2grid_set_status_bar(self.status,self.LblStatus)
        self.lsCSV.clear()
        self.txtError.clear()
        self.BtnInputFolder.clicked.connect(self.read_csv)
        self.pushButton.clicked.connect(self.selectcrs)
        self.pushButton_2.clicked.connect(self.selectcrs_2)
        self.lsCSV.currentRowChanged.connect(self.set_field_names)
        self.BtnApplyClose.button(QtWidgets.QDialogButtonBox.Apply).clicked.connect(self.run)
        self.helpButton_2.clicked.connect(self.help)
        return

    def help(self): 
        import webbrowser
        import marinetools
        MThelp = os.path.dirname(marinetools.__file__) + "\\CSV2Grid\\CSV2Grid.pdf"
        webbrowser.open(MThelp)

    def set_field_names(self):
        try:
            header = self.csv2grid_read_csv_header(self.lsCSV.currentItem().text())
        except:
            return
        if not header:
            return
        self.longitude_field.clear()
        self.latitude_field.clear()
        self.longitude_field.addItems(header)
        self.latitude_field.addItems(header)
        self.DepthZ.addItems(header)
        self.DepthZ.setCurrentIndex(2)
        

        for index, field in enumerate(header):
            #if (field.lower().find("x") >= 0):
            if (field.lower().startswith("x") ):
                self.longitude_field.setCurrentIndex(index)

            elif (field.lower().startswith("y") ):
                self.latitude_field.setCurrentIndex(index)

            elif (field.lower().startswith('lon')):
                self.longitude_field.setCurrentIndex(index)

            elif (field.lower().startswith('lat')):
                self.latitude_field.setCurrentIndex(index)

    def read_csv(self):
        newname = QFileDialog.getExistingDirectory(None, "Input Folder",self.LinInputFolder.displayText())
        if newname != None and os.path.basename(newname)!='' : #prevent choose the whole Disk like C:\
            self.LinInputFolder.setText(newname)
            self.lsCSV.clear()
            PATH = newname
            EXT = "*.csv"
            all_csv_files = [file
                        for path, subdir, files in os.walk(PATH)
                        for file in glob(os.path.join(path, EXT))]
            self.lsCSV.addItems(all_csv_files)
            self.lblCSV.setText (str(self.lsCSV.count()) + " files loaded")
            self.lsCSV.setCurrentRow(0)
            self.LblStatus.clear()
            self.csv2grid_set_status_bar(self.status,self.LblStatus)
        else:
            QMessageBox.warning(None, "Choose Folder", 'Please choose a folder, not a disk like C:/')

    def selectcrs(self):
        # Select a new CRS
        global selectedcrsdef
        try:
            projSelector = QgsGenericProjectionSelector()
            projSelector.exec_()
            projSelector.selectedCrsId()
            selectedcrs=projSelector.selectedCrsId()
        except:
            projSelector = QgsProjectionSelectionDialog()
            projSelector.exec_()
            selectedcrsdef = projSelector.crs()
            selectedcrs=selectedcrsdef.srsid()
        if (selectedcrs=="" or selectedcrs==0 or self.CrsId2AuthID(selectedcrs)=="" or selectedcrs is None):
            self.nocrsselected()
        else:
            self.labelselectedcrs.setText(self.CrsId2AuthID(selectedcrs))
        self.show()
    def CrsId2AuthID(self, crsid=0):
        toconvert = QgsCoordinateReferenceSystem()
        if crsid=="" or crsid==0 or crsid is None:
            converted=""
        else:
            toconvert.createFromId(int(crsid), QgsCoordinateReferenceSystem.InternalCrsId)
            converted=toconvert.authid()
        return converted

    def nocrsselected(self):
        self.labelselectedcrs.setText("No CRS selected")

    def selectcrs_2(self):
        # Select a new CRS
        global selectedcrsdef_2
        try:
            projSelector = QgsGenericProjectionSelector()
            projSelector.exec_()
            projSelector.selectedCrsId()
            selectedcrs_2=projSelector.selectedCrsId()
        except:
            projSelector = QgsProjectionSelectionDialog()
            projSelector.exec_()
            selectedcrsdef_2 = projSelector.crs()
            selectedcrs_2=selectedcrsdef_2.srsid()
        if (selectedcrs_2=="" or selectedcrs_2==0 or self.CrsId2AuthID(selectedcrs_2)=="" or selectedcrs_2 is None):
            self.nocrsselected_2()
        else:
            self.labelselectedcrs_2.setText(self.CrsId2AuthID(selectedcrs_2))
        self.show()

    def nocrsselected_2(self):
        self.labelselectedcrs_2.setText("No CRS selected")

    def run(self):
        self.txtError.append("Opening input files")
        item_count = 0
        error_count = 0
        items = []
        for index in range(self.lsCSV.count()):
            items.append(self.lsCSV.item(index))
        self.txtError.clear()
        self.lsCSV.blockSignals(True)
        self.LinInputFolder.setEnabled(False)
        self.BtnInputFolder.setEnabled(False)
        self.longitude_field.setEnabled(False)
        self.latitude_field.setEnabled(False)
        self.labelselectedcrs.setEnabled(False)
        self.DepthZ.setEnabled(False)
        Zcolumn = self.DepthZ.currentText()
        self.status_bar.setEnabled(False)
        crs = QgsCoordinateReferenceSystem("EPSG:4326")
        CRSin = selectedcrsdef
        CRSout = selectedcrsdef_2
        if selectedcrsdef == "":
            CRSin = crs # default WGS84 Lat/Lon
        if selectedcrsdef_2 == "":
            CRSout = crs # default WGS84 Lat/Lon
        mbox = QMessageBox()
        mbox.setIcon(QMessageBox.Icon.Information)
        mbox.setText("This process may take some time - hidden")
        mbox.setWindowTitle("This process may take some time")
        mbox.show()
        for item in items:
            self.lsCSV.setCurrentRow(item_count)
            input_csv_name = item.text()
            longitude_field = str(self.longitude_field.currentText())
            latitude_field = str(self.latitude_field.currentText())

            temp_file_name = item.text()
            output_file_name = temp_file_name.replace(".csv", ".shp", 1)

            message = self.csv2grid_go(input_csv_name,  latitude_field, longitude_field, \
                output_file_name, CRSin, CRSout, Zcolumn, self.csv2grid_status_callback)
            if message:
                error_count+=1
                self.txtError.append(str(error_count)+ ". "+ input_csv_name + ": " + message)
                continue
            else:
                item_count +=1
                self.LblStatus.setText (str(item_count)+"/ "+ str(self.lsCSV.count()) + " files converted")
        mbox.close()
        """
        self.lsCSV.blockSignals(False)
        self.LinInputFolder.setEnabled(True)
        self.BtnInputFolder.setEnabled(True)
        self.longitude_field.setEnabled(True)
        self.latitude_field.setEnabled(True)
        self.status_bar.setEnabled(True)
        """
        return

    def csv2grid_go(self, input_csv_name, latitude_field, longitude_field, \
            output_file_name, CRSin, CRSout, Zcolumn, status_callback = None):

        # Parameter error checks and conversions
        input_csv = QgsVectorLayer(input_csv_name)
        if input_csv.featureCount() <= 0:
            return "Invalid CSV point file"

        latitude_index = input_csv.fields().indexFromName(latitude_field)
        if latitude_index < 0:
            return "Invalid latitude field"

        longitude_index = input_csv.fields().indexFromName(longitude_field)
        if longitude_index < 0:
            return "Invalid longitude field"

        wkb_type = QgsWkbTypes.Point

        # Create the output shapefile
        fields = QgsFields()
        for field in input_csv.fields():
            #print (str(field))
            fields.append(field)

        newdir = str(os.path.dirname(output_file_name) + "/tempMT")
        if not os.path.exists(newdir):
            os.mkdir(newdir)
        tempfile0 = newdir + "\\tempfile0.shp"

        crs = CRSin

        file_formats = { ".shp":"ESRI Shapefile"}

        if os.path.splitext(output_file_name)[1] not in file_formats:
            return "Unsupported output file format: " + str(output_file_name)

        output_file_format = file_formats[os.path.splitext(output_file_name)[1]]

        context = QgsCoordinateTransformContext()
        options = QgsVectorFileWriter.SaveVectorOptions()
        options.driverName = output_file_format
        outfile = QgsVectorFileWriter.create(tempfile0,  fields, QgsWkbTypes.Point, crs, context, options)


        if (outfile.hasError() != QgsVectorFileWriter.NoError):
            return "Failure creating output file: " + str(outfile.errorMessage())

        shape_count = 0
        for row_number, row in enumerate(input_csv.getFeatures()):
            if status_callback and ((row_number % 10) == 0):
                if status_callback(100 * row_number / input_csv.featureCount(),
                        "Point " + str(row_number) + " of " + str(input_csv.featureCount())):
                    return "Canceled at point " + str(row_number)

            if (latitude_index >= len(row.attributes())) or (longitude_index >= len(row.attributes())):
                return "Node file missing lat/long at row " + str(row_number + 1)

            # Each node is a separate feature in a point file
            newfeature = QgsFeature()
            newfeature.setAttributes(row.attributes())
            try:
                point = QgsPointXY(float(row.attributes()[longitude_index]), float(row.attributes()[latitude_index]))
                geometry = QgsGeometry.fromPointXY(point)
                newfeature.setGeometry(geometry)
            except:
                pass

            outfile.addFeature(newfeature)
            shape_count += 1
        self.txtError.append("Point file created")

        del outfile
        if shape_count > 10:
            # Point file created - now for gridding
            newdir = str(os.path.dirname(output_file_name) + "/tempMT")
            if not os.path.exists(newdir):
                os.mkdir(newdir)
            tempfile1 = newdir + "\\tempfile1.shp"
            tempfile1p = newdir + "\\tempfile1p.shp"
            tempfile2 = newdir + "\\tempfile2.shp"
            tempfile3 = newdir + "\\tempfile3.shp"
            tempfile4 = newdir + "\\tempfile4.shp"
            # convert from column of strings to reals
            formul = ' to_real( "' + Zcolumn + '" )'
            #print(formul)
            result = processing.run("native:fieldcalculator", {'INPUT':tempfile0,'FIELD_NAME':'depth2','FIELD_TYPE':0,
                                                      'FIELD_LENGTH':10,'FIELD_PRECISION':5,
                                                      'FORMULA':formul,'OUTPUT':'TEMPORARY_OUTPUT'})
            tempfile1 = result['OUTPUT']
            processing.run("native:reprojectlayer", {'INPUT':tempfile1,'TARGET_CRS':CRSout,'OUTPUT':output_file_name})
            infile = QgsVectorLayer(output_file_name, "ogr")
            self.txtError.append("Point file indexed")
            processing.run("native:createspatialindex", {'INPUT':output_file_name})
            index = QgsSpatialIndex(infile.getFeatures())
            crs = infile.crs()
            extent = infile.extent()
            xmin = extent.xMinimum()
            ymin = extent.yMinimum()
            xmax = extent.xMaximum()
            ymax = extent.yMaximum()
            resolution = (xmax-xmin)/100.0
            if (ymax-ymin)/100.0  < resolution:
                resolution = (ymax-ymin)/100.0
            # create a grid to find point density
            result = processing.run("native:creategrid", {'TYPE':2,'EXTENT':extent,'HSPACING':resolution,'VSPACING':resolution,
                                                 'HOVERLAY':0,'VOVERLAY':0,'CRS':crs,
                                                 'OUTPUT':'TEMPORARY_OUTPUT'})
            tempfile2 = result['OUTPUT']
            # count number of points in grid polygon            
            result = processing.run("native:countpointsinpolygon", {'POLYGONS':tempfile2,'POINTS':output_file_name,'WEIGHT':'',
                                                           'CLASSFIELD':'','FIELD':'NUMPOINTS','OUTPUT':'TEMPORARY_OUTPUT'})
            tempfile3 = result['OUTPUT']
            self.txtError.append("Finding the best resolution for the Grid")
            # remove empty grid polygons
            result = processing.run("native:extractbyexpression", {'INPUT':tempfile3,'EXPRESSION':'NUMPOINTS > 0','OUTPUT':'TEMPORARY_OUTPUT'})
            tempfile4 = result['OUTPUT']
            # find median number of points in grid polygon
            stats = processing.run("qgis:basicstatisticsforfields", {'INPUT_LAYER':tempfile4,'FIELD_NAME':'NUMPOINTS','OUTPUT_HTML_FILE':'TEMPORARY_OUTPUT'})
            gridMedian = stats["MEDIAN"]
            #print(gridMedian)
            resolution = resolution / (math.sqrt(gridMedian))
            #print(resolution)
            self.txtError.append("Resolution of output Grid = " + str(resolution))
            output_file_grid = output_file_name.replace(".shp", ".img", 1)
            result = processing.run("gdal:rasterize", {'INPUT':output_file_name,'FIELD':'depth2','BURN':0,'USE_Z':False,'UNITS':1,
                                              'WIDTH':resolution,'HEIGHT':resolution,'EXTENT':None,'NODATA':0,
                                              'OPTIONS':'','DATA_TYPE':5,'INIT':None,'INVERT':False,'EXTRA':'',
                                              'OUTPUT':'TEMPORARY_OUTPUT'})
            tempfile5 = result['OUTPUT']
            processing.run("gdal:fillnodata", {'INPUT':tempfile5,'BAND':1,'DISTANCE':2,'ITERATIONS':0,
                                               'NO_MASK':False,'MASK_LAYER':None,'OPTIONS':'','EXTRA':'',
                                               'OUTPUT':output_file_grid})
            QgsProject.instance().addMapLayer(infile, False)
            QgsProject.instance().removeMapLayer(infile.id())

            self.remove_temp_files(tempfile0)
            
            fname = os.path.dirname(str(output_file_grid))
            vlayer = QgsRasterLayer(str(output_file_grid), str(output_file_grid[len(fname)+1:]))
            QgsProject.instance().addMapLayer(vlayer)

        if status_callback:
            #status_callback(100, str(shape_count) + " shapes, " + str(input_csv.featureCount()) + " nodes")
            status_callback(100, None)

        #return None
        return None
    def remove_temp_files(self, tempfilename):
        for f in glob(str(tempfilename[:-4]) + "*"):
            try:
                os.remove(f)
            except:
                self.txtError.append(f + " not removed")

        return

