#  Test input raster file for filetype and projection
#
"""
/***************************************************************************
 
        begin                : 2025-03-17
        git sha              : $Format:%H$
        copyright            : (C) 2025 by Dr Tim Le Bas, National Oceanography Centre, Southampton . UK
        email                : tim.lebas@noc.ac.uk
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
# Import system modules
import math
import sys
import os
from qgis.core import QgsRasterLayer
from qgis.core import QgsVectorLayer
from qgis.core import QgsCoordinateReferenceSystem
from qgis.PyQt.QtWidgets import QMessageBox 

import processing

def main(in_raster=None, type=None):
    print("Input file = "+in_raster)
    print("Type       = "+type)
    
    if in_raster.find('\t'):
        in_raster = in_raster.replace('\t', '\\t')
    elif in_raster.find('\n'):
        in_raster = in_raster.replace('\n', '\\n')
    elif in_raster.find('\r'):
        in_raster = in_raster.replace('\r', '\\r')

    in_raster = in_raster.replace('\\','/')

    #
    if type == "raster":
        out_raster = in_raster
        infile = QgsRasterLayer(in_raster)

        # Checking Format type TIF, IMG or GDB
        result = processing.run("gdal:gdalinfo", {'INPUT':infile,'MIN_MAX':False,'STATS':False,'NOGCP':False,
                                              'NO_METADATA':False,'EXTRA':'','OUTPUT':'TEMPORARY_OUTPUT'})
        f= open(result['OUTPUT'])
        lines = f.read().splitlines()
        formatLine = lines[0][13:] # Driver line
        print("Format     = "+formatLine)
        f.close()
        #   OpenFileGDB/ESRI FileGDB 
        if formatLine != "GTiff/GeoTIFF" and formatLine != "HFA/Erdas Imagine Images (.img)" and \
           formatLine != "OpenFileGDB/ESRI FileGDB":
            QMessageBox.information(None, "TLB_Error:", "Only GeoTiff or Erdas Imagine Images (.img) are supported")
            return "Error: Only GeoTiff or Erdas Imagine Images (.img) are supported"

        # Checking Projections
        crs = infile.crs()
        epsgCount = str(crs).find("EPSG")
        print("Projection = "+str(crs))
        if epsgCount == -1:
            # not EPSG
            QMessageBox.information(None, "Error:", "Projection not supported - only EPSG projections")
            return "Error: Projection not supported - only EPSG projections"

        EPSGin = str(crs)[epsgCount:].replace(">","")
        extent = infile.extent()
        xmin = extent.xMinimum()
        ymin = extent.yMinimum()
        xmax = extent.xMaximum()
        ymax = extent.yMaximum()
        xPixels = infile.width()
        yPixels = infile.height()
        
        # convert Lat/Lon to UTM zone or GDB - special cases
        if EPSGin == "EPSG:4326" or formatLine == "OpenFileGDB/ESRI FileGDB":
            #newdir = str(os.path.dirname(filename1) + "/tempMT")
            #if not os.path.exists(newdir):
            #    os.mkdir(newdir)
            #out_raster = newdir + "/newUTM.img"

            # Find UTM Zone
            xMean = (xmax+xmin)/2.0
            if xMean > 180.0:
                xMean = xMean - 360.0
            zone = 1 + int((xMean+180.0)/6.0)
            if (ymax+ymin)/2.0 < 0.0:
                zoneOut = 32700+zone
            else:
                zoneOut = 32600+zone
            EPSGout = "EPSG:"+str(zoneOut)

            # UTM WGS84 = QgsCoordinateReferenceSystem('EPSG:32620')  Zone 20N
            # UTM WGS84 = QgsCoordinateReferenceSystem('EPSG:32702')  Zone 2S
            result = processing.run("gdal:warpreproject", {'INPUT':infile,'SOURCE_CRS':None,
                                                  'TARGET_CRS':QgsCoordinateReferenceSystem(str(EPSGout)),
                                                  'RESAMPLING':0,'NODATA':None,'TARGET_RESOLUTION':None,
                                                  'OPTIONS':None,'DATA_TYPE':0,'TARGET_EXTENT':None,
                                                  'TARGET_EXTENT_CRS':None,'MULTITHREADING':False,
                                                  'EXTRA':'','OUTPUT':'TEMPORARY_OUTPUT'})
            out_raster = result['OUTPUT']
            #                                     'EXTRA':'','OUTPUT':out_raster})
        return out_raster

    if type == "vector":
        out_raster = in_raster
        infile = QgsVectorLayer(in_raster,"ogs")
        
        # Checking Projections
        crs = infile.crs()
        epsgCount = str(crs).find("EPSG")
        print("Projection = "+str(crs))
        if epsgCount == -1:
            # not EPSG
            QMessageBox.information(None, "Error:", "Projection not supported - only EPSG projections")
            return "Error: Projection not supported - only EPSG projections"

        EPSGin = str(crs)[epsgCount:].replace(">","")
        extent = infile.extent()
        xmin = extent.xMinimum()
        ymin = extent.yMinimum()
        xmax = extent.xMaximum()
        ymax = extent.yMaximum()
        
        # convert Lat/Lon to UTM zone or GDB - special cases
        if EPSGin == "EPSG:4326":
            newdir = str(os.path.dirname(in_raster) + "/tempMT")
            if not os.path.exists(newdir):
                os.mkdir(newdir)
            out_raster = newdir + "/newUTM.shp"

            # Find UTM Zone
            xMean = (xmax+xmin)/2.0
            if xMean > 180.0:
                xMean = xMean - 360.0
            zone = 1 + int((xMean+180.0)/6.0)
            if (ymax+ymin)/2.0 < 0.0:
                zoneOut = 32700+zone
            else:
                zoneOut = 32600+zone
            EPSGout = "EPSG:"+str(zoneOut)

            # UTM WGS84 = QgsCoordinateReferenceSystem('EPSG:32620')  Zone 20N
            # UTM WGS84 = QgsCoordinateReferenceSystem('EPSG:32702')  Zone 2S
            result = processing.run("native:reprojectlayer", {'INPUT':infile,
                                                              'TARGET_CRS':QgsCoordinateReferenceSystem(str(EPSGout)),
                                                              'CONVERT_CURVED_GEOMETRIES':False,
                                                              'OUTPUT':out_raster})
        return out_raster
