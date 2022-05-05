##
# This software was developed and / or modified by Raytheon Company,
# pursuant to Contract DG133W-05-CQ-1067 with the US Government.
# 
# U.S. EXPORT CONTROLLED TECHNICAL DATA
# This software product contains export-restricted data whose
# export/transfer/disclosure is restricted by U.S. law. Dissemination
# to non-U.S. persons whether in the United States or abroad requires
# an export license or other authorization.
# 
# Contractor Name:        Raytheon Company
# Contractor Address:     6825 Pine Street, Suite 340
#                         Mail Stop B8
#                         Omaha, NE 68106
#                         402.291.0100
# 
# See the AWIPS II Master Rights File ("Master Rights File.pdf") for
# further licensing information.
##

##
# Converts NetCDF topo-like datasets to HDF5 files so they can be viewed within
# CAVE.
#  
#    
#     SOFTWARE HISTORY
#    
#    Date            Ticket#       Engineer       Description
#    ------------    ----------    -----------    --------------------------
#    10/03/18        #7306         dgilling       Initial Creation.
#    05/07/19        #7842         dgilling       Update to support netcdf4-python.
#
##

import logging
import os
import sys

import h5py
import numpy as np
from scipy import ndimage
import netCDF4

from ufpy import UsageArgumentParser



logging.basicConfig(format="%(asctime)s %(name)s %(levelname)s:  %(message)s",
                        datefmt="%H:%M:%S",
                        level=logging.INFO)
logger = logging.getLogger('createUserTopoFile')


DATA_SET_OPTIONS = { 'shuffle': True, 'compression': 'lzf', 'chunks': (256, 256) }

CRS = '''PROJCS["Equidistant_Cylindrical (LO: 0.0, CM: 0.0)", 
             GEOGCS["WGS84(DD)", 
               DATUM["WGS84", 
                 SPHEROID["WGS84", 6378137.0, 298.257223563]], 
               PRIMEM["Greenwich", 0.0], 
               UNIT["degree", 0.017453292519943295], 
               AXIS["Geodetic longitude", EAST], 
               AXIS["Geodetic latitude", NORTH]], 
             PROJECTION["Equidistant_Cylindrical"], 
             PARAMETER["semi_major", 6371229.0], 
             PARAMETER["semi_minor", 6371229.0], 
             PARAMETER["central_meridian", 0.0], 
             PARAMETER["latitude_of_origin", 0.0], 
             PARAMETER["standard_parallel_1", 0.0], 
             PARAMETER["false_easting", 0.0], 
             PARAMETER["false_northing", 0.0], 
             UNIT["m", 1.0], 
             AXIS["Easting", EAST], 
             AXIS["Northing", NORTH]]
'''

BUNDLE_TEMPLATE = '''<?xml version="1.0" encoding="UTF-8" standalone="yes"?>
<bundle>
    <displayList>
        <displays xsi:type="mapRenderableDisplay"
            xmlns:xsi="http://www.w3.org/2001/XMLSchema-instance">
            <descriptor xsi:type="mapDescriptor">
                <resource>
                    <loadProperties>
                        <capabilities>
                            <capability xsi:type="imagingCapability" />
                        </capabilities>
                    </loadProperties>
                    <properties />
                    <resourceData xsi:type="topoResourceData">
                        <mapName>{map_name}</mapName>
                        <topoFile>{file_name}</topoFile>
                    </resourceData>
                </resource>
            </descriptor>
        </displays>
    </displayList>
</bundle>
'''


class XMLBundleMaker(object):
    
    def __init__(self, input_path, output_path, h5_file_name, map_name=None):
        self.__input_path = input_path
        self.__output_path = output_path
        self.__h5_file_name = h5_file_name
        self.__map_name = map_name
        
    def run(self):
        map_name = self.__map_name
        if not map_name:
            with netCDF4.Dataset(self.__input_path, 'r') as in_file:
                if len(in_file.variables) != 1:
                    raise ValueError("NetCDF file should contain only 1 gridded dataset to convert.")
                try:
                    input_data = next(iter(in_file.variables.values()))
                    map_name = input_data.long_name
                except AttributeError:
                    logger.warn("Could not find attribute long_name in data set. Checking for global attribute depictorName.")
                    try:
                        map_name = in_file.depictorName
                    except AttributeError:
                        logger.warn("Could not find attribute depictorName in global attributes. Setting bundle's map name to hdf5 file name.")
                        map_name = os.path.splitext(self.__h5_file_name)[0]

        logger.info("Creating bundle XML file [%s].", os.path.abspath(self.__output_path))
        xml_string = BUNDLE_TEMPLATE.format(map_name=map_name, file_name=self.__h5_file_name)
        with open(self.__output_path, 'w') as out_file:
            out_file.write(xml_string)

class HDF5FileMaker(object):
    
    def __init__(self, input_path, output_path, interpolation_levels):
        self.__input_path = input_path
        self.__output_path = output_path
        self.__interpolation_levels = interpolation_levels
        
    def run(self):
        out_file = h5py.File(self.__output_path, 'w')
        logger.info("Creating HDF5 file [%s].", os.path.abspath(self.__output_path))
        try:
            with netCDF4.Dataset(self.__input_path, 'r') as in_file:
                logger.debug("in_file.variables.keys(): %s", list(in_file.variables.keys()))
                if len(in_file.variables) != 1:
                    raise ValueError("NetCDF file should contain only 1 gridded dataset to convert.")

                if str(in_file.projName) not in ['CYLINDRICAL_EQUIDISTANT', 'LATLON']:
                    raise ValueError("NetCDF file uses unsupported projection " 
                                     + str(in_file.projName) + 
                                     ". Only CYLINDRICAL_EQUIDISTANT and LATLON are supported.")
                
                input_data = next(iter(in_file.variables.values()))
                self.__ds_width = in_file.dimensions['x'].size
                self.__ds_height = in_file.dimensions['y'].size
                self.__ds_ul_edge_lat  = in_file.yMax
                self.__ds_ul_edge_lon  = in_file.xMin
                self.__ds_lr_edge_lat  = in_file.yMin
                self.__ds_lr_edge_lon  = in_file.xMax

                logger.debug("input_data.shape: %s", input_data.shape)
                dataset_out = out_file.create_dataset('/full', data=input_data[:].data, 
                                                      **DATA_SET_OPTIONS)
                self.setAttributes(dataset_out, 0)
                
                self.__interpolation_group = out_file.create_group("interpolated")
                if self.__interpolation_levels > 0:
                    for i in range(self.__interpolation_levels):
                        self.interpolate(input_data[:].data, i + 1)
        finally:
            if out_file:
                out_file.flush()
                out_file.close()

    def setAttributes(self, d, level):
        d.attrs['Width'] = self.__ds_width // (2 ** level)
        d.attrs['Height'] = self.__ds_height // (2 ** level)

        dx = abs(self.__ds_ul_edge_lon - self.__ds_lr_edge_lon) / self.__ds_width
        dy = abs(self.__ds_ul_edge_lat - self.__ds_lr_edge_lat) / self.__ds_height
        x_inset = dx * (2.0 ** (level - 1))
        y_inset = dy * (2.0 ** (level - 1))

        d.attrs['ulLon'] = magicfloat(self.__ds_ul_edge_lon + x_inset)
        d.attrs['ulLat'] = magicfloat(self.__ds_ul_edge_lat - y_inset)
        d.attrs['lrLon'] = magicfloat(self.__ds_lr_edge_lon - x_inset)
        d.attrs['lrLat'] = magicfloat(self.__ds_lr_edge_lat + y_inset)
        d.attrs['CRS'] = CRS

    def interpolate(self, src_dataset, level):
        logger.info("Creating interpolation level %d", level)
        zoom_factor = pow(2, -level)
        logger.debug("zoom_factor: %f", zoom_factor)
        rescaled_grid = ndimage.zoom(src_dataset, zoom_factor)
        logger.debug("rescaled_grid.shape: %s", rescaled_grid.shape)
        ds = self.__interpolation_group.create_dataset(str(level), data=rescaled_grid,
                                                        **DATA_SET_OPTIONS)
        self.setAttributes(ds, level)
        

def magicfloat(v):
    return np.array([v],dtype=np.float64)

def parse_args():
    parser = UsageArgumentParser.UsageArgumentParser(conflict_handler="resolve")
    parser.add_argument("-f", action="store", dest="inputFile",
                      help="File to convert. Must be a NetCDF formatted file.", 
                      required=True, metavar="cdfFile")
    parser.add_argument("-o", action="store", dest="outputFile", 
                      help="File to write converted data to. Will be in HDF5 format.",
                      metavar="h5File")
    parser.add_argument("-i", action="store", dest="interpolationLevels", 
                      help="Number of interpolation levels to write. Default is 0.",
                      type=int, default=0, metavar="interpLevels")
    parser.add_argument("-m", action="store", dest="mapName", 
                      help="Display name for this dataset. Defaults to reading name from NetCDF metadata.",
                      metavar="mapName")
    options = parser.parse_args()

    try:
        with netCDF4.Dataset(options.inputFile, 'r') as f:
            f = netCDF4.Dataset(options.inputFile, 'r')
            logger.info("Using NetCDF file [%s].", os.path.abspath(options.inputFile))
    except Exception as e:
        parser.error(e)

    if not options.outputFile:
        setattr(options, 'outputFile', 
                os.path.join(os.getcwd(), os.path.splitext(os.path.basename(options.inputFile))[0] + '.h5'))
        logger.debug("Setting output file to [%s]", options.outputFile)
        
    if os.path.splitext(options.outputFile)[1] not in ['.h5', '.hdf5']:
        setattr(options, 'outputFile', options.outputFile + '.h5')
        
    return options

def main():
    logger.info("Starting createUserTopoFile.")
    options = parse_args()
    logger.debug("Command-line options: %s", options)

    try:
        HDF5FileMaker(options.inputFile, options.outputFile, options.interpolationLevels).run()
    except Exception:
        logger.exception("Error generating HDF5 file [%s]", options.outputFile)
        sys.exit(-1)
    else:
        logger.info("HDF5 file generation complete. Copy HDF5 file to path /awips2/edex/data/hdf5/topo/ on the pypies server.")

    bundle_path = os.path.splitext(options.outputFile)[0] + '.xml'
    h5_file_name = os.path.basename(options.outputFile)
    try:
        XMLBundleMaker(options.inputFile, bundle_path, h5_file_name, options.mapName).run()
    except Exception:
        logger.exception("Error generating bundle XML file [%s]", bundle_path)
        sys.exit(-1)
    else:
        logger.info("Bundle file generation complete. Use Localization Perspective to import bundle to CAVE->Bundles->maps.")



if __name__ == '__main__':
    main()
