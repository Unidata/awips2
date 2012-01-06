import os
import sys
import time
import gzip
import h5py
from scipy import io

# create the hdf5 variable-length string type
strh5 = h5py.new_vlen(str)

def copy_var_dimensions(cdfVariable, toGroup):
    """
copy_var_dimensions(cdfVariable, toGroup)
Copy cdfVariable.dimensions to a /dimensions dataset
in the HDF5 group toGroup.
"""
    lenDims = len(cdfVariable.dimensions)
    varDims = toGroup.create_dataset('dimensions', 
        (lenDims,), dtype=strh5) 
    for idx in xrange(lenDims):
        varDims[idx] = cdfVariable.dimensions[idx]

 
def copy_timeData(fromFile, fromVar, lvlNum, lvlGroup):
    """
Create multiple time groups in lvlGroup and copy data to them.
"""
    reftime = int(fromFile.variables['reftime'][0])
    vtMrt = fromFile.variables['valtimeMINUSreftime']
    duration = fromVar.attributes['duration'][0]
    for timeNum in range(vtMrt.shape[0]):
        # Legacy code uses vtMrt as timestamp, ignores reftime.
        # We will, too, for script compatibility
        tstart = vtMrt[timeNum]
        tend   = tstart + duration
        stuple = time.gmtime(tstart)
        etuple = time.gmtime(tend)
        sstr = time.strftime('%Y_%m_%d_%H', stuple)
        estr = time.strftime('%Y_%m_%d_%H', etuple)
        tGroupName = sstr + '--' + estr
        tGroup = lvlGroup.create_group(tGroupName)
        data=fromVar[timeNum, lvlNum]
        # The data was flipped N/S from GFE's expectations (not sure why).
        data=data[::-1, :]
        # climo grids (temp in K and rainfall in mm) should never be negative.
        # the netcdf files use -9999.0 as "no data". 
        # clamp negative values to zero.
        data[data < 0] = 0.0
        tGroup.create_dataset('Data', data=data)

def convert(fromFile, toFile):
    "Copy the data in netCDF file fromFile to HDF5 file toFile."
    # Copy attributes.
    # NetCDF and HDF5 both support attributes. Unfortunately,
    # the Hdf5DataStore class does not. So, rather than copy 
    # the netCDF file attributes to the HDF5 file's attributes,
    # create an "attributes" group and store the attributes as
    # datasets within the group.
    if len(fromFile.attributes) > 0:
        attVar = toFile.create_group('attributes')
        for att, val in fromFile.attributes.iteritems():
            if val is not None:
                attVar.create_dataset(att, data=val)

    # Copy netCDF dimensions dict to /dimensions group
    # (as datasets; see notes above)
    tofDims = toFile.create_group('dimensions')
    for att, val in fromFile.dimensions.iteritems():
        if val is not None:
            tofDims.create_dataset(att, data=val)

    # copy netCDF variables
    for varName in fromFile.variables.keys():
        if (varName.endswith("Inventory") or varName.endswith("Levels")) and \
           varName != "Inventory" and varName != "Levels":
            pass
        else:
            # create an HDF5 group for the variable
            netVar = fromFile.variables[varName]
            varGroup = toFile.create_group(varName)

            # copy attributes of the variable to the group
            # use 'attributes' group for data store problem
            if len(netVar.attributes) > 0:
                attGroup = varGroup.create_group('attributes')
                for att, val in netVar.attributes.iteritems():
                    if val is not None:
                        attGroup.create_dataset(att, data=val)

           # if <var>Levels exists, create a group for each level
            nvLvlName = varName + "Levels"
            if nvLvlName in fromFile.variables:
                nvLevels = fromFile.variables[nvLvlName]
                for level in range(nvLevels.shape[0]):
                    lvlName = "".join(nvLevels[level])
                    lvlGroup = varGroup.create_group(lvlName)

                    # if <var>Inventory exists, add it as a dataset in the level
                    # so instead of tpInventory, you have /tp/SFC/Inventory
                    nvInvName = varName + 'Inventory'
                    if nvInvName in fromFile.variables:
                        invVar = fromFile.variables[nvInvName]
                        lvlGroup.create_dataset('Inventory', data=invVar[...])
                    
                    # copy the actual data to timerange datasets
                    copy_timeData(fromFile, netVar, level, lvlGroup)
            else:
                copy_var_dimensions(netVar, varGroup)
                varGroup.create_dataset('Data', data=netVar[...])
    
def main():
    """
    Copy the data in a netCDF file to an HDF5 file with the same name except
for an '.h5' extension, which replaces the '.cdf' extension if present and
otherwise is appended.

    This code was created to convert legacy netCDF climo files to HDF5
for use by EDEX. It may not be appropriate for more general use.
"""
    tfname = None
    if len(sys.argv) != 2:
        sys.stderr.write("usage: " + sys.argv[0] + " <netCDFfile>\n")
        return 1
    
    fromPath = sys.argv[1]
    if fromPath.endswith('.gz'):
        name = fromPath[:-3]
        # uncompress fromPath to a temp file
        tfname = os.tempnam()
        gzfile = None
        tempFile = None
        try:
            gzfile = gzip.open(fromPath, "r")
            tempFile = open(tfname, "w")
            unzipped = gzfile.read(512)
            while("" != unzipped):
                tempFile.write(unzipped)
                unzipped = gzfile.read(512)
        finally:
            if gzfile is not None:
                gzfile.close()
            if tempFile is not None:
                tempFile.close()
        
        fromPath = tfname
    else:
        nameTup = fromPath.rpartition('.cdf')
        if nameTup[0] == '':
            name = nameTup[2]
        else:
            name = nameTup[0]
    toPath = name + '.h5'

    fromFile = None
    toFile = None
    delayedExcpt = None
    try:
        fromFile = io.netcdf_file(fromPath, 'r')
        toFile = h5py.File(toPath, 'w')
        convert(fromFile, toFile)
    
    finally:
        if fromFile is not None:
            try:
                fromFile.close()
            except Exception, e:
                delayedExcpt = e
        if toFile is not None:
                toFile.close()
        if tfname is not None:
            os.unlink(tfname)
    
    if delayedExcpt is not None:
        raise delayedExcpt
    return 0
    
if __name__ == '__main__':
    sys.exit( main() )
