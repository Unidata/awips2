/*****************************************************************************
 * Copyright by The HDF Group.                                               *
 * Copyright by the Board of Trustees of the University of Illinois.         *
 * All rights reserved.                                                      *
 *                                                                           *
 * This file is part of the HDF Java Products distribution.                  *
 * The full copyright notice, including terms governing use, modification,   *
 * and redistribution, is contained in the files COPYING and Copyright.html. *
 * COPYING can be found at the root of the source code distribution tree.    *
 * Or, see http://hdfgroup.org/products/hdf-java/doc/Copyright.html.         *
 * If you do not have access to either file, you may request a copy from     *
 * help@hdfgroup.org.                                                        *
 ****************************************************************************/

package ncsa.hdf.object.nc2;

import java.lang.reflect.Array;
import java.util.*;

import ucar.ma2.DataType;
import ucar.nc2.Variable;

import ncsa.hdf.object.Dataset;
import ncsa.hdf.object.Datatype;
import ncsa.hdf.object.FileFormat;
import ncsa.hdf.object.Group;
import ncsa.hdf.object.HObject;
import ncsa.hdf.object.ScalarDS;


/**
 * NC2Dataset describes an multi-dimension array of HDF5 scalar or atomic data
 * types, such as byte, int, short, long, float, double and string,
 * and operations performed on the scalar dataset
 * <p>
 * The library predefines a modest number of datatypes. For details, read
 * <a href="http://hdfgroup.org/HDF5/doc/Datatypes.html">
 * The Datatype Interface (H5T)</a>
 * <p>
 * @version 1.1 9/4/2007
 * @author Peter X. Cao
 */
public class NC2Dataset extends ScalarDS
{
	public static final long serialVersionUID = HObject.serialVersionUID;

    /**
     * The list of attributes of this data object. Members of the list are
     * instance of Attribute.
     */
     private List attributeList;

     private Variable nativeDataset;

    /**
     * Constructs an NC2Dataset object with specific netcdf variable.
     * <p>
     * @param fileFormat the netcdf file.
     * @param ncDataset the netcdf variable.
     * @param oid the unique identifier for this dataset.
     */
    public NC2Dataset(
        FileFormat fileFormat,
        Variable ncDataset,
        long[] oid) {
        super (fileFormat, ncDataset.getName(), HObject.separator, oid);
        unsignedConverted = false;
        nativeDataset = ncDataset;
    }

    /*
     * (non-Javadoc)
     * @see ncsa.hdf.object.DataFormat#hasAttribute()
     */
    public boolean hasAttribute () { return false; }

    //Implementing Dataset
    public Dataset copy(Group pgroup, String dstName, long[] dims, Object buff)
    throws Exception {
        // not supported
        throw new UnsupportedOperationException("Unsupported operation for NetCDF.");
    }

    // implementing Dataset
    public byte[] readBytes() throws Exception {
        // not supported
        throw new UnsupportedOperationException("Unsupported operation for NetCDF.");
    }

    // Implementing DataFormat
    public Object read() throws Exception
    {
        Object theData = null;

        if (nativeDataset == null) {
            return null;
        }

        int[] origin = new int[rank];
        int[] shape = new int[rank];

        for (int i=0; i<rank; i++) {
            origin[i] = (int)startDims[i];
            shape[i]=(int)selectedDims[i];
        }

        ucar.ma2.Array ncArray = null;

        try { ncArray = nativeDataset.read(origin, shape); }
        catch (Exception ex) { ncArray = nativeDataset.read(); }
        Object oneD = ncArray.copyTo1DJavaArray();

        if (oneD == null) {
            return null;
        }

        if (oneD.getClass().getName().startsWith("[C")) {
            char[] charA = (char[])oneD;
            int nCols = (int)selectedDims[selectedIndex[1]];
            int nRows = (int)selectedDims[selectedIndex[0]];
            
            String[] strA = new String[nRows];
            String allStr = new String(charA);

            int indx0 = 0;
            for (int i=0; i<nRows; i++) {
                indx0 = i*nCols;
                strA[i] = allStr.substring(indx0, indx0+nCols);
            }
             theData = strA;
        } else {
            theData = oneD;
        }
        
        return theData;
    }

    //Implementing DataFormat
    public void write(Object buf) throws Exception {
        // not supported
        throw new UnsupportedOperationException("Unsupported operation for NetCDF.");
    }

    // Implementing DataFormat
    public List getMetadata() throws Exception {
        if (attributeList != null) {
            return attributeList;
        }

        if (nativeDataset == null) {
            return (attributeList=null);
        }

        List ncAttrList = nativeDataset.getAttributes();
        if (ncAttrList == null) {
            return (attributeList=null);
        }

        int n = ncAttrList.size();
        attributeList = new Vector(n);
        ucar.nc2.Attribute ncAttr = null;
        for (int i=0; i<n; i++) {
            ncAttr = (ucar.nc2.Attribute)ncAttrList.get(i);
            attributeList.add(NC2File.convertAttribute(ncAttr));
        }

        return attributeList;
    }

    // implementing DataFormat
    public void writeMetadata(Object info) throws Exception {
        // not supported
        throw new UnsupportedOperationException("Unsupported operation for NetCDF.");
    }

    // implementing DataFormat
    public void removeMetadata(Object info) throws Exception {
        // not supported
        throw new UnsupportedOperationException("Unsupported operation for NetCDF.");
    }

    // Implementing HObject
    public int open() { return -1;}

    // Implementing HObject
    public void close(int did) {}

    /**
     * Retrieve and initialize dimensions and member information.
     */
    public void init() {
        if (nativeDataset == null) {
            return;
        }

        if (rank>0) {
            return; // already called. Initialize only once
        }

        isText = nativeDataset.getDataType().equals(DataType.STRING);
        boolean isChar = nativeDataset.getDataType().equals(DataType.CHAR);

        rank = nativeDataset.getRank();

        if (rank == 0) {
            // a scalar data point
            rank = 1;
            dims = new long[1];
            dims[0] = 1;
        }
        else {
            dims = new long[rank];
            for (int i=0; i<rank; i++) {
                dims[i] = (nativeDataset.getDimension(i).getLength());
            }
        }

        startDims = new long[rank];
        selectedDims = new long[rank];
        for (int i=0; i<rank; i++) {
            startDims[i] = 0;
            selectedDims[i] = 1;
        }

        if (rank == 1) {
            selectedIndex[0] = 0;
            selectedDims[0] = dims[0];
        }
        else if (rank == 2) {
            selectedIndex[0] = 0;
            selectedIndex[1] = 1;
            selectedDims[0] = dims[0];
            selectedDims[1] = dims[1];
        }
        else if (rank > 2) {
            selectedIndex[0] = 0;
            selectedIndex[1] = 1;
            selectedIndex[2] = 2;
            selectedDims[0] = dims[0];
            selectedDims[1] = dims[1];
        }

        if ((rank > 1) && isText) {
            selectedDims[1] = 1;
        }
    }

    // Implementing ScalarDS
    public byte[][] getPalette()
    {
        if (palette == null) {
            palette = readPalette(0);
        }

        return palette;
    }

    /**
     * read specific image palette from file.
     * @param idx the palette index to read
     * @return the palette data into two-dimension byte array, byte[3][256]
     */
    public byte[][] readPalette(int idx) {
        return null;
    }

    /**
     * Creates a new dataset.
     * @param name the name of the dataset to create.
     * @param pgroup the parent group of the new dataset.
     * @param type the datatype of the dataset.
     * @param dims the dimension size of the dataset.
     * @param maxdims the max dimension size of the dataset.
     * @param chunk the chunk size of the dataset.
     * @param gzip the level of the gzip compression.
     * @param data the array of data values.
     * @return the new dataset if successful. Otherwise returns null.
     */
    public static NC2Dataset create(
         String name,
        Group pgroup,
        Datatype type,
        long[] dims,
        long[] maxdims,
        long[] chunks,
        int gzip,
        Object data) throws Exception {
        // not supported
        throw new UnsupportedOperationException("Unsupported operation for NetCDF.");
    }

    /** returns the byte array of palette refs.
     *  returns null if there is no palette attribute attached to this dataset.
     */
    public byte[] getPaletteRefs() {
        return null;
    }

    // implementing ScalarDS
    public Datatype getDatatype() {
        if (datatype == null) {
            datatype = new NC2Datatype(nativeDataset.getDataType());
        }

        return datatype;
    }

    /**
     * Sets the name of the data object.
     * <p>
     * @param newName the new name of the object.
     */
    public void setName (String newName) throws Exception {
        // not supported
        throw new UnsupportedOperationException("Unsupported operation for NetCDF.");
    }

}
