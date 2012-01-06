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

package ncsa.hdf.object.fits;

import java.util.*;
import ncsa.hdf.object.*;

import java.lang.reflect.Array;
import nom.tam.fits.*;

/**
 * FitsDataset describes an multi-dimension array of HDF5 scalar or atomic data
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
public class FitsDataset extends ScalarDS
{
    public static final long serialVersionUID = HObject.serialVersionUID;

    /**
     * The list of attributes of this data object. Members of the list are
     * instance of Attribute.
     */
    private List attributeList;

    private BasicHDU nativeDataset;

    /**
     * Constructs an FitsDataset object with specific netcdf variable.
     * <p>
     * @param fileFormat the netcdf file.
     * @param ncDataset the netcdf variable.
     * @param oid the unique identifier for this dataset.
     */
    public FitsDataset(
            FileFormat fileFormat,
            BasicHDU hdu,
            String dName,
            long[] oid) {
        super (fileFormat, dName, HObject.separator, oid);
        unsignedConverted = false;
        nativeDataset = hdu;
    }
    
    /*
     * (non-Javadoc)
     * @see ncsa.hdf.object.DataFormat#hasAttribute()
     */
    public boolean hasAttribute () { return false; }

    /*
     * (non-Javadoc)
     * @see ncsa.hdf.object.Dataset#copy(ncsa.hdf.object.Group, java.lang.String, long[], java.lang.Object)
     */
    public Dataset copy(Group pgroup, String dstName, long[] dims, Object buff)
    throws Exception {
        // not supported
        throw new UnsupportedOperationException("Unsupported operation for NetCDF.");
    }

    /*
     * (non-Javadoc)
     * @see ncsa.hdf.object.Dataset#readBytes()
     */
    public byte[] readBytes() throws Exception {
        // not supported
        throw new UnsupportedOperationException("Unsupported operation for NetCDF.");
    }

    /*
     * (non-Javadoc)
     * @see ncsa.hdf.object.Dataset#read()
     */
    public Object read() throws Exception {
        Object theData = null;
        Object fitsData = null;

        if (nativeDataset == null) {
            return null;
        }

        try { 
            fitsData = nativeDataset.getData().getData();
        } catch (Exception ex) {
            throw new UnsupportedOperationException("This implementation only supports integer and float dataset. " +
                    "It may not work for other datatypes. \n"+ex);
        }

        int n = get1DLength(fitsData);

        theData = FitsDatatype.allocateArray(nativeDataset.getBitPix(), n);

        to1Darray(fitsData, theData, 0);

        return theData;
    }

    /*
     * (non-Javadoc)
     * @see ncsa.hdf.object.Dataset#write(java.lang.Object)
     */
    public void write(Object buf) throws Exception {
        // not supported
        throw new UnsupportedOperationException("Unsupported operation for NetCDF.");
    }

    /*
     * (non-Javadoc)
     * @see ncsa.hdf.object.DataFormat#getMetadata()
     */
    public List getMetadata() throws Exception {
        if (attributeList != null) {
            return attributeList;
        }

        if (nativeDataset == null) {
            return null;
        }

        Header header = nativeDataset.getHeader();
        if (header == null) {
            return null;
        }

        attributeList = new Vector();
        HeaderCard hc = null;
        Iterator it = header.iterator();
        Attribute attr = null;
        Datatype dtype = new FitsDatatype(Datatype.CLASS_STRING, 80, 0, 0);
        long[] dims = {1};
        String value = null;
        while (it.hasNext()) {
            value = "";
            hc = (HeaderCard)it.next();
            attr = new Attribute(hc.getKey(), dtype, dims);
            String tvalue = hc.getValue();
            if (tvalue != null) {
                value += tvalue;
            }
            tvalue = hc.getComment();
            if (tvalue != null) {
                value += " / " + tvalue;
            }
            attr.setValue(value);
            attributeList.add(attr);
        }

        return attributeList;
    }

    /*
     * (non-Javadoc)
     * @see ncsa.hdf.object.DataFormat#writeMetadata(java.lang.Object)
     */
    public void writeMetadata(Object info) throws Exception {
        // not supported
        throw new UnsupportedOperationException("Unsupported operation for NetCDF.");
    }

    /*
     * (non-Javadoc)
     * @see ncsa.hdf.object.DataFormat#removeMetadata(java.lang.Object)
     */
    public void removeMetadata(Object info) throws Exception {
        // not supported
        throw new UnsupportedOperationException("Unsupported operation for NetCDF.");
    }

    /*
     * (non-Javadoc)
     * @see ncsa.hdf.object.HObject#open()
     */
    public int open() { return -1;}

    /*
     * (non-Javadoc)
     * @see ncsa.hdf.object.HObject#close(int)
     */
    public void close(int did) {}

    /*
     * (non-Javadoc)
     * @see ncsa.hdf.object.Dataset#init()
     */
    public void init() {
        if (nativeDataset == null) {
            return;
        }

        if (rank>0) {
            return; // already called. Initialize only once
        }

        int[] axes= null;
        try { axes = nativeDataset.getAxes(); }
        catch (Exception ex) {}

        if (axes == null) {
            return;
        }


        rank = axes.length;
        if (rank == 0) {
            // a scalar data point
            rank = 1;
            dims = new long[1];
            dims[0] = 1;
        }
        else {
            dims = new long[rank];
            for (int i=0; i<rank; i++) {
                dims[i] = axes[i];
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

    /*
     * (non-Javadoc)
     * @see ncsa.hdf.object.ScalarDS#getPalette()
     */
    public byte[][] getPalette()
    {
        if (palette == null) {
            palette = readPalette(0);
        }

        return palette;
    }

    /*
     * (non-Javadoc)
     * @see ncsa.hdf.object.ScalarDS#readPalette(int)
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
    public static FitsDataset create(
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

    /*
     * (non-Javadoc)
     * @see ncsa.hdf.object.ScalarDS#getPaletteRefs()
     */
    public byte[] getPaletteRefs() {
        return null;
    }

    /*
     * (non-Javadoc)
     * @see ncsa.hdf.object.Dataset#getDatatype()
     */
    public Datatype getDatatype() {
        if (datatype == null) {
            try {datatype = new FitsDatatype(nativeDataset.getBitPix());}
            catch (Exception ex) {}
        }

        return datatype;
    }

    /*
     * (non-Javadoc)
     * @see ncsa.hdf.object.HObject#setName(java.lang.String)
     */
    public void setName (String newName) throws Exception {
        // not supported
        throw new UnsupportedOperationException("Unsupported operation for NetCDF.");
    }

    private int get1DLength(Object data) throws Exception {

        if (!data.getClass().isArray()) {
            return 1;
        }

        int len = Array.getLength(data);

        int total = 0;
        for (int i = 0; i < len; i++) {
            total += get1DLength(Array.get(data, i));
        }

        return total;
    }

    /** copy multi-dimension array of fits data into 1D array */
    private int to1Darray(Object dataIn, Object dataOut, int offset) throws Exception {
        Class component = dataIn.getClass().getComponentType();
        if (component == null) {
            return offset;
        }

        int size = Array.getLength(dataIn);
        if (!component.isArray()) {
            System.arraycopy(dataIn, 0, dataOut, offset, size);
            return offset+size;
        }

        for (int i = size - 1; i >= 0; i--) {
            offset = to1Darray(Array.get(dataIn, i), dataOut, offset);
        }

        return offset;
    }
}
