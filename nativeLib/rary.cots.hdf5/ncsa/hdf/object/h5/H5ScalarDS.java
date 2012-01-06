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

package ncsa.hdf.object.h5;

import java.lang.reflect.Array;
import java.util.*;

import ncsa.hdf.hdf5lib.*;
import ncsa.hdf.hdf5lib.exceptions.*;
import ncsa.hdf.object.*;

/**
 * H5ScalarDS describes a multi-dimension array of HDF5 scalar or atomic data
 * types, such as byte, int, short, long, float, double and string, and
 * operations performed on the scalar dataset.
 * <p>
 * The library predefines a modest number of datatypes. For details, read
 * <a href="http://hdfgroup.org/HDF5/doc/Datatypes.html">The Datatype Interface (H5T).</a>
 * <p>
 * @version 1.1 9/4/2007
 * @author Peter X. Cao
 */
public class H5ScalarDS extends ScalarDS
{
    /**
     * @see ncsa.hdf.object.HObject#serialVersionUID
     */
  public static final long serialVersionUID = HObject.serialVersionUID;

    /**
     * The list of attributes of this data object. Members of the list are
     * instance of Attribute.
     */
     private List attributeList;

     /** The byte array containing references of palettes.
      * Each reference requires  eight bytes storage. Therefore, the array length
      * is 8*numberOfPalettes.
     */
     private byte[] paletteRefs;

     /** flag to indicate if the dataset is a variable length */
     private boolean isVLEN = false;

     /** flag to indicate if the dataset is enum */
     private boolean isEnum = false;
     
     /** flag to indicate if the dataset is an external dataset */
     private boolean isExternal = false;
     
     /** flag to indicate if the datatype in file is the same as dataype in memory*/
     private boolean isNativeDatatype = true;
     
     /** flag to indicate is the datatype is reg. ref. */
     private boolean isRegRef = false;
     
     private int nAttributes = -1;
     
     /**
      * Constructs an instance of a H5ScalarDS object with specific name and path.
      * <p>
      * For example, in H5ScalarDS(h5file, "dset", "/arrays/"), "dset" is the
      * name of the dataset, "/arrays" is the group path of the dataset.
      *
      * @param theFile the file that contains the data object.
      * @param theName the name of the data object, e.g. "dset".
      * @param thePath the full path of the data object, e.g. "/arrays/".
      */
    public H5ScalarDS(FileFormat theFile, String theName, String thePath)
    {
        this(theFile, theName, thePath, null);
    }

    /**
     * @deprecated  Not for public use in the future.<br>
     *  Using {@link #H5ScalarDS(FileFormat, String, String)}
     */
    public H5ScalarDS(
        FileFormat theFile,
        String theName,
        String thePath,
        long[] oid)
    {
        super (theFile, theName, thePath, oid);
        unsignedConverted = false;
        paletteRefs = null;
        
        if ((oid == null) && (theFile != null)) {
            // retrieve the object ID
            try {
                byte[] ref_buf = H5.H5Rcreate(theFile.getFID(), this.getFullName(), HDF5Constants.H5R_OBJECT, -1);
                this.oid = new long[1];
                this.oid[0] = HDFNativeData.byteToLong(ref_buf, 0);
             } catch (Exception ex) {}
        }
    }

    /*
     * (non-Javadoc)
     * @see ncsa.hdf.object.DataFormat#hasAttribute()
     */
    public boolean hasAttribute () 
    { 
        if (nAttributes < 0) {
            
            // test if it is an image
            int did = open();
            nAttributes = 0;
            
            int aid=-1, atid=-1, tid=0;
            try
            {
                nAttributes = H5.H5Aget_num_attrs(did);
                tid= H5.H5Dget_type(did);
                
                int tclass = H5.H5Tget_class(tid);
                isText = (tclass==HDF5Constants.H5T_STRING);
                isVLEN = ((tclass==HDF5Constants.H5T_VLEN) || H5.H5Tis_variable_str(tid));
                isEnum = (tclass==HDF5Constants.H5T_ENUM);

                // try to find out if the dataset is an image
                aid = H5.H5Aopen_name(did, "CLASS");
                
                atid = H5.H5Aget_type(aid);
                
                int aclass = H5.H5Tget_class(atid);
                if (aclass == HDF5Constants.H5T_STRING)
                {
                    int size = H5.H5Tget_size(atid);
                    byte[] attrValue = new byte[size];
                    H5.H5Aread(aid, atid, attrValue);
                    String strValue = new String(attrValue).trim();
                    isImage = strValue.equalsIgnoreCase("IMAGE");
                    isImageDisplay = isImage;
                }
            } catch (Exception ex) { ;}
            finally
            {
                try { H5.H5Tclose(atid); } catch (HDF5Exception ex) {;}
                try { H5.H5Aclose(aid); } catch (HDF5Exception ex) {;}
                try { H5.H5Tclose(tid); } catch (HDF5Exception ex) {;}
            }

            // retrieve the IMAGE_MINMAXRANGE
            int asid=-1;
            try
            {
                // try to find out if the dataset is an image
                aid = H5.H5Aopen_name(did, "IMAGE_MINMAXRANGE");
                if (aid > 0)
                {
                    atid = H5.H5Aget_type(aid);
                    int tmptid = atid;
                    atid = H5.H5Tget_native_type(tmptid);
                    try {H5.H5Tclose(tmptid); } catch (Exception ex) {}
                    
                    asid = H5.H5Aget_space(aid);
                    long adims[] = null;

                    int arank = H5.H5Sget_simple_extent_ndims(asid);
                    if (arank > 0)
                    {
                        adims = new long[arank];
                        H5.H5Sget_simple_extent_dims(asid, adims, null);
                    }

                    // retrieve the attribute value
                    long lsize = 1;
                    for (int j=0; j<adims.length; j++) {
                        lsize *= adims[j];
                    }
                    Object avalue = H5Datatype.allocateArray(atid, (int)lsize);
                    if (avalue != null)
                    {
                        H5.H5Aread(aid, atid, avalue);
                        double x0=0, x1=0;
                        try {
                            x0 = Double.valueOf(java.lang.reflect.Array.get(avalue, 0).toString()).doubleValue();
                            x1 = Double.valueOf(java.lang.reflect.Array.get(avalue, 1).toString()).doubleValue();
                        } catch (Exception ex2) { x0=x1=0;}
                        if (x1 > x0)
                        {
                            imageDataRange = new double[2];
                            imageDataRange[0] = x0;
                            imageDataRange[1] = x1;
                        }
                    }
                } // if (aid > 0)
            } catch (Exception ex) {}
            finally
            {
                try { H5.H5Tclose(atid); } catch (HDF5Exception ex) {;}
                try { H5.H5Sclose(asid); } catch (HDF5Exception ex) {;}
                try { H5.H5Aclose(aid); } catch (HDF5Exception ex) {;}
            }
            
            close(did);
        }
        
        return (nAttributes>0);
    }


    /*
     * (non-Javadoc)
     * @see ncsa.hdf.object.Dataset#init()
     */
    public void init()
    {
        if (rank > 0) {
            resetSelection();
            return; // already called. Initialize only once
        }

        int did=-1, sid=-1, tid=-1;

        did = open();
        paletteRefs = getPaletteRefs(did);
        
        int pid=-1;
        try {
            sid = H5.H5Dget_space(did);
            tid= H5.H5Dget_type(did);
            
            int tclass = H5.H5Tget_class(tid);
            rank = H5.H5Sget_simple_extent_ndims(sid);
            
            isText = (tclass==HDF5Constants.H5T_STRING);
            isVLEN = ((tclass==HDF5Constants.H5T_VLEN) || H5.H5Tis_variable_str(tid));
            isEnum = (tclass==HDF5Constants.H5T_ENUM);
            isUnsigned = H5Datatype.isUnsigned(tid);
            isRegRef=H5.H5Tequal(tid, HDF5Constants.H5T_STD_REF_DSETREG);
            
            // check if it is an external dataset
           try {
                pid = H5.H5Dget_create_plist(did);
                int nfiles = H5.H5Pget_external_count(pid);
                isExternal = (nfiles>1);
            } catch (Exception ex) {}

            // check if datatype in file is ntive datatype
            int tmptid = 0;
            try {
                tmptid = H5.H5Tget_native_type(tid);
                isNativeDatatype = H5.H5Tequal(tid, tmptid);
                
                /* see if fill value is defined */
                int[] fillStatus = {0};
                if (H5.H5Pfill_value_defined(pid, fillStatus)>=0)
                {
                    if (fillStatus[0] == HDF5Constants.H5D_FILL_VALUE_USER_DEFINED)
                    {
                        fillValue = H5Datatype.allocateArray(tmptid, 1);
                        try { 
                            H5.H5Pget_fill_value(pid, tmptid, fillValue ); 
                        }   catch (Exception ex2) { fillValue = null; }
                    }
                }
            } catch (HDF5Exception ex) {;}
            finally {
                try { H5.H5Tclose(tmptid); } catch (HDF5Exception ex) {;}
                try {H5.H5Pclose(pid);} catch (Exception ex) {;}
            }

            if (rank == 0) {
                // a scalar data point
                rank = 1;
                dims = new long[1];
                dims[0] = 1;
            }
            else
            {
                dims = new long[rank];
                maxDims = new long[rank];
                H5.H5Sget_simple_extent_dims(sid, dims, maxDims);
            }
        } catch (HDF5Exception ex) {;}
        finally
        {
            try { H5.H5Tclose(tid); } catch (HDF5Exception ex2) {}
            try { H5.H5Sclose(sid); } catch (HDF5Exception ex2) {}
        }

        // check for the type of image and interlace mode
        // it is a true color image at one of three cases:
        // 1) IMAGE_SUBCLASS = IMAGE_TRUECOLOR,
        // 2) INTERLACE_MODE = INTERLACE_PIXEL,
        // 3) INTERLACE_MODE = INTERLACE_PLANE
        if ((rank >=3) && isImage)
        {
            interlace = -1;
            isTrueColor = isStringAttributeOf(did, "IMAGE_SUBCLASS", "IMAGE_TRUECOLOR");            

            if (isTrueColor) {
                interlace = INTERLACE_PIXEL;
                if (isStringAttributeOf(did, "INTERLACE_MODE", "INTERLACE_PLANE")) {
                    interlace = INTERLACE_PLANE;
                }
            }
        }
        
        close(did);
        
        startDims = new long[rank];
        selectedDims = new long[rank];
        resetSelection();
    }
    
    private boolean isStringAttributeOf(int objID, String name, String value) {
        boolean retValue = false;
        int aid=-1, atid=-1;
       
        try
        {
            // try to find out interlace mode
            aid = H5.H5Aopen_name(objID, name);
            atid = H5.H5Aget_type(aid);
            int size = H5.H5Tget_size(atid);
            byte[] attrValue = new byte[size];
            H5.H5Aread(aid, atid, attrValue);
            String strValue = new String(attrValue).trim();
            retValue = strValue.equalsIgnoreCase(value);
        } catch (Exception ex) {}
        finally
        {
            try { H5.H5Tclose(atid); } catch (HDF5Exception ex) {;}
            try { H5.H5Aclose(aid); } catch (HDF5Exception ex) {;}
        }

        return retValue;
    }
    
    /**
     * Resets selection of dataspace
     */
    private void resetSelection() {
        
        for (int i=0; i<rank; i++)
        {
            startDims[i] = 0;
            selectedDims[i] = 1;
            if (selectedStride != null) {
                selectedStride[i] = 1;
            }
        }

        if (interlace == INTERLACE_PIXEL)
        {
            // 24-bit TRUE color image
            // [height][width][pixel components]
            selectedDims[2] = 3;
            selectedDims[0] = dims[0];
            selectedDims[1] = dims[1];
            selectedIndex[0] = 0; // index for height
            selectedIndex[1] = 1; // index for width
            selectedIndex[2] = 2; // index for depth
        }
        else if (interlace == INTERLACE_PLANE)
        {
            // 24-bit TRUE color image
            // [pixel components][height][width]
            selectedDims[0] = 3;
            selectedDims[1] = dims[1];
            selectedDims[2] = dims[2];
            selectedIndex[0] = 1; // index for height
            selectedIndex[1] = 2; // index for width
            selectedIndex[2] = 0; // index for depth
        }
        else if (rank == 1)
        {
            selectedIndex[0] = 0;
            selectedDims[0] = dims[0];
        }
        else if (rank == 2)
        {
            selectedIndex[0] = 0;
            selectedIndex[1] = 1;
            selectedDims[0] = dims[0];
            selectedDims[1] = dims[1];
        }
        else if (rank > 2)
        {
//            // hdf-java 2.5 version: 3D dataset is arranged in the order of [frame][height][width] by default
//            selectedIndex[1] = rank-1; // width, the fastest dimension
//            selectedIndex[0] = rank-2; // height
//            selectedIndex[2] = rank-3; // frames
            
//
//            (5/4/09) Modified the default dimension order. See bug#1379
//            We change the default order to the following. In most situation, 
//            users want to use the nature order of
//               selectedIndex[0] = 0
//               selectedIndex[1] = 1
//               selectedIndex[2] = 2
//            Most of NPOESS data is the the order above.  
            
            if (isImage) {
                // 3D dataset is arranged in the order of [frame][height][width]
                selectedIndex[1] = rank-1; // width, the fastest dimension
                selectedIndex[0] = rank-2; // height
                selectedIndex[2] = rank-3; // frames          
            } else {
                selectedIndex[0] = 0; // width, the fastest dimension
                selectedIndex[1] = 1; // height
                selectedIndex[2] = 2; // frames                  
            }
            
            selectedDims[selectedIndex[0]] = dims[selectedIndex[0]];
            selectedDims[selectedIndex[1]] = dims[selectedIndex[1]];
        }

        // only can display one-D a time for text data
        if ((rank > 1) && isText)
        {
            selectedIndex[0] = rank-1;
            selectedIndex[1] = 0;
            selectedDims[0] = 1;
            selectedDims[selectedIndex[0]] = dims[selectedIndex[0]];
        }
        
        isDataLoaded = false;
        isDefaultImageOrder = true;
    }

    /*
     * (non-Javadoc)
     * @see ncsa.hdf.object.Dataset#clear()
     */
    public void clear() {
        super.clear(); 
        
      if (attributeList != null) {
            ((Vector)attributeList).setSize(0);
        }
    }

    /*
     * (non-Javadoc)
     * @see ncsa.hdf.object.Dataset#copy(ncsa.hdf.object.Group, java.lang.String, long[], java.lang.Object)
     */
    public Dataset copy(Group pgroup, String dstName, long[] dims, Object buff) throws Exception
    {
        // must give a location to copy
        if (pgroup == null) {
            return null;
        }
        
        Dataset dataset = null;
        int srcdid=-1, dstdid=-1, tid=-1, sid=-1, plist=-1;
        String dname=null, path=null;

        if (pgroup.isRoot()) {
            path = HObject.separator;
        } else {
            path = pgroup.getPath()+pgroup.getName()+HObject.separator;
        }
        dname = path + dstName;

        try {
            srcdid = open();
            tid = H5.H5Dget_type(srcdid);
            sid = H5.H5Screate_simple(dims.length, dims, null);
            plist = H5.H5Dget_create_plist(srcdid);
            
            try {
                dstdid = H5.H5Dcreate(pgroup.getFID(), dname, tid, sid, plist);
            } finally {
                try { H5.H5Dclose(dstdid); } catch(Exception ex2) {}
            }

            dataset = new H5ScalarDS(pgroup.getFileFormat(), dstName, path);

            dstdid = dataset.open();
            try {H5File.copyAttributes(srcdid, dstdid);}
            catch (Exception ex) {}

            if (buff != null) {
                dataset.init();
                dataset.write(buff);
            }       
        }
        finally {
            try { H5.H5Pclose(plist); } catch(Exception ex) {}
            try { H5.H5Sclose(sid); } catch(Exception ex) {}
            try { H5.H5Tclose(tid); } catch(Exception ex) {}
            try { H5.H5Dclose(srcdid); } catch(Exception ex) {}
            try { H5.H5Dclose(dstdid); } catch(Exception ex) {}
        }

        pgroup.addToMemberList(dataset);
        
        ((ScalarDS)dataset).setIsImage(isImage);
        
        return dataset;
    }

    /*
     * (non-Javadoc)
     * @see ncsa.hdf.object.Dataset#readBytes()
     */
    public byte[] readBytes() throws HDF5Exception
    {
        byte[] theData = null;

        if (rank <= 0) {
            init();
        }

        int did = open();
        int fspace=-1, mspace=-1, tid=-1;

        try
        {
            long[] lsize = {1};
            for (int j=0; j<selectedDims.length; j++) {
                lsize[0] *= selectedDims[j];
            }

            fspace = H5.H5Dget_space(did);
            mspace = H5.H5Screate_simple(rank, selectedDims, null);

            // set the rectangle selection
            // HDF5 bug: for scalar dataset, H5Sselect_hyperslab gives core dump
            if (rank*dims[0] > 1)
            {
                H5.H5Sselect_hyperslab(
                    fspace,
                    HDF5Constants.H5S_SELECT_SET,
                    startDims,
                    selectedStride,
                    selectedDims,
                    null );   // set block to 1
            }

            tid = H5.H5Dget_type(did);
            int size = H5.H5Tget_size(tid)*(int)lsize[0];
            theData = new byte[size];
            H5.H5Dread(did, tid, mspace, fspace, HDF5Constants.H5P_DEFAULT, theData);
        }
        finally
        {
            try { H5.H5Sclose(fspace); } catch (Exception ex2) {}
            try { H5.H5Sclose(mspace); } catch (Exception ex2) {}
            try { H5.H5Tclose(tid); } catch (HDF5Exception ex2) {}
            close(did);
        }

        return theData;
    }

    /*
     * (non-Javadoc)
     * @see ncsa.hdf.object.Dataset#read()
     */
    public Object read() throws HDF5Exception
    {
        Object theData = null;
        int did=-1, tid=-1;
        int spaceIDs[] = {-1, -1}; // spaceIDs[0]=mspace, spaceIDs[1]=fspace
        
        if (rank <= 0) {
            init();
        }
        
         if (isExternal) {
            String pdir = this.getFileFormat().getAbsoluteFile().getParent();
            if (pdir == null) {
                pdir = ".";
            }
            H5.H5Dchdir_ext(pdir);
        }

        long[] lsize = {1};
        try {
            did = open();
            lsize[0] = selectHyperslab(did, spaceIDs);
 
            if (lsize[0] == 0) {
                throw new HDF5Exception("No data to read.\nEither the dataset or the selected subset is empty.");
            }
            
            // check is storage space is allocated
            try {
                long ssize = H5.H5Dget_storage_size(did);
                if (ssize <=0) {
                    throw new HDF5Exception("Storage space is not allocated.");
                }
            } catch (Exception ex) {}

            tid = H5.H5Dget_type(did);
            if (!isNativeDatatype) {
                int tmptid = -1;
                try {
                    tmptid = tid;
                    tid = H5.H5Tget_native_type(tmptid);
                } finally {
                    try { H5.H5Tclose(tmptid); } catch (Exception ex2) {}
                }
            }

            boolean isREF = (H5.H5Tequal(tid, HDF5Constants.H5T_STD_REF_OBJ));
            
            if ( (originalBuf ==null) || isText || isREF ||
                ((originalBuf!=null) && (lsize[0] !=nPoints))) {
              try {
                  theData = H5Datatype.allocateArray(tid, (int)lsize[0]);             
              } catch (OutOfMemoryError err) {
                throw new HDF5Exception("Out Of Memory.");
              }
            } else {
                theData = originalBuf; // reuse the buffer if the size is the same
            }

            if (theData != null) {
                if (isVLEN)
                {
                    H5.H5DreadVL(did, tid, spaceIDs[0], spaceIDs[1], HDF5Constants.H5P_DEFAULT, (Object[])theData);
                }
                else
                {
                    H5.H5Dread( did, tid, spaceIDs[0], spaceIDs[1], HDF5Constants.H5P_DEFAULT, theData);

                    if (isText && convertByteToString) {
                        theData = byteToString((byte[])theData, H5.H5Tget_size(tid));
                    } else if (isREF) {
                        theData = HDFNativeData.byteToLong((byte[])theData);
                    }
                    else if (isEnum && isEnumConverted())
                    {
                        theData = H5Datatype.convertEnumValueToName(tid, theData, null);
                    }
                }
            } // if (theData != null)
        } finally {
            try { H5.H5Sclose(spaceIDs[0]); } catch (Exception ex) {}
            try { H5.H5Sclose(spaceIDs[1]); } catch (Exception ex) {}
            try { H5.H5Tclose(tid); } catch (Exception ex2) {}
            close(did);
        }

        return theData;
    }

    /*
     * (non-Javadoc)
     * @see ncsa.hdf.object.Dataset#write(java.lang.Object)
     */
    public void write(Object buf) throws HDF5Exception
    {
        int did=-1, tid=-1;
        int spaceIDs[] = {-1, -1}; // spaceIDs[0]=mspace, spaceIDs[1]=fspace
        Object tmpData = null;

        if (buf == null) {
            return;
        }

        if (isVLEN && !isText) { 
            throw(new HDF5Exception("Writing non-string variable-length data is not supported"));
        } else if (isEnum && isEnumConverted()) {
            throw(new HDF5Exception("Writing converted enum data is not supported"));
        } else if (isRegRef) {
            throw(new HDF5Exception("Writing region references data is not supported"));
        }

        long[] lsize = {1};
        try  {
            did = open();
            lsize[0] = selectHyperslab(did, spaceIDs);
            tid = H5.H5Dget_type(did);
            
            if (!isNativeDatatype) {
                int tmptid = -1;
                try {
                    tmptid = tid;
                    tid = H5.H5Tget_native_type(tmptid);
                } finally {
                    try { H5.H5Tclose(tmptid); } catch (Exception ex2) {}
                }
            }
             
            isText = (H5.H5Tget_class(tid)==HDF5Constants.H5T_STRING);

            // check if need to convert integer data
            int tsize = H5.H5Tget_size(tid);
            String cname = buf.getClass().getName();
            char dname = cname.charAt(cname.lastIndexOf("[")+1);
            boolean doConversion = (((tsize==1) && (dname=='S')) || 
                    ((tsize==2) && (dname=='I')) || 
                    ((tsize==4) && (dname=='J')) ||
                    (isUnsigned && unsignedConverted) );
            if ( doConversion) {
                tmpData = convertToUnsignedC(buf, null);
            } 
            // do not convert v-len strings, regardless of conversion request type 
            else if (isText && convertByteToString && !H5.H5Tis_variable_str(tid)) { 
                tmpData = stringToByte((String[])buf, H5.H5Tget_size(tid)); 
            } else {
                tmpData = buf;
            }
            H5.H5Dwrite(did, tid, spaceIDs[0], spaceIDs[1], HDF5Constants.H5P_DEFAULT, tmpData);

        } finally {
            tmpData = null;
            try { H5.H5Sclose(spaceIDs[0]); } catch (Exception ex) {}
            try { H5.H5Sclose(spaceIDs[1]); } catch (Exception ex) {}
            try { H5.H5Tclose(tid); } catch (Exception ex) {}
            close(did);
        }
    }

    /**
     * Set up the selection of hyperslab
     * @param did IN dataset ID
     * @param spaceIDs IN/OUT memory and file space IDs -- spaceIDs[0]=mspace, spaceIDs[1]=fspace
     * @return total number of data point selected
     */
    private long selectHyperslab (int did, int[] spaceIDs) throws HDF5Exception {
        long lsize = 1;
        
        boolean isAllSelected = true;
        for (int i=0; i<rank; i++)
        {
            lsize *= selectedDims[i];
            if (selectedDims[i] < dims[i]) {
                isAllSelected = false;
            }
        }

        if (isAllSelected)
        {
            spaceIDs[0] = HDF5Constants.H5S_ALL;
            spaceIDs[1] = HDF5Constants.H5S_ALL;
        }
        else
        {
            spaceIDs[1] = H5.H5Dget_space(did);
            
            // When 1D dataspace is used in chunked dataset, reading is very slow.
            // It is a known problem on HDF5 library for chunked dataset. 
            //mspace = H5.H5Screate_simple(1, lsize, null);
            spaceIDs[0] = H5.H5Screate_simple(rank, selectedDims, null);
            H5.H5Sselect_hyperslab(spaceIDs[1], HDF5Constants.H5S_SELECT_SET, startDims, selectedStride, selectedDims, null );
        }
        
        if ( (rank >1) && (selectedIndex[0]>selectedIndex[1]))
            isDefaultImageOrder = false;
        else
            isDefaultImageOrder = true;
        
        return lsize;
    }
    
    /*
     * (non-Javadoc)
     * @see ncsa.hdf.object.DataFormat#getMetadata()
     */
    public List getMetadata() throws HDF5Exception
    {
        if (rank <= 0) {
            init();
        }
        
        if (attributeList != null) {
            return attributeList;
        }
        
        // load attributes first
        int did=-1, pid=-1;
        try {
            did = open();
            attributeList = H5File.getAttribute(did);

            // get the compresson and chunk information
            pid = H5.H5Dget_create_plist(did);
            if (H5.H5Pget_layout(pid) == HDF5Constants.H5D_CHUNKED)
            {
                chunkSize = new long[rank];
                H5.H5Pget_chunk(pid, rank, chunkSize);
            } else {
                chunkSize = null;
            }

            int[] flags = {0, 0};
            int[] cd_nelmts = {2};
            int[] cd_values = {0,0};
            String[] cd_name ={"", ""};
            int nfilt = H5.H5Pget_nfilters(pid);
            int filter = -1;
            compression = "";

            for (int i=0; i<nfilt; i++)
            {
                if (i>0) {
                    compression += ", ";
                }
                
                try {
                    filter = H5.H5Pget_filter(pid, i, flags, cd_nelmts, cd_values, 120, cd_name);
                } catch (Throwable err) {
                    compression += "ERROR";
                    continue;
                }
                
                if (filter == HDF5Constants.H5Z_FILTER_DEFLATE)
                {
                    compression += "GZIP: level = "+cd_values[0];
                }
                else if (filter == HDF5Constants.H5Z_FILTER_FLETCHER32)
                {
                    compression += "Error detection filter";
                }
                else if (filter == HDF5Constants.H5Z_FILTER_SHUFFLE)
                {
                    compression += "SHUFFLE: Nbytes = "+cd_values[0];
                }
                else if (filter == HDF5Constants.H5Z_FILTER_SZIP)
                {
                    compression += "SZIP: Pixels per block = "+cd_values[1];
                    int flag = -1;
                    try { flag = H5.H5Zget_filter_info(filter); }
                    catch (Exception ex) { flag = -1; }
                    if (flag==HDF5Constants.H5Z_FILTER_CONFIG_DECODE_ENABLED) {
                        compression += ": H5Z_FILTER_CONFIG_DECODE_ENABLED";
                    } else if ((flag==HDF5Constants.H5Z_FILTER_CONFIG_ENCODE_ENABLED) ||
                             (flag >= (HDF5Constants.H5Z_FILTER_CONFIG_ENCODE_ENABLED+HDF5Constants.H5Z_FILTER_CONFIG_DECODE_ENABLED))) {
                        compression += ": H5Z_FILTER_CONFIG_ENCODE_ENABLED";
                    }
                }
            } // for (int i=0; i<nfilt; i++)

            if (compression.length() == 0) {
                compression = "NONE";
            }

            try {
                int[] at = {0};
                H5.H5Pget_alloc_time(pid, at);
                compression += ",         Storage allocation time: ";
                if (at[0] == HDF5Constants.H5D_ALLOC_TIME_EARLY) {
                    compression += "Early";
                } else if (at[0] == HDF5Constants.H5D_ALLOC_TIME_INCR) {
                    compression += "Incremental";
                } else if (at[0] == HDF5Constants.H5D_ALLOC_TIME_LATE) {
                    compression += "Late";
                }
            } catch (Exception ex) { ;}
        } finally 
        {
            try {H5.H5Pclose(pid); } catch(Exception ex){}            
            close(did);
        } // if (attributeList == null)

        return attributeList;
    }

    /*
     * (non-Javadoc)
     * @see ncsa.hdf.object.DataFormat#writeMetadata(java.lang.Object)
     */
    public void writeMetadata(Object info) throws Exception
    {
        // only attribute metadata is supported.
        if (!(info instanceof Attribute)) {
            return;
        }

        boolean attrExisted = false;
        Attribute attr = (Attribute)info;
        String name = attr.getName();

        if (attributeList == null) {
            this.getMetadata();
        } else {
            attrExisted = attributeList.contains(attr);
        }

        getFileFormat().writeAttribute(this, attr, attrExisted);

        // add the new attribute into attribute list
        if (!attrExisted) {
            attributeList.add(attr);
            nAttributes = attributeList.size();
        }
    }

    /*
     * (non-Javadoc)
     * @see ncsa.hdf.object.DataFormat#removeMetadata(java.lang.Object)
     */
    public void removeMetadata(Object info) throws HDF5Exception
    {
        // only attribute metadata is supported.
        if (!(info instanceof Attribute)) {
            return;
        }

        Attribute attr = (Attribute)info;
        int did = open();
        try {
            H5.H5Adelete(did, attr.getName());
            List attrList = getMetadata();
            attrList.remove(attr);
            nAttributes = attributeList.size();
        } finally {
            close(did);
        }
    }

    /*
     * (non-Javadoc)
     * @see ncsa.hdf.object.HObject#open()
     */
    public int open()
    {
        int did = -1;

        try
        {
            did = H5.H5Dopen(getFID(), getPath()+getName());
        } catch (HDF5Exception ex)
        {
            did = -1;
        }

        return did;
    }

    /*
     * (non-Javadoc)
     * @see ncsa.hdf.object.HObject#close(int)
     */
    public void close(int did)
    {
        try { H5.H5Fflush(did, HDF5Constants.H5F_SCOPE_LOCAL); } catch (Exception ex) {}
        try { H5.H5Dclose(did); }
        catch (HDF5Exception ex) {;}
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
    public byte[][] readPalette(int idx)
    {
        byte[][] thePalette = null;
        byte[] refs = getPaletteRefs();
        int did=-1, pal_id=-1, tid=-1;
       
        if (refs == null) {
            return null;
        }

        byte[] p = null;
        byte[] ref_buf = new byte[8];
        
        try {
            System.arraycopy(refs, idx*8, ref_buf, 0, 8);
        } catch (Throwable err) {
            return null;
        }

        try {
            did = open();
            pal_id =  H5.H5Rdereference(getFID(), HDF5Constants.H5R_OBJECT, ref_buf);
            tid = H5.H5Dget_type(pal_id);

            // support only 3*256 byte palette data
            if (H5.H5Dget_storage_size(pal_id) <= 768) {
                p = new byte[3*256];
                H5.H5Dread( pal_id, tid, HDF5Constants.H5S_ALL, HDF5Constants.H5S_ALL, HDF5Constants.H5P_DEFAULT, p);
            }
        } catch (HDF5Exception ex) { p = null; } 
        finally {
            try { H5.H5Tclose(tid); } catch (HDF5Exception ex2) {}
            close(pal_id);
            close(did);
        }

        if (p != null)
        {
            thePalette = new byte[3][256];
            for (int i=0; i<256; i++)
            {
                thePalette[0][i] = p[i*3];
                thePalette[1][i] = p[i*3+1];
                thePalette[2][i] = p[i*3+2];
            }
        }

        return thePalette;
    }

    /**
     * Creates a new dataset in a file.
     * <p>
     * The following example shows how to create a string dataset using this function.
     * <pre>
        H5File file = new H5File("test.h5", H5File.CREATE);
        int max_str_len = 120;
        Datatype strType = new H5Datatype(Datatype.CLASS_STRING, max_str_len, -1, -1);
        int size = 10000;
        long dims[] = {size};
        long chunks[] = {1000};
        int gzip = 9;
        String strs[] = new String[size];
        
        for (int i=0; i<size; i++)
            strs[i] = String.valueOf(i);

        file.open();
        file.createScalarDS("/1D scalar strings", null, strType, dims, null, chunks, 
                gzip, strs);

        try { file.close(); } catch (Exception ex) {}
     * </pre>
     * 
     * @param name the name of the dataset to create.
     * @param pgroup the parent group of the new dataset.
     * @param type the datatype of the dataset.
     * @param dims the dimension size of the dataset. 
     * @param maxdims the max dimension size of the dataset. maxdims is set to dims if maxdims = null.
     * @param chunks the chunk size of the dataset. No chunking if chunk = null.
     * @param gzip the level of the gzip compression. No compression if gzip<=0.
     * @param data the array of data values.
     * 
     * @return the new dataset if successful. Otherwise returns null.
     */
    public static H5ScalarDS create(
        String name,
        Group pgroup,
        Datatype type,
        long[] dims,
        long[] maxdims,
        long[] chunks,
        int gzip,
        Object data) throws Exception
    {
        H5ScalarDS dataset = null;
        String fullPath = null;
        int did=-1, tid=-1, sid=-1, plist=-1;

        if ((pgroup == null) ||
            (name == null) ||
            (dims == null) ||
            ((gzip>0) && (chunks==null))) {
            return null;
        }

        H5File file = (H5File)pgroup.getFileFormat();
        if (file == null) {
            return null;
        }

        String path = HObject.separator;
        if (!pgroup.isRoot()) {
            path = pgroup.getPath()+pgroup.getName()+HObject.separator;
            if (name.endsWith("/")) {
                name = name.substring(0, name.length()-1);
            }
                int idx = name.lastIndexOf("/");
                if (idx >=0) {
                    name = name.substring(idx+1);
                }
        }

        fullPath = path +  name;

        boolean isExtentable = false;
        if (maxdims != null)
        {
            for (int i=0; i<maxdims.length; i++)
            {
                if (maxdims[i] == 0) {
                    maxdims[i] = dims[i];
                } else if (maxdims[i] < 0) {
                    maxdims[i] = HDF5Constants.H5S_UNLIMITED;
                }

                if (maxdims[i] != dims[i]) {
                    isExtentable = true;
                }
            }
        }
 
        // HDF 5 requires you to use chunking in order to define extendible
        // datasets. Chunking makes it possible to extend datasets efficiently,
        // without having to reorganize storage excessively
        if ((chunks == null) && isExtentable) {
            chunks = dims;
        }

        // prepare the dataspace and datatype
        int rank = dims.length;
        
        try {
            tid = type.toNative();
            sid = H5.H5Screate_simple(rank, dims, maxdims);
     
            // figure out creation properties
            plist = HDF5Constants.H5P_DEFAULT;

            if (chunks != null)
            {
                plist = H5.H5Pcreate (HDF5Constants.H5P_DATASET_CREATE);
                H5.H5Pset_layout(plist, HDF5Constants.H5D_CHUNKED);
                H5.H5Pset_chunk(plist, rank, chunks);
            }

            if (gzip > 0) {
                H5.H5Pset_deflate(plist, gzip);
            }
            int fid = file.getFID();
            did = H5.H5Dcreate(fid, fullPath, tid, sid, plist);

            byte[] ref_buf = H5.H5Rcreate( fid, fullPath, HDF5Constants.H5R_OBJECT, -1);
            long l = HDFNativeData.byteToLong(ref_buf, 0);
            long[] oid = {l};
            dataset = new H5ScalarDS(file, name, path, oid);
        } finally
        {
            try {H5.H5Pclose(plist);} catch (HDF5Exception ex) {};
            try {H5.H5Sclose(sid);} catch (HDF5Exception ex) {};
            try {H5.H5Tclose(tid);} catch (HDF5Exception ex) {};
            try {H5.H5Dclose(did);} catch (HDF5Exception ex) {};
        }

        if (dataset != null) {
           pgroup.addToMemberList(dataset);
           if (data != null) {
               dataset.write(data);
            }
         }

        return dataset;
    }

    /*
     * (non-Javadoc)
     * @see ncsa.hdf.object.ScalarDS#getPaletteRefs()
     */
    public byte[] getPaletteRefs()
    {
        if (rank <=0) {
            init(); // init will be called to get refs
        }
        
        return paletteRefs;
    }

    /** reads references of palettes into a byte array
     * Each reference requires  eight bytes storage. Therefore, the array length
     * is 8*numberOfPalettes.
    */
    private byte[] getPaletteRefs(int did)
    {
        int aid=-1, sid=-1, size=0, rank=0, atype=-1;
        byte[] ref_buf = null;

        try {
            aid = H5.H5Aopen_name(did, "PALETTE");
            sid = H5.H5Aget_space(aid);
            rank = H5.H5Sget_simple_extent_ndims(sid);
            size = 1;
            if (rank > 0)
            {
                long[] dims = new long[rank];
                H5.H5Sget_simple_extent_dims(sid, dims, null);
                for (int i=0; i<rank; i++) {
                    size *= (int)dims[i];
                }
            }

            ref_buf = new byte[size*8];
            atype = H5.H5Aget_type(aid);

            H5.H5Aread( aid, atype, ref_buf);
        } catch (HDF5Exception ex)
        {
            ref_buf = null;
        } finally {
            try { H5.H5Tclose(atype); } catch (HDF5Exception ex2) {}
            try { H5.H5Sclose(sid); } catch (HDF5Exception ex2) {}
            try { H5.H5Aclose(aid); } catch (HDF5Exception ex2) {}
        }

        return ref_buf;
    }

    /*
     * (non-Javadoc)
     * @see ncsa.hdf.object.Dataset#getDatatype()
     */
    public Datatype getDatatype()
    {
        if (datatype == null)
        {
            int did=-1, tid=-1;
            
            did = open();

            try {
                tid = H5.H5Dget_type(did);
                
                if (!isNativeDatatype) {
                    int tmptid = -1;
                    try {
                        tmptid = tid;
                        tid = H5.H5Tget_native_type(tmptid);
                    } finally {
                        try { H5.H5Tclose(tmptid); } catch (Exception ex2) {}
                    }
                }  
                datatype = new H5Datatype(tid);
            } catch (Exception ex) {} 
            finally {
                try {H5.H5Tclose(tid);} catch (HDF5Exception ex) {};
                try {H5.H5Dclose(did);} catch (HDF5Exception ex) {};
            }
        }

        return datatype;
    }

    /*
     * (non-Javadoc)
     * @see ncsa.hdf.object.HObject#setName(java.lang.String)
     */
    public void setName (String newName) throws Exception
    {
        String currentFullPath = this.getPath()+this.getName();
        String newFullPath = this.getPath()+newName;
        
        currentFullPath = currentFullPath.replaceAll("//", "/");
        newFullPath = newFullPath.replaceAll("//", "/");

        if ( currentFullPath.equals("/") ) {
            throw new HDF5Exception( "Can't rename the root group." );
        }
             
        if ( currentFullPath.equals(newFullPath) ) {
            throw new HDF5Exception( 
                "The new name is the same as the current name." );
        }
       
        // Call the library to move things in the file
        H5.H5Gmove(this.getFID(), currentFullPath, newFullPath);

        super.setName(newName);
    }
    
    /**
     * H5Dextend verifies that the dataset is at least of size size, extending 
     * it if necessary. The dimensionality of size is the same as that of the 
     * dataspace of the dataset being changed. 
     * 
     * This function can be applied to the following datasets:
     *     1) Any dataset with unlimited dimensions
     *     2) A dataset with fixed dimensions if the current dimension sizes 
     *        are less than the maximum sizes set with maxdims (see H5Screate_simple) 
     */
    public void extend(long[] newDims) throws HDF5Exception { 
        int did = -1, sid = -1; 

        did = open(); 
        try { 
            H5.H5Dextend(did, newDims); 
            H5.H5Fflush(did, HDF5Constants.H5F_SCOPE_GLOBAL); 
            sid = H5.H5Dget_space(did); 
            long[] checkDims = new long[rank]; 
            H5.H5Sget_simple_extent_dims(sid, checkDims, null); 
            for (int i = 0; i < rank; i++) { 
                if (checkDims[i] != newDims[i]) { 
                    throw new HDF5Exception("error extending dataset "+ getName()); 
                } 
            } 
            dims = checkDims; 
        } catch (Exception e) { 
            throw new HDF5Exception(e.getMessage()); 
        } 
        finally 
        {
          if (sid > 0)
             H5.H5Sclose(sid);
          
            close(did); 
        } 
    } 
    
}
