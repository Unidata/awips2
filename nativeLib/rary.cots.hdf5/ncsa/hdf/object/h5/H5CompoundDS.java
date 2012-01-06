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

import java.util.*;

import ncsa.hdf.hdf5lib.*;
import ncsa.hdf.hdf5lib.exceptions.*;
import ncsa.hdf.hdflib.HDFLibrary;
import ncsa.hdf.object.*;

import java.lang.reflect.Array;

/**
 * The H5CompoundDS class defines an HDF5 dataset of compound datatypes.
 * <p>
 * An HDF5 dataset is an object composed of a collection of data elements, 
 * or raw data, and metadata that stores a description of the data elements, 
 * data layout, and all other information necessary to write, read, and 
 * interpret the stored data.
 * <p>
 * A HDF5 compound datatype is similar to a struct in C or a common block in
 * Fortran: it is a collection of one or more atomic types or small arrays of
 * such types. Each member of a compound type has a name which is unique within
 * that type, and a byte offset that determines the first byte (smallest byte
 * address) of that member in a compound datum. 
 * <p>
 * For more information on HDF5 datasets and datatypes, read the 
 * <a href="http://hdfgroup.org/HDF5/doc/UG/index.html">HDF5 User's Guide</a>.
 * <p>
 * There are two basic types of compound datasets: simple compound data and
 * nested compound data. Members of a simple compound dataset have atomic datatyes.
 * Members of a nested compound dataset are compound or array of compound data. 
 * <p>
 * Since Java does not understand C structures, we cannot directly read/write compound
 * data values as in the following C example.  
 * <pre>
    typedef struct s1_t {
        int    a;
        float  b;
        double c; 
        } s1_t;
    s1_t       s1[LENGTH];
    ...
    H5Dwrite(..., s1);
    H5Dread(..., s1);
 * </pre>
 * 
 * Values of compound data fields are stored in java.util.Vector object. We read and write
 * compound data by fields instead of compound structure. As for the example above, the 
 * java.util.Vector object has three elements: int[LENGTH], float[LENGTH] and double[LENGTH].
 * Since Java understands the primitive datatypes of int, float and double, we will be able
 * to read/write the compound data by field.
 * <p>
 * <p>
 * @version 1.1 9/4/2007
 * @author Peter X. Cao
 */
public class H5CompoundDS extends CompoundDS
{
    /**
     * @see ncsa.hdf.object.HObject#serialVersionUID
     */
	public static final long serialVersionUID = HObject.serialVersionUID;

    /**
     * The list of attributes attached data object.
     */
    private List attributeList;
    
    private int nAttributes = -1;
    
    /**
     * A list of names of all fields including nested fields.
     * <p>
     * The nested names are separated by CompoundDs.separator. For example, if
     * compound dataset "A" has the following nested structure,
     * <pre>
     * A --> m01
     * A --> m02
     * A --> nest1 --> m11
     * A --> nest1 --> m12
     * A --> nest1 --> nest2 --> m21
     * A --> nest1 --> nest2 --> m22
     * i.e.
     * A = { m01, m02, nest1{m11, m12, nest2{ m21, m22}}}
     * </pre>
     * The flatNameList of compound dataset "A" will be
     * {m01, m02, nest1[m11, nest1[m12, nest1[nest2[m21, nest1[nest2[m22}
     *
     */
    private List flatNameList;

    /**
     * A list of datatypes of all fields including nested fields.
     */
    private List flatTypeList;

    /** flag to indicate is the dataset is an external dataset */
    private boolean isExternal = false;
    

    /**
     * Constructs an HDF5 compound dataset with given file, dataset name and path.
     * <p>
     * The dataset object represents an existing dataset in the file. For example, 
     * new H5CompoundDS(file, "dset1", "/g0/") constructs a dataset object that corresponds to
     * the dataset,"dset1", at group "/g0/".
     * <p>
     * This object is usually constructed at FileFormat.open(), which loads the
     * file structure and object informatoin into tree structure (TreeNode). It
     * is rarely used elsewhere.
     * <p>
     * @param theFile the file that contains the dataset.
     * @param name the name of the CompoundDS, e.g. "compDS".
     * @param path the path of the CompoundDS, e.g. "/g1".
     */
    public H5CompoundDS(FileFormat theFile, String name, String path)
    {
        this(theFile, name, path, null);
    }

    /**
     * @deprecated  Not for public use in the future.<br>
     * Using {@link #H5CompoundDS(FileFormat, String, String)}
     */
    public H5CompoundDS(
        FileFormat theFile,
        String name,
        String path,
        long[] oid)
    {
        super (theFile, name, path, oid);

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
            int did = -1;
            try { 
                did = H5.H5Dopen(getFID(), getPath()+getName());
                nAttributes = H5.H5Aget_num_attrs(did); 
            } catch (Exception ex) { nAttributes = 0;}
            close(did);
        }
        
        return (nAttributes>0);
    }

    /*
     * (non-Javadoc)
     * @see ncsa.hdf.object.Dataset#getDatatype()
     */
    public Datatype getDatatype()
    {
        if (datatype == null) {
            datatype = new H5Datatype(Datatype.CLASS_COMPOUND, -1, -1, -1);
        }

        return datatype;
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
        } finally
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
        List list = null;

        Object member_data = null;
        String member_name = null;
        int tid=-1, atom_tid=-1, member_class=-1, member_size=0;
        int spaceIDs[] = {-1, -1}; // spaceIDs[0]=mspace, spaceIDs[1]=fspace

        if (rank <= 0 ) {
            init(); // read data informatin into memory
        }

        if (numberOfMembers <= 0) {
            return null; // this compound dataset does not have any member
        }

        if (isExternal) {
            String pdir = this.getFileFormat().getAbsoluteFile().getParent();
            if (pdir == null) {
                pdir = ".";
            }
            H5.H5Dchdir_ext(pdir);
        }

        int did = open();
        list = new Vector(flatNameList.size());
        Vector atomicList = new Vector();
        try // to match finally for closing resources
        {
            long[] lsize = {1};
            lsize[0] = selectHyperslab(did, spaceIDs);

            // read each of member data into a byte array, then extract
            // it into its type such, int, long, float, etc.
            int n = flatNameList.size();
            tid = H5.H5Dget_type(did);
            extractCompoundInfo(tid, null, null, atomicList);
            
            for (int i=0; i<n; i++)
            {
                if (!isMemberSelected[i]) {
                    continue; // the field is not selected
                }

                member_name = new String(memberNames[i]);
                atom_tid = ((Integer)atomicList.get(i)).intValue();

                try {
                    member_class = H5.H5Tget_class(atom_tid);                    
                    member_size = H5.H5Tget_size(atom_tid);
                    member_data = H5Datatype.allocateArray(atom_tid, (int)lsize[0]);
                } catch (Exception ex) {member_data = null; }

                if (member_data == null || H5.H5Tequal(atom_tid, HDF5Constants.H5T_STD_REF_DSETREG))
                {
                    String[] nullValues = new String[(int)lsize[0]];
                    String errorStr = "unsupported: "+ H5Datatype.getDatatypeDescription(atom_tid);
                    for (int j=0; j<lsize[0]; j++) {
                        nullValues[j] = errorStr;
                    }
                    list.add(nullValues);
                    continue;
                }

                 boolean isVL = false;
                 int comp_tid=-1;
                 int compInfo[] = {member_class, member_size, 0};
                 try {
                    comp_tid = createCompoundFieldType(atom_tid, member_name, compInfo);  
                    try { 
                        // See BUG#951 isVL = H5.H5Tdetect_class(atom_tid, HDF5Constants.H5T_VLEN); 
                        isVL = H5.H5Tis_variable_str(atom_tid);
                    } catch (Exception ex) {}
  
                    if (isVL) {
                        H5.H5DreadVL( did, comp_tid, spaceIDs[0], spaceIDs[1], HDF5Constants.H5P_DEFAULT, (Object[])member_data);
                    } else {
                        H5.H5Dread( did, comp_tid, spaceIDs[0], spaceIDs[1], HDF5Constants.H5P_DEFAULT, member_data);
                    }
                } catch (HDF5Exception ex2)
                {
                    String[] nullValues = new String[(int)lsize[0]];
                    for (int j=0; j<lsize[0]; j++) {
                        nullValues[j] = "";
                    }
                    list.add(nullValues);
                    continue;
                } finally {
                    try { H5.H5Tclose(comp_tid); } catch (Exception ex2) {}
                }
                
				if (member_class == HDF5Constants.H5T_ARRAY) {
					int tmptid = H5.H5Tget_super(atom_tid);
					member_class = H5.H5Tget_class(tmptid);
					try { H5.H5Tclose(tmptid); } catch (Exception ex) {}
				}
                
                if (!isVL)
                {
                    if ((member_class == HDF5Constants.H5T_STRING) && convertByteToString) {
                        member_data = byteToString((byte[])member_data, member_size/memberOrders[i]);
                    }
                    else if (member_class == HDF5Constants.H5T_REFERENCE) {
                        member_data = HDFNativeData.byteToLong((byte[])member_data);
                    }
                    else if (compInfo[2] !=0) {
                        member_data = Dataset.convertFromUnsignedC(member_data, null);
                    }
                    else if (member_class == HDF5Constants.H5T_ENUM && enumConverted)
                    {
                        try {
                            String[] strs = H5Datatype.convertEnumValueToName(atom_tid, member_data, null);
                            if (strs != null) {
                                member_data = strs;
                            }
                        } catch (Exception ex) {}
                    }
                }
                
                list.add(member_data);
            } // end of for (int i=0; i<num_members; i++)

        } finally
        {
            try { H5.H5Sclose(spaceIDs[0]); } catch (Exception ex2) {}
            try { H5.H5Sclose(spaceIDs[1]); } catch (Exception ex2) {}
            try { H5.H5Tclose(tid); } catch (Exception ex2) {}
            
            // close atomic types
            int ntypes = atomicList.size();
            for (int i=0; i<ntypes; i++) {
                atom_tid = ((Integer)atomicList.get(i)).intValue();
                try { H5.H5Tclose(atom_tid); } catch (Exception ex2) {}
             }
             
            close(did);
        }

        return list;
    }

    /**
     * Writes the given data buffer into this dataset in a file.
     * <p>
     * The data buffer is a vector that contains the data values of compound fields. 
     * The data is written into file field by field.
     * @param buf The vector that contains the data values of compound fields.
     */
    public void write(Object buf) throws HDF5Exception
    {
        if ((buf == null) || (numberOfMembers <= 0) || !(buf instanceof List)) {
            return;
        }

        List list = (List)buf;

        Object member_data = null;
        String member_name = null;
        int tid=-1, atom_tid=-1, member_class=-1, member_size=0; 
        int spaceIDs[] = {-1, -1}; // spaceIDs[0]=mspace, spaceIDs[1]=fspace

        int did = open();
        Vector atomicList = new Vector();
        try // to match finally for closing resources
        {
            long[] lsize = {1};
            lsize[0] = selectHyperslab(did, spaceIDs);

            // read each of member data into a byte array, then extract
            // it into its type such, int, long, float, etc.
            int idx=0;
            int n = flatNameList.size();
            tid = H5.H5Dget_type(did);

            extractCompoundInfo(tid, null, null, atomicList);
            for (int i=0; i<n; i++)
            {
                if (!isMemberSelected[i]) {
                    continue; // the field is not selected
                }

                member_name = new String(memberNames[i]);
                atom_tid = ((Integer)atomicList.get(i)).intValue();
                member_data = list.get(idx++);

                if (member_data == null) {
                    continue;
                }

                try {
                    member_class = H5.H5Tget_class(atom_tid);                    
                    member_size = H5.H5Tget_size(atom_tid);
                } catch (Exception ex) {}

                boolean isVL = false;
                try { 
                    isVL = (H5.H5Tdetect_class(atom_tid, HDF5Constants.H5T_VLEN)); 
                } catch (Exception ex) {}

                if ( (member_data == null) || isVL || (member_class== HDF5Constants.H5T_ENUM) ) {
                    continue;
                }

                Object tmpData = member_data;
                
                int comp_tid = -1;
                int compInfo[] = {member_class, member_size, 0};
                try {
                    comp_tid = createCompoundFieldType(atom_tid, member_name, compInfo);  
                    if (compInfo[2] !=0) {
                        tmpData = convertToUnsignedC(member_data, null);
                    } else if ((member_class == HDF5Constants.H5T_STRING) &&
                         (Array.get(member_data, 0) instanceof String)) {
                        tmpData = stringToByte((String[])member_data, member_size);
                    }
     
                    if (tmpData != null) {
                        // BUG!!! does not write nested compound data and no exception was caught
                        //        need to check if it is a java error or C library error
                        H5.H5Dwrite(did, comp_tid, spaceIDs[0], spaceIDs[1], HDF5Constants.H5P_DEFAULT, tmpData);
                    }
                } finally {
                    try { H5.H5Tclose(comp_tid); } catch (Exception ex2) {}
                }
            } // end of for (int i=0; i<num_members; i++)
        } finally
        {
            try { H5.H5Sclose(spaceIDs[0]); } catch (Exception ex2) {}
            try { H5.H5Sclose(spaceIDs[1]); } catch (Exception ex2) {}
            try { H5.H5Tclose(tid); } catch (Exception ex2) {}
            
            // close atomic types
            int ntypes = atomicList.size();
            for (int i=0; i<ntypes; i++) {
                atom_tid = ((Integer)atomicList.get(i)).intValue();
                try { H5.H5Tclose(atom_tid); } catch (Exception ex2) {}
            }
        }
        
        close(did);
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
            nAttributes = attrList.size();
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
        try { H5.H5Dclose(did); } catch (HDF5Exception ex) { ; }
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

        int did=-1, sid=-1, tid=-1, tclass=-1;
        String fullName = getPath()+getName();
        flatNameList = new Vector();
        flatTypeList = new Vector();
        int[] memberTIDs = null;

        did = open();
        
        // check if it is an external dataset
        int pid=-1;
        try {
            pid = H5.H5Dget_create_plist(did);
            int nfiles = H5.H5Pget_external_count(pid);
            isExternal = (nfiles>1);
        } catch (Exception ex) {}
        finally {
            try {H5.H5Pclose(pid);} catch (Exception ex) {}
        }

        try {
            sid = H5.H5Dget_space(did);
            rank = H5.H5Sget_simple_extent_ndims(sid);

            if (rank == 0)
            {
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

            startDims = new long[rank];
            selectedDims = new long[rank];

            // initialize member information
            tid= H5.H5Dget_type(did);
            tclass = H5.H5Tget_class(tid);
            
            // check if datatype in file is ntive datatype
            int tmptid = 0;
            if (tclass == HDF5Constants.H5T_ARRAY)
            {
                // array of compound
                tmptid = tid;
                tid = H5.H5Tget_super(tmptid);
                try { H5.H5Tclose(tmptid); } catch (HDF5Exception ex) {}
            }

            extractCompoundInfo(tid, "", flatNameList, flatTypeList);
            numberOfMembers = flatNameList.size();

            memberNames = new String[numberOfMembers];
            memberTIDs = new int[numberOfMembers];
            memberTypes = new Datatype[numberOfMembers];
            memberOrders = new int[numberOfMembers];
            isMemberSelected = new boolean[numberOfMembers];
            memberDims = new Object[numberOfMembers];
            
            for (int i=0; i<numberOfMembers; i++)
            {
                isMemberSelected[i] = true;
                memberTIDs[i] = ((Integer)flatTypeList.get(i)).intValue();
                memberTypes[i] = new H5Datatype(memberTIDs[i]);
                memberNames[i] = (String)flatNameList.get(i);
                memberOrders[i] = 1;
                memberDims[i] = null;

                try { tclass = H5.H5Tget_class(memberTIDs[i]); }
                catch (HDF5Exception ex ) {}

                if (tclass == HDF5Constants.H5T_ARRAY)
                {
                    int n = H5.H5Tget_array_ndims(memberTIDs[i]);
                    int mdim[] = new int[n];
                    H5.H5Tget_array_dims(memberTIDs[i], mdim, null);
                    memberDims[i] = mdim;
                    tmptid = H5.H5Tget_super(memberTIDs[i]);
                    memberOrders[i] = (H5.H5Tget_size(memberTIDs[i])/H5.H5Tget_size(tmptid));
                    try { H5.H5Tclose(tmptid); } catch (HDF5Exception ex) {}
                }
            } //for (int i=0; i<numberOfMembers; i++)
        } catch (HDF5Exception ex)
        {
            numberOfMembers = 0;
            memberNames = null;
            memberTypes = null;
            memberOrders = null;
        }
        finally
        {
            try { H5.H5Tclose(tid); } catch (HDF5Exception ex2) {}
            try { H5.H5Sclose(sid); } catch (HDF5Exception ex2) {}
            
            if (memberTIDs !=null) {
                for (int i=0; i<memberTIDs.length; i++) {
                    try { H5.H5Tclose(memberTIDs[i]); } 
                    catch (Exception ex) {}
                }
            }
        }

        close(did);
        resetSelection();
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

        if (rank == 1)
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
            selectedIndex[0] = rank-2; // columns
            selectedIndex[1] = rank-1; // rows
            selectedIndex[2] = rank-3;
            selectedDims[rank-1] = dims[rank-1];
            selectedDims[rank-2] = dims[rank-2];
        }
        
        isDataLoaded = false;
        setMemberSelection(true);
    }

    /**
     * Extracts compound information into flat structure.
     * <p>
     * For example, compound datatype "nest" has {nest1{a, b, c}, d, e}
     * then extractCompoundInfo() will put the names of nested compound
     * fields into a flat list as
     * <pre>
     * nest.nest1.a
     * nest.nest1.b
     * nest.nest1.c
     * nest.d
     * nest.e
     * </pre>
     */
    private void extractCompoundInfo(int tid, String name, List names, List types)
    {
        int nMembers=0, mclass=-1, mtype=-1;
        String mname = null;

        try { nMembers = H5.H5Tget_nmembers(tid); }
        catch (Exception ex) { nMembers = 0; }

        if (nMembers <=0) {
            return;
        }

        int tmptid = -1;
        for (int i=0; i<nMembers; i++)
        {
 
            try {mtype = H5.H5Tget_member_type(tid, i);}
            catch (Exception ex ) { continue; }

            try { 
                tmptid = mtype;
                mtype = H5.H5Tget_native_type(tmptid);
            } catch (HDF5Exception ex) { continue; } 
            finally {
                try { H5.H5Tclose(tmptid); } catch (HDF5Exception ex) {}
             }

            try { mclass = H5.H5Tget_class(mtype); }
            catch (HDF5Exception ex ) { continue; }

            if (names !=null) {
                mname = name+H5.H5Tget_member_name(tid, i);
            }

            if (mclass == HDF5Constants.H5T_COMPOUND)
            {
                extractCompoundInfo(mtype, mname+CompoundDS.separator, names, types);
                continue;
            }
            else if (mclass == HDF5Constants.H5T_ARRAY)
            {
                try {
                    tmptid = H5.H5Tget_super(mtype);
                    int tmpclass = H5.H5Tget_class(tmptid);

                    // cannot deal with ARRAY of COMPOUND or ARRAY of ARRAY
                    // support only ARRAY of atomic types
                    if ((tmpclass == HDF5Constants.H5T_COMPOUND) ||
                        (tmpclass == HDF5Constants.H5T_ARRAY)) {
                        continue;
                    }
                } catch (Exception ex) {continue;}
                finally {
                    try { H5.H5Tclose(tmptid); } catch (Exception ex) {}
                }
            }

            if (names !=null) {
                names.add(mname);
            }
            
            types.add(new Integer(mtype));
            
        } //for (int i=0; i<nMembers; i++)
    } //extractNestedCompoundInfo

    /**
     * @deprecated  Not for public use in the future. <br>
     * Using {@link #create(String, Group, long[], long[], long[], int, String[], Datatype[], int[], int[][], Object)}
     */
    public static Dataset create(
        String name,
        Group pgroup,
        long[] dims,
        String[] memberNames,
        Datatype[] memberDatatypes,
        int[] memberSizes,
        Object data) throws Exception
    {
        if ((pgroup == null) ||
            (name == null) ||
            (dims == null) ||
            (memberNames == null) ||
            (memberDatatypes == null) ||
            (memberSizes == null)) {
            return null;
        }

        int nMembers = memberNames.length;
        int memberRanks[] = new int[nMembers];
        int memberDims[][] = new int[nMembers][1];
        for (int i=0; i<nMembers; i++)
        {
            memberRanks[i] = 1;
            memberDims[i][0] = memberSizes[i];
        }

        return H5CompoundDS.create(name, pgroup, dims, memberNames,
               memberDatatypes, memberRanks, memberDims, data);
    }

    /**
     * @deprecated  Not for public use in the future. <br>
     * Using {@link #create(String, Group, long[], long[], long[], int, String[], Datatype[], int[], int[][], Object)}
     */
    public static Dataset create(
        String name,
        Group pgroup,
        long[] dims,
        String[] memberNames,
        Datatype[] memberDatatypes,
        int[] memberRanks,
        int[][] memberDims,
        Object data) throws Exception
    {
        return H5CompoundDS.create(name, pgroup, dims, null, null, -1,
               memberNames, memberDatatypes, memberRanks, memberDims, data);
    }

    /**
     * Creates a simple compound dataset in a file with/without chunking and compression
     * <p>
     * This function provides an easy way to create a simple compound dataset
     * in file by hiding tedious details of creating a compound dataset from users.  
     * <p>
     * This functoin calls H5.H5Dcreate() to create a simple compound dataset in file.
     * Nested compound dataset is not supported. The required information to create
     * a compound dataset includes the name, the parent group  and data space of the dataset, 
     * the names, datatypes and data spaces of the compound fields. Other information such
     * as chunks, compression and the data buffer is optional.
     * <p>
     * The following example shows how to use this function to create a compound dataset
     * in file.
     * 
     * <pre>
        H5File file=null;
        String message = "";
        Group pgroup = null;
        int[] DATA_INT = new int[DIM_SIZE];
        float[] DATA_FLOAT = new float[DIM_SIZE];
        String[] DATA_STR = new String[DIM_SIZE];
        long[] DIMs = {50, 10};
        long[] CHUNKs = {25, 5};

        try {
            file = (H5File)H5FILE.open(fname, H5File.CREATE);
            file.open();
            pgroup = (Group)file.get("/");
        } catch (Exception ex) {}

        Vector data = new Vector();
        data.add(0, DATA_INT);
        data.add(1, DATA_FLOAT);
        data.add(2, DATA_STR);

        // create groups
        Datatype[]  mdtypes = new H5Datatype[3];
        String[] mnames = {"int", "float", "string"};
        Dataset dset = null;
        try {
            mdtypes[0] = new H5Datatype(Datatype.CLASS_INTEGER, 4, -1, -1);
            mdtypes[1] = new H5Datatype(Datatype.CLASS_FLOAT, 4, -1, -1);
            mdtypes[2] = new H5Datatype(Datatype.CLASS_STRING, STR_LEN, -1, -1);
            dset = file.createCompoundDS("/CompoundDS", pgroup, DIMs, null, CHUNKs, 9, mnames, mdtypes, null, data);
        } catch (Exception ex) { failed(message, ex, file); return 1;}
     * </pre>
     *
     * @param name the name of the new dataset
     * @param pgroup parent group where the new dataset is created.
     * @param dims the dimension size
     * @param maxdims maximum dimension sizes of the new dataset, null if maxdims is the same as dims.
     * @param chunks chunk sizes of the new dataset, null if no chunking
     * @param gzip GZIP compression level (1 to 9), 0 or negative values if no compression.
     * @param memberNames the names of compound datatype
     * @param memberDatatypes the datatypes of the compound datatype
     * @param memberRanks the ranks of the members
     * @param memberDims the dim sizes of the members
     * @param data list of data arrays written to the new dataset, null if no data is written to the new dataset.
     * 
     * @return the new compound dataset if successful; otherwise returns null
     */
    public static Dataset create(
        String name,
        Group pgroup,
        long[] dims,
        long[] maxdims,
        long[] chunks,
        int gzip,
        String[] memberNames,
        Datatype[] memberDatatypes,
        int[] memberRanks,
        int[][] memberDims,
        Object data) throws Exception
    {
        H5CompoundDS dataset = null;
        String fullPath = null;
        int did=-1, sid=-1, tid=-1, plist=-1;

        if ((pgroup == null) ||
            (name == null) ||
            (dims == null) ||
            (memberNames == null) ||
            (memberDatatypes == null) ||
            (memberRanks == null) ||
            (memberDims == null)) {
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

        int typeSize = 0;
        int nMembers = memberNames.length;
        int[] mTypes = new int[nMembers];
        int memberSize = 1;
        int rank = dims.length;
        for (int i=0; i<nMembers; i++)
        {
            memberSize = 1;
            for (int j=0; j<memberRanks[i]; j++) {
                memberSize *= memberDims[i][j];
            }

            // the member is an array
            if ((memberSize > 1) && (memberDatatypes[i].getDatatypeClass() != Datatype.CLASS_STRING)) {
                int tmptid = -1;
                try {
                    tmptid = memberDatatypes[i].toNative();
                    mTypes[i] = H5.H5Tarray_create(tmptid, memberRanks[i], memberDims[i], null);
                } finally {
                    try {H5.H5Tclose(tmptid); } catch (Exception ex) {}
                }
            } else {
                mTypes[i] = memberDatatypes[i].toNative();
            }
            typeSize += H5.H5Tget_size(mTypes[i]);
        }

        try {
            tid = H5.H5Tcreate(HDF5Constants.H5T_COMPOUND, typeSize);
            int offset = 0;
            for (int i=0; i<nMembers; i++)
            {
                H5.H5Tinsert(tid, memberNames[i], offset, mTypes[i]);
                offset += H5.H5Tget_size(mTypes[i]);
            }

            sid = H5.H5Screate_simple(rank, dims, maxdims);

            // setup chunking and compression
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

            plist = HDF5Constants.H5P_DEFAULT;
            if (chunks != null)
            {
                plist = H5.H5Pcreate (HDF5Constants.H5P_DATASET_CREATE);
                H5.H5Pset_layout(plist, HDF5Constants.H5D_CHUNKED);
                H5.H5Pset_chunk(plist, rank, chunks);
                
                // compression requires chunking
                if (gzip > 0) {
                    H5.H5Pset_deflate(plist, gzip);
                }
            }

            int fid = file.getFID();
            did = H5.H5Dcreate(fid, fullPath, tid, sid, plist);
        } finally {
            try {H5.H5Pclose(plist);} catch (HDF5Exception ex) {};
            try {H5.H5Sclose(sid);} catch (HDF5Exception ex) {};
            try {H5.H5Tclose(tid);} catch (HDF5Exception ex) {};
            try {H5.H5Dclose(did);} catch (HDF5Exception ex) {};
            
            for (int i=0; i<nMembers; i++)
            {
                try {H5.H5Tclose(mTypes[i]);} catch (HDF5Exception ex) {};
            }
        }

        dataset = new H5CompoundDS(file, name, path);

        if (dataset != null)
        {
            pgroup.addToMemberList(dataset);

            if (data != null) {
                dataset.init();
                long selected[] = dataset.getSelectedDims();
                for (int i=0; i<rank; i++) {
                    selected[i] = dims[i];
                }
                dataset.write(data);
            }
        }

        return dataset;
    }

    /*
     * (non-Javadoc)
     * @see ncsa.hdf.object.Dataset#isString(int)
     */
    public boolean isString(int tid)
    {
        boolean b = false;
        try { b = (HDF5Constants.H5T_STRING == H5.H5Tget_class(tid) ); }
        catch (Exception ex) { b = false; }

        return b;
    }

    /*
     * (non-Javadoc)
     * @see ncsa.hdf.object.Dataset#getSize(int)
     */
    public int getSize(int tid)
    {
        int tsize = -1;

        try { tsize = H5.H5Tget_size(tid); }
        catch (Exception ex) { tsize = -1; }

        return tsize;
    }
    
    /**
     * Creates a datatype of a compound with one field.
     * <p>
     * This function is needed to read/write data field by field.
     * <p>
     * @param member_tid The datatype identifier of the compound to create
     * @param member_name The name of the datatype
     * @param compInfo compInfo[0]--IN: class of member datatype; 
     *                 compInfo[1]--IN: size of member datatype;
     *                 compInfo[2]--OUT: non-zero if the base type of the compound field is unsigned; zero, otherwise.
     * @return the identifier of the compound datatype.
     */
    private final int createCompoundFieldType(int member_tid, String member_name,
            int[] compInfo)  throws HDF5Exception    
    {
        int nested_tid = -1;

        int arrayType = member_tid;
        int baseType = arrayType;
        int tmp_tid1=-1, tmp_tid2=-1, tmp_tid3=-1, tmp_tid4=-1;
        
        try {
            int member_class = compInfo[0];
            int member_size = compInfo[1];
            
            if (member_class == HDF5Constants.H5T_ARRAY)
            {
                int mn = H5.H5Tget_array_ndims(member_tid);
                int[] marray = new int[mn];
                H5.H5Tget_array_dims(member_tid, marray, null);
                baseType = H5.H5Tget_super(member_tid);
                tmp_tid2 = baseType;
                tmp_tid4 = H5.H5Tget_native_type(baseType);
                arrayType = H5.H5Tarray_create (tmp_tid4, mn, marray, null);
                tmp_tid3 = arrayType;
            }

            try { 
                if (H5Datatype.isUnsigned(baseType)) {
                    compInfo[2] = 1;
                }
            }  catch (Exception ex2) {}
            
            member_size = H5.H5Tget_size(member_tid);

            // construct nested compound structure with a single field
            String theName = member_name;
            tmp_tid1 = H5.H5Tcopy(arrayType);
            int sep = member_name.lastIndexOf(CompoundDS.separator);

            while (sep > 0)
            {
                theName = member_name.substring(sep+1);
                nested_tid = H5.H5Tcreate(HDF5Constants.H5T_COMPOUND, member_size);
                H5.H5Tinsert(nested_tid, theName, 0, tmp_tid1);
                try {H5.H5Tclose(tmp_tid1);} catch (Exception ex) {}
                tmp_tid1 = nested_tid;
                member_name = member_name.substring(0, sep);
                sep = member_name.lastIndexOf(CompoundDS.separator);
            }

            nested_tid = H5.H5Tcreate(HDF5Constants.H5T_COMPOUND, member_size);
            H5.H5Tinsert(nested_tid, member_name, 0, tmp_tid1);
        } finally {
            try { H5.H5Tclose(tmp_tid1); } catch (HDF5Exception ex3) {}
            try { H5.H5Tclose(tmp_tid2); } catch (HDF5Exception ex3) {}
            try { H5.H5Tclose(tmp_tid3); } catch (HDF5Exception ex3) {}
            try { H5.H5Tclose(tmp_tid4); } catch (HDF5Exception ex3) {}
        }
        
        return nested_tid;
    }    
    
}
