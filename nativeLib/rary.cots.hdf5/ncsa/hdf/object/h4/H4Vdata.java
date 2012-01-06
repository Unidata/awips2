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

package ncsa.hdf.object.h4;

import java.util.*;
import ncsa.hdf.hdflib.*;
import ncsa.hdf.object.*;

/**
 * H4Vdata describes a multi-dimension array of HDF4 vdata, inheriting CompoundDS.
 * <p>
 * A vdata is like a table that consists of a collection of records whose values
 * are stored in fixed-length fields. All records have the same structure and
 * all values in each field have the same data type. Vdatas are uniquely
 * identified by a name, a class, and a series of individual field names.
 * <p>
 * <b>How to Select a Subset</b>
 * <p>
 * Dataset defines APIs for read, write and subet a dataset. No function is defined
 * to select a subset of a data array. The selection is done in an implicit way.
 * Function calls to dimension information such as getSelectedDims() return an array
 * of dimension values, which is a reference to the array in the dataset object.
 * Changes of the array outside the dataset object directly change the values of
 * the array in the dataset object. It is like pointers in C.
 * <p>
 *
 * The following is an example of how to make a subset. In the example, the dataset
 * is a 4-dimension with size of [200][100][50][10], i.e.
 * dims[0]=200; dims[1]=100; dims[2]=50; dims[3]=10; <br>
 * We want to select every other data points in dims[1] and dims[2]
 * <pre>
     int rank = dataset.getRank();   // number of dimension of the dataset
     long[] dims = dataset.getDims(); // the dimension sizes of the dataset
     long[] selected = dataset.getSelectedDims(); // the selected size of the dataet
     long[] start = dataset.getStartDims(); // the off set of the selection
     long[] stride = dataset.getStride(); // the stride of the dataset
     int[]  selectedIndex = dataset.getSelectedIndex(); // the selected dimensions for display

     // select dim1 and dim2 as 2D data for display,and slice through dim0
     selectedIndex[0] = 1;
     selectedIndex[1] = 2;
     selectedIndex[1] = 0;

     // reset the selection arrays
     for (int i=0; i<rank; i++) {
         start[i] = 0;
         selected[i] = 1;
         stride[i] = 1;
    }

    // set stride to 2 on dim1 and dim2 so that every other data points are selected.
    stride[1] = 2;
    stride[2] = 2;

    // set the selection size of dim1 and dim2
    selected[1] = dims[1]/stride[1];
    selected[2] = dims[1]/stride[2];

    // when dataset.read() is called, the slection above will be used since
    // the dimension arrays is passed by reference. Changes of these arrays
    // outside the dataset object directly change the values of these array
    // in the dataset object.

 * </pre>
 *
 * <p>
 * @version 1.1 9/4/2007
 * @author Peter X. Cao
 */
public class H4Vdata extends CompoundDS
{
	public static final long serialVersionUID = HObject.serialVersionUID;

    /**
     * The list of attributes of this data object. Members of the list are
     * instance of Attribute.
     */
    private List attributeList;

    /**
     * Number of records of this Vdata table.
     */
    private int numberOfRecords;
    
    /**
     * The data types of the members of the compound dataset.
     */
    private int[] memberTIDs;
    
    private int nAttributes = -1;
    

    public H4Vdata(FileFormat theFile, String name, String path)
    {
        this(theFile, name, path, null);
    }

    /**
     * Creates an H4Vdata object with specific name and path.
     * <p>
     * @param theFile the HDF file.
     * @param name the name of this H4Vdata.
     * @param path the full path of this H4Vdata.
     * @param oid the unique identifier of this data object.
     */
    public H4Vdata(
        FileFormat theFile,
        String name,
        String path,
        long[] oid)
    {
        super (theFile, name, path, oid);
        numberOfRecords = 0;
        numberOfMembers = 0;
        memberOrders = null;

    }

    /*
     * (non-Javadoc)
     * @see ncsa.hdf.object.DataFormat#hasAttribute()
     */
    public boolean hasAttribute () 
    { 
        if (nAttributes < 0) {
            int id = open();
            try { nAttributes = HDFLibrary.VSnattrs(id); }
            catch (Exception ex ) { nAttributes = 0;}
            close(id);
        }
        
        return (nAttributes>0);
    }
    
    // implementing Dataset
    public Datatype getDatatype()
    {
        if (datatype == null)
        {
            datatype = new H4Datatype(-1);
        }

        return datatype;
    }

    // Implementing Dataset
    public byte[] readBytes() throws HDFException
    {
        byte[] theData = null;

        if (rank <=0 ) {
            init();
        }
        if (numberOfMembers <= 0) {
            return null; // this Vdata does not have any filed
        }

        int id = open();
        if (id < 0) {
            return null;
        }

        String allNames = memberNames[0];
        for (int i=0; i<numberOfMembers; i++)
        {
            allNames += ","+memberNames[i];
        }

        try {
            // moves the access pointer to the start position
            HDFLibrary.VSseek(id, (int)startDims[0]);
            // Specify the fields to be accessed
            HDFLibrary.VSsetfields(id, allNames);
            int[] recordSize = {0};
            HDFLibrary.VSQueryvsize(id, recordSize);
            int size =recordSize[0] * (int)selectedDims[0];
            theData = new byte[size];
            int read_num = HDFLibrary.VSread(
                id,
                theData,
                (int)selectedDims[0],
                HDFConstants.FULL_INTERLACE);
        } finally {
            close(id);
        }

        return theData;
    }

    // Implementing DataFormat
    public Object read() throws HDFException
    {
        List list = null;

        if (rank <=0 ) {
            init();
        }
        if (numberOfMembers <= 0) {
            return null; // this Vdata does not have any filed
        }

        int id = open();
        if (id < 0) {
            return null;
        }

        list = new Vector();

        // assume external data files are located in the same directory as the main file.
        HDFLibrary.HXsetdir(getFileFormat().getParent());

        Object member_data = null;
        for (int i=0; i<numberOfMembers; i++)
        {
            if (!isMemberSelected[i]) {
                continue;
            }

            try {
                // moves the access pointer to the start position
                HDFLibrary.VSseek(id, (int)startDims[0]);
                // Specify the fields to be accessed
                HDFLibrary.VSsetfields(id, memberNames[i]);
            } catch (HDFException ex) {
                isMemberSelected[i] = false;
                continue;
            }

            int n = memberOrders[i]*(int)selectedDims[0];
            member_data = H4Datatype.allocateArray(memberTIDs[i], n);

            if (member_data == null)
            {
                String[] nullValues = new String[n];
                for (int j=0; j<n; j++) {
                    nullValues[j] = "*error*";
                }
                list.add(nullValues);
                continue;
            }

            try {
                int read_num = HDFLibrary.VSread(
                    id,
                    member_data,
                    (int)selectedDims[0],
                    HDFConstants.FULL_INTERLACE);
                if ((memberTIDs[i] == HDFConstants.DFNT_CHAR) ||
                    (memberTIDs[i] ==  HDFConstants.DFNT_UCHAR8))
                {
                    // convert characters to string
                    member_data = Dataset.byteToString((byte[])member_data, memberOrders[i]);
                    memberTypes[i] = new H4Datatype(Datatype.CLASS_STRING, memberOrders[i], -1, -1);
                    memberOrders[i] = 1; //one String
                } else if (H4Datatype.isUnsigned(memberTIDs[i]))
                {
                    // convert unsigned integer to appropriate Java integer
                    member_data = Dataset.convertFromUnsignedC(member_data);
                }
            } catch (HDFException ex)
            {
                String[] nullValues = new String[n];
                for (int j=0; j<n; j++) {
                    nullValues[j] = "*error*";
                }
                list.add(nullValues);
                continue;
            }

            list.add(member_data);
        } // for (int i=0; i<numberOfMembers; i++)

        close(id);

        return list;
    }

    // Implementing DataFormat
    public void write(Object buf) throws HDFException
    {
        //For writing to a vdata, VSsetfields can only be called once, to set
        //up the fields in a vdata. Once the vdata fields are set, they may
        //not be changed. Thus, to update some fields of a record after the
        //first write, the user must read all the fields to a buffer, update
        //the buffer, then write the entire record back to the vdata.
/*
        if (buf == null || numberOfMembers <= 0 || !(buf instanceof List))
            return; // no data to write

        List list = (List)buf;
        Object member_data = null;
        String member_name = null;

        int vid = open();
        if (vid < 0) return;

        int idx = 0;
        for (int i=0; i<numberOfMembers; i++)
        {
            if (!isMemberSelected[i])
                continue;

            HDFLibrary.VSsetfields(vid, memberNames[i]);

            try {
                // Specify the fields to be accessed

                // moves the access pointer to the start position
                HDFLibrary.VSseek(vid, (int)startDims[0]);
            } catch (HDFException ex) {
                continue;
            }

            member_data = list.get(idx++);
            if (member_data == null)
                continue;

            if (memberTIDs[i] == HDFConstants.DFNT_CHAR ||
                memberTIDs[i] ==  HDFConstants.DFNT_UCHAR8)
            {
                member_data = Dataset.stringToByte((String[])member_data, memberOrders[i]);
            } else if (H4Datatype.isUnsigned(memberTIDs[i]))
            {
                // convert unsigned integer to appropriate Java integer
                member_data = Dataset.convertToUnsignedC(member_data);
            }


            int interlace = HDFConstants.NO_INTERLACE;
            try {
                int write_num = HDFLibrary.VSwrite(
                    vid, member_data, (int)selectedDims[0], interlace);
            } catch (HDFException ex) {}
        } // for (int i=0; i<numberOfMembers; i++)

        close(vid);
*/
    }

    // Implementing DataFormat
    public List getMetadata() throws HDFException
    {
        if (attributeList != null) {
            return attributeList;
        }

        int id = open();

        if (id < 0) {
            return attributeList;
        }

        int n=0;
        try {
            n = HDFLibrary.VSnattrs(id);

            if (n <=0 ) {
                return attributeList;
            }

            attributeList = new Vector(n, 5);
            boolean b = false;
            String[] attrName = new String[1];
            int[] attrInfo = new int[3];
            for (int i=0; i<n; i++)
            {
                attrName[0] = "";
                try {
                    b = HDFLibrary.VSattrinfo(id, -1, i, attrName, attrInfo);
                    // mask off the litend bit
                    attrInfo[0] = attrInfo[0] & (~HDFConstants.DFNT_LITEND);
                } catch (HDFException ex)
                {
                    b = false;
                }

                if (!b) {
                    continue;
                }

                long[] attrDims = {attrInfo[1]};
                Attribute attr = new Attribute(attrName[0], new H4Datatype(attrInfo[0]), attrDims);;
                attributeList.add(attr);

                Object buf = H4Datatype.allocateArray(attrInfo[0], attrInfo[1]);
                try {
                    HDFLibrary.VSgetattr(id, -1, i, buf);
                } catch (HDFException ex)
                {
                    buf = null;
                }

                if (buf != null)
                {
                    if ((attrInfo[0] == HDFConstants.DFNT_CHAR) ||
                        (attrInfo[0] ==  HDFConstants.DFNT_UCHAR8))
                    {
                        buf = Dataset.byteToString((byte[])buf, attrInfo[1]);
                    }

                    attr.setValue(buf);
                }
            } // for (int i=0; i<n; i++)
        } finally
        {
            close(id);
        }

        // todo: We shall also load attributes of fields

        return attributeList;
    }

    // To do: Implementing DataFormat
    public void writeMetadata(Object info) throws Exception
    {
        // only attribute metadata is supported.
        if (!(info instanceof Attribute)) {
            return;
        }

        getFileFormat().writeAttribute(this, (Attribute)info, true);

        if (attributeList == null) {
            attributeList = new Vector();
        }

        attributeList.add(info);
    }

    // To do: Implementing DataFormat
    public void removeMetadata(Object info) throws HDFException { ; }

    // Implementing DataFormat
    public int open()
    {
        int vsid = -1;

        // try to open with write permission
        try {
            vsid = HDFLibrary.VSattach(getFID(), (int)oid[1], "w");
        } catch (HDFException ex)
        {
            vsid = -1;
        }

        // try to open with read-only permission
        if (vsid < 0)
        {
            try {
                vsid = HDFLibrary.VSattach(getFID(), (int)oid[1], "r");
            } catch (HDFException ex)
            {
                vsid = -1;
            }
        }

        return vsid;
    }

    // Implementing DataFormat
    public void close(int vsid)
    {
        try { HDFLibrary.VSdetach(vsid); }
        catch (Exception ex) { ; }
    }

    /**
     * Initializes the H4Vdata such as dimension sizes of this dataset.
     */
    public void init()
    {
        if (rank>0) {
            return; // already called. Initialize only once
        }

        int id = open();
        if (id < 0) {
            return;
        }

        try {
            numberOfMembers = HDFLibrary.VFnfields(id);
            numberOfRecords = HDFLibrary.VSelts(id);
        } catch (HDFException ex) {
            numberOfMembers = 0;
            numberOfRecords = 0;
        }

        if ((numberOfMembers <=0) || (numberOfRecords <= 0))
        {
            // no table field is defined or no records
            close(id);
            return;
        }

        // a Vdata table is an one dimension array of records.
        // each record has the same fields
        rank = 1;
        dims = new long[1];
        dims[0] = numberOfRecords;
        selectedDims = new long[1];
        selectedDims[0] = numberOfRecords;
        selectedIndex[0] = 0;
        startDims = new long[1];
        startDims[0] = 0;

        memberNames = new String[numberOfMembers];
        memberTIDs = new int[numberOfMembers];
        memberTypes = new Datatype[numberOfMembers];
        memberOrders = new int[numberOfMembers];
        isMemberSelected = new boolean[numberOfMembers];

        for (int i=0; i<numberOfMembers; i++)
        {
            isMemberSelected[i] = true;
            try {
                memberNames[i] = HDFLibrary.VFfieldname(id, i);
                memberTIDs[i] = HDFLibrary.VFfieldtype(id, i);
                memberTypes[i] = new H4Datatype(memberTIDs[i]);
                // mask off the litend bit
                memberTIDs[i] = memberTIDs[i] & (~HDFConstants.DFNT_LITEND);
                memberOrders[i] = HDFLibrary.VFfieldorder(id, i);
            } catch (HDFException ex) {
                continue;
            }
        } // for (int i=0; i<numberOfMembers; i++)

        close(id);
    }

    /**
     * Returns the number of records.
     */
    public int getRecordCount()
    {
        return numberOfRecords;
    }

    /**
     * Returns the number of fields.
     */
    public int getFieldCount()
    {
        return numberOfMembers;
    }

    /**
     * Returns the orders of fields
     */
    public int[] getFieldOrders()
    {
        return memberOrders;
    }
}
