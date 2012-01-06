package test.object;

import java.io.*;
import java.util.*;
import java.lang.reflect.Array;
import ncsa.hdf.object.*;
import ncsa.hdf.object.h5.*;
import ncsa.hdf.hdf5lib.*;
import ncsa.hdf.hdf5lib.exceptions.*;

/**
 * Test object at the ncsa.hdf.object package.
 * <p>
 *
 * @version 1.3.0 9/21/2006
 * @author Peter X. Cao
 *
 */
public class TestH5Object
{
    private final static String FILE_NAME = "TestH5Object.h5";
    private final static String FILE_NAME2 = "./samples/TestH5Obejct2.h5";
    
    private final static String NAME_GROUP = "/g0";
    private final static String NAME_GROUP_ATTR = "/g0_attr";
    private final static String NAME_GROUP_SUB = "/g0/g00";
    private final static String NAME_DATASET_INT = "/dataset_int";
    private final static String NAME_DATASET_FLOAT = "/dataset_float";
    private final static String NAME_DATASET_CHAR = "/dataset_byte";
    private final static String NAME_DATASET_STR = "/dataset_str";
    private final static String NAME_DATASET_ENUM = "/dataset_enum";    
    private final static String NAME_DATASET_ATTR = "/dataset_with_attr";
    private final static String NAME_DATASET_COMPOUND = "/comp_dataset";
    private final static String NAME_DATASET_SUB = "/g0/dataset_int";
    private final static String NAME_DATASET_SUB_SUB = "/g0/g00/dataset_float";
    private final static H5File H5FILE = new H5File();
    private final static long DIM1 = 50;
    private final static long DIM2 = 10;
    private final static long[] DIMs = {DIM1, DIM2};
    private final static long[] CHUNKs = {DIM1/2, DIM2/2};
    private final static int RANK = 2;
    private final static int STR_LEN = 20;
    private final static int DIM_SIZE = (int)(DIM1*DIM2);;
    
    /* testing data */
    private final static int[] DATA_INT = new int[DIM_SIZE];
    private final static float[] DATA_FLOAT = new float[DIM_SIZE];
    private final static byte[] DATA_BYTE = new byte[DIM_SIZE];
    private final static String[] DATA_STR = new String[DIM_SIZE];
    private final static int[] DATA_ENUM = new int[DIM_SIZE];
    private final static Vector DATA_COMP = new Vector(3);

    private static PrintStream out = null;

    /**
     * Constructs an instance of TestH5Object.
     * @param out_stream the out stream for printing the test results.
     */
    public TestH5Object(final PrintStream print_stream)
    {
        if (print_stream == null) {
            out = System.out;
        } else {
            out = print_stream;
        }
        
        for (int i=0; i<DIM_SIZE; i++) {
            DATA_INT[i] = i;
            DATA_FLOAT[i] = i+i/100.0f;
            DATA_BYTE[i] = (byte)Math.IEEEremainder(i, 127);
            DATA_STR[i] = "str"+i;
            DATA_ENUM[i] = (int)Math.IEEEremainder(i, 2);
        }
        
        DATA_COMP.add(0, DATA_INT);
        DATA_COMP.add(1, DATA_FLOAT);
        DATA_COMP.add(2, DATA_STR);
    }

    private static final void passed(final String message) {
        out.println("PASSED:\t"+message);
    }

    private static final void failed(final String message, final Exception err, final H5File file) {
        out.println("FAILED***:\t"+message +"--"+err);
        try { file.close(); } catch (final Exception ex) {}
    }

    /**
     * Check if all the data values of two data buffers are the same
     * @param buf1 the first buffer to compare
     * @param buf2 the second buffer to compare
     * @return true if all the vlaues equal; otherwise, returns false.
     */
    private static final boolean dataEquals(final Object buf1, final Object buf2) {

        // array cannot be null
        if ((buf1 == null) || (buf2==null)) {
            return false;
        }

        // must be array
        if (!buf1.getClass().isArray() || !buf2.getClass().isArray()) {
            return false;
        }

        // must be the same kind of array
        final String cname = buf1.getClass().getName();
        if (!cname.equals(buf2.getClass().getName())) {
            return false;
        }

        // must have the same size
        final int n = Array.getLength(buf1);
        if (n != Array.getLength(buf2)) {
            return false;
        }

        if (cname.equals("[I")) {
            final int[] data1 = (int[])buf1;
            final int[] data2 = (int[])buf2;
            for (int i=0; i<n; i++) {
                if (data1[i] != data2[i]) {
                    return false;
                }
            }
        }
        else if (cname.equals("[F")) {
            final float[] data1 = (float[])buf1;
            final float[] data2 = (float[])buf2;
            for (int i=0; i<n; i++) {
                if (data1[i] != data2[i]) {
                    return false;
                }
            }
        }
        else if (cname.equals("[B")) {
            final byte[] data1 = (byte[])buf1;
            final byte[] data2 = (byte[])buf2;
            for (int i=0; i<n; i++) {
                if (data1[i] != data2[i]) {
                    return false;
                }
            }
        }
        else if (cname.equals("[Ljava.lang.String;")) {
            final String[] data1 = (String[])buf1;
            final String[] data2 = (String[])buf2;
            for (int i=0; i<n; i++) {
                if (!data1[i].equals(data2[i])) {
                   return false;
                }
            }
        } else {
            return false;
        }

        return true;
    }
    
    /**
     * Create a test file.
     *
     * @param fname the name of the file to open
     * @return true if successful; otherwise returns false
     */
    private static final boolean create_test_file(final String fname, String message)
    {
        H5File file=null;
        Group g0, g1, g00;
        
        message += "\tCreate a test file: "+fname;
        try {
            file = (H5File)H5FILE.open(fname, H5File.CREATE);
            file.open();
        } catch (final Exception ex) {failed(message, ex, file); return false;}

        // create groups
        try {
            g0 = file.createGroup(NAME_GROUP, null);
            g1 = file.createGroup(NAME_GROUP_ATTR, null);
            g00 = file.createGroup(NAME_GROUP_SUB, null);

            final long[] attrDims = {1};
            final String attrName = "Test attribute";
            final String[] attrValue = {"Test for group attribute"};
            final Datatype attrType = new H5Datatype(Datatype.CLASS_STRING, attrValue[0].length()+1, -1, -1);
            final Attribute attr = new Attribute(attrName, attrType, attrDims);
            attr.setValue(attrValue);
            g1.writeMetadata(attr);
        } catch (final Exception ex) { failed(message, ex, file); return false;}
        
        // create datasets
        try {
            file.createScalarDS(NAME_DATASET_INT, null, new H5Datatype(Datatype.CLASS_INTEGER, -1, -1, -1), DIMs, null, CHUNKs, 9, DATA_INT);
            file.createScalarDS(NAME_DATASET_FLOAT, null, new H5Datatype(Datatype.CLASS_FLOAT, -1, -1, -1), DIMs, null, CHUNKs, 9, DATA_FLOAT);
            file.createScalarDS(NAME_DATASET_CHAR, null, new H5Datatype(Datatype.CLASS_CHAR, -1, -1, -1), DIMs, null, CHUNKs, 9, DATA_BYTE);
            file.createScalarDS(NAME_DATASET_STR, null, new H5Datatype(Datatype.CLASS_STRING, STR_LEN, -1, -1), DIMs, null, CHUNKs, 9, DATA_STR);
            file.createScalarDS(NAME_DATASET_ENUM, null, new H5Datatype(Datatype.CLASS_ENUM, -1, -1, -1), DIMs, null, CHUNKs, 9, DATA_ENUM);
            file.createScalarDS(NAME_DATASET_SUB, g0, new H5Datatype(Datatype.CLASS_INTEGER, -1, -1, -1), DIMs, null, CHUNKs, 9, DATA_INT);
            file.createScalarDS(NAME_DATASET_SUB_SUB, g00, new H5Datatype(Datatype.CLASS_FLOAT, -1, -1, -1), DIMs, null, CHUNKs, 9, DATA_FLOAT);
            file.createImage(NAME_DATASET_ATTR, null, new H5Datatype(Datatype.CLASS_INTEGER, 1, -1, -1), DIMs, null, CHUNKs, 9, 1, -1, DATA_BYTE);
            final Datatype[]  mdtypes = {new H5Datatype(Datatype.CLASS_INTEGER, -1, -1, -1), new H5Datatype(Datatype.CLASS_FLOAT, -1, -1, -1), new H5Datatype(Datatype.CLASS_STRING, STR_LEN, -1, -1)};
            final String[] mnames = {"int", "float", "string"};
            file.createCompoundDS(NAME_DATASET_COMPOUND, null, DIMs, null, CHUNKs, 9, mnames, mdtypes, null, DATA_COMP);
        } catch (final Exception ex) { failed(message, ex, file); return false;}

        try { file.close(); } catch (final Exception ex) {}
        return true;
    }

    /**
     * Test H5File create() function.
     *
     * @param fname the name of the file to create
     * @return zero if successful; otherwise returns one
     */
    private int test_H5File_create(final String fname)
    {
        H5File file=null;
        String message = "";

        message = "Create a new file -- new H5File()";
        try {
            file = new H5File(fname, H5File.CREATE);
            file.open();
            file.close();
        } catch (final Exception ex) { failed(message, ex, file); return 1;}
        passed(message);

        message = "Create a new file -- H5File.create()";
        try {
            file = (H5File)H5FILE.create(fname);
            file.open();
            file.close();
        } catch (final Exception ex) { failed(message, ex, file); return 1;}
        passed(message);

        message = "Create a new file -- H5File.open()";
        try {
            file = (H5File)H5FILE.open(fname, H5File.CREATE);
            file.open();
            file.close();
        } catch (final Exception ex) { failed(message, ex, file); return 1;}

        passed(message);
        try { file.close(); } catch (final Exception ex) {}
        return 0;
    }

    /**
     * Test H5File open() function.
     *
     * @param fname the name of the file to open
     * @return zero if successful; otherwise returns one
     */
    private int test_H5File_open(final String fname)
    {
        H5File file=null;
        String message = "";
        
        message = "Testing open file with read/write access";
        try {
            file = new H5File(fname, H5File.CREATE);
            file.open();
            file.close();
        } catch (final Exception ex) { failed(message, ex, file); return 1;}
        
        message = "Open file with READ-ONLY access -- H5File.open()";
        try {
            file = (H5File)H5FILE.open(fname, H5File.READ);
            file.close();
        } catch (final Exception ex) { failed(message, ex, file); return 1;}
        passed(message);
        
        message = "Open file with WRITE access -- H5File.open()";
        try {
            file = (H5File)H5FILE.open(fname, H5File.WRITE);
            file.close();
        } catch (final Exception ex) { failed(message, ex, file); return 1;}
        passed(message);

        try { file.close(); } catch (final Exception ex) {}
        return 0;
    }
    
    /**
     * Test H5File open() function with relative file path.
     * The cwd may be changed at Dataset.read() by H5Dchdir_ext()  
     * to make it work for external datasets. We need to set it 
     * back before the file is closed/opened.
     * 
     * @param fname the name of the file to open
     * @return zero if successful; otherwise returns one
     */
    private int test_H5File_open_relative_path(final String fname)
    {
        H5File file=null;
        String message = "";

        message = "H5File open() function with relative file path";
        
        if (!create_test_file(fname, message)) {
            return 1;
        }

        try {
            
            // test open/close file and open/close dataset
            file = new H5File(fname, H5File.READ);
            Dataset dset = (Dataset)file.get(NAME_DATASET_ATTR);
            dset.getData();
            file.close();
            file = new H5File(fname, H5File.READ);
            dset = (Dataset)file.get(NAME_DATASET_ATTR);
            dset.getData();
            file.close();
            
            // test open file and open multiple datasets
            file = new H5File(fname, H5File.READ);
            dset = (Dataset)file.get(NAME_DATASET_ATTR);
            dset.getData();
            dset = (Dataset)file.get(NAME_DATASET_COMPOUND);
            dset.getData();
            dset = (Dataset)file.get(NAME_DATASET_SUB);
            dset.getData();
            file.close();

         } catch (final Exception ex) { failed(message, ex, file); return 1;}

        passed(message);
        try { file.close(); } catch (final Exception ex) {}
        return 0;
    }    

    /**
     * Test H5File createGroup() function.
     *
     * @param fname the name of the file to open
     * @return zero if successful; otherwise returns one
     */
    private int test_H5File_createGroup(final String fname)
    {
        H5File file=null;
        String message = "";
        
        file = new H5File(fname);

        // create groups
        Group g0 = null;
        message = "Create a group at root -- H5Group.create()";
        try {
            g0 = file.createGroup("/g0", null);
        } catch (final Exception ex) { failed(message, ex, file); return 1;}
        passed(message);

        Group g00 = null;
        message = "Create a group with absolute path -- H5Group.create()";
        try {
            g00 = file.createGroup("g0/g00", null);
        } catch (final Exception ex) { failed(message, ex, file); return 1;}
        passed(message);

        Group g01 = null;
        message = "Create a group at non-root group -- H5Group.create()";
        try {
            g01 = file.createGroup("/g0/g01/", g0);
        } catch (final Exception ex) { failed(message, ex, file); return 1;}
        passed(message);

        try { file.close(); } catch (final Exception ex) {}
        return 0;
    }

    /**
     * Test H5File createDatatype() function.
     *
     * @param fname the name of the file to open
     * @return zero if successful; otherwise returns one
     */
    private int test_H5File_createDatatype(final String fname)
    {
        H5File file=null;
        String message = "";

        final int N = 5;
        final int dtype_cls[] = {Datatype.CLASS_INTEGER, Datatype.CLASS_FLOAT,
            Datatype.CLASS_CHAR, Datatype.CLASS_STRING, Datatype.CLASS_ENUM};
        final String dtype_names[] = {"INTEGER", "FLOAT", "CHAR", "STRING", "ENUM"};
        final String msgs[] = { "H5File.createDatatype(..., "+dtype_names[0]+")",
                          "H5File.createDatatype(..., "+dtype_names[1]+")",
                          "H5File.createDatatype(..., "+dtype_names[2]+")",
                          "H5File.createDatatype(..., "+dtype_names[3]+")",
                          "H5File.createDatatype(..., "+dtype_names[4]+")"};

        message = "Test creating named datatypes";
        try {
            file = (H5File)H5FILE.open(fname, H5File.CREATE);
            file.open();
        } catch (final Exception ex) {failed(message, ex, file); return 1;}

        // create groups
        Datatype dtype = null;
        for (int i=0; i<N; i++)
        {
            message = "Create a named datatype -- "+msgs[i];
            try {
                dtype = file.createDatatype(dtype_cls[i],Datatype.NATIVE, Datatype.NATIVE, Datatype.NATIVE, dtype_names[i]);
            } catch (final Exception ex) { failed(message, ex, file); return 1;}

            passed(message);
        }

        try { file.close(); } catch (final Exception ex) {}
        return 0;
    }

    /**
     * Test H5File createScalarDS function.
     *
     * @param fname the name of the file to open
     * @return zero if successful; otherwise returns one
     */
    private int test_H5File_createScalarDS(final String fname)
    {
        H5File file=null;
        String message = "";

        Group pgroup = null;
        final int N=5;
        final int dtype_cls[] = {Datatype.CLASS_INTEGER, Datatype.CLASS_FLOAT,
            Datatype.CLASS_CHAR, Datatype.CLASS_STRING, Datatype.CLASS_ENUM};
        final int dtype_sizes[] = {-1, -1, 1, 80, -1};
        final String names[] = {"INTEGER", "FLOAT", "CHAR", "STRING", "ENUM"};

        message = "Test creating ScalarDS";
        try {
            file = (H5File)H5FILE.open(fname, H5File.CREATE);
            file.open();
            pgroup = (Group)file.get("/");
        } catch (final Exception ex) {failed(message, ex, file); return 1;}

        final Object[] all_data = new Object[N];
        all_data[0] = DATA_INT;
        all_data[1] = DATA_FLOAT;
        all_data[2] = DATA_BYTE;
        all_data[3] = DATA_STR;
        all_data[4] = DATA_ENUM;

        // create groups
        Datatype dtype = null;
        Dataset dset = null;
        Object data_read = null;
        for (int i=0; i<N; i++)
        {
            message = "Create/read/write a H5ScalarDS -- H5ScalarDS.create ()";
            try {
                dtype = new H5Datatype(dtype_cls[i], dtype_sizes[i], -1, -1);
                dset = file.createScalarDS(names[i], pgroup, dtype, DIMs, null, CHUNKs, 9, all_data[i]);
            } catch (final Exception ex) { failed(message, ex, file); return 1;}

            // test data valuse
            try {
                dset.init();
                final long[] selectedDims = dset.getSelectedDims();
                final long[] dims = dset.getDims();
                final long[] starts = dset.getStartDims();
                final int rank = dset.getRank();
                // read all data
                for (int j=0; j<rank; j++) {
                    starts[j] = 0;
                    selectedDims[j] = dims[j];
                }
                data_read = dset.read();
            } catch (final Exception ex) { failed(message, ex, file); return 1;}
            if ( !dataEquals(all_data[i], data_read) ) {
                failed(message, new HDF5LibraryException("Incorrect data values in file"), file);
                return 1;
            }

            passed(message);
        }

        try { file.close(); } catch (final Exception ex) {}
        return 0;
    }

    /**
     * Test H5File createLink function.
     *
     * @param fname the name of the file to open
     * @return zero if successful; otherwise returns one
     */
    private int test_H5File_createLink(final String fname)
    {
        H5File file=null;
        String message = "";

        Group pgroup = null;
        message = "Create a hard link -- H5File.createLink()";

        try {
            file = (H5File)H5FILE.open(fname, H5File.CREATE);
            file.open();
            pgroup = (Group)file.get("/");
        } catch (final Exception ex) {failed(message, ex, file); return 1;}

        final String gname = "Group";
        Group grp = null;
        try {
            grp = file.createGroup(gname, null);
        } catch (final Exception ex) { failed(message, ex, file); return 1;}

        HObject hobj = null;
        try {
            hobj = file.createLink(pgroup, "link to "+gname, grp);
        } catch (final Exception ex) { failed(message, ex, file); return 1;}

        final long oid[] = grp.getOID();
        if (!hobj.equalsOID(oid))
        {
            failed(message, new HDF5LibraryException("link to the wrong object"), file);
            return 1;
        }

        passed(message);

        try { file.close(); } catch (final Exception ex) {}
        return 0;
    }

    /**
     * Test H5File createImage function.
     *
     * @param fname the name of the file to open
     * @return zero if successful; otherwise returns one
     */
    private int test_H5File_createImage(final String fname)
    {
        H5File file=null;
        String message = "";

        Group pgroup = null;
        message = "Ceate an image -- H5File.createImage()";

        try {
            file = (H5File)H5FILE.open(fname, H5File.CREATE);
            file.open();
            pgroup = (Group)file.get("/");
        } catch (final Exception ex) {failed(message, ex, file); return 1;}

        // create groups
        Datatype dtype = null;
        Dataset dset = null;
        Object data_read = null;
        try {
            dtype = new H5Datatype(Datatype.CLASS_INTEGER, 1, -1, -1);
            dset = file.createImage("Image", pgroup, dtype, DIMs, null, CHUNKs, 9, 1, -1, DATA_BYTE);
        } catch (final Exception ex) { failed(message, ex, file); return 1;}

        // test data valuse
        try {
            data_read = dset.read();
        } catch (final Exception ex) { failed(message, ex, file); return 1;}

        if ( !dataEquals(DATA_BYTE, data_read) ) {
            failed(message, new HDF5LibraryException("Incorrect data values in file"), file);
            return 1;
        }

        passed(message);
        try { file.close(); } catch (final Exception ex) {}
        return 0;
    }

    /**
     * Test H5File createCompoundDS function.
     *
     * @param fname the name of the file to open
     * @return zero if successful; otherwise returns one
     */
    private int test_H5File_createCompoundDS(final String fname)
    {
        H5File file=null;
        String message = "";

        Group pgroup = null;
        message = "Create/read/write a compound dataset -- H5CompoundDS.create()";

        try {
            file = (H5File)H5FILE.open(fname, H5File.CREATE);
            file.open();
            pgroup = (Group)file.get("/");
        } catch (final Exception ex) {failed(message, ex, file); return 1;}

        final Vector data = new Vector();
        data.add(0, DATA_INT);
        data.add(1, DATA_FLOAT);
        data.add(2, DATA_STR);


        // create groups
        final Datatype[]  mdtypes = new H5Datatype[3];
        final String[] mnames = {"int", "float", "string"};
        Dataset dset = null;
        try {
            mdtypes[0] = new H5Datatype(Datatype.CLASS_INTEGER, -1, -1, -1);
            mdtypes[1] = new H5Datatype(Datatype.CLASS_FLOAT, -1, -1, -1);
            mdtypes[2] = new H5Datatype(Datatype.CLASS_STRING, STR_LEN, -1, -1);
            dset = file.createCompoundDS("/CompoundDS", pgroup, DIMs, null, CHUNKs, 9, mnames, mdtypes, null, data);
        } catch (final Exception ex) { failed(message, ex, file); return 1;}

        // test data valuse
        List data_read = null;
        try {
            data_read = (List)dset.read();
        } catch (final Exception ex) { failed(message, ex, file); return 1;}

        if ( !dataEquals(DATA_INT, data_read.get(0)) ||
             !dataEquals(DATA_FLOAT, data_read.get(1)) ||
             !dataEquals(DATA_STR, data_read.get(2))) {
            failed(message, new HDF5LibraryException("Incorrect data values in file"), file);
            return 1;
        }

        // tests for subset
        final H5CompoundDS compDS = (H5CompoundDS)dset;
        int rank = compDS.getRank();
        try { if (rank<=0) {
            compDS.init();
        } } catch (final Exception ex) {}
        rank = compDS.getRank();

        // read only one column but all rows
        compDS.setMemberSelection(false); //unselect all members
        compDS.selectMember(1); // select the second column
        try {
            data_read = (List)dset.read();
        } catch (final Exception ex) { failed(message, ex, file); return 1;}
        if ( !dataEquals(DATA_FLOAT, data_read.get(0)) || (data_read.size()!=1)) {
            failed(message, new HDF5LibraryException("incorrect data values from file"), file);
            return 1;
        }

        // read only one row but all columns
        compDS.setMemberSelection(true); //select all members, it is default
        final int nmembers = compDS.getSelectedMemberCount();
        final long[] count = compDS.getSelectedDims();
        final long[] start = compDS.getStartDims();
        for (int i=0; i<rank; i++) {
            start[i] = 2; // start the third data point
            count[i] = 1; // select only one row (the third row)
        }
        try {
            data_read = (List)dset.read();
        } catch (final Exception ex) { failed(message, ex, file); return 1;}

        if (nmembers != compDS.getMemberCount())
        {
            failed(message, new HDF5LibraryException("incorrect members selection"), file);
            return 1;
        }

        for (int i=0; i<nmembers; i++) {
            if (Array.getLength(data_read.get(i)) != 1)
            {
                failed(message, new HDF5LibraryException("incorrect row selection"), file);
                return 1;
            }
        }

        // create dataset at non root group
        Group g0 = null;
        try {
            g0 = file.createGroup("/gg0", null);
        } catch (final Exception ex) { failed(message, ex, file); return 1;}
        try {
            mdtypes[0] = new H5Datatype(Datatype.CLASS_INTEGER, -1, -1, -1);
            mdtypes[1] = new H5Datatype(Datatype.CLASS_FLOAT, -1, -1, -1);
            mdtypes[2] = new H5Datatype(Datatype.CLASS_STRING, STR_LEN, -1, -1);
            dset = file.createCompoundDS("/g0/CompoundDS/", g0, DIMs, null, CHUNKs, 9, mnames, mdtypes, null, data);
        } catch (final Exception ex) { failed(message, ex, file); return 1;}

        passed(message);
        try { file.close(); } catch (final Exception ex) {}
        return 0;
    }

    /**
     * Test H5File copy function.
     *
     * @param fname the name of the file to open
     * @return zero if successful; otherwise returns one
     */
    private int test_H5File_copy(final String fname)
    {
        H5File file=null;
        String message = "";

        Group pgroup = null;
        message = "Copy dataset, group and attributes -- H5File.copy()";

        try {
            file = (H5File)H5FILE.open(fname, H5File.CREATE);
            file.open();
            pgroup = (Group)file.get("/");
        } catch (final Exception ex) {failed(message, ex, file); return 1;}

        final int size = (int) (DIM1*DIM2);
        final byte[] bdata = new byte[size];
        for (int i=0; i<size; i++) {
            bdata[i] = (byte)Math.IEEEremainder(i, 127);
        }

        Group grp = null;
        Datatype dtype = null;
        Dataset dset = null;
        try {
            grp = file.createGroup("/Group", null);
            dtype = new H5Datatype(Datatype.CLASS_INTEGER, 1, -1, -1);
            dset = file.createImage("Dataset", pgroup, dtype, DIMs, null, CHUNKs, 9, 1, -1, bdata);
        } catch (final Exception ex) { failed(message, ex, file); return 1;}

        try {
            file.copy(dset, grp);
            file.copy(grp, pgroup, "~Group");
        } catch (final Exception ex) { failed(message, ex, file); return 1;}

        passed(message);
        try { file.close(); } catch (final Exception ex) {}
        return 0;
    }

    /**
     * Test H5File getAttribute function.
     *
     * @param fname the name of the file to open
     * @return zero if successful; otherwise returns one
     */
    private int test_H5File_getAttribute(final String fname)
    {
        H5File file=null;
        String message = "";

        message = "Read/write attributes from a group/dataset";
        if (!create_test_file(fname, message)) {
            return 1;
        }
        
        try {
            file = new H5File(fname);
            final Dataset dset = (Dataset)file.get(NAME_DATASET_ATTR);

            final int did = dset.open();
            List attrs = H5File.getAttribute(did);
            try {dset.close(did);} catch (final Exception ex2) {}
            if ((attrs==null) || (attrs.size()<1)) {
                failed(message, new HDF5LibraryException("failed to read attributes from dataset"), file);
                return 1;
            }
            
            attrs.clear();
            final Group grp = (Group)file.get(NAME_GROUP_ATTR);
            final int gid = grp.open();
            attrs = H5File.getAttribute(gid);
            try {grp.close(gid);} catch (final Exception ex2) {}
            if ((attrs==null) || (attrs.size()<1)) {
                failed(message, new HDF5LibraryException("failed to read attributes from group"), file);
                return 1;
            }
         } catch (final Exception ex) { failed(message, ex, file); return 1;}

        passed(message);
        try { file.close(); } catch (final Exception ex) {}
        return 0;
    }

    /**
     * Test H5File getHObject() function.
     *
     * @param fname the name of the file to open
     * @return zero if successful; otherwise returns one
     */
    private int test_H5File_getHObject(final String fname)
    {
        final H5File file=null;
        String message = "";

        message = "Get a group for a given path -- H5File.getHObject()";
        if (!create_test_file(fname, message)) {
            return 1;
        }
        
        try {
            HObject obj = FileFormat.getHObject(fname, NAME_GROUP);
            if (obj == null) {
                failed(message, new HDF5Exception("Failed to get a group"), file);
                return 1;
            }
            try { obj.getFileFormat().close(); } catch (final Exception ex2) {}

            obj = FileFormat.getHObject(fname+"#//"+NAME_GROUP_SUB);
            if (obj == null) {
                failed(message, new HDF5Exception("Failed to get a group"), file);
                return 1;
            }
            try { obj.getFileFormat().close(); } catch (final Exception ex2) {}
         
        } catch (final Exception ex) { failed(message, ex, file); return 1;}
        passed(message);
        
        message = "Get a ScalarDS for a given path -- H5File.getHObject()";
        try {
            HObject obj = FileFormat.getHObject(fname, NAME_DATASET_INT);
            if (obj == null) {
                failed(message, new HDF5Exception("Failed to get a dataset"), file);
                return 1;
            }
            try { obj.getFileFormat().close(); } catch (final Exception ex2) {}
            
            obj = FileFormat.getHObject(fname+"#//"+NAME_DATASET_FLOAT);
            if (obj == null) {
                failed(message, new HDF5Exception("Failed to get a dataset"), file);
                return 1;
            }
            try { obj.getFileFormat().close(); } catch (final Exception ex2) {}
        } catch (final Exception ex) { failed(message, ex, file); return 1;}
        passed(message);
    
        message = "Get a CompoundDS for a given path -- H5File.getHObject()";
        try {
            final HObject obj = FileFormat.getHObject(fname, NAME_DATASET_COMPOUND);
            if (obj == null) {
                failed(message, new HDF5Exception("Failed to get a compound dataset"), file);
                return 1;
            }
            try { obj.getFileFormat().close(); } catch (final Exception ex2) {}
        } catch (final Exception ex) { failed(message, ex, file); return 1;}
        passed(message);

        try { file.close(); } catch (final Exception ex) {}
        return 0;
    }

    /**
     * Test HObject getFID()  function.
     *
     * @param fname the name of the file to open
     * @return zero if successful; otherwise returns one
     */
    private int test_HObject_getFID(final String fname)
    {
        H5File file=null;
        String message = "";

        int fid = 0;
        Group pgroup = null;
        message = "Get file identifier -- Group.getFID(), Dataset.getFID()";

        try {
            file = (H5File)H5FILE.open(fname, H5File.CREATE);
            fid = file.open();
            pgroup = (Group)file.get("/");
        } catch (final Exception ex) {failed(message, ex, file); return 1;}

        if (fid != pgroup.getFID()) {
            failed(message, new HDF5LibraryException("wrong object ID in group"), file);
            return 1;
        }

        Datatype dtype = null;
        Dataset dset = null;
        try {
            dtype = new H5Datatype(Datatype.CLASS_INTEGER, 1, -1, -1);
            dset = file.createScalarDS("Dataset", pgroup, dtype, DIMs, null, CHUNKs, 9, null);
        } catch (final Exception ex) { failed(message, ex, file); return 1;}

        if (fid != dset.getFID()) {
            failed(message, new HDF5LibraryException("wrong object ID in dataset"), file);
            return 1;
        }

        passed(message);
        try { file.close(); } catch (final Exception ex) {}
        return 0;
    }

    /**
     * Test HObject getName()  function.
     *
     * @param fname the name of the file to open
     * @return zero if successful; otherwise returns one
     */
    private int test_HObject_getName(final String fname)
    {
        H5File file=null;
        String message = "";

        int fid = 0;
        Group pgroup = null;
        message = "Get object name and path -- Group.getName(), Group.getPath()";

        try {
            file = (H5File)H5FILE.open(fname, H5File.CREATE);
            fid = file.open();
            pgroup = (Group)file.get("/");
        } catch (final Exception ex) {failed(message, ex, file); return 1;}

        Group grp = null;
        try {
            grp = file.createGroup("/Group", null);
        } catch (final Exception ex) {failed(message, ex, file); return 1;}

        Group grp2 = null;
        try {
            grp2 = file.createGroup("/Group/Group2", grp);
        } catch (final Exception ex) { failed(message, ex, file); return 1;}

        if (!grp2.getName().endsWith("Group2")) {
            failed(message, new HDF5LibraryException("wrong name for the group"), file);
            return 1;
        }

        if (!grp2.getPath().endsWith("/Group/")) {
            failed(message, new HDF5LibraryException("wrong path for the group"), file);
            return 1;
        }

        if (!grp2.getFullName().endsWith("/Group/Group2")) {
            failed(message, new HDF5LibraryException("wrong full path for the group"), file);
            return 1;
        }

        Datatype dtype = null;
        Dataset dset = null;
        try {
            dtype = new H5Datatype(Datatype.CLASS_INTEGER, 1, -1, -1);
            dset = file.createScalarDS("Dataset", pgroup, dtype, DIMs, null, CHUNKs, 9, null);
        } catch (final Exception ex) { failed(message, ex, file); return 1;}

        if (!dset.getName().endsWith("Dataset")) {
            failed(message, new HDF5LibraryException("wrong name for the dataset"), file);
            return 1;
        }

        if (!dset.getPath().endsWith("/")) {
            failed(message, new HDF5LibraryException("wrong path for the dataset"), file);
            return 1;
        }

        if (!dset.getFullName().endsWith("/Dataset")) {
            failed(message, new HDF5LibraryException("wrong full path for the dataset"), file);
            return 1;
        }

        passed(message);
        try { file.close(); } catch (final Exception ex) {}
        return 0;
    }

    /**
     * Test Group isRoot function.
     *
     * @param fname the name of the file to open
     * @return zero if successful; otherwise returns one
     */
    private int test_Group_isRoot(final String fname)
    {
        H5File file=null;
        String message = "";

        Group pgroup = null;
        message = "Check root group -- Group.isRoot()";

        try {
            file = (H5File)H5FILE.open(fname, H5File.CREATE);
            file.open();
            pgroup = (Group)file.get("/");
        } catch (final Exception ex) {failed(message, ex, file); return 1;}

        if (!pgroup.isRoot())
        {
            failed(message, new HDF5LibraryException("failed to test root group"), file);
            return 1;
        }

        Group grp = null;
        try {
            grp = file.createGroup("/Group", null);
        } catch (final Exception ex) { failed(message, ex, file); return 1;}
        try { file.close(); } catch (final Exception ex) {}

        try {
            grp = (Group)FileFormat.getHObject(fname, "/Group");
        } catch (final Exception ex) {failed(message, ex, file); return 1;}

        if (grp.isRoot())
        {
            failed(message, new HDF5LibraryException("failed to test non-root group"), file);
            return 1;
        }
        try { grp.getFileFormat().close(); } catch (final Exception ex) {}

        passed(message);
        try { file.close(); } catch (final Exception ex) {}
        return 0;
    }

    /**
     * Test Group getParent function.
     *
     * @param fname the name of the file to open
     * @return zero if successful; otherwise returns one
     */
    private int test_Group_getParent(final String fname)
    {
        H5File file=null;
        String message = "";

        Group pgroup = null;
        message = "Get parent group -- Group.getParent()";

        try {
            file = (H5File)H5FILE.open(fname, H5File.CREATE);
            file.open();
            pgroup = (Group)file.get("/");
        } catch (final Exception ex) {failed(message, ex, file); return 1;}

        if(pgroup.getParent() != null) {
            failed(message, new HDF5Exception("the parent of root group is not null"), file);
            return 1;
        }

        // create groups
        Group g0 = null;
        try {
            g0 = file.createGroup("/g0", pgroup);
        } catch (final Exception ex) { failed(message, ex, file); return 1;}

        if(g0.getParent() == null) {
            failed(message, new HDF5Exception("the parent of the group is null"), file);
            return 1;
        }

        passed(message);
        try { file.close(); } catch (final Exception ex) {}
        return 0;
    }

    /**
     * Test Dataset byteToString function.
     *
     * @param fname the name of the file to open
     * @return zero if successful; otherwise returns one
     */
    private int test_Dataset_byteToString(final String fname)
    {
        H5File file=null;
        String message = "";

        Group pgroup = null;
        message = "Convert byte array to strings -- Dataset.byteToString()";

        try {
            file = (H5File)H5FILE.open(fname, H5File.CREATE);
            file.open();
            pgroup = (Group)file.get("/");
        } catch (final Exception ex) {failed(message, ex, file); return 1;}

        final String[] sdata = new String[(int)DIM1];

        for (int i=0; i<DIM1; i++) {
            sdata[i] = "str"+i;
        }

        // create groups
        Datatype dtype = null;
        Dataset dset = null;
        String[] sdata_read = null;
        byte[] bdata_read = null;
        final long[] dims = {DIM1};
       byte[] bdata = null;

        try {
            dtype = new H5Datatype(Datatype.CLASS_STRING, STR_LEN, -1, -1);
            dset = file.createScalarDS("String", pgroup, dtype, dims, null, null, -1, sdata);
        } catch (final Exception ex) { failed(message, ex, file); return 1;}

        // test data valuse
        try {
            dset.setConvertByteToString(false);
            bdata = dset.readBytes();
            bdata_read = (byte[])dset.read();
            sdata_read = Dataset.byteToString(bdata_read, STR_LEN);
            bdata_read = Dataset.stringToByte(sdata, STR_LEN);
        } catch (final Exception ex) { failed(message, ex, file); return 1;}

        if ( !dataEquals(bdata, bdata_read) ) {
            failed(message, new HDF5LibraryException("Incorrect data from stringToByte()"), file);
            return 1;
        }

        if ( !dataEquals(sdata, sdata_read) ) {
            failed(message, new HDF5LibraryException("Incorrect data from byteToString()"), file);
            return 1;
        }

        passed(message);
        try { file.close(); } catch (final Exception ex) {}
        return 0;
    }

    /**
     * Test H5Datatype toNative() function.
     *
     * @param fname the name of the file to open
     * @return zero if successful; otherwise returns one
     */
    private int test_H5Datatype_toNative(final String fname)
    {
        H5File file=null;
        String message = "";

        Group pgroup = null;
        Datatype dtype = null;
        int tid=-1, tid2=-1;

        try {
            file = (H5File)H5FILE.open(fname, H5File.CREATE);
            file.open();
            pgroup = (Group)file.get("/");
        } catch (final Exception ex) {failed(message, ex, file); return 1;}

        message = "Decode/encode datatypes -- H5Datatype.toNative()";
        
        try {
            dtype = file.createDatatype(Datatype.CLASS_INTEGER,-1, -1, -1);
            tid = dtype.toNative();
            if (!H5.H5Tequal(tid, HDF5Constants.H5T_NATIVE_INT)) {
                failed(message, new HDF5Exception("Failed to convert native integer"), file);
                return 1;
            }
        } catch (final Exception ex) { failed(message, ex, file); return 1;}
        finally {
            try {H5.H5Tclose(tid); } catch (final Exception ex) {}
        }

        try {
            dtype = file.createDatatype(Datatype.CLASS_FLOAT,-1, -1, -1);
            tid = dtype.toNative();
            if (!H5.H5Tequal(tid, HDF5Constants.H5T_NATIVE_FLOAT)) {
                failed(message, new HDF5Exception("Failed to convert native float"), file);
                return 1;
            }
        } catch (final Exception ex) { failed(message, ex, file); return 1;}
        finally {
            try {H5.H5Tclose(tid); } catch (final Exception ex) {}
        }

        try {
            dtype = file.createDatatype(Datatype.CLASS_CHAR, 1, -1, -1);
            tid = dtype.toNative();
            if (!H5.H5Tequal(tid, HDF5Constants.H5T_NATIVE_CHAR)) {
                failed(message, new HDF5Exception("Failed to convert native char"), file);
                return 1;
            }
        } catch (final Exception ex) { failed(message, ex, file); return 1;}
        finally {
            try {H5.H5Tclose(tid); } catch (final Exception ex) {}
        }

        try {
            dtype = file.createDatatype(Datatype.CLASS_STRING, STR_LEN, -1, -1);
            tid = dtype.toNative();
            tid2 = H5.H5Tcopy(HDF5Constants.H5T_C_S1);
            H5.H5Tset_size(tid2, STR_LEN);
            H5.H5Tset_strpad(tid2, HDF5Constants.H5T_STR_NULLPAD);
            if (!H5.H5Tequal(tid, tid2)) {
                failed(message, new HDF5Exception("Failed to convert string"), file);
                return 1;
            }
        } catch (final Exception ex) { failed(message, ex, file); return 1;}
        finally {
            try {H5.H5Tclose(tid2); } catch (final Exception ex) {}
            try {H5.H5Tclose(tid); } catch (final Exception ex) {}
        }
        
        passed(message);
        try { file.close(); } catch (final Exception ex) {}
        return 0;
    }

    /**
     * Gets compound dataset information.
     *
     * @param fname the name of the file to open
     * @return zero if successful; otherwise returns one
     */
    private int test_H5CompoundDS_init(final String fname)
    {
        final H5File file=null;
        String message = "";

        message = "Get information from a compound dataset -- H5CompoundDS.init()";
        if (!create_test_file(fname, message)) {
            return 1;
        }
        
        try {
            final CompoundDS obj = (CompoundDS) FileFormat.getHObject(fname, NAME_DATASET_COMPOUND);
            if (obj == null) {
                failed(message, new HDF5Exception("Failed to get "+NAME_DATASET_COMPOUND), file);
                return 1;
            }
            obj.init();
            
            final int nmembers = obj.getMemberCount();
            final String[] mnames = obj.getMemberNames();
            if ((nmembers <=0) || (mnames == null) || (mnames.length != nmembers)) {
                failed(message, new HDF5Exception("Failed to get information from "+NAME_DATASET_COMPOUND), file);
                return 1;
            }

            try { obj.getFileFormat().close(); } catch (final Exception ex2) {}
        } catch (final Exception ex) { failed(message, ex, file); return 1; }

        passed(message);
        try { file.close(); } catch (final Exception ex) {}
        return 0;
    }
    
    /**
     * Updates scalar dataset values.
     *
     * @param fname the name of the file to open
     * @return zero if successful; otherwise returns one
     */
    private int test_H5ScalarDS_write(final String fname)
    {
        final H5File file=null;
        String message = "";

        message = "Updates scalar dataset values -- H5ScalarDS.write()";
        if (!create_test_file(fname, message)) {
            return 1;
        }
        
        final int temp_value = 99999;
        try {
            final ScalarDS obj = (ScalarDS) FileFormat.getHObject(fname, NAME_DATASET_INT);
            if (obj == null) {
                failed(message, new HDF5Exception("Failed to get "+NAME_DATASET_INT), file);
                return 1;
            }
            
            final int[] data_int1 = (int[])obj.getData();
             if (data_int1 == null) {
                failed(message, new HDF5Exception("Failed to read data from "+NAME_DATASET_INT), file);
                try { obj.getFileFormat().close(); } catch (final Exception ex2) {}
                return 1;
            }

            data_int1[0] = data_int1[1] = data_int1[2] = data_int1[3] = temp_value;
            obj.write();
            
            obj.clear();
            final int[] data_int2 = (int[])obj.getData();
            if (data_int2 == null) {
                failed(message, new HDF5Exception("Failed to read data from "+NAME_DATASET_INT), file);
                try { obj.getFileFormat().close(); } catch (final Exception ex2) {}
                return 1;
            }

            if ( !dataEquals(data_int1, data_int2) ) {
                failed(message, new HDF5LibraryException("Incorrect data values in file"), file);
                try { obj.getFileFormat().close(); } catch (final Exception ex2) {}
                return 1;
            }

            try { obj.getFileFormat().close(); } catch (final Exception ex2) {}
        } catch (final Exception ex) { failed(message, ex, file); return 1; }

        passed(message);
        try { file.close(); } catch (final Exception ex) {}
        return 0;
    }
    
    /**
     * Updates compound dataset values.
     *
     * @param fname the name of the file to open
     * @return zero if successful; otherwise returns one
     */
    private int test_H5CompoundDS_write(final String fname)
    {
        final H5File file=null;
        String message = "";

        message = "Updates compound dataset values -- H5CompoundDS.write()";
        if (!create_test_file(fname, message)) {
            return 1;
        }
        
        final int temp_value = 99999;
        try {
            final CompoundDS obj = (CompoundDS) FileFormat.getHObject(fname, NAME_DATASET_COMPOUND);
            if (obj == null) {
                failed(message, new HDF5Exception("Failed to get "+NAME_DATASET_COMPOUND), file);
                return 1;
            }
            
            final Vector buf1 = (Vector)obj.getData();
             if (buf1 == null) {
                failed(message, new HDF5Exception("Failed to read data from "+NAME_DATASET_COMPOUND), file);
                try { obj.getFileFormat().close(); } catch (final Exception ex2) {}
                return 1;
            }

            final int[] data_int1 = (int[])buf1.get(0);
            data_int1[0] = data_int1[1] = data_int1[2] = data_int1[3] = temp_value;
            obj.write();
            
            obj.clearData();
            final Vector buf2 = (Vector)obj.getData();
            if (buf2 == null) {
                failed(message, new HDF5Exception("Failed to read data from "+NAME_DATASET_COMPOUND), file);
                try { obj.getFileFormat().close(); } catch (final Exception ex2) {}
                return 1;
            }

            final int[] data_int2 = (int[])buf2.get(0);
             
            if ( !dataEquals(data_int1, data_int2) ) {
                failed(message, new HDF5LibraryException("Incorrect data values in file"), file);
                try { obj.getFileFormat().close(); } catch (final Exception ex2) {}
                return 1;
            }

            try { obj.getFileFormat().close(); } catch (final Exception ex2) {}
        } catch (final Exception ex) { failed(message, ex, file); return 1; }

        passed(message);
        try { file.close(); } catch (final Exception ex) {}
        return 0;
    }

    /**
     * Updates compound dataset values row by row (bug#847).
     *
     * @param fname the name of the file to open
     * @return zero if successful; otherwise returns one
     */
    private int test_H5CompoundDS_write_row_by_row(final String fname)
    {
        List list=null;
        final int TEST_INT_VALUE = 999999999;
        long[] count, start, dims;
        int rank, nmembers, nrows=1;
        H5File file=null;
        CompoundDS dset;
        String message = "";

        message = "Updates compound dataset values row by row -- H5CompoundDS.write()";
        if (!create_test_file(fname, message)) {
            return 1;
        }
        
        try {
            for (int rowIdx=0; rowIdx<nrows; rowIdx++) {
                // open the test file
                file = new H5File(fname, H5File.WRITE);
                file.open();

                // retrieve the compound dataset
                dset = (CompoundDS)file.get(NAME_DATASET_COMPOUND);
                dset.init();
               
                // get dataspace information
                rank = dset.getRank();
                count = dset.getSelectedDims();
                start = dset.getStartDims();
                dims = dset.getDims();
                nmembers = dset.getMemberCount();
                nrows = (int)dims[0];
                
                // select one row only
                for (int i=0; i<rank; i++) {
                    count[i] = 1;
                }
               
                // select different rows
                start[0] = rowIdx;
                
                // 1)  read the table cell (using dataset selection to select only that row of the table)
                list = (List)dset.read();
               
                // 2)  re-initialize the Dataset
                dset.init();
                
                // 3)  call 'Dataset.clearData()'
                dset.clearData();
         
                // 4)  call 'Dataset.getData()'
                list = (List)dset.read();
                
                // 5)  change the correct column/row **, col0/row0
                final int[] read_row_data = (int []) list.get(0);
                read_row_data[rowIdx] = TEST_INT_VALUE; 
                 
                // 6)  call 'Dataset.write()'
                dset.write(list);
                
                // 7)  close the file
                file.close();
              
                // 8)  reopen the file and read the table cell as in step 1
                file.open();
               
                // 9)  assert that the value has been changed and is correct
                dset = (CompoundDS)file.get(NAME_DATASET_COMPOUND);
                dset.init();
                rank = dset.getRank();
                count = dset.getSelectedDims();
                start = dset.getStartDims();
                dims = dset.getDims();
                nmembers = dset.getMemberCount();
                for (int i=0; i<rank; i++) {
                    start[i] = 0;
                    count[i] = 1;
                }
                list = (List)dset.read();
                
                final int[] write_row_data = (int[]) list.get(0);
                if (write_row_data[0] != TEST_INT_VALUE) {
                    failed(message, new HDF5LibraryException("Incorrect data values in file"), file);
                    return 1;
                }
               
                file.close();
            }
        } catch (final Exception ex) { failed(message, ex, file); return 1; }

        passed(message);
        return 0;
    }
    
    /**
     * Test read/re-read String datasets.
     *
     * @param fname the name of the file to open
     * @return zero if successful; otherwise returns one
     */
    private int test_HDF5ScalarDS_str(final String fname)
    {
        H5File file=null;
        final String message = "Test read/re-read String datasets";
        
        if (!create_test_file(fname, message)) {
            return 1;
        }
        
        file = new H5File(fname);
        
        try {
            final H5ScalarDS dataset = (H5ScalarDS)file.get(NAME_DATASET_STR);

            dataset.init();
            final long[] start = dataset.getStartDims();
            final long[] count = dataset.getSelectedDims();
            start[0] = 0;
            count[0] = 1;
            dataset.getData();

            dataset.init();
            dataset.clearData();
            dataset.getData();
           
        } catch (final Exception ex ) { failed(message, ex, file); return 1;}

        passed(message);
        try { file.close(); } catch (final Exception ex) {}
        return 0;
    }

    /**
     * Test ***func name*** function.
     *
     * @param fname the name of the file to open
     * @return zero if successful; otherwise returns one
     */
    private int test_temp(final String fname)
    {
        final H5File file=null;
        final String message = "***********func name*********";
        
        if (!create_test_file(fname, message)) {
            return 1;
        }
        
        try {
            
        } catch (final Exception ex ) { failed(message, ex, file); return 1;}

        passed(message);
        try { file.close(); } catch (final Exception ex) {}
        return 0;
    }

    /*****************************************************************************
     * Main routine for the testing. Use "-f" to save the test result to a log file.
     * If "-f" flag is specified, the test results will printed to System.out.
     * <p>
     * For example, "test.object.TestH5Object -f test.log" to save the test results
     *     at file test.log.
     * @param args
     ****************************************************************************/
    public static void main(final String[] args)
    {
        PrintStream printStream = null;
        int numOfFails = 0;

        final int n = args.length;
        if ((n > 1) && args[0].equals("-f"))
        {
            try {
                printStream = new PrintStream(new BufferedOutputStream(
                        new FileOutputStream(args[1])));
            } catch (final FileNotFoundException ex)
            {
                printStream = null;
                ex.printStackTrace();
            }
        }

        final TestH5Object test = new TestH5Object(printStream);

        numOfFails += test.test_H5File_create(FILE_NAME);
        numOfFails += test.test_H5File_open(FILE_NAME);
        numOfFails += test.test_H5File_open_relative_path(FILE_NAME2);
        numOfFails += test.test_H5File_createGroup(FILE_NAME);
        numOfFails += test.test_H5File_createDatatype(FILE_NAME);
        numOfFails += test.test_H5File_createLink(FILE_NAME);
        numOfFails += test.test_H5File_createImage(FILE_NAME);
        numOfFails += test.test_H5File_createScalarDS(FILE_NAME);
        numOfFails += test.test_H5File_createCompoundDS(FILE_NAME);
        numOfFails += test.test_H5File_copy(FILE_NAME);
        numOfFails += test.test_H5File_getAttribute(FILE_NAME);
        numOfFails += test.test_H5File_getHObject(FILE_NAME);
        numOfFails += test.test_HObject_getFID(FILE_NAME);
        numOfFails += test.test_HObject_getName(FILE_NAME);
        numOfFails += test.test_Group_isRoot(FILE_NAME);
        numOfFails += test.test_Group_getParent(FILE_NAME);
        numOfFails += test.test_Dataset_byteToString(FILE_NAME);
        numOfFails += test.test_H5Datatype_toNative(FILE_NAME);
        numOfFails += test.test_H5CompoundDS_init(FILE_NAME);
        numOfFails += test.test_H5ScalarDS_write(FILE_NAME);
        numOfFails += test.test_H5CompoundDS_write(FILE_NAME);
        numOfFails += test.test_H5CompoundDS_write_row_by_row(FILE_NAME);
        numOfFails += test.test_HDF5ScalarDS_str(FILE_NAME);
        
        if (numOfFails<=0) {
            TestH5Object.out.println("\nAll tests passed.\n\n");
        } else if (numOfFails==1) {
            TestH5Object.out.println("\n*** 1 test failed.\n\n");
        } else {
            TestH5Object.out.println("\n*** "+numOfFails+" tests failed.\n\n");
        }

    }
}
