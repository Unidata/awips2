/****************************************************************************
 * NCSA HDF                                                                 *
 * National Comptational Science Alliance                                   *
 * University of Illinois at Urbana-Champaign                               *
 * 605 E. Springfield, Champaign IL 61820                                   *
 *                                                                          *
 * For conditions of distribution and use, see the accompanying             *
 * hdf-java/COPYING file.                                                   *
 *                                                                          *
 ****************************************************************************/

package ncsa.hdf.hdflib;

import java.io.*;

/**
 *  This is the Java interface for the HDF 4.1 library.
 *  <p>
 *  This code is the called by Java programs to access the
 *  entry points of the HDF 4.1 library.
 *  Each routine wraps a single HDF entry point, generally with the
 *  arguments and return codes analogous to the C interface.
 * <p>
 *  For details of the HDF libraries, see the HDF Documentation at:
 *     <a href="http://hdf.ncsa.uiuc.edu">http://hdf.ncsa.uiuc.edu</a>
 * <p>
 *  <hr>
 * <p>
 *  These routines use the class <a href="./ncsa.hdf.hdflib.HDFArray.html">HDFArray</a>
 *  to handle arrays of arbitrary type and shape.
 * <p>
 *  <hr>
 *  <p>
 *  <b>Mapping of arguments for Java</b>
 *
 *  <p>
 *  In general, arguments to the HDF Java API are straightforward
 *  translations from the 'C' API described in the HDF Reference
 *  Manual.
 *  <p>
 *
 *  <center>
 *  <table border=2 cellpadding=4>
 *  <caption>
 *  <b>C types to Java types</b>
 *  </caption>
 *  <tr>
 *  <td>
 *  C
 *  </td>
 *  <td>
 *  Java
 *  </td>
 *  </tr>
 *  <tr>
 *  <td>
 *  int, intn, int32, uint32
 *  </td>
 *  <td>
 *  int
 *  </td>
 *  </tr>
 *  <tr>
 *  <td>
 *  short, uint16, int16
 *  </td>
 *  <td>
 *  short
 *  </td>
 *  </tr>
 *  <tr>
 *  <td>
 *  float, float32
 *  </td>
 *  <td>
 *  float
 *  </td>
 *  </tr>
 *  <tr>
 *  <td>
 *  double, float64
 *  </td>
 *  <td>
 *  double
 *  </td>
 *  </tr>
 *  <tr>
 *  <td>
 *  char, uchar, int8, uint8
 *  </td>
 *  <td>
 *  byte
 *  </td>
 *  </tr>
 *  <tr>
 *  <td>
 *  char * (<em>i.e.</em>, string)
 *  </td>
 *  <td>
 *  java.lang.String
 *  </td>
 *  </tr>
 *  <tr>
 *  <td>
 *  void
 *  </td>
 *  <td>
 *  void
 *  </td>
 *  </tr>
 *  <tr>
 *  <td>
 *  void *, VOIDP, char * (meaning ``any'')
 *  </td>
 *  <td>
 *  Special -- see HDFArray
 *  </td>
 *  </tr>
 *  </table>
 *  </center>
 *  <p>
 *  <center>
 *  <b>General Rules for Passing Arguments and Results</b>
 *  </center>
 *  <p>
 *  In general, arguments passed <b>IN</b> to Java are the analogous
 *  basic types, as above.
 *  The exception is for arrays, which are discussed below.
 *  <p>
 *  The <i>return value</i> of Java methods is also the analogous
 *  type, as above.
 *  A major exception to that rule is that all HDF functions that
 *  return SUCCEED/FAIL are declared <i>boolean</i> in the
 *  Java version, rather than
 *  <i>intn</i> or whatever.
 *  (Functions that return a value or else FAIL are declared
 *  the equivalent to the C function.)
 *  <p>
 *  Java does not support pass by reference of arguments, so
 *  arguments that are returned through <b>OUT</b> parameters
 *  must be wrapped in an object or array.
 *  The Java API for HDF consistently wraps arguments in
 *  arrays.
 *  <p>
 *  For instance, a function that returns two integers is
 *  declared:
 *  <p>
 *  <pre>
 *       void HDFdummy( int32* a1, int32* a2)
 *  </pre>
 *  For the Java interface, this would be declared:
 *  <p>
 *  <pre>
 *       public static native void HDFdummy( int args[] );
 *  </pre>
 *  where <i>a1</i> is <i>args[0]</i>
 *  and <i>a2</i> is <i>args[1]</i>.
 *  <p>
 *  All the routines where this convention is used will have
 *  specific documentation of the details, given below.
 *  <p>
 *  <b>Arrays</b>
 *  <p>
 *  HDF needs to read and write multi-dimensional arrays
 *  of many types.
 *  The HDF API is self-describing, with the data for the
 *  array passed as a block of bytes, for instance,
 *  <p>
 *  <pre>
 *      int SDreaddata(int sdsid, int32 *start, int32 * stride,
 *             int32 *count, VOIDP data);
 *  </pre>
 *  <p>
 *  where ``VOIDP'' means that the data may be any valid numeric
 *  type, and is a contiguous block of bytes that is the data
 *  for a multi-dimensional array.
 *  <p>
 *  For Java, this is a problem, as the type of data must
 *  be declared.  Furthermore, multidimensional arrays
 *  are definitely <i>not</i> layed out contiguously
 *  in memory.
 *  It would be infeasible to declare a separate routine for
 *  every combination of number type and dimensionality.
 *  For that reason, the <b>HDFArray</b> class is used to
 *  discover the type, shape, and size of the data array
 *  at run time, and to convert to and from contigous
 *  bytes.
 *  The upshot is that the data can be passed as an ``Object'',
 *  and the Java API will translate to and from the appropriate
 *  bytes.
 *  So the function above would be declared:
 *  <p>
 *  <pre>
 *      int SDreaddata(int sdsid, int[] start, int[] stride,
 *             int[] count, Object data);
 *  </pre>
 *  and the parameter <i>data</i> can be any multi-dimensional
 *  array of numbers, such as float[][], or int[][][].
 *  <p>
 *  <center>
 *  <b>Compression and Chunk Information</b>
 *  </center>
 *  <p>
 *  The HDF library passes the parameters needed by compression
 *  and chunking through  C structures (actually, unions).
 *  The Java interface passes these as instances of subclasses
 *  of class HDFCompInfo and HDFChunkInfo respectively.
 *
 *  <p><b> See: </b><a href="ncsa.hdf.hdflib.HDFChunkInfo.html">
 *  ncsa.hdf.hdflib.HDFChunkInfo</a>,
 *  and
 *  <p><a href="ncsa.hdf.hdflib.HDFCompInfo.html">
 *  ncsa.hdf.hdflib.HDFCompInfo</a>.
 *  <p>
 *  <hr>
 */
public class HDFLibrary 
{
    public final static String HDFPATH_PROPERTY_KEY = "ncsa.hdf.hdflib.HDFLibrary.hdflib";

    private final static String JHI_VERSION= "2.5";
    private static boolean isLibraryLoaded = false;

    static { loadH4Lib(); }
    
    public static void loadH4Lib()
    {
        if (isLibraryLoaded) // load only once
            return;
        
        String filename = System.getProperty(HDFPATH_PROPERTY_KEY, null);
      
        if ((filename != null) && (filename.length() > 0))
        {
            File h5dll = new File(filename);
            if (h5dll.exists() && h5dll.canRead() && h5dll.isFile()) {
                try {
                    System.load(filename);
                    isLibraryLoaded = true;
                    } catch (Throwable err) { isLibraryLoaded = false; }
            } else {
                isLibraryLoaded = false;
                throw (new UnsatisfiedLinkError("Invalid HDF4 library, "+filename));
            }
        }
        
        if (!isLibraryLoaded)
        {
            try {
                System.loadLibrary("jhdf");
                isLibraryLoaded = true;
            } catch (Throwable err) { isLibraryLoaded = false; }
        }

        try { 
            HDFLibrary.HDdont_atexit();
        } catch (HDFException e) {
            System.exit(1);
        }
        
        /* Important!  Exit quietly */
    }

    public static final String getJHIVersion() { return JHI_VERSION; }

    public  static int Hopen(String filename) throws HDFException {
        return Hopen(filename, HDFConstants.DFACC_RDONLY);
    }

    public static native int Hopen(String filename, int access) throws HDFException;

    public static native  boolean Hclose(int fid) throws HDFException;
    
    public static native int HDdont_atexit() throws HDFException;

    public static native boolean Hishdf(String fileName)  throws HDFException;

    public static native int Hnumber(int fid)  throws HDFException;

    public static native int DFKNTsize(int numbertype)  throws HDFException;

    public static native String HDgetNTdesc(int nt) throws HDFException;

    public static native boolean Hcache(int file_id, int cache_switch) throws HDFException;

/*  not yet implemented
    public static native boolean Hflushdd(int file_id) throws HDFException;
*/

    /**
     *
     *  <b>Note:</b> the version of an HDF file is not well defined,
     *  it is not recommended that programs rely on these numbers.
     *  <p>
     *  @param file_id <b>IN</b>: int, the file descriptor returned by Hopen
     *  @param vers <b>OUT</b>: int[3], the major version, minor version,
     *  and release number of the file.
     *  @param string <b>OUT</b>: String[1], the version string
     *
     *  @exception ncsa.hdf.hdflib.HDFException
     *             should be thrown for errors in the
     *             HDF library call, but is not yet implemented.
     *
     *  @return the major, minor, and release number are returned
     * in the array of ints, and a string is returned in the string.
     */
    public static native boolean Hgetfileversion(int file_id, int[] vers,
        String []string) throws HDFException;

    /**
     *  @param vers <b>OUT</b>: int[3], the major version, minor version,
     *  and release number of the HDF library.
     *  @param string <b>OUT</b>: String[1], the version string
     *
     *  @return the major, minor, and release number are returned
     * in the array of ints, and a string is returned in the string.
     *
     *  @exception ncsa.hdf.hdflib.HDFException
     *             should be thrown for errors in the
     *             HDF library call, but is not yet implemented.
     */
    public static native boolean Hgetlibversion(int[] vers,String []string) throws HDFException;

    public static native boolean Hsetaccesstype(int h_id, int  access_type) throws HDFException;

    public static native boolean Hsync(int file_id) throws HDFException;

    public static native int ANstart(int fid) throws HDFException;

    public static native boolean ANend(int an_id) throws HDFException;

    public static native boolean ANendaccess( int an_id) throws HDFException;

    /**
     *  @param an_id <b>IN</b>: the AN interface id, returned by ANstart
     *  @param info <b>OUT</b>: int[4], n_file_label, n_file_desc,
     *  n_data_label, n_data_desc
     *
     *  @return four integer parameters:
     *  info[0] = n_file_label, info[1] = n_file_desc,
     *  info[2] = n_data_label, info[3] = n_data_desc
     *
     *  @exception ncsa.hdf.hdflib.HDFException
     *             should be thrown for errors in the
     *             HDF library call, but is not yet implemented.
     *
     */
    public static native boolean ANfileinfo(int an_id, int [] info) throws HDFException;

    public static native int ANselect(int an_id, int index, int anntype) throws HDFException;

    public static native int ANnumann(int an_id, int anntype, short tag, short ref) throws HDFException;

    public static native short ANatype2tag(int antag) throws HDFException;

    public static native int ANtag2atype(short anttype) throws HDFException;

    /**
     *  @param an_id <b>IN</b>: the AN interface id, returned by ANstart
     *  @param anntype <b>IN</b>: the number type, as defined in HDFConstants
     *  @param tag <b>IN</b>: the HDF tag
     *  @param ref <b>IN</b>: the HDF ref
     *  @param ann_list <b>OUT</b>: int[], an array of annotation identifiers.
     *  The array must be long enough to hold the number of annotations
     *  returned by ANnumann
     *
     *  @exception ncsa.hdf.hdflib.HDFException
     *             should be thrown for errors in the
     *             HDF library call, but is not yet implemented.
     *
     *
     *  @return an array of integers, which are the identifiers of
     *  the annotations
     */
    public static native int ANannlist(int an_id,  int anntype, int tag, int ref,
                int[] ann_list) throws HDFException;

    public static native int ANannlen( int ann_id) throws HDFException;

    /**
     *  @param ann_id <b>IN</b>: the AN interface id, returned by ANstart
     *  @param annbuf <b>OUT</b>: String[1], the annotation is returned as annbuf[0].
     *  @param maxlen <b>IN</b>: int, the maximum length of the string.
     *
     *  @exception ncsa.hdf.hdflib.HDFException
     *             should be thrown for errors in the
     *             HDF library call, but is not yet implemented.
     *
     *
     *  @return an annotation string: annbuf[0] = the annotation
     */
    public static native boolean ANreadann( int ann_id, String[] annbuf, int maxlen) throws HDFException;

    public static native int ANcreate(int an_id, short tag, short ref, int type) throws HDFException;

    public static native int ANcreatef(int an_id, int type) throws HDFException;

    public static native boolean ANdestroy() throws HDFException;

    /**
     *  @param an_id <b>IN</b>: the AN interface id, returned by ANstart
     *  @param index <b>IN</b>: the index of the annotation
     *  @param type <b>IN</b>: the type of the annotation
     *  @param tagref <b>OUT</b>: short[2], the tag and ref of the annotation
     *
     *  @exception ncsa.hdf.hdflib.HDFException
     *             should be thrown for errors in the
     *             HDF library call, but is not yet implemented.
     *
     *
     *  @return the tag and ref:  tagref[0] = tag, tagref[1] = ref
     */
    public static native int ANget_tagref(int an_id, int index,  int type, short[] tagref) throws HDFException;

    /**
     *  @param an_id <b>IN</b>: the AN interface id, returned by ANstart
     *  @param tagref <b>OUT</b>: short[2], the tag and ref of the annotation
     *
     *  @exception ncsa.hdf.hdflib.HDFException
     *             should be thrown for errors in the
     *             HDF library call, but is not yet implemented.
     *
     *
     *  @return the tag and ref:  tagref[0] = tag, tagref[1] = ref
     */
    public static native boolean ANid2tagref(int an_id, short [] tagref) throws HDFException;

    public static native int ANtagref2id(int an_id, short tag,  short ref) throws HDFException;

    public static native boolean ANwriteann(int ann_id, String label, int ann_length) throws HDFException;

    public static native boolean DFPaddpal(String filename, byte[] palette) throws HDFException;

    public static native boolean DFPgetpal(String filename, byte[] palette) throws HDFException;

    public static native short DFPlastref( ) throws HDFException;

    public static native int DFPnpals(String filename)  throws HDFException;

    public static native boolean DFPputpal (String filename, byte[] palette, int overwrite, String filemode) throws HDFException;

    /**
     *  @param filename <b>IN</b>: String, the name of the HDF file
     *  @param palette <b>IN</b>: byte[] the palette
     *  @param overwrite <b>IN</b>: boolean, converted to 1 == true, 0 == false
     *  to call the HDF library
     *
     *  @exception ncsa.hdf.hdflib.HDFException
     *             should be thrown for errors in the
     *             HDF library call, but is not yet implemented.
     *
     */
    public static boolean DFPputpal (String filename, byte[] palette, boolean overwrite, String filemode)
     throws HDFException{
        if (overwrite) {
            return DFPputpal (filename, palette, 1, filemode);
        } else {
            return DFPputpal (filename, palette, 0, filemode);
        }
    }


    public static native boolean DFPreadref(String filename, short ref) throws HDFException;

    public static native short DFPrestart( ) throws HDFException;

    public static native boolean DFPwriteref(String filename, short ref) throws HDFException;

    public static native int GRstart(int fid) throws HDFException;


    public static native boolean GRend(int grid) throws HDFException;

    /**
     *  @param grid <b>IN</b>: the GR interface id, returned by GRstart
     *  @param args <b>OUT</b>: int[2], n_datasets and n_file_attrs
     *
     *  @exception ncsa.hdf.hdflib.HDFException
     *             should be thrown for errors in the
     *             HDF library call, but is not yet implemented.
     *
     *
     *  @return the file info:  args[0] = n_datasets, args[1] = n_file_attrs
     */
    public static native boolean GRfileinfo(int grid, int [] args) throws HDFException;

    /**
     *  @param sdsid <b>IN</b>: the SD identifier returned by SDselect
     *  @param chunk_def <b>OUT</b>: HDFChunkInfo, the chunking info
     *  @param flag <b>OUT</b>: int[1], the type of chunking
     *
     *  @exception ncsa.hdf.hdflib.HDFException
     *             should be thrown for errors in the
     *             HDF library call, but is not yet implemented.
     *
     *
     *  @return c_info contains information about the chunking method,
     *  flags[0] == the chunking flags
     *
     *  <p><b>NOTE:</b>The chunking algorithm-specific information is
     *  passed in an appropriate sub-class of HDFChunkInfo.
     */
    public static native boolean GRgetchunkinfo( int sdsid, HDFChunkInfo chunk_def,
        int[] flag) throws HDFException;

    public static native int GRselect( int grid, int index) throws HDFException;

    public static native int GRnametoindex( int grid, String name) throws HDFException;

    /**
     *  @param grid <b>IN</b>: the GR interface id, returned by GRstart
     *  @param args <b>OUT</b>: int[5], image info:
     *          number of components in the image,
     *          data type of the image data,
     *          interlace mode of the stored image data,
     *          sizes of each image dimension ,
     *          number of attributes assigned to the image
     *  @param dim_sizes <b>OUT</b>: int[2], dim_sizes
     *
     *  @exception ncsa.hdf.hdflib.HDFException
     *             should be thrown for errors in the
     *             HDF library call, but is not yet implemented.
     *
     *
     *  @return the file info:  String[0] = gr_name, args[0] = ncomp,
     *  args[1] = data_type, args[2] = interlace, args[3] = num_attrs
     *
     *  <p><b>NOTE:</b> the parameters for the Java interface are not in
     *  the same order as the C interface.
     */
    public static native boolean GRgetiminfo( int grid, String[] gr_name, int[] args,
    int[] dim_sizes) throws HDFException;

    /**
     *  @param grid <b>IN</b>: the GR interface id, returned by GRstart
     *  @param start <b>IN</b>: int[], start
     *  @param stride <b>IN</b>: int[], stride
     *  @param count <b>IN</b>: int[], count
     *  @param data <b>OUT</b>: byte[], data
     *
     *  @exception ncsa.hdf.hdflib.HDFException
     *             should be thrown for errors in the
     *             HDF library call, but is not yet implemented.
     *
     *
     *  @return the the data in the form of a continous array of
     *  bytes.
     *
     *  <p><b>NOTE:</b> to read into a Java 2D array use the alternative
     *  routine below.
     */
    public static native boolean GRreadimage( int grid, int[] start, int[] stride,
                       int[] count, byte[] data) throws HDFException;

    /**
     *  @param grid <b>IN</b>: the GR interface id, returned by GRstart
     *  @param start <b>IN</b>: int[], start
     *  @param stride <b>IN</b>: int[], stride
     *  @param count <b>IN</b>: int[], count
     *  @param theData <b>OUT</b>: Object, a Java array of appropriate
     *  type, dimensions, and size.
     *
     *  @exception ncsa.hdf.hdflib.HDFException
     *             should be thrown for errors in the
     *             HDF library call, but is not yet implemented.
     *
     *
     *  @return the the data in the Java array.
     *
     *  <p><b>Note:</b> reads the data as bytes and converts to
     *  the Java array.
     */
    public static boolean GRreadimage( int grid, int[] start, int[] stride,
                       int[] count, Object theData)
     throws HDFException{
        byte[] data;
        boolean rval;

        HDFArray theArray = new HDFArray(theData);
        data = theArray.emptyBytes();
        rval = GRreadimage( grid, start, stride, count, data);
        theData = theArray.arrayify( data );
        return rval;
    }

    public static native boolean GRendaccess( int riid) throws HDFException;

/*
   "[Later]"
    public static native int  GRgetdimid( int riid, int index) throws HDFException;
*/

/*
   "[Later]"
    public static native boolean  GRdiminfo( int dimid, char[] name, int[] diminfo[]) throws HDFException;
*/

    public static native short  GRidtoref( int riid) throws HDFException;

    public static native int  GRreftoindex( int grid, short ref) throws HDFException;

    public static native boolean  GRreqlutil( int riid, int interlace) throws HDFException;

    public static native boolean  GRreqimageil( int rrid, int interlace) throws HDFException;

    public static native int  GRgetlutid( int rrid, int index) throws HDFException;

    public static native int  GRgetnluts( int rrid) throws HDFException;

    /**
     *  @param lutid <b>IN</b>: the palette identifier returned by GRgetlutid
     *  @param args <b>OUT</b>: int[4], palette info:
     *            Number of components in the palette,
     *            Data type of the palette data,
     *            Interlace mode of the stored palette data,
     *            Number of color lookup table entries in the palette.
     *
     *  @exception ncsa.hdf.hdflib.HDFException
     *             should be thrown for errors in the
     *             HDF library call, but is not yet implemented.
     *
     *
     *  @return the palette info:  args[0] = ncomp, args[1] = data_type,
     *  args[2] = interlace, args[3] = num_entries
     *
     */
    public static native boolean  GRgetlutinfo( int lutid, int[] args) throws HDFException;

    /**
     *  @param lutid <b>IN</b>: the palette identifier returned by GRgetlutid
     *  @param data <b>OUT</b>: byte[], palette data, in bytes
     *
     *  @exception ncsa.hdf.hdflib.HDFException
     *             should be thrown for errors in the
     *             HDF library call, but is not yet implemented.
     *
     *
     *  @return the palette data:  as bytes
     *
     *  <p><b>NOTE:</b> to read into a Java 1D array use the alternative
     *  routine below.
     */
    public static native boolean  GRreadlut( int lutid, byte[] data) throws HDFException;

    /**
     *  @param lutid <b>IN</b>: the palette identifier returned by GRgetlutid
     *  @param theData <b>OUT</b>: Object, palette data, an Java array
     *
     *  @exception ncsa.hdf.hdflib.HDFException
     *             should be thrown for errors in the
     *             HDF library call, but is not yet implemented.
     *
     *
     *  @return the palette data:  as a Java array
     *
     *  <p><b>Note:</b> reads the data as bytes and converts to
     *  the Java array.
     */
    public static boolean  GRreadlut( int lutid, Object theData)
    throws HDFException {
        byte[] data;
        boolean rval;

        HDFArray theArray = new HDFArray(theData);
        data = theArray.emptyBytes();
            rval = GRreadlut( lutid, data);
        theData = theArray.arrayify( data );
        return rval;
    }

    /**
     *  @param id <b>IN</b>: the GR identifier returned by GRstart
     *  @param index <b>IN</b>: the index of the attribute
     *  @param name <b>OUT</b>: String[1], the name of the attribute
     *  @param argv <b>OUT</b>: int[2], the type and length of the
     *  attribute
     *
     *  @exception ncsa.hdf.hdflib.HDFException
     *             should be thrown for errors in the
     *             HDF library call, but is not yet implemented.
     *
     *
     *  @return the name, type, and lenght of the attribute:
     *  name[0] = name, argv[0] = data_type, argv[1] = length
     */
    public static native boolean  GRattrinfo( int id, int index, String []name,
        int[] argv) throws HDFException;

    /**
     *  @param id <b>IN</b>: the GR identifier returned by GRstart
     *  @param data <b>OUT</b>: byte[], attribute data, in bytes
     *
     *  @exception ncsa.hdf.hdflib.HDFException
     *             should be thrown for errors in the
     *             HDF library call, but is not yet implemented.
     *
     *
     *  @return the attribute data:  as bytes
     *
     *  <p><b>NOTE:</b> to read into a Java 1D array use the alternative
     *  routine below.
     */
    public static native boolean  GRgetattr( int id, int index, byte[] data) throws HDFException;

    /**
     *  @param id <b>IN</b>: the GR identifier returned by GRstart
     *  @param theData <b>OUT</b>: Object, attribute data, an Java array
     *
     *  @exception ncsa.hdf.hdflib.HDFException
     *             should be thrown for errors in the
     *             HDF library call, but is not yet implemented.
     *
     *
     *  @return the attribute data:  as a Java array
     *
     *  <p><b>Note:</b> reads the data as bytes and converts to
     *  the Java array.
     */
    public static boolean  GRgetattr( int id, int index, Object theData)
    throws HDFException {
        byte[] data;
        boolean rval;

        HDFArray theArray = new HDFArray(theData);
        data = theArray.emptyBytes();
            rval = GRgetattr( id, index,  data);
        theData = theArray.arrayify( data );
        return rval;
    }

    public static native int  GRfindattr( int id,  String name) throws HDFException;

    public static native int GRcreate(int gr_id, String name, int ncomp,
    int data_type, int interlace_mode, int[] dim_sizes) throws HDFException;

    public static native short  GRluttoref( int pal_id) throws HDFException;

    /**
     *  @param gr_id <b>IN</b>: the GR identifier returned by GRstart
     *  @param attr_name <b>IN</b>: the name of the attribute
     *  @param data_type <b>IN</b>: the number type of the data (should
     *  be DFNT_CHAR)
     *  @param count <b>IN</b>: the length the data (lenght of 'values')
     *  @param values <b>IN</b>: the the attribute to write -- A String
     *
     *  @exception ncsa.hdf.hdflib.HDFException
     *             should be thrown for errors in the
     *             HDF library call, but is not yet implemented.
     *
     *
     *  <p><b>NOTE:</b>This routine writes a attribute that is
     *  a String.  Alternative methods write data of other types.
     */
    public static native boolean GRsetattr(int gr_id, String attr_name,
        int data_type, int count, String values) throws HDFException;

    /**
     *  @param gr_id <b>IN</b>: the GR identifier returned by GRstart
     *  @param attr_name <b>IN</b>: the name of the attribute
     *  @param data_type <b>IN</b>: the number type of the data
     *  @param count <b>IN</b>: the length the data (lenght of 'values')
     *  @param values <b>IN</b>: the the attribute to write -- in an
     *  array of bytes
     *
     *  @exception ncsa.hdf.hdflib.HDFException
     *             should be thrown for errors in the
     *             HDF library call, but is not yet implemented.
     *
     *
     *  <p><b>NOTE:</b>This routine writes the attribute as an array
     *  of bytes.  <b>DO NOT USE THIS TO WRITE A STRING.</b>  This
     *  is intended for numeric data that has been flattened into
     *  bytes.
     */
    public static native boolean GRsetattr(int gr_id, String attr_name,
        int data_type, int count, byte[] values) throws HDFException;

    /**
     *  @param gr_id <b>IN</b>: the GR identifier returned by GRstart
     *  @param attr_name <b>IN</b>: the name of the attribute
     *  @param data_type <b>IN</b>: the number type of the data
     *  @param count <b>IN</b>: the length the data (lenght of 'values')
     *  @param theData <b>IN</b>: Object -- the value to be written,
     *  a Java array of numbers.
     *
     *  @exception ncsa.hdf.hdflib.HDFException
     *             should be thrown for errors in the
     *             HDF library call, but is not yet implemented.
     *
     *
     *  <p><b>NOTE:</b>This routine converts the Java array to bytes
     *  then writes it.
     *  <b>DO NOT USE THIS TO WRITE A STRING.</b>
     */
    public static boolean GRsetattr(int gr_id, String attr_name,
        int data_type, int count, Object theData) throws HDFException {
        byte[] data;
        HDFArray theArray = new HDFArray(theData);
        data = theArray.byteify();
        return GRsetattr(gr_id, attr_name, data_type, count, data);
    }
    /**
     *  @param sdsid <b>IN</b>: the SD identifier returned by SDselect
     *  @param chunk_def <b>IN</b>: HDFChunkInfo, the chunking info
     *  @param flags <b>IN</b>: the type of chunking
     *
     *  @exception ncsa.hdf.hdflib.HDFException
     *             should be thrown for errors in the
     *             HDF library call, but is not yet implemented.
     *
     *
     *  <p><b>NOTE:</b>The chunking algorithm-specific information is
     *  passed in an appropriate sub-class of HDFChunkInfo.
     */
    public static native boolean GRsetchunk( int sdsid, HDFChunkInfo chunk_def,
        int flags) throws HDFException;

    public static native int GRsetchunkcache( int sdsid, int maxcache, int flags) throws HDFException;
    /**
     *  @param ri_id <b>IN</b>: the GR identifier returned by GRstart
     *  @param comp_type <b>IN</b>: the type of compression
     *  @param c_info <b>IN</b>: HDFCompInfo, the compression info
     *
     *  @exception ncsa.hdf.hdflib.HDFException
     *             should be thrown for errors in the
     *             HDF library call, but is not yet implemented.
     *
     *
     *  <p><b>NOTE:</b>The compression algorithm specific information is
     *  passed in an appropriate sub-class of HDFCompInfo.
     */
    public static native boolean GRsetcompress(int ri_id, int comp_type, HDFCompInfo c_info) throws HDFException;

    public static native boolean GRgetcompress(int ri_id, HDFCompInfo c_info) throws HDFException;

    public static native boolean GRsetexternalfile(int ri_id, String filename, int offset) throws HDFException;

    /**
     *  @param grid <b>IN</b>: the GR interface id, returned by GRstart
     *  @param start <b>IN</b>: int[], start
     *  @param stride <b>IN</b>: int[], stride
     *  @param edge <b>IN</b>: int[], count
     *  @param data <b>IN</b>: byte[], data to be written
     *
     *  @exception ncsa.hdf.hdflib.HDFException
     *             should be thrown for errors in the
     *             HDF library call, but is not yet implemented.
     *
     *
     *  <p><b>NOTE:</b> to write from a Java 2D array use the alternative
     *  routine below.
     */

    public static native boolean GRwriteimage(int grid, int [] start, int[] stride,
        int[] edge, byte[] data) throws HDFException;

    /**
     *  @param grid <b>IN</b>: the GR interface id, returned by GRstart
     *  @param start <b>IN</b>: int[], start
     *  @param stride <b>IN</b>: int[], stride
     *  @param edge <b>IN</b>: int[], count
     *  @param theData <b>IN</b>: Object, a Java array of appropriate
     *  type, dimensions, and size.
     *
     *  @exception ncsa.hdf.hdflib.HDFException
     *             should be thrown for errors in the
     *             HDF library call, but is not yet implemented.
     *
     *
     *  <p><b>Note:</b> converts the Java array to a contiguous
     *  block of bytes appropriate for C, and then writes the bytes.
     */
    public static boolean GRwriteimage(int grid, int [] start, int[] stride,
        int[] edge, Object theData) throws HDFException
    {
        byte[] data;
        HDFArray theArray = new HDFArray(theData);
        data = theArray.byteify();
        return GRwriteimage(grid, start, stride, edge, data);
    }

    /**
     *  @param pal_id <b>IN</b>: the palette identifier returned by GRgetlutid
     *  @param ncomp <b>IN</b>: int, number of components
     *  @param data_type <b>IN</b>: int, number type
     *  @param interlace <b>IN</b>: int, interlace
     *  @param num_entries <b>IN</b>: int, number of entries
     *  @param pal_data <b>IN</b>: byte[], palette data to be written--as bytes
     *
     *  @exception ncsa.hdf.hdflib.HDFException
     *             should be thrown for errors in the
     *             HDF library call, but is not yet implemented.
     *
     *
     *  <p><b>NOTE:</b> to write from a Java array of numbers use
     *  the alternative routine below.
     */

    public static native boolean GRwritelut(int pal_id, int ncomp, int data_type,
        int interlace, int num_entries, byte[] pal_data) throws HDFException;

    /**
     *  @param pal_id <b>IN</b>: the palette identifier returned by GRgetlutid
     *  @param ncomp <b>IN</b>: int, number of components
     *  @param data_type <b>IN</b>: int, number type
     *  @param interlace <b>IN</b>: int, interlace
     *  @param num_entries <b>IN</b>: int, number of entries
     *  @param theData <b>IN</b>: Object, palette data to be written, any
     *  number type.
     *
     *  @exception ncsa.hdf.hdflib.HDFException
     *             should be thrown for errors in the
     *             HDF library call, but is not yet implemented.
     *
     *
     *  <p><b>Note:</b> converts the Java array to a contiguous
     *  block of bytes appropriate for C, and then writes the bytes.
     */
    public static boolean GRwritelut(int pal_id, int ncomp, int data_type,
        int interlace, int num_entries, Object theData) throws HDFException
        {
        byte[] data;
        HDFArray theArray = new HDFArray(theData);
        data = theArray.byteify();
        return GRwritelut(pal_id, ncomp, data_type, interlace, num_entries,
            data);
    }

    /**
     *  @param sdsid <b>IN</b>: the GR interface id, returned by SDselect
     *  @param origin <b>IN</b>: int[], origin
     *  @param theData <b>OUT</b>: byte[], the data in an array of bytes
     *
     *  @exception ncsa.hdf.hdflib.HDFException
     *             should be thrown for errors in the
     *             HDF library call, but is not yet implemented.
     *
     *
     *  <p><b>NOTE:</b> to read into a Java array use the alternative
     *  routine below.
     */
    public static native boolean GRreadchunk( int sdsid, int[] origin, byte[] theData) throws HDFException;

    /**
     *  @param grid <b>IN</b>: the GR interface id, returned by SDselect
     *  @param origin <b>IN</b>: int[], origin
     *  @param theData <b>IN</b>: Object, a Java array of appropriate
     *  type, dimensions, and size.
     *
     *  @exception ncsa.hdf.hdflib.HDFException
     *             should be thrown for errors in the
     *             HDF library call, but is not yet implemented.
     *
     *
     *  <p><b>Note:</b> reads the data as a contiguous
     *  array of bytes and then converts it to an appropriate Java object.
     */
    public static boolean GRreadchunk( int grid, int[] origin, Object theData) throws HDFException {
        byte[] data;
        boolean rval;

        HDFArray theArray = new HDFArray(theData);
        data = theArray.emptyBytes();
            rval = GRreadchunk( grid, origin,  data);
        theData = theArray.arrayify( data );
        return rval;
    }

    public static native boolean HDFclose(int file_id) throws HDFException;

    public static native int HDFopen(String filename, int access, short n_dds) throws HDFException;

    public static native short HEvalue(int level) throws HDFException;

    public static native String HEstring(int error_code) throws HDFException;

    public static native boolean HXsetcreatedir(String dir) throws HDFException;

    public static native boolean HXsetdir(String dir) throws HDFException ;

    public static native int SDstart(String filename, int accessmode) throws HDFException;

    public static native boolean SDend(int sdid) throws HDFException;

    /**
     *  @param sdid <b>IN</b>: the SD interface id, returned by SDstart
     *  @param argv <b>OUT</b>: int[2],
     *          Number of datasets in the file,
     *          Number of global attributes in the file
     *
     *  @exception ncsa.hdf.hdflib.HDFException
     *             should be thrown for errors in the
     *             HDF library call, but is not yet implemented.
     *
     *
     *  @return the file info:  argv[0] = n_datasets, argv[1] = n_file_attrs
     */
    public static native boolean SDfileinfo(int sdid, int[] argv) throws HDFException;

    public static native int SDselect( int sdid, int index) throws HDFException;

    public static native int SDnametoindex( int sdid, String name) throws HDFException;

    /**
     *  @param sdsid <b>IN</b>: the SD interface id, returned by SDselect
     *  @param name <b>OUT</b>: String[1], the name of the dataset
     *  @param args <b>OUT</b>: int[3], dataset info:
     *         number of dimensions (rank),
     *         data type for the data stored in the dataset,
     *         number of "netCDF-style" attributes for this dataset
     *  @param dimsizes <b>OUT</b>: int[(rank)], sizes of dimensions
     *
     *  @exception ncsa.hdf.hdflib.HDFException
     *             should be thrown for errors in the
     *             HDF library call, but is not yet implemented.
     *
     *
     *  @return the file info:  name[0] = gr_name, args[0] = rank,
     *  args[1] = data_type, args[2] = nattrs, dim_sizes[] = dimensions
     *
     *  <p><b>NOTE:</b> the parameters for the Java interface are not in
     *  the same order as the C interface.
     */
    public static native boolean SDgetinfo( int sdsid, String []name,
            int [] dimsizes, int[] args) throws HDFException;


    /**
     *  @param sdsid <b>IN</b>: the SD interface id, returned by SDselect
     *  @param emptySDS <b>OUT</b>: int[1], 1 if the SDS is empty, 0 if has data
     *
     *  @exception ncsa.hdf.hdflib.HDFException
     *             should be thrown for errors in the
     *             HDF library call, but is not yet implemented.
     */
    public static native boolean SDcheckempty( int sdsid, int[] emptySDS) throws HDFException;

    /**
     *  @param sdsid <b>IN</b>: the SD interface id, returned by SDselect
     *  @param start <b>IN</b>: int[], start
     *  @param stride <b>IN</b>: int[], stride
     *  @param count <b>IN</b>: int[], count
     *  @param data <b>OUT</b>: byte[], data
     *
     *  @exception ncsa.hdf.hdflib.HDFException
     *             should be thrown for errors in the
     *             HDF library call, but is not yet implemented.
     *
     *
     *  @return the the data in the form of a continous array of
     *  bytes.
     *
     *  <p><b>NOTE:</b> to read into a Java array use the alternative
     *  routine below.
     */
    public static native boolean SDreaddata(  int sdsid, int[] start, int[] stride,
                       int[] count, byte[] data) throws HDFException;

    /**
     *  @param sdsid <b>IN</b>: the SD interface id, returned by SDselect
     *  @param start <b>IN</b>: int[], start
     *  @param stride <b>IN</b>: int[], stride
     *  @param count <b>IN</b>: int[], count
     *  @param theData <b>OUT</b>: Object, a Java array of appropriate
     *  type, dimensions, and size.
     *
     *  @exception ncsa.hdf.hdflib.HDFException
     *             should be thrown for errors in the
     *             HDF library call, but is not yet implemented.
     *
     *
     *  @return the the data in the Java array.
     *
     *  <p><b>Note:</b> reads the data as bytes and converts to
     *  the Java array.
     */
    public static boolean SDreaddata(  int sdsid, int[] start, int[] stride,
                       int[] count, Object theData ) throws HDFException {
        boolean status = false;
        boolean is1D = false;

        Class dataClass = theData.getClass();
         if (!dataClass.isArray()) {
            throw (new HDFJavaException("SDreaddata: data is not an array"));
        }

        String cname = dataClass.getName();
        is1D = (cname.lastIndexOf('[') ==cname.indexOf('['));
        char dname = cname.charAt(cname.lastIndexOf("[")+1);

        if (is1D && (dname == 'B')) {
            status = SDreaddata(sdsid, start, stride, count, (byte[])theData);
        }
        else if (is1D && (dname == 'S')) {
            status = SDreaddata_short(sdsid, start, stride, count, (short[])theData);
        }
        else if (is1D && (dname == 'I')) {
            status = SDreaddata_int(sdsid, start, stride, count, (int[])theData);
        }
        else if (is1D && (dname == 'J')) {
            status = SDreaddata_long(sdsid, start, stride, count, (long[])theData);
        }
        else if (is1D && (dname == 'F')) {
            status = SDreaddata_float(sdsid, start, stride, count, (float[])theData);
        }
        else if (is1D && (dname == 'D')) {
            status = SDreaddata_double(sdsid, start, stride, count, (double[])theData);
        } else {
            byte[] data;
            HDFArray theArray = new HDFArray(theData);
            data = theArray.emptyBytes();
            status = SDreaddata(  sdsid, start, stride, count, data);
            theData = theArray.arrayify( data );
        }

        return status;
    }

    public static native boolean SDendaccess( int sdsid) throws HDFException;

    public static native int  SDgetdimid( int sdsid, int index) throws HDFException;

    /**
     *  @param dimid <b>IN</b>: the dimension id, returned by SDgetdimid
     *  @param name <b>OUT</b>: String[1], the dimension name
     *  @param argv <b>OUT</b>: int[3], size of the name string,
     *  number type of data in the array, # attributes for the dimension
     *
     *  @exception ncsa.hdf.hdflib.HDFException
     *             should be thrown for errors in the
     *             HDF library call, but is not yet implemented.
     *
     *
     *  @return name[0] = name, argv[0] = count, argv[1] = data_type,
     *  argv[2] = nattr
     */
    public static native boolean  SDdiminfo( int dimid, String [] name, int[] argv)
    throws HDFException;

    public static native int  SDidtoref( int sdsid) throws HDFException;

    public static native int  SDreftoindex( int sdid, int ref) throws HDFException;

    /**
     *  @param id <b>IN</b>: id of a file, SDS, or dimension
     *  @param index <b>IN</b>: index of the attribute
     *  @param name <b>OUT</b>: String[1], the name of the attribute
     *  @param argv <b>OUT</b>: int[2],  number type of the attribute,
     *  number of values in the attribute
     *
     *  @exception ncsa.hdf.hdflib.HDFException
     *             should be thrown for errors in the
     *             HDF library call, but is not yet implemented.
     *
     *
     *  @return name[0] = attr_name, argv[0] = data_type,
     *  argv[1] = count
     */
    public static native boolean  SDattrinfo( int id, int index, String[] name, int[] argv) throws HDFException;

    /**
     *  @param id <b>IN</b>: id of a file, SDS, or dimension
     *  @param index <b>IN</b>: index of the attribute
     *  @param data <b>OUT</b>: byte[], data
     *
     *  @exception ncsa.hdf.hdflib.HDFException
     *             should be thrown for errors in the
     *             HDF library call, but is not yet implemented.
     *
     *
     *  @return the the data in the form of a continous array of
     *  bytes.
     *
     *  <p><b>NOTE:</b> to read into a Java array use the alternative
     *  routine below.
     */
    public static native boolean  SDreadattr( int id, int index, byte[] data) throws HDFException;

    /**
     *  @param id <b>IN</b>: id of a file, SDS, or dimension
     *  @param index <b>IN</b>: index of the attribute
     *  @param theData <b>OUT</b>: Object, a Java array of appropriate
     *  type, dimensions, and size.
     *
     *  @exception ncsa.hdf.hdflib.HDFException
     *             should be thrown for errors in the
     *             HDF library call, but is not yet implemented.
     *
     *
     *  @return the the data in the Java array.
     *
     *  <p><b>Note:</b> reads the data as bytes and converts to
     *  the Java array.
     */
    public static boolean  SDreadattr( int id, int index, Object theData) throws HDFException {
        byte[] data;
        boolean rval;
        Class theClass = theData.getClass();
        String name = theClass.getName();
        if (name.equals("java.lang.String")) {
        data = ((String)theData).getBytes();
            rval = SDreadattr( id, index,  data);
        theData = new String(data);
        } else {
        HDFArray theArray = new HDFArray(theData);
        data = theArray.emptyBytes();
            rval = SDreadattr( id, index,  data);
        theData = theArray.arrayify( data );
        }
        return rval;
    }

    public static native int  SDfindattr( int id,  String name) throws HDFException;

    public static native boolean  SDiscoordvar( int sdsid) throws HDFException;

    /**
     *  @param sdsid <b>IN</b>: id of the SDS as returned by SDselect
     *  @param argv <b>OUT</b>: double[4], calibration information:
     *          calibration factor
     *          calibration error
     *          offset
     *          offset error
     *  @param NT <b>OUT</b>: int[1],  number type of uncalibrated data
     *
     *  @exception ncsa.hdf.hdflib.HDFException
     *             should be thrown for errors in the
     *             HDF library call, but is not yet implemented.
     *
     *
     *  @return argv[0] = cal, argv[1] = cal_err,
     *  argv[2] = offset, argv[3] = offset_err,
     *  NT[0] = data_type
     */
    public static native boolean  SDgetcal( int sdsid, double[] argv, int[] NT) throws HDFException;

    /**
     *  @param sdsid <b>IN</b>: id of the SDS as returned by SDselect
     *  @param strings <b>OUT</b>: String[4], data information strings:
     *            label
     *            unit
     *            print format
     *            coordinate system
     *  @param len <b>IN</b>: int,  max len of string (not needed by
     *  Java -- the HDFLibrary interface will handle this)
     *
     *  @exception ncsa.hdf.hdflib.HDFException
     *             should be thrown for errors in the
     *             HDF library call, but is not yet implemented.
     *
     *
     *  @return strings[0] = label, strings[1] = unit,
     *  strings[2] = format, strings[3] = coordsys,
     */
    public static native boolean  SDgetdatastrs( int sdsid, String []strings,
    int len) throws HDFException;

    /**
     *  @param dimid <b>IN</b>: id of the SDS as returned by SDselect
     *  @param args <b>OUT</b>: String[4], data information strings:
     *            label
     *            unit
     *            print format
     *  @param len <b>IN</b>: int,  max len of string (not needed by
     *  Java -- the HDFLibrary interface will handle this)
     *
     *  @exception ncsa.hdf.hdflib.HDFException
     *             should be thrown for errors in the
     *             HDF library call, but is not yet implemented.
     *
     *
     *  @return strings[0] = label, strings[1] = unit,
     *  strings[2] = format
     */
    public static native boolean  SDgetdimstrs( int dimid, String[] args,
                      int len) throws HDFException;

    /**
     *  @param dimid <b>IN</b>: id of a dimension as returned by SDgetdimid
     *  @param data <b>OUT</b>: byte[], data
     *
     *  @exception ncsa.hdf.hdflib.HDFException
     *             should be thrown for errors in the
     *             HDF library call, but is not yet implemented.
     *
     *
     *  @return the the data in the form of a continous array of
     *  bytes.
     *
     *  <p><b>NOTE:</b> to read into a Java array use the alternative
     *  routine below.
     */
    public static native boolean  SDgetdimscale( int dimid, byte[] data) throws HDFException;

    /**
     *  @param dimid <b>IN</b>: id of a dimension as returned by SDgetdimid
     *  @param theData <b>OUT</b>: Object, a Java array of appropriate
     *  type and size.
     *
     *  @exception ncsa.hdf.hdflib.HDFException
     *             should be thrown for errors in the
     *             HDF library call, but is not yet implemented.
     *
     *
     *  @return the the data in the Java array.
     *
     *  <p><b>Note:</b> reads the data as bytes and converts to
     *  the Java array.
     */
    public static boolean  SDgetdimscale( int dimid, Object theData) throws HDFException {
        byte[] data;
        boolean rval;

        HDFArray theArray = new HDFArray(theData);
        data = theArray.emptyBytes();
        rval = SDgetdimscale( dimid, data);
        theData = theArray.arrayify( data );
        return rval;
    }

    /**
     *  @param sdsid <b>IN</b>: id of the SDS as returned by SDselect
     *  @param fillValue <b>OUT</b>: byte[], data
     *
     *  @exception ncsa.hdf.hdflib.HDFException
     *             should be thrown for errors in the
     *             HDF library call, but is not yet implemented.
     *
     *
     *  @return the the fill value in the form of a continous array of
     *  bytes.
     *
     *  <p><b>NOTE:</b> to read into a Java variable use the alternative
     *  routine below.
     */
    public static native boolean  SDgetfillvalue( int sdsid, byte[] fillValue) throws HDFException;

    /**
     *  @param sdsid <b>IN</b>: id of the SDS as returned by SDselect
     *  @param theFillValue <b>OUT</b>: Object[1], one object of
     *  appropriate type
     *
     *  @exception ncsa.hdf.hdflib.HDFException
     *             should be thrown for errors in the
     *             HDF library call, but is not yet implemented.
     *
     *
     *  @return the the data in the Java array: theFillValue[0] = fillValue
     *
     *  <p><b>Note:</b> the routine calls SDgetinfo to determine the
     *  correct type, reads the data as bytes, and converts to the
     *  appropriate Java object.
     */
    public static boolean  SDgetfillvalue( int sdsid, Object [] theFillValue) throws HDFException {
    int [] SDInfo = new int[3];
    int NT;
        String datasetname = new String(" ");
    String ss[] = new String[1];
    ss[0] = datasetname;
        int  dimsize[]     = new int[16];
        SDgetinfo(sdsid, ss, dimsize, SDInfo );
        datasetname = ss[0];
        byte[] d1 = new byte[8];
        boolean rval;
        rval = SDgetfillvalue( sdsid, d1 );
        if (rval == false) {
            return(rval);
        }
        NT = SDInfo[1];
        if ((NT & HDFConstants.DFNT_LITEND) != 0) {
            NT -= HDFConstants.DFNT_LITEND;
        }
        if ((NT == HDFConstants.DFNT_INT8 )
         || (NT == HDFConstants.DFNT_CHAR8 )
         || (NT == HDFConstants.DFNT_CHAR )
            ) {
            theFillValue[0] = new Byte(d1[0]);
        } else if ((NT == HDFConstants.DFNT_UINT8 )
                 || (NT == HDFConstants.DFNT_UCHAR8 )
                 || (NT == HDFConstants.DFNT_UCHAR8 )
                        ) {
                        Byte f = new Byte(d1[0]);
                        if (f.shortValue() < 0) {
                                theFillValue[0] = new Short((short)(f.intValue() + 256));
                        } else {
                               theFillValue[0] = new Short(f.shortValue());
                        }
        } else if ((NT == HDFConstants.DFNT_INT16 )
         || (NT == HDFConstants.DFNT_CHAR16 )
            ) {
            short [] fx = HDFNativeData.byteToShort(0,1,d1);
            theFillValue[0] = new Short(fx[0]);
         } else if ( (NT == HDFConstants.DFNT_UINT16 )
         || (NT == HDFConstants.DFNT_UCHAR16 )
            ) {
            short[] fmx = HDFNativeData.byteToShort(0,1,d1);
            Short f = new Short(fmx[0]);
            if (f.intValue() < 0) {
                theFillValue[0] = new Integer(f.intValue() + 65536);
            } else {
                theFillValue[0] = new Integer(f.intValue());
            }
        } else if ((NT == HDFConstants.DFNT_INT32 )
            ) {
            int [] fx = HDFNativeData.byteToInt(0,1,d1);
            theFillValue[0] = new Integer(fx[0]);
        } else if ((NT == HDFConstants.DFNT_UINT32 )
            ) {
            int[] fmx = HDFNativeData.byteToInt(0,1,d1);
            Integer i = new Integer(fmx[0]);
            if (i.floatValue() < 0) {
                theFillValue[0] = new Float((float)(i.floatValue() + 4294967296.0));
            } else {
                theFillValue[0] = new Float(i.floatValue());
            }
        } else if (NT == HDFConstants.DFNT_FLOAT32 ) {
            float [] fx = HDFNativeData.byteToFloat(0,1,d1);
            theFillValue[0] = new Float(fx[0]);
        } else if (NT == HDFConstants.DFNT_FLOAT64 ) {
            double [] fx = HDFNativeData.byteToDouble(0,1,d1);
            theFillValue[0] = new Double(fx[0]);
        } else {
            System.out.println("Error: SDgetfillvalue not converting, type "+NT);
        }
        return rval;
    }

    /**
     *  @param sdsid <b>IN</b>: id of the SDS as returned by SDselect
     *  @param max <b>OUT</b>: byte[], max value, as bytes
     *  @param min <b>OUT</b>: byte[], min value, as bytes
     *
     *  @exception ncsa.hdf.hdflib.HDFException
     *             should be thrown for errors in the
     *             HDF library call, but is not yet implemented.
     *
     *
     *  @return the the max and min values in the form of a continous array of
     *  bytes.
     *
     *  <p><b>NOTE:</b> to read into Java doubles, use the alternative
     *  routine below.
     */
    public static native boolean  SDgetrange( int sdsid, byte[] max, byte[] min) throws HDFException;

    /**
     *  @param sdsid <b>IN</b>: id of the SDS as returned by SDselect
     *  @param maxmin <b>OUT</b>: double[2], the max and min values
     *  converted to doubles
     *
     *  @exception ncsa.hdf.hdflib.HDFException
     *             should be thrown for errors in the
     *             HDF library call, but is not yet implemented.
     *
     *
     *  @return the the data in the Java array: maxmin[0] = max,
     *  maxmin[1] = min
     *
     *  <p><b>Note:</b> the routine calls SDgetinfo to determine the
     *  correct type, reads the data as bytes, and converts to the
     *  double.
     */
    public static boolean  SDgetrange( int sdsid, double maxmin[]) throws HDFException {
    int [] SDInfo = new int[3];
    int NT;
        String datasetname = new String(" ");
    String ss[] = new String[1];
    ss[0] = datasetname;
        int  dimsize[]     = new int[16];
        SDgetinfo(sdsid, ss, dimsize, SDInfo);
        datasetname = ss[0];
        byte[] max = new byte[8];
        byte[] min = new byte[8];
        boolean rval;
        rval = SDgetrange( sdsid, max, min);
        if (rval == false) {
            return(rval);
        }
        NT = SDInfo[1];
        if ((NT & HDFConstants.DFNT_LITEND) != 0) {
            NT -= HDFConstants.DFNT_LITEND;
        }
        if ((NT == HDFConstants.DFNT_INT8 )
         || (NT == HDFConstants.DFNT_CHAR8 )
         || (NT == HDFConstants.DFNT_CHAR )
            ) {
            Byte f = new Byte(max[0]);
            maxmin[0] = (f.doubleValue());
            f = new Byte(min[0]);
            maxmin[1] = (f.doubleValue());
        } else if ((NT == HDFConstants.DFNT_UINT8 )
         || (NT == HDFConstants.DFNT_UCHAR8 )
         || (NT == HDFConstants.DFNT_UCHAR8 )
            ) {
            Byte f = new Byte(max[0]);
            Short fmx;
            if (f.shortValue() < 0) {
                fmx = new Short((short)(f.intValue() + 256));
            } else {
                fmx = new Short(f.shortValue());
            }
            maxmin[0] = (fmx.doubleValue());
            f = new Byte(min[0]);
            fmx = new Short(f.shortValue());
            maxmin[1] = (fmx.doubleValue());
        } else if ((NT == HDFConstants.DFNT_INT16 )
         || (NT == HDFConstants.DFNT_CHAR16 )
            ) {
            short [] fmx = HDFNativeData.byteToShort(0,1,max);
            short [] fmn = HDFNativeData.byteToShort(0,1,min);
            Short f = new Short(fmx[0]);
            maxmin[0] = (f.doubleValue());
            f = new Short(fmn[0]);
            maxmin[1] = (f.doubleValue());
        } else if ((NT == HDFConstants.DFNT_UINT16 )
         || (NT == HDFConstants.DFNT_UINT16 )
            ) {
            short[] fmx = HDFNativeData.byteToShort(0,1,max);
            Short f = new Short(fmx[0]);
            Integer i;
            if (f.intValue() < 0) {
                i = new Integer(f.intValue() + 65536);
            } else {
                i = new Integer(f.intValue());
            }
            maxmin[0] = (i.doubleValue());
            fmx = HDFNativeData.byteToShort(0,1,min);
            f = new Short(fmx[0]);
            if (f.intValue() < 0) {
                i = new Integer(f.intValue() + 65536);
            } else {
                i = new Integer(f.intValue());
            }
            maxmin[1] = (i.doubleValue());
        } else if ((NT == HDFConstants.DFNT_INT32 ) ) {
            int [] fmx = HDFNativeData.byteToInt(0,1,max);
            int [] fmn = HDFNativeData.byteToInt(0,1,min);
            Integer f = new Integer(fmx[0]);
            maxmin[0] = (f.doubleValue());
            f = new Integer(fmn[0]);
            maxmin[1] = (f.doubleValue());
        } else if ( (NT == HDFConstants.DFNT_UINT32 )) {
            int[] fmx = HDFNativeData.byteToInt(0,1,max);
            Integer i = new Integer(fmx[0]);
            Float f;
            if (i.floatValue() < 0) {
                f = new Float((float)(i.floatValue() + 4294967296.0));
            } else {
                f = new Float(i.floatValue());
            }
            maxmin[0] = (f.doubleValue());
            fmx = HDFNativeData.byteToInt(0,1,max);
            i = new Integer(fmx[0]);
            if (i.floatValue() < 0) {
                f = new Float((float)(i.floatValue() + 4294967296.0));
            } else {
                f = new Float(i.floatValue());
            }
            maxmin[1] = (f.doubleValue());
        } else if (NT == HDFConstants.DFNT_FLOAT32 ) {
            float [] fmx = HDFNativeData.byteToFloat(0,1,max);
            float [] fmn = HDFNativeData.byteToFloat(0,1,min);
            Float f = new Float(fmx[0]);
            maxmin[0] = (f.doubleValue());
            f = new Float(fmn[0]);
            maxmin[1] = (f.doubleValue());
        } else if (NT == HDFConstants.DFNT_FLOAT64 ) {
            double [] fmx = HDFNativeData.byteToDouble(0,1,max);
            double [] fmn = HDFNativeData.byteToDouble(0,1,min);
            Double f = new Double(fmx[0]);
            maxmin[0] = (f.doubleValue());
            f = new Double(fmn[0]);
            maxmin[1] = (f.doubleValue());

        } else {
            System.out.println("Error: SDgetrange not converting, type "+NT);
        }
            return rval;
    }

    public static native int SDcreate(int sd_id, String name, int number_type, int rank, int[] dimsizes) throws HDFException;

    public static native boolean SDisrecord(int sdsid) throws HDFException;

    public static native boolean SDsetattr(int s_id, String attr_name, int num_type, int count,
    byte[] values) throws HDFException;

    public static boolean SDsetattr(int s_id, String attr_name, int num_type, int count,
    Object theValues) throws HDFException {
        byte[] data;
        HDFArray theArray = new HDFArray(theValues);
        data = theArray.byteify();
            return SDsetattr(s_id, attr_name, num_type, count, data);
    }

   public static native boolean SDsetcal(int sds_id, double cal, double cal_err,
    double offset, double offset_err, int number_type) throws HDFException;

   public static native boolean SDsetdatastrs(int sds_id, String label, String unit, String format,
        String coordsys) throws HDFException;

   public static native boolean SDsetdimname(int dim_id, String dim_name) throws HDFException;

    /**
     *  @param dim_id <b>IN</b>: id of a dimension
     *  @param count <b>IN</b>: number of values
     *  @param number_type <b>IN</b>: number type of the values
     *  @param data <b>IN</b>: byte[], the values, in an array of
     *  bytes.
     *
     *  @exception ncsa.hdf.hdflib.HDFException
     *             should be thrown for errors in the
     *             HDF library call, but is not yet implemented.
     *
     *
     *  <p><b>Note:</b> the calling program must assure that the
     *  data is correctly formatted for C.  To write an array
     *  of Java objects, use the alternative routine below.
     */
   public static native boolean SDsetdimscale(int dim_id, int count, int number_type, byte[] data) throws HDFException;

    /**
     *  @param dim_id <b>IN</b>: id of a dimension
     *  @param count <b>IN</b>: number of values
     *  @param number_type <b>IN</b>: number type of the values
     *  @param theData <b>OUT</b>: Object, a Java array of appropriate
     *  type and size.
     *
     *  @exception ncsa.hdf.hdflib.HDFException
     *             should be thrown for errors in the
     *             HDF library call, but is not yet implemented.
     *
     *
     *  <p><b>Note:</b> converts the Java array to an array of
     *  bytes, and writes the bytes.
     */
   public static boolean SDsetdimscale(int dim_id, int count, int number_type, Object theData) throws HDFException
    {
        byte[] data;
        HDFArray theArray = new HDFArray(theData);
        data = theArray.byteify();
            return  SDsetdimscale(dim_id, count, number_type, data) ;
    }

   public static native boolean SDsetdimstrs(int dim_id, String label, String unit, String format) throws HDFException;

   public static native boolean SDsetexternalfile(int sds_id, String filename, int offset) throws HDFException;

    /**
     *  @param sds_id <b>IN</b>: id of a dataset
     *  @param fill_val <b>IN</b>: byte[], the fill values in an array of
     *  bytes.
     *
     *  @exception ncsa.hdf.hdflib.HDFException
     *             should be thrown for errors in the
     *             HDF library call, but is not yet implemented.
     *
     *
     *  <p><b>Note:</b> the calling program must assure that the
     *  data is correctly formatted for C.  To set the fill value
     *  with a Java object, use the alternative routine below.
     */
    public static native boolean SDsetfillvalue(int sds_id, byte[] fill_val)  throws HDFException;

    /**
     *  @param sds_id <b>IN</b>: id of a dataset
     *  @param the_fill_val <b>IN</b>: Object, a Java object of appropriate
     *  type
     *
     *  @exception ncsa.hdf.hdflib.HDFException
     *             should be thrown for errors in the
     *             HDF library call, but is not yet implemented.
     *
     *
     *  <p><b>Note:</b> converts the Java array to an array of
     *  bytes, and writes the bytes.
     */
    public static boolean SDsetfillvalue(int sds_id, Object the_fill_val) throws HDFException {
        byte[] data;
        HDFArray theArray = new HDFArray(the_fill_val);
        data = theArray.byteify();
        return  SDsetfillvalue(sds_id, data) ;
    }

    /**
     *  @param sdsid <b>IN</b>: id of a dataset
     *  @param max <b>IN</b>: byte[], the max value in an array of bytes
     *  @param min <b>IN</b>: byte[], the min value in an array of bytes
     *
     *  @exception ncsa.hdf.hdflib.HDFException
     *             should be thrown for errors in the
     *             HDF library call, but is not yet implemented.
     *
     *
     *  <p><b>Note:</b> the calling program must assure that the
     *  data is correctly formatted for C.  To set the max and min value
     *  with Java objects, use the alternative routine below.
     */
    public static native boolean  SDsetrange( int sdsid, byte[] max, byte[] min) throws HDFException;

    /**
     *  @param sdsid <b>IN</b>: id of a dataset
     *  @param max <b>IN</b>: Object, a Java object of appropriate type
     *  @param min <b>IN</b>: Object, a Java object of appropriate type
     *
     *  @exception ncsa.hdf.hdflib.HDFException
     *             should be thrown for errors in the
     *             HDF library call, but is not yet implemented.
     *
     *
     *  <p><b>Note:</b> converts the Java array to an array of
     *  bytes, and writes the bytes.
     */
    public static boolean  SDsetrange( int sdsid, Object max, Object min) throws HDFException {
        byte[] d1;
        byte[] d2;
        HDFArray theArray1 = new HDFArray(max);
        d1 = theArray1.byteify();
        HDFArray theArray2 = new HDFArray(min);
        d2 = theArray2.byteify();
        return  SDgetrange( sdsid, d1, d2);
    }


    /**
     *  @param sdsid <b>IN</b>: the SD interface id, returned by SDselect
     *  @param start <b>IN</b>: int[], start
     *  @param stride <b>IN</b>: int[], stride
     *  @param count <b>IN</b>: int[], count
     *  @param data <b>IN</b>: byte[], data in an array of bytes
     *
     *  @exception ncsa.hdf.hdflib.HDFException
     *             should be thrown for errors in the
     *             HDF library call, but is not yet implemented.
     *
     *
     *  <p><b>NOTE:</b> to write from a Java array use the alternative
     *  routine below.
     */
    public static native boolean SDwritedata(  int sdsid, int[] start, int[] stride,
                       int[] count, byte[] data) throws HDFException;

    /**
     *  @param sdsid <b>IN</b>: the SD interface id, returned by SDselect
     *  @param start <b>IN</b>: int[], start
     *  @param stride <b>IN</b>: int[], stride
     *  @param count <b>IN</b>: int[], count
     *  @param theData <b>IN</b>: Object, a Java array of appropriate
     *  type, dimensions, and size.
     *
     *  @exception ncsa.hdf.hdflib.HDFException
     *             should be thrown for errors in the
     *             HDF library call, but is not yet implemented.
     *
     *
     *  <p><b>Note:</b> converts to the Java array to a contiguous
     *  array of bytes and then writes to the file.
     */
    public static boolean SDwritedata(  int sdsid, int[] start, int[] stride,
                       int[] count, Object theData ) throws HDFException {
        byte[] data;

        HDFArray theArray = new HDFArray(theData);
        data = theArray.byteify();
            return SDwritedata( sdsid, start, stride, count, data);
    }

    public static native boolean SDsetnbitdataset(int id, int start_bit, int bit_len,
    int sign_ext, int fill_one) throws HDFException;

    /**
     *  @param id <b>IN</b>: the SD identifier returned by SDselect
     *  @param type <b>IN</b>: the type of compression
     *  @param cinfo <b>IN</b>: HDFCompInfo, the compression info
     *
     *  @exception ncsa.hdf.hdflib.HDFException
     *             should be thrown for errors in the
     *             HDF library call, but is not yet implemented.
     *
     *
     *  <p><b>NOTE:</b>The compression algorithm specific information is
     *  passed in an appropriate sub-class of HDFCompInfo.
     */
    public static native boolean SDsetcompress( int id, int type, HDFCompInfo cinfo) throws HDFException;

    public static native boolean SDgetcompress( int id, HDFCompInfo cinfo) throws HDFException;

    public static native boolean SDsetaccesstype( int id, int accesstype ) throws HDFException;

    public static native boolean SDsetblocksize( int sdsid, int block_size ) throws HDFException;

    /**
     *  @param sdsid  <b>IN</b>: the SD id
     *  @param fill_enable  <b>IN</b>:  boolean, true calls library with
     *  SD_FILL, false calls library with SD_NOFILL
     *
     *  @exception ncsa.hdf.hdflib.HDFException
     *             should be thrown for errors in the
     *             HDF library call, but is not yet implemented.
     *
     */
    public static boolean SDsetfillmode( int sdsid, boolean fill_enable ) throws HDFException
    {
        int fm;

        if (fill_enable) {
            fm = HDFConstants.SD_FILL;
        } else {
            fm = HDFConstants.SD_NOFILL;
        }
        return SDsetfillmode( sdsid, fm );
    }

    public static native boolean SDsetfillmode( int sdsid, int fillmode ) throws HDFException;

    public static native boolean SDsetdimval_comp( int dimid, int comp_mode ) throws HDFException;

    public static native boolean SDisdimval_bwcomp( int dimid )  throws HDFException;

    /**
     *  @param sdsid <b>IN</b>: the SD identifier returned by SDselect
     *  @param chunk_def <b>IN</b>: HDFChunkInfo, the chunking info
     *  @param flags <b>IN</b>: the type of chunking
     *
     *  @exception ncsa.hdf.hdflib.HDFException
     *             should be thrown for errors in the
     *             HDF library call, but is not yet implemented.
     *
     *
     *  <p><b>NOTE:</b>The chunking algorithm-specific information is
     *  passed in an appropriate sub-class of HDFChunkInfo.
     */
    public static native boolean SDsetchunk( int sdsid, HDFChunkInfo chunk_def,
        int flags) throws HDFException;

    /**
     *  @param sdsid <b>IN</b>: the SD identifier returned by SDselect
     *  @param chunk_def <b>OUT</b>: HDFChunkInfo, the chunking info
     *  @param clflags <b>OUT</b>: int[1], the type of chunking
     *
     *  @exception ncsa.hdf.hdflib.HDFException
     *             should be thrown for errors in the
     *             HDF library call, but is not yet implemented.
     *
     *
     *  @return c_info contains information about the chunking method,
     *  clflags[0] == the chunking flags
     *
     *  <p><b>NOTE:</b>The chunking algorithm-specific information is
     *  passed in an appropriate sub-class of HDFChunkInfo.
     */
    public static native boolean SDgetchunkinfo( int sdsid, HDFChunkInfo chunk_def,
        int[] clflags) throws HDFException;

    /**
     *  @param sdsid <b>IN</b>: the SD interface id, returned by SDselect
     *  @param origin <b>IN</b>: int[], origin
     *  @param theData <b>OUT</b>: byte[], the data in an array of bytes
     *
     *  @exception ncsa.hdf.hdflib.HDFException
     *             should be thrown for errors in the
     *             HDF library call, but is not yet implemented.
     *
     *
     *  <p><b>NOTE:</b> to read into a Java array use the alternative
     *  routine below.
     */
    public static native boolean SDreadchunk( int sdsid, int[] origin, byte[] theData) throws HDFException;

    /**
     *  @param sdsid <b>IN</b>: the SD interface id, returned by SDselect
     *  @param origin <b>IN</b>: int[], origin
     *  @param theData <b>IN</b>: Object, a Java array of appropriate
     *  type, dimensions, and size.
     *
     *  @exception ncsa.hdf.hdflib.HDFException
     *             should be thrown for errors in the
     *             HDF library call, but is not yet implemented.
     *
     *
     *  <p><b>Note:</b> reads the data as a contiguous
     *  array of bytes and then converts it to an appropriate Java object.
     */
    public static boolean SDreadchunk( int sdsid, int[] origin, Object theData) throws HDFException {
        byte[] data;
        boolean rval;

        HDFArray theArray = new HDFArray(theData);
        data = theArray.emptyBytes();
            rval = SDreadchunk( sdsid, origin,  data);
        theData = theArray.arrayify( data );
        return rval;
    }

    public static native int SDsetchunkcache( int sdsid, int maxcache, int flags) throws HDFException;

    /**
     *  @param sdsid <b>IN</b>: the SD interface id, returned by SDselect
     *  @param origin <b>IN</b>: int[], origin
     *  @param data <b>IN</b>: byte[], data to be written, in an array of bytes
     *
     *  @exception ncsa.hdf.hdflib.HDFException
     *             should be thrown for errors in the
     *             HDF library call, but is not yet implemented.
     *
     *
     *  <p><b>NOTE:</b> to write from a Java array use the alternative
     *  routine below.
     */
    public static native boolean SDwritechunk( int sdsid, int[] origin, byte[] data) throws HDFException;

    /**
     *  @param sdsid <b>IN</b>: the SD interface id, returned by SDselect
     *  @param origin <b>IN</b>: int[], origin
     *  @param theData <b>IN</b>: Object, a Java array of appropriate
     *  type, dimensions, and size.
     *
     *  @exception ncsa.hdf.hdflib.HDFException
     *             should be thrown for errors in the
     *             HDF library call, but is not yet implemented.
     *
     *
     *  <p><b>Note:</b> converts to the Java array to a contiguous
     *  array of bytes and then writes to the file.
     */
    public static boolean SDwritechunk( int sdsid, int[] origin, Object theData) throws HDFException {
        byte[] data;

        HDFArray theArray = new HDFArray(theData);
        data = theArray.byteify();
            return SDwritechunk( sdsid, origin, data);
    }

    public static native int VFfieldesize(int vdata_id,  int field_index) throws HDFException;

    public static native int VFfieldisize(int vdata_id,  int field_index) throws HDFException;

    public static native String VFfieldname(int vdata_id,  int field_index) throws HDFException;

    public static native int VFfieldorder(int vdata_id,  int field_index) throws HDFException;

    public static native int VFfieldtype(int vdata_id,  int field_index) throws HDFException;

    public static native int VFnfields(int vkey) throws HDFException;

    public static native int VHmakegroup(int file_id, int[] tag_array,
        int[] ref_array, int n_objects, String vgroup_name,
        String vgroup_class) throws HDFException;

    /**
     *  @param file_id <b>IN</b>: the SD interface id, returned by SDselect
     *  @param fieldname <b>IN</b>: String, the name of the field to be filled
     *  @param buf <b>IN</b>: byte[], data to be written, in an array of bytes
     *  @param n_records <b>IN</b>: int, the number of records being written
     *  @param data_type <b>IN</b>: int, the number type of the data
     *  @param vdata_name <b>IN</b>: String, the name of the Vdata
     *  @param vdata_class <b>IN</b>: String, the class of the Vdata
     *
     *  @exception ncsa.hdf.hdflib.HDFException
     *             should be thrown for errors in the
     *             HDF library call, but is not yet implemented.
     *
     *
     *  <p><b>NOTE:</b> to write from a Java array use the alternative
     *  routine below.
     */
    public static native int VHstoredata(int file_id, String fieldname,
        byte[] buf, int n_records, int data_type, String vdata_name,
        String vdata_class) throws HDFException;
    /**
     *  @param file_id <b>IN</b>: the SD interface id, returned by SDselect
     *  @param fieldname <b>IN</b>: String, the name of the field to be filled
     *  @param thebuf <b>IN</b>: Object, data to be written, in a Java array
     *  of appropriate type and size
     *  @param n_records <b>IN</b>: int, the number of records being written
     *  @param data_type <b>IN</b>: int, the number type of the data
     *  @param vdata_name <b>IN</b>: String, the name of the Vdata
     *  @param vdata_class <b>IN</b>: String, the class of the Vdata
     *
     *  @exception ncsa.hdf.hdflib.HDFException
     *             should be thrown for errors in the
     *             HDF library call, but is not yet implemented.
     *
     *
     *  <p><b>Note:</b> converts to the Java array to a contiguous
     *  array of bytes and then writes to the file.
     */
    public static int VHstoredata(int file_id, String fieldname,
        Object thebuf, int n_records, int data_type, String vdata_name,
        String vdata_class) throws HDFException
        {
        byte[] data;

        HDFArray theArray = new HDFArray(thebuf);
        data = theArray.byteify();
            return VHstoredata(file_id, fieldname, data, n_records,
            data_type, vdata_name, vdata_class);
        }

    /**
     *  @param file_id <b>IN</b>: the SD interface id, returned by SDselect
     *  @param fieldname <b>IN</b>: String, the name of the field to be filled
     *  @param buf <b>IN</b>: byte[], data to be written, in an array of bytes
     *  @param n_records <b>IN</b>: int, the number of records being written
     *  @param data_type <b>IN</b>: int, the number type of the data
     *  @param vdata_name <b>IN</b>: String, the name of the Vdata
     *  @param vdata_class <b>IN</b>: String, the class of the Vdata
     *  @param order <b>IN</b>: int, the number of components per field
     *
     *  @exception ncsa.hdf.hdflib.HDFException
     *             should be thrown for errors in the
     *             HDF library call, but is not yet implemented.
     *
     *
     *  <p><b>NOTE:</b> to write from a Java array use the alternative
     *  routine below.
     */
    public static native int VHstoredatam(int file_id, String fieldname, byte[] buf,
        int n_records, int data_type, String vdata_name, String vdata_class,
        int order) throws HDFException;

    /**
     *  @param file_id <b>IN</b>: the SD interface id, returned by SDselect
     *  @param fieldname <b>IN</b>: String, the name of the field to be filled
     *  @param buf <b>IN</b>: Object, data to be written, in a Java array
     *  of appropriate type, dimension, and size
     *  @param n_records <b>IN</b>: int, the number of records being written
     *  @param data_type <b>IN</b>: int, the number type of the data
     *  @param vdata_name <b>IN</b>: String, the name of the Vdata
     *  @param vdata_class <b>IN</b>: String, the class of the Vdata
     *  @param order <b>IN</b>: int, the number of components per field
     *
     *  @exception ncsa.hdf.hdflib.HDFException
     *             should be thrown for errors in the
     *             HDF library call, but is not yet implemented.
     *
     *
     *  <p><b>Note:</b> converts to the Java array to a contiguous
     *  array of bytes and then writes to the file.
     */
    public static int VHstoredatam(int file_id, String fieldname, Object buf,
        int n_records, int data_type, String vdata_name,
        String vdata_class, int order) throws HDFException
        {
        byte[] data;

        HDFArray theArray = new HDFArray(buf);
        data = theArray.byteify();
            return VHstoredatam(file_id, fieldname, data, n_records,
            data_type, vdata_name, vdata_class, order);
        }

    public static native int VQueryref(int vkey) throws HDFException;
    public static native int VQuerytag(int vkey) throws HDFException;

   /**
    *  @param vdata_id <b>IN</b>, vdata id  as returned by VSattach
    *  @param n_records <b>OUT</b>, int[1], the number of records in the vdata
     *
     *  @exception ncsa.hdf.hdflib.HDFException
     *             should be thrown for errors in the
     *             HDF library call, but is not yet implemented.
     *
    *
    *  @return  n_records[0] == the number of records
    */
    public static native boolean VSQuerycount(int vdata_id, int[] n_records) throws HDFException;

   /**
    *  @param vdata_id <b>IN</b>, vdata id  as returned by VSattach
    *  @param fields <b>OUT</b>, String[1], the names of the fields
     *
     *  @exception ncsa.hdf.hdflib.HDFException
     *             should be thrown for errors in the
     *             HDF library call, but is not yet implemented.
     *
    *
    *  @return  fields[0] == a comma delimited string with the names
    *  of the fields.
    */
    public static native boolean VSQueryfields(int vdata_id, String[] fields) throws HDFException;

   /**
    *  @param vdata_id <b>IN</b>, vdata id  as returned by VSattach
    *  @param interlace <b>OUT</b>, int[1], the interlace mode,
     *
     *  @exception ncsa.hdf.hdflib.HDFException
     *             should be thrown for errors in the
     *             HDF library call, but is not yet implemented.
     *
    *
    *  @return  interlace[0] == the number of records,
    *  HDFConstants.FULL_INTERLACE or HDFConstants.NO_INTERLACE
    */
    public static native boolean VSQueryinterlace(int vdata_id, int[] interlace) throws HDFException;

   /**
    *  @param vdata_id <b>IN</b>, vdata id  as returned by VSattach
    *  @param vdata_name <b>OUT</b>, String[1], the name of the vdata
     *
     *  @exception ncsa.hdf.hdflib.HDFException
     *             should be thrown for errors in the
     *             HDF library call, but is not yet implemented.
     *
    *
    *  @return  vdata_name[0] == the name
    */
    public static native boolean VSQueryname(int vdata_id, String[] vdata_name) throws HDFException;

    public static native int VSQueryref(int vdata_id) throws HDFException;

    public static native int VSQuerytag(int vdata_id) throws HDFException;

   /**
    *  @param vdata_id <b>IN</b>, vdata id  as returned by VSattach
    *  @param vdata_size <b>OUT</b>, int[1], the size of the vdata
     *
     *  @exception ncsa.hdf.hdflib.HDFException
     *             should be thrown for errors in the
     *             HDF library call, but is not yet implemented.
     *
    *
    *  @return  vdata_size[0] == Native size, in bytes, of a record in the vdata
    */
    public static native boolean VSQueryvsize(int vdata_id, int[] vdata_size) throws HDFException;

    public static  native int VSattach(int fid, int vdata_ref, String access) throws HDFException;

    public static  native void VSdetach(int vdata_id) throws HDFException;

    public  static native  int VSgetid(int file_id, int vdata_ref) throws HDFException;

   /**
    *  @param vdata_id <b>IN</b>, vdata id  as returned by VSattach
    *  @param hdfclassname <b>OUT</b>, String[1], the class name of the vdata
    *
    *  @exception ncsa.hdf.hdflib.HDFException
    *             should be thrown for errors in the
    *             HDF library call, but is not yet implemented.
    */
    public  static native  void VSgetclass(int vdata_id, String[] hdfclassname) throws HDFException;

   /**
    *  @param vdata_id <b>IN</b>, vdata id  as returned by VSattach
    *  @param hdfname <b>OUT</b>, String[1], the name of the vdata
    *
    *  @exception ncsa.hdf.hdflib.HDFException
    *             should be thrown for errors in the
    *             HDF library call, but is not yet implemented.
    */
    public  static native  void VSgetname(int vdata_id, String[] hdfname) throws HDFException;

    public static native int VSelts(int vdata_id) throws HDFException;

    public static native boolean VSfdefine(int vdata_id, String fieldname,
                    int numbertype, int order) throws HDFException;

    public static native boolean VSfexist(int vdata_id, String fields) throws HDFException;

    public static native int VSfind(int file_id, String vdataname) throws HDFException;

    public static native int VSsetblocksize(int vdata_id, int blocksize) throws HDFException;

    public static native int VSsetnumblocks(int vdata_id, int numblocks) throws HDFException;

   /**
    *  @param vdata_id <b>IN</b>, vdata id  as returned by VSattach
    *  @param fieldname <b>OUT</b>, String[1], the names of the fields
     *
     *  @exception ncsa.hdf.hdflib.HDFException
     *             should be thrown for errors in the
     *             HDF library call, but is not yet implemented.
     *
    *
    *  @return  fieldname[0] == a comma delimited string with the names
    *  of the fields.
    */
    public static native int VSgetfields(int vdata_id, String[] fieldname) throws HDFException;

    public static native int VSgetinterlace(int vdata_id) throws HDFException;

   /**
    *  @param vdata_id <b>IN</b>, vdata id  as returned by VSattach
    *  @param iargs <b>OUT</b>, int[3], n_records, interlace, vdata_size
    *  @param sargs <b>OUT</b>, String[2], names the dataset, fields
     *
     *  @exception ncsa.hdf.hdflib.HDFException
     *             should be thrown for errors in the
     *             HDF library call, but is not yet implemented.
     *
    *
    *  @return iargs[0] = n_records, iargs[1] = interlace, iargs[2] = vdata_size,
    *  sargs[0] = vdata_name, sargs[1] = comma delimited list of fields
    *
    *  <p><b>NOTE:</b> the parameters for the Java interface are not in
    *  the same order as the C interface.
    */
    public static native boolean VSinquire(int vdata_id, int[] iargs, String[] sargs) throws HDFException;

   /**
    *  @param vdata_id <b>IN</b>, vdata id  as returned by VSattach
    *  @param iargs <b>OUT</b>, int[2], block_size, num_blocks
     *
     *  @exception ncsa.hdf.hdflib.HDFException
     *             should be thrown for errors in the
     *             HDF library call, but is not yet implemented.
     *
    *
    *  @return iargs[0] = blocksize, iargs[1] = num_blocks
    *
    *  <p><b>NOTE:</b> the parameters for the Java interface are not in
    *  the same order as the C interface.
    */
    public static native boolean VSinquire(int vdata_id, int[] iargs ) throws HDFException;

   /**
    *  @param fid <b>IN</b>, File identifier returned by Hopen
    *  @param ref_array <b>OUT</b>, int[?], the refs
    *  @param buffersize <b>IN</b>, int, the max number of refs to
    *  return.
     *
     *  @exception ncsa.hdf.hdflib.HDFException
     *             should be thrown for errors in the
     *             HDF library call, but is not yet implemented.
     *
    *
    *  @return ref_array contains refs.  The Java API ignores the buffersize
    *  argument, returns as many as possible.
    */
    public  static native  int VSlone(int fid, int[] ref_array, int buffersize) throws HDFException;

    /**
     *  @param vdata_id <b>IN</b>: the Vdata id
     *  @param databuf <b>OUT</b>: byte[], the data in an array of bytes
     *  @param nrecord <b>IN</b>: int, number of records
     *  @param interlace <b>IN</b>: int, interlace
     *
     *  @exception ncsa.hdf.hdflib.HDFException
     *             should be thrown for errors in the
     *             HDF library call, but is not yet implemented.
     *
     *
     *  <p><b>NOTE:</b> to read into a Java array use the alternative
     *  routine below.
     */
    public  static native int VSread(int vdata_id, byte[] databuf, int nrecord, int interlace) throws HDFException;

    /**
     *  @param vdata_id <b>IN</b>: the Vdata id
     *  @param theData <b>OUT</b>: Object, a Java array of appropriate
     *  type, dimensions, and size.
     *  @param nrecord <b>IN</b>: int, number of records
     *  @param interlace <b>IN</b>: int, interlace
     *
     *  @exception ncsa.hdf.hdflib.HDFException
     *             should be thrown for errors in the
     *             HDF library call, but is not yet implemented.
     *
     *
     *  <p><b>Note:</b> reads the data as a contiguous
     *  array of bytes and then converts it to an appropriate Java object.
     */
    public  static int VSread(int vdata_id, Object theData, int nrecord, int interlace) throws HDFException
    {
        byte[] data;
        int rval;

        HDFArray theArray = new HDFArray(theData);
        data = theArray.emptyBytes();
                rval = VSread(vdata_id, data, nrecord, interlace);
        theData = theArray.arrayify( data );
        return rval;
    }

    public static native int VSseek(int vdata_id, int record) throws HDFException;

    public static native boolean VSsetfields(int vdata_id, String fields) throws HDFException;

    public static native boolean  VSsetinterlace(int vdata_id, int interlace) throws HDFException;

    public static native int VSsizeof(int vdata_id, String fields) throws HDFException;

    public static native boolean VSappendable(int vkey, int block_size) throws HDFException;

    public static native int VSfindclass(int file_id, String vgclass) throws HDFException;

    public static native int VSgetversion(int vkey) throws HDFException;

    public static native void VSsetclass(int vdata_id, String vdata_class) throws HDFException;

    public static native boolean VSsetexternalfile(int vkey, String filename, int offset) throws HDFException;

    public static native void VSsetname(int vdata_id, String vdata_name) throws HDFException;

    /**
     *  @param vdata_id <b>IN</b>: the Vdata id
     *  @param databuf <b>IN</b>: byte[], the data in an array of bytes
     *  @param n_records <b>IN</b>: int, number of records
     *  @param interlace <b>IN</b>: int, interlace
     *
     *  @exception ncsa.hdf.hdflib.HDFException
     *             should be thrown for errors in the
     *             HDF library call, but is not yet implemented.
     *
     *
     *  <p><b>NOTE:</b> to write a Java array use the alternative
     *  routine below.
     */
    public static native int VSwrite(int vdata_id, byte [] databuf, int n_records, int interlace) throws HDFException;

    /**
     *  @param vdata_id <b>IN</b>: the Vdata id
     *  @param databuf <b>IN</b>: Object, a Java array of appropriate
     *  type, dimensions, and size.
     *  @param n_records <b>IN</b>: int, number of records
     *  @param interlace <b>IN</b>: int, interlace
     *
     *  @exception ncsa.hdf.hdflib.HDFException
     *             should be thrown for errors in the
     *             HDF library call, but is not yet implemented.
     *
     *
     *  <p><b>Important Note:</b> This interface only supports
     *  records which are all of the same numeric type, with
     *  no character fields.  Heterogeneous fields can be
     *  written as bytes (see above), but the calling program
     *  must make sure the data is in proper order to
     *  write to the HDF library.
     *
     *  <p><b>Note:</b> converts the data into a contiguous
     *  array of bytes and then writes it
     */
    public static int VSwrite(int vdata_id, Object databuf, int n_records, int interlace) throws HDFException
    {
        byte[] data;

        HDFArray theArray = new HDFArray(databuf);
        data = theArray.byteify();
            return VSwrite( vdata_id, data, n_records, interlace);
    }

    public static native boolean Vstart(int fid)  throws HDFException;

    public  static native int Vattach(int fid, int vgroup_ref, String access) throws HDFException;

    public  static native void Vdetach(int vgroup_id) throws HDFException;

    public  static native  void Vend(int file_id) throws HDFException;

    public  static native  int Vgetid(int file_id, int vgroup_ref) throws HDFException;

    /**
     *  @param vgroup_id <b>IN</b>: the Vgroup id
     *  @param hdfclassname <b>OUT</b>: String[1], the HDF class of
     *  the vgroup.
     *
     *  @exception ncsa.hdf.hdflib.HDFException
     *             should be thrown for errors in the
     *             HDF library call, but is not yet implemented.
     */
    public  static native  void Vgetclass(int vgroup_id, String[] hdfclassname) throws HDFException;

    /**
     *  @param vgroup_id <b>IN</b>: the Vgroup id
     *  @param hdfname <b>OUT</b>: String[1], the name of
     *  the vgroup.
     *
     *  @exception ncsa.hdf.hdflib.HDFException
     *             should be thrown for errors in the
     *             HDF library call, but is not yet implemented.
     */
    public  static native  void Vgetname(int vgroup_id, String[] hdfname) throws HDFException;

    public  static native  boolean Visvg(int vgroup_id, int vgroup_ref) throws HDFException;

    public  static native  boolean Visvs(int vgroup_id, int vdata_ref) throws HDFException;

    /**
     *  @param vgroup_id <b>IN</b>: the Vgroup id
     *  @param tags <b>OUT</b>: int[arraysize], the tags
     *  @param refs <b>OUT</b>: int[arraysize], the refs
     *  @param arraysize <b>IN</b>: int, the number of tags/refs to
     *  return
     *
     *  @exception ncsa.hdf.hdflib.HDFException
     *             should be thrown for errors in the
     *             HDF library call, but is not yet implemented.
     *
     *
     *  @return  tags[] = tags for objects 0 - n, refs[] = refs for
     *  objects 0 - n
     */
    public  static native  int Vgettagrefs(int vgroup_id, int[] tags, int[] refs,
                    int arraysize) throws HDFException;

    /**
     *  @param vgroup_id - IN: the Vgroup id
     *  @param index - IN: the index of the object
     *  @param tagref - OUT: tagref[0]=tag, tagref[1]=ref
     *
     *  @exception ncsa.hdf.hdflib.HDFException
     *             should be thrown for errors in the
     *             HDF library call, but is not yet implemented.
     *
     *
     *  @return  tags[0] = tag for object #index, refs[0] = ref for
     *  objects #index
     */
    public  static native  boolean Vgettagref(int vgroup_id, int index, int[] tagref) throws HDFException;

    public  static native  int Vntagrefs(int vgroup_id) throws HDFException;

    public  static native  boolean  Vinqtagref(int vgroup_id, int tag, int ref) throws HDFException;

    /**
     *  @param fid <b>IN</b>: the file identifier returned by Hopen
     *  @param ref_array <b>OUT</b>: int[], the refs for Vdata not part
     *  of Vgroups
     *  @param buffersize <b>IN</b>: the max size of the ref_array
     *
     *  @exception ncsa.hdf.hdflib.HDFException
     *             should be thrown for errors in the
     *             HDF library call, but is not yet implemented.
     *
     *
     *  @return  ref_array[] = refs 0 - ...
     */
    public  static native  int Vlone(int fid, int[] ref_array, int buffersize) throws HDFException;

    public static native int Vaddtagref(int vgroup_id, int tag, int ref) throws HDFException;

    public static native int Vdeletetagref(int vgroup_id, int tag, int ref) throws HDFException;

    public static native int Vfind(int file_id, String vgroup_name) throws HDFException;

    public static native int Vfindclass(int file_id, String vgclassname) throws HDFException;

    public static native int Vflocate(int key, String vgclassname) throws HDFException;

    public static native int Vgetnext(int key, int ref) throws HDFException;

    /**
     *  @param vgroup_id <b>IN</b>: the Vgroup id
     *  @param n_entries <b>OUT</b>: int[1], the number of objects in the Vgroup
     *  @param vgroup_name <b>OUT</b>: String[1], the name of the Vgroup
     *
     *  @exception ncsa.hdf.hdflib.HDFException
     *             should be thrown for errors in the
     *             HDF library call, but is not yet implemented.
     *
     *
     *  @return n_entries[0] = number of entries, vgroup_name[0] = the name
     */
    public static native boolean Vinquire(int vgroup_id, int[] n_entries, String[] vgroup_name) throws HDFException;

    public static native int Vinsert(int vgroup_id, int v_id) throws HDFException;

    public static native int Vnrefs(int key, int ref) throws HDFException;

    public static native boolean Vsetclass(int vgroup_id, String vgclassname) throws HDFException;

    public static native boolean Vsetname(int vgroup_id, String vgname) throws HDFException;

    /**
     *  @param id <b>IN</b>: Vgroup identifier returned by Vattach
     *  @param index <b>IN</b>: the index of the attribute
     *  @param name <b>OUT</b>: String[1], the name of the attribute
     *  @param argv <b>OUT</b>: int[3],
     *         Data type of the target attribute,
     *         Number of values in the target attribute,
     *         Size, in bytes, of the values of the target attribute,
     *
     *  @exception ncsa.hdf.hdflib.HDFException
     *             should be thrown for errors in the
     *             HDF library call, but is not yet implemented.
     *
     *
     *  @return name[0] = name, argv[0] = data_type, argv[1] = count,
     *  argv[2] = size
     */
    public static native boolean  Vattrinfo( int id, int index, String[] name, int[] argv) throws HDFException;

    public static native int  Vfindattr(int id,  String name) throws HDFException;

    /**
     *  @param id <b>IN</b>: the Vdata id
     *  @param index <b>IN</b>: the index of the attribute
     *  @param data <b>OUT</b>: byte[], the data in an array of bytes
     *
     *  @exception ncsa.hdf.hdflib.HDFException
     *             should be thrown for errors in the
     *             HDF library call, but is not yet implemented.
     *
     *
     *  <p><b>NOTE:</b> to read into a Java array use the alternative
     *  routine below.
     *
     *  @return data = the value of the attribute, in an array of bytes
     */
    public static native boolean Vgetattr(int id, int index, byte[] data) throws HDFException;

    /**
     *  @param id <b>IN</b>: the Vdata id
     *  @param index <b>IN</b>: the index of the attribute
     *  @param theData <b>OUT</b>: Object, a Java array of appropriate
     *  type, dimensions, and size.
     *
     *  @exception ncsa.hdf.hdflib.HDFException
     *             should be thrown for errors in the
     *             HDF library call, but is not yet implemented.
     *
     *
     *  <p><b>Note:</b> reads the data as a contiguous
     *  array of bytes and then converts it to an appropriate Java object.
     *
     *  @return data = the value of the attribute, in an array of Java
     *  objects
     */
    public static boolean  Vgetattr(int id, int index, Object theData)
    throws HDFException {
        byte[] data;
        boolean rval;

        HDFArray theArray = new HDFArray(theData);
        data = theArray.emptyBytes();
            rval = Vgetattr( id, index,  data);
        theData = theArray.arrayify( data );
        return rval;
    }

   public static native int Vgetversion(int id) throws HDFException;

   public static native int Vnattrs(int id) throws HDFException;

   public static native boolean Vsetattr(int id, String attr_name,
        int data_type, int count, String values) throws HDFException;

    /**
     *  @param id <b>IN</b>: the Vdata id
     *  @param attr_name <b>IN</b>: String, the name of the attribute
     *  @param data_type <b>IN</b>: int, the number_type of the attribute
     *  @param count <b>IN</b>: the number of values
     *  @param data <b>IN</b>: byte[], the data in an array of bytes
     *
     *  @exception ncsa.hdf.hdflib.HDFException
     *             should be thrown for errors in the
     *             HDF library call, but is not yet implemented.
     *
     *
     *  <p><b>NOTE:</b> to write into a Java array use the alternative
     *  routine below.
     */
   public static native boolean Vsetattr(int id, String attr_name,
        int data_type, int count, byte[] data) throws HDFException;

    /**
     *  @param id <b>IN</b>: the Vdata id
     *  @param attr_name <b>IN</b>: String, the name of the attribute
     *  @param data_type <b>IN</b>: int, the number_type of the attribute
     *  @param count <b>IN</b>: the number of values
     *  @param theData <b>IN</b>: Object, a Java array of appropriate
     *  type, dimensions, and size.
     *
     *  @exception ncsa.hdf.hdflib.HDFException
     *             should be thrown for errors in the
     *             HDF library call, but is not yet implemented.
     *
     *
     *  <p><b>Note:</b> converts the data to a contiguous
     *  array of bytes and then converts writes it.
     */
   public static boolean Vsetattr(int id, String attr_name,
        int data_type, int count, Object theData) throws HDFException {
        byte[] data;
        HDFArray theArray = new HDFArray(theData);
        data = theArray.byteify();
        return Vsetattr(id, attr_name, data_type, count, data);
    }

    /**
     *  @param id <b>IN</b>: the Vdata id
     *  @param index <b>IN</b>: int, the index of the attribute
     *  @param attr_index <b>IN</b>: int, the index of the attribute
     *  @param name <b>OUT</b>: String[1], the name of the attribute
     *  @param argv <b>OUT</b>: int[3],
     *         Data type of the target attribute,
     *         Number of values in the target attribute,
     *         Size, in bytes, of the values of the target attribute,
     *
     *  @exception ncsa.hdf.hdflib.HDFException
     *             should be thrown for errors in the
     *             HDF library call, but is not yet implemented.
     *
     *
     *  @return name[0] = name, argv[0] = data_type, argv[1] = count,
     *  argv[2] = size
     */
    public static native boolean VSattrinfo( int id, int index, int attr_index, String[] name, int[] argv) throws HDFException;

    /**
     *  @param id <b>IN</b>: the Vdata id
     *  @param name <b>IN</b>: the name of the attribute
     *  @param findex <b>IN</b>: int[1], the index of the attribute
     *
     *  @exception ncsa.hdf.hdflib.HDFException
     *             should be thrown for errors in the
     *             HDF library call, but is not yet implemented.
     *
     *
     *  @return findex[0] = the index
     */
    public static native int  VSfindex( int id,  String name, int[] findex) throws HDFException;

    public static native int  VSfindattr( int id, int index, String name) throws HDFException;

    public static native int  VSfnattrs( int id, int fnattrs) throws HDFException;

    /**
     *  @param id <b>IN</b>: the Vdata id
     *  @param index <b>IN</b>: the index of the vdata
     *  @param attr_index <b>IN</b>: the index of the attribute
     *  @param data <b>OUT</b>: byte[], the data in an array of bytes
     *
     *  @exception ncsa.hdf.hdflib.HDFException
     *             should be thrown for errors in the
     *             HDF library call, but is not yet implemented.
     *
     *
     *  <p><b>NOTE:</b> to read into a Java array use the alternative
     *  routine below.
     *
     *  @return data = the value of the attribute, in an array of bytes
     */
    public static native boolean  VSgetattr( int id, int index, int attr_index, byte[] data) throws HDFException;

    /**
     *  @param id <b>IN</b>: the Vdata id
     *  @param index <b>IN</b>: the index of the vdata
     *  @param attr_index <b>IN</b>: the index of the attribute
     *  @param theData <b>OUT</b>: Object, a Java array of appropriate
     *  type, dimensions, and size.
     *
     *  @exception ncsa.hdf.hdflib.HDFException
     *             should be thrown for errors in the
     *             HDF library call, but is not yet implemented.
     *
     *
     *  <p><b>Note:</b> reads the data as a contiguous
     *  array of bytes and then converts it to an appropriate Java object.
     *
     *  @return data = the value of the attribute, in an array of Java
     *  objects
     */
    public static boolean  VSgetattr( int id, int index, int attr_index, Object theData)
    throws HDFException {
        byte[] data;
        boolean rval;

        HDFArray theArray = new HDFArray(theData);
        data = theArray.emptyBytes();
            rval = VSgetattr( id, index, attr_index,  data);
        theData = theArray.arrayify( data );
        return rval;
    }
    public static native boolean  VSisattr( int id ) throws HDFException;

    public static native int  VSnattrs( int id ) throws HDFException;

    /**
     *  @param id <b>IN</b>: the Vdata id
     *  @param index <b>IN</b>: the index of the vdata
     *  @param attr_name <b>IN</b>: String, the name of the attribute
     *  @param data_type <b>IN</b>: int, the number_type of the attribute
     *  @param count <b>IN</b>: the number of values
     *  @param values <b>IN</b>: Strin, the data in an String
     *
     *  @exception ncsa.hdf.hdflib.HDFException
     *             should be thrown for errors in the
     *             HDF library call, but is not yet implemented.
     *
     *
     *  <p><b>NOTE:</b> to write from a Java array use the alternative
     *  routine below.
     */
    public static native boolean VSsetattr(int id, int index, String attr_name,
        int data_type, int count, String values) throws HDFException;

    /**
     *  @param id <b>IN</b>: the Vdata id
     *  @param index <b>IN</b>: the index of the vdata
     *  @param attr_name <b>IN</b>: String, the name of the attribute
     *  @param data_type <b>IN</b>: int, the number_type of the attribute
     *  @param count <b>IN</b>: the number of values
     *  @param values <b>IN</b>: byte[], the data in an array of bytes
     *
     *  @exception ncsa.hdf.hdflib.HDFException
     *             should be thrown for errors in the
     *             HDF library call, but is not yet implemented.
     *
     *
     *  <p><b>NOTE:</b> to write into a Java array use the alternative
     *  routine below.
     */
    public static native boolean VSsetattr(int id, int index, String attr_name,
        int data_type, int count, byte[] values) throws HDFException;

    /**
     *  @param id <b>IN</b>: the Vdata id
     *  @param index <b>IN</b>: the index of the vdata
     *  @param attr_name <b>IN</b>: String, the name of the attribute
     *  @param data_type <b>IN</b>: int, the number_type of the attribute
     *  @param count <b>IN</b>: the number of values
     *  @param theData <b>IN</b>: Object, a Java array of appropriate
     *  type, dimensions, and size.
     *
     *  @exception ncsa.hdf.hdflib.HDFException
     *             should be thrown for errors in the
     *             HDF library call, but is not yet implemented.
     *
     *
     *  <p><b>Note:</b> converts the data to a contiguous
     *  array of bytes and then converts writes it.
     */
    public static boolean VSsetattr(int id, int index, String attr_name,
        int data_type, int count, Object theData) throws HDFException {
        byte[] data;
        HDFArray theArray = new HDFArray(theData);
        data = theArray.byteify();
        return VSsetattr(id, index, attr_name, data_type, count, data);
    }

    /*
     *  @param filename <b>IN</b>: String, the file
     *  @param argv <b>OUT</b>: int[3], the width, height, and interlace mode
     *
     *  @exception ncsa.hdf.hdflib.HDFException
     *             should be thrown for errors in the
     *             HDF library call, but is not yet implemented.
     *
     *
     *  @return argv[0] = width, argv[1] = height, argv[2] = interlace
     */
    public static native boolean DF24getdims(String fileName, int[] argv) throws HDFException;

    public static native boolean  DF24reqil(int il)  throws HDFException;

    /**
     *  @param fileName <b>IN</b>: String, the file
     *  @param imagedata <b>OUT</b>: byte[], the image, in an array of
     *  bytes
     *  @param width <b>IN</b>: int, the width of the image
     *  @param height <b>IN</b>: int, the height of the image
     *
     *  @exception ncsa.hdf.hdflib.HDFException
     *             should be thrown for errors in the
     *             HDF library call, but is not yet implemented.
     *
     *
     *  <p><b>NOTE:</b> to read into a Java array use the alternative
     *  routine below.
     *
     *  @return data = the image in an array of bytes
     */
    public static native boolean DF24getimage(String fileName, byte[] imagedata,
            int width, int height) throws HDFException;

    /**
     *  @param fileName <b>IN</b>: String, the file
     *  @param theImagedata <b>OUT</b>: Object, the image, in a java
     *  array of appropriate size and type
     *  @param width <b>IN</b>: int, the width of the image
     *  @param height <b>IN</b>: int, the height of the image
     *
     *  @exception ncsa.hdf.hdflib.HDFException
     *             should be thrown for errors in the
     *             HDF library call, but is not yet implemented.
     *
     *
     *  <p><b>Note:</b> reads the data as a contiguous
     *  array of bytes and then converts it to an appropriate Java object.
     *
     *  @return data = the value of the attribute, in an array of Java
     *  objects
     */
     public static boolean DF24getimage(String fileName, Object theImagedata,int width,
                int height) throws HDFException
    {
        byte[] data;
        boolean rval;

        HDFArray theArray = new HDFArray(theImagedata);
        data = theArray.emptyBytes();
            rval = DF24getimage(fileName, data, width, height);
        theImagedata = theArray.arrayify( data );
        return rval;
    }

     public  static native short DF24lastref() throws HDFException;

     public  static native boolean DF24restart() throws HDFException;

     public static native boolean DF24readref(String filename, int ref) throws HDFException;

     public static native int DF24nimages(String fileName) throws HDFException;

    /**
     *  @param filename <b>IN</b>: String, the file
     *  @param image <b>IN</b>: byte[], the image, in an array of
     *  bytes
     *  @param width <b>IN</b>: int, the width of the image
     *  @param height <b>IN</b>: int, the height of the image
     *
     *  @exception ncsa.hdf.hdflib.HDFException
     *             should be thrown for errors in the
     *             HDF library call, but is not yet implemented.
     *
     *
     *  <p><b>NOTE:</b> to write from a Java array use the alternative
     *  routine below.
     */
     public static native boolean DF24addimage(String filename, byte[] image,
        int width, int height) throws HDFException;

    /**
     *  @param filename <b>IN</b>: String, the file
     *  @param theImage <b>IN</b>: Object, the image, in a java
     *  array of appropriate size and type
     *  @param width <b>IN</b>: int, the width of the image
     *  @param height <b>IN</b>: int, the height of the image
     *
     *  @exception ncsa.hdf.hdflib.HDFException
     *             should be thrown for errors in the
     *             HDF library call, but is not yet implemented.
     *
     *
     *  <p><b>Note:</b> converts the data into a contiguous
     *  array of bytes and then writes it to the file
     */
     public static boolean DF24addimage(String filename, Object theImage, int width,
        int height) throws HDFException {
                 byte[] data;
                 HDFArray theArray = new HDFArray(theImage);
                 data = theArray.byteify();
                 return DF24addimage(filename, data, width, height);
         }

    /**
     *  @param filename <b>IN</b>: String, the file
     *  @param image <b>IN</b>: byte[], the image, in an array of
     *  bytes
     *  @param width <b>IN</b>: int, the width of the image
     *  @param height <b>IN</b>: int, the height of the image
     *
     *  @exception ncsa.hdf.hdflib.HDFException
     *             should be thrown for errors in the
     *             HDF library call, but is not yet implemented.
     *
     *
     *  <p><b>NOTE:</b> to write from a Java array use the alternative
     *  routine below.
     */
     public static native boolean DF24putimage(String filename, byte[] image,
        int width, int height) throws HDFException;

    /**
     *  @param filename <b>IN</b>: String, the file
     *  @param theImage <b>IN</b>: Object, the image, in a java
     *  array of appropriate size and type
     *  @param width <b>IN</b>: int, the width of the image
     *  @param height <b>IN</b>: int, the height of the image
     *
     *  @exception ncsa.hdf.hdflib.HDFException
     *             should be thrown for errors in the
     *             HDF library call, but is not yet implemented.
     *
     *
     *  <p><b>Note:</b> converts the data into a contiguous
     *  array of bytes and then writes it to the file
     *
     */
     public static boolean DF24putimage(String filename, Object theImage, int width,
         int height) throws HDFException {
                 byte[] data;
                 HDFArray theArray = new HDFArray(theImage);
                 data = theArray.byteify();
                 return DF24putimage(filename, data, width, height);
         }

    /**
     *  @param type <b>IN</b>: int, the type of compression
     *  @param cinfo <b>IN</b>: HDFCompInfo, the compression parameters
     *
     *  @exception ncsa.hdf.hdflib.HDFException
     *             should be thrown for errors in the
     *             HDF library call, but is not yet implemented.
     *
     */
     public static native boolean DF24setcompress(int type, HDFCompInfo cinfo) throws HDFException;

     public static native boolean DF24setdims(int width, int height) throws HDFException;

     public static native boolean DF24setil(int il) throws HDFException;

    /*
     *  @param filename <b>IN</b>: String, the file
     *  @param argv <b>OUT</b>: int[2], the width and height
     *  @param haspalette <b>OUT</b>: boolean[1], has a palette
     *
     *  @exception ncsa.hdf.hdflib.HDFException
     *             should be thrown for errors in the
     *             HDF library call, but is not yet implemented.
     *
     *
     *  @return argv[0] = width, argv[1] = height, haspalette[0] = palette
     */
     public static native boolean DFR8getdims(String fileName, int[] argv, boolean[] haspalette) throws HDFException;

    /**
     *  @param fileName <b>IN</b>: String, the file
     *  @param imagedata <b>OUT</b>: byte[], the image, in an array of
     *  bytes
     *  @param width <b>IN</b>: int, the width of the image
     *  @param height <b>IN</b>: int, the height of the image
     *  @param palette <b>OUT</b>: byte[], the color look up table
     *
     *  @exception ncsa.hdf.hdflib.HDFException
     *             should be thrown for errors in the
     *             HDF library call, but is not yet implemented.
     *
     *
     *  <p><b>NOTE:</b> to read into a Java array use the alternative
     *  routine below.
     *
     *  @return data = imagedata: the image in an array of bytes,
     *  palette:  the look up table, in an array of bytes
     */
    public static native boolean DFR8getimage(String fileName, byte[] imagedata,
            int width, int height, byte[] palette) throws HDFException;

    /**
     *  @param fileName <b>IN</b>: String, the file
     *  @param theImagedata <b>OUT</b>: Object, the image, in a java
     *  array of appropriate size and type
     *  @param width <b>IN</b>: int, the width of the image
     *  @param height <b>IN</b>: int, the height of the image
     *  @param palette <b>OUT</b>: byte[], the color look up table
     *
     *  @exception ncsa.hdf.hdflib.HDFException
     *             should be thrown for errors in the
     *             HDF library call, but is not yet implemented.
     *
     *
     *  <p><b>Note:</b> reads the data as a contiguous
     *  array of bytes and then converts it to an appropriate Java object.
     *
     *  @return data = theImagedata: the value of the attribute,
     *  in an array of Java objects
     *  palette:  the look up table, in an array of bytes
     */
     public static boolean DFR8getimage(String fileName, Object theImagedata,int width,
        int height, byte[] palette) throws HDFException
    {
        byte[] data;
        boolean rval;

        HDFArray theArray = new HDFArray(theImagedata);
        data = theArray.emptyBytes();
            rval = DFR8getimage(fileName, data, width, height, palette);
        theImagedata = theArray.arrayify( data );
        return rval;
    }

     public  static native short DFR8lastref() throws HDFException;

     public  static native boolean DFR8restart() throws HDFException;

     public static native boolean DFR8readref(String filename, int ref) throws HDFException;

     public static native int DFR8nimages(String fileName) throws HDFException;

    /**
     *  @param filename <b>IN</b>: String, the file
     *  @param image <b>IN</b>: byte[], the image, in an array of
     *  bytes
     *  @param width <b>IN</b>: int, the width of the image
     *  @param height <b>IN</b>: int, the height of the image
     *  @param compress <b>IN</b>: short, the type of compression
     *
     *  @exception ncsa.hdf.hdflib.HDFException
     *             should be thrown for errors in the
     *             HDF library call, but is not yet implemented.
     *
     *
     *  <p><b>NOTE:</b> to write from a Java array use the alternative
     *  routine below.
     */
     public static native boolean DFR8addimage(String filename, byte[] image,
        int width, int height, short compress) throws HDFException;

    /**
     *  @param filename <b>IN</b>: String, the file
     *  @param theImage <b>IN</b>: Object, the image, in a java
     *  array of appropriate size and type
     *  @param width <b>IN</b>: int, the width of the image
     *  @param height <b>IN</b>: int, the height of the image
     *  @param compress <b>IN</b>: short, the type of compression
     *
     *  @exception ncsa.hdf.hdflib.HDFException
     *             should be thrown for errors in the
     *             HDF library call, but is not yet implemented.
     *
     *
     *  <p><b>Note:</b> converts the data into a contiguous
     *  array of bytes and then writes it to the file
     */
     public static boolean DFR8addimage(String filename, Object theImage, int width, int height,
        short compress) throws HDFException {
                 byte[] data;
                 HDFArray theArray = new HDFArray(theImage);
                 data = theArray.byteify();
                 return DFR8addimage(filename, data, width, height, compress);
         }

    /**
     *  @param filename <b>IN</b>: String, the file
     *  @param image <b>IN</b>: byte[], the image, in an array of
     *  bytes
     *  @param width <b>IN</b>: int, the width of the image
     *  @param height <b>IN</b>: int, the height of the image
     *  @param compress <b>IN</b>: short, the type of compression
     *
     *  @exception ncsa.hdf.hdflib.HDFException
     *             should be thrown for errors in the
     *             HDF library call, but is not yet implemented.
     *
     *
     *  <p><b>NOTE:</b> to write from a Java array use the alternative
     *  routine below.
     */
     public static native boolean DFR8putimage(String filename, byte[] image,
        int width, int height, short compress) throws HDFException;

    /**
     *  @param filename <b>IN</b>: String, the file
     *  @param theImage <b>IN</b>: Object, the image, in a java
     *  array of appropriate size and type
     *  @param width <b>IN</b>: int, the width of the image
     *  @param height <b>IN</b>: int, the height of the image
     *  @param compress <b>IN</b>: short, the type of compression
     *
     *  @exception ncsa.hdf.hdflib.HDFException
     *             should be thrown for errors in the
     *             HDF library call, but is not yet implemented.
     *
     *
     *  <p><b>Note:</b> converts the data into a contiguous
     *  array of bytes and then writes it to the file
     */
     public static boolean DFR8putimage(String filename, Object theImage, int width, int height,
        short compress) throws HDFException {
                 byte[] data;
                 HDFArray theArray = new HDFArray(theImage);
                 data = theArray.byteify();
                 return DFR8putimage(filename, data, width, height, compress);
     }

    /**
     *  @param type <b>IN</b>: int, the type of compression
     *  @param cinfo <b>IN</b>: HDFCompInfo, the compression parameters
     *
     *  @exception ncsa.hdf.hdflib.HDFException
     *             should be thrown for errors in the
     *             HDF library call, but is not yet implemented.
     *
     */
     public static native boolean DFR8setcompress(int type, HDFCompInfo cinfo) throws HDFException;

    /**
     *  @param palref <b>OUT</b>: short[1], the HDF ref of the palette
     *
     *  @exception ncsa.hdf.hdflib.HDFException
     *             should be thrown for errors in the
     *             HDF library call, but is not yet implemented.
     *
     *
     *  @return palref[0] = the ref of the palette
     */
     public static native boolean DFR8getpalref(short[] palref) throws HDFException;

     public static native boolean DFR8setpalette(byte[] palette) throws HDFException;

     public static native boolean DFR8writeref(String filename, short ref) throws HDFException;

    ////////////////////////////////////////////////////////////////////
    //                                                                //
    //         New APIs for read data from library                    //
    //  Using SDreaddata(..., Object buf) requires function calls        //
    //  theArray.emptyBytes() and theArray.arrayify( buf), which      //
    //  triples the actual memory needed by the data set.             //
    //  Using the following APIs solves the problem.                  //
    //                                                                //
    ////////////////////////////////////////////////////////////////////

    public static native boolean SDreaddata_short(  int sdsid, int[] start, int[] stride,
                       int[] count, short[] theData ) throws HDFException;

    public static native boolean SDreaddata_int(  int sdsid, int[] start, int[] stride,
                       int[] count, int[] theData ) throws HDFException;

    public static native boolean SDreaddata_long(  int sdsid, int[] start, int[] stride,
                       int[] count, long[] theData ) throws HDFException;

    public static native boolean SDreaddata_float(  int sdsid, int[] start, int[] stride,
                       int[] count, float[] theData ) throws HDFException;

    public static native boolean SDreaddata_double(  int sdsid, int[] start, int[] stride,
                       int[] count, double[] theData ) throws HDFException;

    /**
     *  New API for hdf-42r1
     */
    public static native int HCget_config_info(int coder_type) throws HDFException;

}
