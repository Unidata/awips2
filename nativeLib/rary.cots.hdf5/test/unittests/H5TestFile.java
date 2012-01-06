/**
 * 
 */
package test.unittests;

import java.util.Vector;
import ncsa.hdf.object.*;
import ncsa.hdf.object.h5.*;

/**
 * Creates an HDF5 file for unit tests.
 * 
 * @author xcao
 *
 */
public class H5TestFile {
    public final static String NAME_FILE_H5="TestHDF5.h5";
    public final static String NAME_GROUP = "/g0";
    public final static String NAME_GROUP_ATTR = "/g0_attr";
    public final static String NAME_GROUP_SUB = NAME_GROUP+"/g00";
    public final static String NAME_DATASET_INT = "/dataset_int";
    public final static String NAME_DATASET_FLOAT = "/dataset_float";
    public final static String NAME_DATASET_CHAR = "/dataset_byte";
    public final static String NAME_DATASET_STR = "/dataset_str";
    public final static String NAME_DATASET_ENUM = "/dataset_enum";    
    public final static String NAME_DATASET_IMAGE = "/dataset_image";
    public final static String NAME_DATASET_IMAGE_PALETTE = "/wave_palete";
    public final static String NAME_DATASET_OBJ_REF = "/dataset_obj_ref";
    public final static String NAME_DATASET_COMPOUND = "/dataset_comp";
    public final static String NAME_DATASET_INT_SUB = NAME_GROUP + "/dataset_int";
    public final static String NAME_DATASET_FLOAT_SUB_SUB = NAME_GROUP_SUB+ "/dataset_float";
    public final static String NAME_DATASET_COMPOUND_SUB = NAME_GROUP + "/dataset_comp";
    public final static String NAME_DATATYPE_INT = NAME_GROUP + "/datatype_int";
    public final static String NAME_DATATYPE_UINT = NAME_GROUP + "/datatype_uint";
    public final static String NAME_DATATYPE_FLOAT = NAME_GROUP + "/datatype_float";
    public final static String NAME_DATATYPE_STR = NAME_GROUP + "/datatype_str";
    public final static String NAME_HARD_LINK_TO_IMAGE = "a_link_to_the_image";
    
    public final static String OBJ_NAMES[] = {NAME_GROUP, NAME_GROUP_ATTR, NAME_GROUP_SUB, 
        NAME_DATASET_INT, NAME_DATASET_FLOAT, NAME_DATASET_CHAR, NAME_DATASET_STR, 
        NAME_DATASET_ENUM, NAME_DATASET_IMAGE, NAME_DATASET_COMPOUND, NAME_DATASET_INT_SUB, 
        NAME_DATASET_FLOAT_SUB_SUB, NAME_DATASET_COMPOUND_SUB, NAME_DATATYPE_INT, 
        NAME_DATATYPE_UINT, NAME_DATATYPE_FLOAT, NAME_DATATYPE_STR, NAME_DATASET_OBJ_REF};
    
    // data space information
    public  final static int DATATYPE_SIZE = 4;
    public  final static int RANK = 2;
    public  final static long DIM1 = 50;
    public  final static long DIM2 = 10;
    public  static final long DIM3 = 20;
    public  final static long[] DIMs = {DIM1, DIM2};
    public  final static long[] CHUNKs = {DIM1/2, DIM2/2};
    public  final static int STR_LEN = 20;
    public  final static int DIM_SIZE = (int)(DIM1*DIM2);;
    
    /* testing data */
    public  final static int[] DATA_INT = new int[DIM_SIZE];
    public  final static long[] DATA_LONG = new long[DIM_SIZE];
    public  final static float[] DATA_FLOAT = new float[DIM_SIZE];
    public  final static byte[] DATA_BYTE = new byte[DIM_SIZE];
    public  final static String[] DATA_STR = new String[DIM_SIZE];
    public  final static int[] DATA_ENUM = new int[DIM_SIZE];
    public  final static Vector DATA_COMP = new Vector(3);
    public  final static byte[] DATA_PALETTE = createWavePalette();
    
    // compound names and datatypes
    public final static String[] COMPOUND_MEMBER_NAMES = {"int32", "float32", "string", "uint32"};
    public final static H5Datatype[] COMPOUND_MEMBER_DATATYPES = {
        new H5Datatype(Datatype.CLASS_INTEGER, DATATYPE_SIZE, -1, -1), 
        new H5Datatype(Datatype.CLASS_FLOAT, DATATYPE_SIZE, -1, -1), 
        new H5Datatype(Datatype.CLASS_STRING, STR_LEN, -1, -1),
        new H5Datatype(Datatype.CLASS_INTEGER, DATATYPE_SIZE, -1, Datatype.SIGN_NONE)}; 
    
    // attributes
    public final static Attribute ATTRIBUTE_STR = new Attribute(
            "strAttr", 
            new H5Datatype(Datatype.CLASS_STRING, STR_LEN, -1, -1), 
            new long[] {1}, 
            new String[] {"String attribute."});
    public final static Attribute ATTRIBUTE_INT_ARRAY = new Attribute( 
            "arrayInt", 
            new H5Datatype(Datatype.CLASS_INTEGER, DATATYPE_SIZE, -1, -1), 
            new long[] {10}, 
            new int[] {1, 2, 3, 4, 5, 6, 7, 8, 9, 10});

    
    /**
     * Creates an HDF5 test file. 
     * <p>
     * The test file contains the following objects:
     * <pre>
          /dataset_byte            Dataset {50, 10}
          /dataset_comp            Dataset {50, 10}
          /dataset_enum            Dataset {50, 10}
          /dataset_float           Dataset {50, 10}
          /dataset_int             Dataset {50, 10}
          /dataset_image           Dataset {50, 10}
          /dataset_str             Dataset {50, 10}
          /g0                      Group
          /g0/dataset_int          Dataset {50, 10}
          /g0/g00                  Group
          /g0/g00/dataset_float    Dataset {50, 10}
          /g0_attr                 Group
     * </pre> 
     * @throws Exception
     */
    public static final H5File createTestFile(String fileName)  throws Exception
    {
        H5File file=null;
        Group g0, g1, g00;
        final Dataset[] dsets = new Dataset[11];
        
        if ((fileName == null) || (fileName.length()<1)) {
            fileName = NAME_FILE_H5;
        }
        
        final H5Datatype typeInt = new H5Datatype(Datatype.CLASS_INTEGER, DATATYPE_SIZE, -1, -1);
        final H5Datatype typeByte = new H5Datatype(Datatype.CLASS_INTEGER, 1, -1, Datatype.SIGN_NONE);
        final H5Datatype typeFloat = new H5Datatype(Datatype.CLASS_FLOAT, DATATYPE_SIZE, -1, -1);
        final H5Datatype typeStr = new H5Datatype(Datatype.CLASS_STRING, STR_LEN, -1, -1);
        final H5Datatype typeChar = new H5Datatype(Datatype.CLASS_CHAR, 1, -1, -1);
        final H5Datatype typeEnum = new H5Datatype(Datatype.CLASS_ENUM, DATATYPE_SIZE, -1, -1);
        final H5Datatype typeRef = new H5Datatype(Datatype.CLASS_REFERENCE, -1, -1, -1);
       
        for (int i=0; i<DIM_SIZE; i++) {
            DATA_INT[i] = i;
            DATA_LONG[i] = i;
            DATA_FLOAT[i] = i+i/100.0f;
            DATA_BYTE[i] = (byte)Math.IEEEremainder(i, 127);
            DATA_STR[i] = "str"+i;
            DATA_ENUM[i] = (int)Math.IEEEremainder(i, 2);
        }
        
        DATA_COMP.add(0, DATA_INT);
        DATA_COMP.add(1, DATA_FLOAT);
        DATA_COMP.add(2, DATA_STR);
        DATA_COMP.add(3, DATA_LONG);

        file = new H5File(fileName, FileFormat.CREATE);
        file.open();
       
        g0 = file.createGroup(NAME_GROUP, null);
        g1 = file.createGroup(NAME_GROUP_ATTR, null);
        g00 = file.createGroup(NAME_GROUP_SUB, null);

        g1.writeMetadata(ATTRIBUTE_STR);
        g1.writeMetadata(ATTRIBUTE_INT_ARRAY);

        dsets[0]  = file.createScalarDS  (NAME_DATASET_INT, null, typeInt, DIMs, null, CHUNKs, 9, DATA_INT);
        dsets[1]  = file.createScalarDS  (NAME_DATASET_FLOAT, null, typeFloat, DIMs, null, CHUNKs, 9, DATA_FLOAT);
        dsets[2]  = file.createScalarDS  (NAME_DATASET_CHAR, null, typeChar, DIMs, null, CHUNKs, 9, DATA_BYTE);
        dsets[3]  = file.createScalarDS  (NAME_DATASET_STR, null, typeStr, DIMs, null, CHUNKs, 9, DATA_STR);
        dsets[4]  = file.createScalarDS  (NAME_DATASET_ENUM, null, typeEnum, DIMs, null, CHUNKs, 9, DATA_ENUM);
        dsets[5]  = file.createScalarDS  (NAME_DATASET_INT_SUB, g0, typeInt, DIMs, null, CHUNKs, 9, DATA_INT);
        dsets[6]  = file.createScalarDS  (NAME_DATASET_FLOAT_SUB_SUB, g00, typeFloat, DIMs, null, CHUNKs, 9, DATA_FLOAT);
        dsets[7]  = file.createImage     (NAME_DATASET_IMAGE, null, typeByte, DIMs, null, CHUNKs, 9, 1, -1, DATA_BYTE);
        dsets[8]  = file.createCompoundDS(NAME_DATASET_COMPOUND, null, DIMs, null, CHUNKs, 9, COMPOUND_MEMBER_NAMES, COMPOUND_MEMBER_DATATYPES, null, DATA_COMP);
        dsets[9]  = file.createCompoundDS(NAME_DATASET_COMPOUND_SUB, null, DIMs, null, CHUNKs, 9, COMPOUND_MEMBER_NAMES, COMPOUND_MEMBER_DATATYPES, null, DATA_COMP);
        dsets[10] = file.createScalarDS  (NAME_DATASET_OBJ_REF, null, typeRef, DIMs, null, CHUNKs, 9, null);

        // attach attributes to all datasets
        for (int i=0; i<dsets.length; i++) {
            dsets[i].writeMetadata(ATTRIBUTE_STR);
            dsets[i].writeMetadata(ATTRIBUTE_INT_ARRAY);
        }
        
        // create a wave palette and attach it to the image
        final Dataset pal = file.createScalarDS(NAME_DATASET_IMAGE_PALETTE, null, typeByte, new long[] {256, 3}, null, null, -1, DATA_PALETTE); 
        long[] oid = pal.getOID();
        final Vector attrs = (Vector)dsets[7].getMetadata();
        final int n = attrs.size();
        for (int i=0; i<n; i++) {
            final Attribute attr = (Attribute) attrs.get(i);
            if ("PALETTE".equals(attr.getName())) {
                attr.setValue(oid);
                dsets[7].writeMetadata(attr);
            }
        }
        
        Datatype dtype = file.createDatatype(Datatype.CLASS_INTEGER, DATATYPE_SIZE, -1, -1, NAME_DATATYPE_INT);
        dtype.writeMetadata(ATTRIBUTE_STR);
        dtype.writeMetadata(ATTRIBUTE_INT_ARRAY);

        dtype = file.createDatatype(Datatype.CLASS_INTEGER, DATATYPE_SIZE, -1, Datatype.SIGN_NONE, NAME_DATATYPE_UINT);
        dtype.writeMetadata(ATTRIBUTE_STR);
        dtype.writeMetadata(ATTRIBUTE_INT_ARRAY);
        
        dtype = file.createDatatype(Datatype.CLASS_FLOAT, DATATYPE_SIZE, -1, -1, NAME_DATATYPE_FLOAT);
        dtype.writeMetadata(ATTRIBUTE_STR);
        dtype.writeMetadata(ATTRIBUTE_INT_ARRAY);
        
        dtype = file.createDatatype(Datatype.CLASS_STRING, STR_LEN, -1, -1, NAME_DATATYPE_STR);
        dtype.writeMetadata(ATTRIBUTE_STR);
        dtype.writeMetadata(ATTRIBUTE_INT_ARRAY);
        
        file.createLink(g0, NAME_HARD_LINK_TO_IMAGE, dsets[7]);

        try { file.close(); } catch (final Exception ex) {}
        
        // write object refs to the ref dataset
        file.open();
        final long[] refs = new long[DIM_SIZE];
        for (int i=0; i<OBJ_NAMES.length; i++) {
            oid = file.get(OBJ_NAMES[i]).getOID();
            refs[i] = oid[0];
        }
        dsets[10].write(refs);
        
        try { file.close(); } catch (final Exception ex) {}
        
        return file;
    }
    
    /**
     *  Creates the wave palette of the indexed 256-color table.
     *  <p>
     *  The palette values are stored in a two-dimensional byte array and arrange
     *  by color components of red, green and blue. palette[][] = byte[3][256],
     *  where, palette[0][], palette[1][] and palette[2][] are the red, green and
     *  blue components respectively.
     *  @return the wave palette in the form of byte[3][256]
     */
    private static final byte[] createWavePalette()
    {
        final byte[] p = new byte[768]; //256*3

        for (int i=1; i<255; i++)
        {
            p[3*i] = (byte) ((Math.sin(((double)i/40-3.2))+1)*128);
            p[3*i+1] = (byte) ((1-Math.sin((i/2.55-3.1)))*70+30);
            p[3*i+2] = (byte) ((1-Math.sin(((double)i/40-3.1)))*128);
        }

        p[0] = p[1] = p[2] = 0;
        p[765] = p[766] = p[767] = (byte)255;

        return p;
    }
    
}
