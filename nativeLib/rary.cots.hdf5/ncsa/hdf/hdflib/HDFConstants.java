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

/**
 *  <p>
 *  This interface defines the values of constants defined
 *  by the HDF 4.1 API.
 * <p>
 *  For details of the HDF libraries, see the HDF Documentation at:
 *     <a href="http://hdf.ncsa.uiuc.edu">http://hdf.ncsa.uiuc.edu</a>
 */
public class HDFConstants
{
    /** FAIL */
    public static final int FAIL = -1;

    // file access code definitions
    public static final int     DFACC_READ = 1;
    public static final int     DFACC_WRITE= 2;
    public static final int     DFACC_RDWR = 3;
    public static final int     DFACC_CREATE=4;
    public static final int     DFACC_RDONLY=DFACC_READ;
    public static final int     DFACC_DEFAULT=000;
    public static final int     DFACC_SERIAL=001;
    public static final int     DFACC_PARALLEL=011;

    // annotation type in HDF
    public static final int     AN_DATA_LABEL  = 0;
    public static final int     AN_DATA_DESC   = AN_DATA_LABEL + 1;
    public static final int     AN_FILE_LABEL  = AN_DATA_LABEL + 2;
    public static final int     AN_FILE_DESC   = AN_DATA_LABEL + 3;

    // HDF Tag Definations

    public static final int DFREF_WILDCARD     = 0;
    public static final int DFTAG_WILDCARD     = 0;

    public static final int DFREF_NONE         = 0;  // used by mfhdf/libsrc/putget.c

    // tags and refs
    public static final int DFTAG_NULL         = 1;
    public static final int DFTAG_LINKED       = 20;  // linked-block special element
    public static final int DFTAG_VERSION      = 30;
    public static final int DFTAG_COMPRESSED   = 40;  // compressed special element
    public static final int DFTAG_VLINKED      = 50;  // variable-len linked-block header
    public static final int DFTAG_VLINKED_DATA = 51;  // variable-len linked-block data
    public static final int DFTAG_CHUNKED      = 60;  // chunked special element header
    public static final int DFTAG_CHUNK        = 61;  // chunk element

    // utility set
    public static final int DFTAG_FID   = 100;   // File identifier
    public static final int DFTAG_FD    = 101;   // File description
    public static final int DFTAG_TID   = 102;   // Tag identifier
    public static final int DFTAG_TD    = 103;   // Tag descriptor
    public static final int DFTAG_DIL   = 104;   // data identifier label
    public static final int DFTAG_DIA   = 105;   // data identifier annotation
    public static final int DFTAG_NT    = 106;   // number type
    public static final int DFTAG_MT    = 107;   // machine type
    public static final int DFTAG_FREE  = 108;   // free space in the file

    // raster-8 set
    public static final int DFTAG_ID8   = 200;   // 8-bit Image dimension
    public static final int DFTAG_IP8   = 201;   // 8-bit Image palette
    public static final int DFTAG_RI8   = 202;   // Raster-8 image
    public static final int DFTAG_CI8   = 203;   // RLE compressed 8-bit image
    public static final int DFTAG_II8   = 204;   // IMCOMP compressed 8-bit image

    // Raster Image set
    public static final int DFTAG_ID    = 300;   // Image DimRec
    public static final int DFTAG_LUT   = 301;   // Image Palette
    public static final int DFTAG_RI    = 302;   // Raster Image
    public static final int DFTAG_CI    = 303;   // Compressed Image
    public static final int DFTAG_NRI   = 304;   // New-format Raster Image

    public static final int DFTAG_RIG   = 306;   // Raster Image Group
    public static final int DFTAG_LD    = 307;   // Palette DimRec
    public static final int DFTAG_MD    = 308;   // Matte DimRec
    public static final int DFTAG_MA    = 309;   // Matte Data
    public static final int DFTAG_CCN   = 310;   // color correction
    public static final int DFTAG_CFM   = 311;   // color format
    public static final int DFTAG_AR    = 312;   // aspect ratio

    public static final int DFTAG_DRAW  = 400;   // Draw these images in sequence
    public static final int DFTAG_RUN   = 401;   // run this as a program/script

    public static final int DFTAG_XYP   = 500;   // x-y position
    public static final int DFTAG_MTO   = 501;   // machine-type override

    // Tektronix
    public static final int DFTAG_T14   = 602;   // TEK 4014 data
    public static final int DFTAG_T105  = 603;   // TEK 4105 data

    // Scientific Data set
    // Objects of tag 721 are never actually written to the file.  The tag is
    // needed to make things easier mixing DFSD and SD style objects in the
    // same file

    public static final int DFTAG_SDG   = 700;   // Scientific Data Group
    public static final int DFTAG_SDD   = 701;   // Scientific Data DimRec
    public static final int DFTAG_SD    = 702;   // Scientific Data
    public static final int DFTAG_SDS   = 703;   // Scales
    public static final int DFTAG_SDL   = 704;   // Labels
    public static final int DFTAG_SDU   = 705;   // Units
    public static final int DFTAG_SDF   = 706;   // Formats
    public static final int DFTAG_SDM   = 707;   // Max/Min
    public static final int DFTAG_SDC   = 708;   // Coord sys
    public static final int DFTAG_SDT   = 709;   // Transpose
    public static final int DFTAG_SDLNK = 710;   // Links related to the dataset
    public static final int DFTAG_NDG   = 720;   // Numeric Data Group
    public static final int DFTAG_CAL   = 731;   // Calibration information
    public static final int DFTAG_FV    = 732;   // Fill Value information
    public static final int DFTAG_BREQ  = 799;   // Beginning of required tags
    public static final int DFTAG_SDRAG = 781;   // List of ragged array line lengths
    public static final int DFTAG_EREQ  = 780;   // Current end of the range

    // VSets
    public static final int DFTAG_VG     = 1965;   // Vgroup
    public static final int DFTAG_VH     = 1962;   // Vdata Header
    public static final int DFTAG_VS     = 1963;   // Vdata Storage

    // compression schemes
    public static final int DFTAG_RLE       = 11;   // run length encoding
    public static final int DFTAG_IMC       = 12;   // IMCOMP compression alias
    public static final int DFTAG_IMCOMP    = 12;   // IMCOMP compression
    public static final int DFTAG_JPEG      = 13;   // JPEG compression (24-bit data)
    public static final int DFTAG_GREYJPEG  = 14;   // JPEG compression (8-bit data)
    public static final int DFTAG_JPEG5     = 15;   // JPEG compression (24-bit data)
    public static final int DFTAG_GREYJPEG5 = 16;   // JPEG compression (8-bit data)

    /** pixel interlacing scheme */
    public static final int MFGR_INTERLACE_PIXEL = 0;

    /** line interlacing scheme */
    public static final int MFGR_INTERLACE_LINE = MFGR_INTERLACE_PIXEL +1;

    /** component interlacing scheme */
    public static final int MFGR_INTERLACE_COMPONENT = MFGR_INTERLACE_PIXEL +2;

    /** interlacing supported by the vset.*/
    public static final int FULL_INTERLACE = 0;
    public static final int NO_INTERLACE   = 1;

    /** unsigned char */
    public static final int DFNT_UCHAR8 = 3;
    public static final int DFNT_UCHAR  = 3;

    /** char */
    public static final int DFNT_CHAR8  = 4;
    public static final int DFNT_CHAR   = 4;

    /** No supported by HDF */
    public static final int DFNT_CHAR16 = 42;
    public static final int DFNT_UCHAR16= 43;


    /** float */
    public static final int  DFNT_FLOAT32   =  5;
    public static final int  DFNT_FLOAT     =  5 ;

    //** double */
    public static final int  DFNT_FLOAT64   =  6;
    public static final int  DFNT_FLOAT128  =  7 ;
    public static final int  DFNT_DOUBLE    =  6  ;

    /** 8-bit integer */
    public static final int  DFNT_INT8      =  20;

    /** unsigned 8-bit interger */
    public static final int  DFNT_UINT8    =  21;

    /** short */
    public static final int  DFNT_INT16    =  22;

    /** unsigned interger */
    public static final int  DFNT_UINT16   =  23;

    /** interger */
    public static final int  DFNT_INT32    =  24;

    /** unsigned interger */
    public static final int  DFNT_UINT32   =  25;

    /** No supported */
    public static final int  DFNT_INT64    =  26;
    public static final int  DFNT_UINT64   =  27;
    public static final int  DFNT_INT128   =  28;
    public static final int  DFNT_UINT128  =  30;
    public static final int  DFNT_LITEND =  0x00004000;

    public static final int DF_FORWARD  = 1;
    public static final int  DFS_MAXLEN = 255;

    public static final int COMP_NONE     =  0;
    public static final int COMP_JPEG     =  2;
    public static final int COMP_RLE      =  11;
    public static final int COMP_IMCOMP   =  12;
    public static final int COMP_CODE_NONE     =  0;
    public static final int COMP_CODE_RLE     =  1;
    public static final int COMP_CODE_NBIT     =  2;
    public static final int COMP_CODE_SKPHUFF  =  3;
    public static final int COMP_CODE_DEFLATE  =  4;
    public static final int COMP_CODE_SZIP  =  5;
    public static final int COMP_CODE_INVALID  =  6;
    public static final int COMP_CODE_JPEG  =  7;

    // Interlace schemes
    public static final int DFIL_PIXEL  = 0;  /* Pixel Interlacing */
    public static final int DFIL_LINE   = 1;  /* Scan Line Interlacing */
    public static final int DFIL_PLANE  = 2;  /* Scan Plane Interlacing */

    public static final int SD_UNLIMITED  = 0;
    public static final int SD_FILL  = 0;
    public static final int SD_NOFILL  = 0x100;
    public static final int SD_DIMVAL_BW_COMP  = 1;
    public static final int SD_DIMVAL_BW_INCOMP  = 0;

    public static final int HDF_NONE  = 0x0;
    public static final int HDF_CHUNK  = 0x1;
    public static final int HDF_COMP  = 0x3;
    public static final int HDF_NBIT  = 0x5;
    public static final int MAX_VAR_DIMS =32;

    //the names of the Vgroups created by the GR interface
    public static final String GR_NAME = "RIG0.0";
    public static final String RI_NAME = "RI0.0";
    public static final String RIGATTRNAME = "RIATTR0.0N";
    public static final String RIGATTRCLASS = "RIATTR0.0C";

    // names of classes of the Vdatas/Vgroups created by the SD interface
    public static final String  HDF_ATTRIBUTE = "Attr0.0";
    public static final String  HDF_VARIABLE = "Var0.0";
    public static final String  HDF_DIMENSION = "Dim0.0";
    public static final String  HDF_UDIMENSION = "UDim0.0";
    public static final String  DIM_VALS = "DimVal0.0";
    public static final String  DIM_VALS01 = "DimVal0.1";
    public static final String  HDF_CHK_TBL = "_HDF_CHK_TBL_";
    public static final String  HDF_CDF = "CDF0.0";

    // names of data object types
    public static final String ANNOTATION = "HDF_ANNOTATION";
    public static final String RI8 = "HDF_RI8";
    public static final String RI24 = "HDF_RI24";
    public static final String GR = "HDF_GR";
    public static final String SDS = "HDF_SDS";
    public static final String VDATA = "HDF_VDATA";
    public static final String VGROUP = "HDF_GROUP";

    // data types represented by Strings
    public static final String UCHAR8   = "UCHAR8";
    public static final String CHAR8    = "CHAR8";
    public static final String UCHAR16  = "UCHAR16";
    public static final String CHAR16   = "CHAR16";
    public static final String FLOAT32  = "FLOAT32";
    public static final String FLOAT64  = "FLOAT64";
    public static final String FLOAT128 = "FLOAT128";
    public static final String INT8     = "INT8";
    public static final String UINT8    = "UINT8";
    public static final String INT16    = "INT16";
    public static final String UINT16   = "UINT16";
    public static final String INT32    = "INT32";
    public static final String UINT32   = "UINT32";
    public static final String INT64    = "INT64";
    public static final String UINT64   = "UINT64";
    public static final String INT128   = "INT128";
    public static final String UINT128  = "UINT128";


    /**
     *  convert number type to string type
     *  params type  the number representing the data type
     *  return the string representing the data type
     */
    public static String getType(int type)
    {
        if   (type == HDFConstants.DFNT_UCHAR8) {
            return HDFConstants.UCHAR8;
        } else if (type == HDFConstants.DFNT_CHAR8) {
            return HDFConstants.CHAR8;
        } else if (type == HDFConstants.DFNT_UCHAR16) {
            return HDFConstants.UCHAR16;
        } else if (type == HDFConstants.DFNT_CHAR16) {
            return HDFConstants.CHAR16;
        } else if (type == HDFConstants.DFNT_FLOAT32) {
            return HDFConstants.FLOAT32;
        } else if (type == HDFConstants.DFNT_FLOAT64) {
            return HDFConstants.FLOAT64;
        } else if (type == HDFConstants.DFNT_FLOAT128) {
            return HDFConstants.FLOAT128;
        } else if (type == HDFConstants.DFNT_INT8) {
            return HDFConstants.INT8;
        } else if (type == HDFConstants. DFNT_UINT8) {
            return HDFConstants.UINT8;
        } else if (type == HDFConstants.DFNT_INT16) {
            return HDFConstants.INT16;
        } else if (type == HDFConstants.DFNT_UINT16) {
            return HDFConstants.UINT16;
        } else if (type == HDFConstants.DFNT_INT32) {
            return HDFConstants.INT32;
        } else if (type == HDFConstants.DFNT_UINT32) {
            return HDFConstants.UINT32;
        } else if (type == HDFConstants.DFNT_INT64) {
            return HDFConstants.INT64;
        } else if (type == HDFConstants.DFNT_UINT64) {
            return HDFConstants.UINT64;
        } else if (type == HDFConstants.DFNT_INT128) {
            return HDFConstants.INT128;
        } else if (type == HDFConstants.DFNT_UINT128) {
            return HDFConstants.UINT128;
        } else {
            return "Undefined Data Type";
        }
    }

    /**
     *  convert string type to number type
     *  params type  the string representing the data type
     *  return the integer representing the data type
     */
    public static int getType(String type)
    {
        if   (type.equalsIgnoreCase(HDFConstants.UCHAR8)) {
            return HDFConstants.DFNT_UCHAR8;
        } else if (type.equalsIgnoreCase(HDFConstants.CHAR8)) {
            return HDFConstants.DFNT_CHAR8;
        } else if (type.equalsIgnoreCase(HDFConstants.UCHAR16)) {
            return HDFConstants.DFNT_UCHAR16;
        } else if (type.equalsIgnoreCase(HDFConstants.CHAR16)) {
            return HDFConstants.DFNT_CHAR16;
        } else if (type.equalsIgnoreCase(HDFConstants.FLOAT32)) {
            return HDFConstants.DFNT_FLOAT32;
        } else if (type.equalsIgnoreCase(HDFConstants.FLOAT64)) {
            return HDFConstants.DFNT_FLOAT64;
        } else if (type.equalsIgnoreCase(HDFConstants.FLOAT128)) {
            return HDFConstants.DFNT_FLOAT128;
        } else if (type.equalsIgnoreCase(HDFConstants.INT8)) {
            return HDFConstants.DFNT_INT8;
        } else if (type.equalsIgnoreCase(HDFConstants. UINT8)) {
            return HDFConstants.DFNT_UINT8;
        } else if (type.equalsIgnoreCase(HDFConstants.INT16)) {
            return HDFConstants.DFNT_INT16;
        } else if (type.equalsIgnoreCase(HDFConstants.UINT16)) {
            return HDFConstants.DFNT_UINT16;
        } else if (type.equalsIgnoreCase(HDFConstants.INT32)) {
            return HDFConstants.DFNT_INT32;
        } else if (type.equalsIgnoreCase(HDFConstants.UINT32)) {
            return HDFConstants.DFNT_UINT32;
        } else if (type.equalsIgnoreCase(HDFConstants.INT64)) {
            return HDFConstants.DFNT_INT64;
        } else if (type.equalsIgnoreCase(HDFConstants.UINT64)) {
            return HDFConstants.DFNT_UINT64;
        } else if (type.equalsIgnoreCase(HDFConstants.INT128)) {
            return HDFConstants.DFNT_INT128;
        } else if (type.equalsIgnoreCase(HDFConstants.UINT128)) {
            return HDFConstants.DFNT_UINT128;
        } else {
            return -1;
        }
    }

    /**
     *  gets the size of the data type in bytes,
     *  e.g size of DFNT_FLOAT32 = 4
     *
     *  the size of the data type
     */
    public static int getTypeSize(int type)
    {
        int size = 0;

        switch(type)
        {
            case HDFConstants.DFNT_UCHAR16:
            case HDFConstants.DFNT_CHAR16:
            case HDFConstants.DFNT_INT16:
            case HDFConstants.DFNT_UINT16:
                size = 2;
                break;
            case HDFConstants.DFNT_FLOAT32:
            case HDFConstants.DFNT_INT32:
            case HDFConstants.DFNT_UINT32:
                size = 4;
                break;
            case HDFConstants.DFNT_FLOAT64:
            case HDFConstants.DFNT_INT64:
            case HDFConstants.DFNT_UINT64:
                size = 8;
                break;
            case HDFConstants.DFNT_FLOAT128:
            case HDFConstants.DFNT_INT128:
            case HDFConstants.DFNT_UINT128:
                size = 16;
                break;
            default:
                size = 1;
                break;
        }

        return size;
    }

}
