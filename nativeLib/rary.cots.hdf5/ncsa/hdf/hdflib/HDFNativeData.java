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

public class HDFNativeData
{
    public HDFNativeData() {}
    public static native int[] byteToInt( byte[] data );
    public static native float[] byteToFloat( byte[] data );
    public static native short[] byteToShort( byte[] data );
    public static native long[] byteToLong( byte[] data );
    public static native double[] byteToDouble( byte[] data );

    public static native int[] byteToInt( int start, int len, byte[] data );
    public static int byteToInt( byte[] data, int start)
    {
        int []ival = new int[1];
        ival = byteToInt(start,1,data);
        return(ival[0]);
    }

    public static native short[] byteToShort( int start, int len, byte[] data );
    public static short byteToShort( byte[] data, int start)
    {
        short []sval = new short[1];
        sval = byteToShort(start,1,data);
        return(sval[0]);
    }

    public static native float[] byteToFloat( int start, int len, byte[] data );
    public static float byteToFloat( byte[] data, int start)
    {
        float []fval = new float[1];
        fval = byteToFloat(start,1,data);
        return(fval[0]);
    }

    public static native long[] byteToLong( int start, int len, byte[] data );
    public static long byteToLong( byte[] data, int start)
    {
        long []lval = new long[1];
        lval = byteToLong(start,1,data);
        return(lval[0]);
    }

    public static native double[] byteToDouble( int start, int len, byte[] data );
    public static double byteToDouble( byte[] data, int start)
    {
        double []dval = new double[1];
        dval = byteToDouble(start,1,data);
        return(dval[0]);
    }

    public static native byte[] intToByte( int start, int len, int[] data);
    public static native byte[] shortToByte( int start, int len, short[] data);
    public static native byte[] floatToByte( int start, int len, float[] data);
    public static native byte[] longToByte( int start, int len, long[] data);
    public static native byte[] doubleToByte( int start, int len, double[] data);

    public static native byte[] byteToByte( byte data);
    static byte[] byteToByte( Byte data){return byteToByte(data.byteValue());}
    public static native byte[] intToByte( int data);
    static byte[] intToByte( Integer data){return intToByte(data.intValue());}
    public static native byte[] shortToByte(short data);
    static byte[] shortToByte( Short data){return shortToByte(data.shortValue());}
    public static native byte[] floatToByte( float data);
    static byte[] floatToByte( Float data){return floatToByte(data.floatValue());};
    public static native byte[] longToByte( long data);
    static byte[] longToByte(Long data){ return longToByte(data.longValue());}
    public static native byte[] doubleToByte( double data);
    static byte[] doubleToByte( Double data){return doubleToByte(data.doubleValue());}

    public Object byteToNumber( byte[] barray, Object obj)
        throws HDFException
    {
        Class theClass = obj.getClass();
        String type = theClass.getName();
        Object retobj = null;

        if (type.equals("java.lang.Integer")) {
            int[] i = ncsa.hdf.hdflib.HDFNativeData.byteToInt(0,1,barray);
            retobj = new Integer(i[0]);
        } else  if (type.equals("java.lang.Byte")) {
            retobj = new Byte(barray[0]);
        } else  if (type.equals("java.lang.Short")) {
            short[] f = ncsa.hdf.hdflib.HDFNativeData.byteToShort(0,1,barray);
            retobj = new Short(f[0]) ;
        } else  if (type.equals("java.lang.Float")) {
            float[] f = ncsa.hdf.hdflib.HDFNativeData.byteToFloat(0,1,barray);
            retobj = new Float(f[0]) ;
        } else  if (type.equals("java.lang.Long")) {
            long[] f = ncsa.hdf.hdflib.HDFNativeData.byteToLong(0,1,barray);
            retobj = new Long(f[0]) ;
        } else  if (type.equals("java.lang.Double")) {
            double[] f = ncsa.hdf.hdflib.HDFNativeData.byteToDouble(0,1,barray);
            retobj = new Double(f[0] );
        } else {
            /* exception: unsupprted type */
            HDFException ex =
            new HDFJavaException("byteToNumber: setfield bad type: "+obj+" "+type);
            throw(ex);
        }
        return(retobj);
    }

    /**
     *  Allocate a 1D array large enough to hold a multidimensional
     *  array of 'datasize' elements of 'dataType' numbers.
     *  This is called from ncsa.hdf.hdfobject.HDFGR and
     *  ncsa.hdf.hdfobject.HDFSDS, and hdf.ncsa.io.ASCII2HDF
     *
     *  @param dataType  the type of the iamge data
     *  @param datasize  the size of the image data array
     *  @return         an array of 'datasize' numbers of 'dataType
     *
     */
public static Object defineDataObject(int dataType, int datasize)
    {
        Object data = null;

        if ((dataType & HDFConstants.DFNT_LITEND) != 0) {
          dataType -= HDFConstants.DFNT_LITEND;
        }

        switch(dataType)
        {
            case HDFConstants.DFNT_INT16:
            case HDFConstants.DFNT_UINT16:
                data = new short[datasize];
                break;
            case HDFConstants.DFNT_INT32:
            case HDFConstants.DFNT_UINT32:
                data = new int[datasize];
                break;
            case HDFConstants.DFNT_INT64:
            case HDFConstants.DFNT_UINT64:
                data = new long[datasize];
                break;
            case HDFConstants.DFNT_FLOAT32:
                data = new float[datasize];
                break;
            case HDFConstants.DFNT_FLOAT64:
                data = new double[datasize];
                break;
            default:
            case HDFConstants.DFNT_CHAR:
            case HDFConstants.DFNT_UCHAR8:
            case HDFConstants.DFNT_UINT8:
            case HDFConstants.DFNT_INT8:
                data = new byte[datasize];
                break;
        }
        return data;
    }
}
