
/****************************************************************************
 * NCSA HDF                                                                 *
 * National Comptational Science Alliance                                   *
 * University of Illinois at Urbana-Champaign                               *
 * 605 E. Springfield, Champaign IL 61820                                   *
 *                                                                          *
 * This product includes software developed by the Apache Software
 * Foundation (http://www.apache.org/).
 *
 * The h5gen tool uses the Apache Java Xerces classes to parse
 * XML.  The xerces classes (xerces.jar) are included in the Java
 * HDF5 product.  See the Apache license (LICENSE.html) for
 * terms and conditions of use of Xerces.
 *
 * For conditions of distribution and use, see the accompanying             *
 * java-hdf5/COPYING file.                                                  *
 *                                                                          *
 ****************************************************************************/
package ncsa.hdf.tools.h5gen;


import java.util.*;

import ncsa.hdf.object.*;
import ncsa.hdf.object.h5.*;
import ncsa.hdf.hdf5lib.*;
import ncsa.hdf.hdf5lib.exceptions.*;
import org.xml.sax.SAXException;

public class H5gDatatype extends H5Datatype
{
   public static final String BYTE_ORDER = "ByteOrder";
   public static final String SIGN       = "Sign";
   public static final String SIZE       = "Size";

   public static final String SIGN_BIT_LOCATION = "SignBitLocation";
   public static final String EXPONENT_BITS     = "ExponentBits";
   public static final String EXPONENT_LOCATION = "ExponentLocation";
   public static final String MANTISSA_BITS     = "MantissaBits";
   public static final String MANTISSA_LOCATION = "MantissaLocation";
   public static final String TYPE_CODE         = "TypeCode";

   public static final String C_SET    = "Cset";
   public static final String STR_SIZE = "StrSize";
   public static final String STR_PAD  = "StrPad";

   public static final String N_ELEMS    = "Nelems";
   public static final String DIM_SIZE   = "DimSize";
   public static final String N_DIMS     = "Ndims";
   public static final String FIELD_NAME = "FieldName";

   public static final int BIG_ENDIAN    = 0;
   public static final int LITTLE_ENDIAN = 1;

   public static final int ATOMIC_TYPE    = 0;
   public static final int COMPOUND_TYPE  = 100;
   public static final int VL_TYPE        = 200;
   public static final int ARRAY_TYPE        = 300;
   public static final int ENUM_TYPE        = 400;

   public static final String TAG       = "Tag";

   public static final int MAX_RANK = 4;

   private HObject xmlParent;
   private Hashtable xmlAttributes;
   private boolean isNamed;

    public H5gDatatype(
        FileFormat fileFormat,
        String name,
        String path,
        long[] oid)
    {
        super (fileFormat, name, path, oid);
        xmlParent = null;
        xmlAttributes = new Hashtable();
        isNamed = true;
    }

    public H5gDatatype(int tclass, int tsize, int torder, int tsign)
    {
        super(tclass, tsize, torder, tsign);
        xmlParent = null;
        isNamed = true;
    }

    public H5gDatatype(int nativeID)
    {
        super(nativeID);
        xmlParent = null;
    }

    public H5gDatatype(boolean named)
    {
        super(-1);
        isNamed = named;
    }

    public HObject getXMLParent() {
        return xmlParent;
    }

    public void setXMLParent(HObject obj) {
        xmlParent = obj;
    }



    public Hashtable getAttributes() { return xmlAttributes; }

}
