
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

import ncsa.hdf.object.*;
import java.util.Hashtable;
import java.util.List;
import org.xml.sax.SAXException;

public class H5gPointer extends HObject
{
   public static final int POINTER = 10;

   public static final int GROUP_PTR = 0;
   public static final int DATASET_PTR = 1;
   public static final int NAMED_DATATYPE_PTR = 2;

   protected int ptrType;
   protected HObject object;
   protected HObject xmlParent;
   protected int fid;


   public void init() {}
   public void setObject( HObject o )
   {
      object = o;
   }

   public void setXMLParent( HObject p ) throws SAXException
   {
      xmlParent = p;
      fid = xmlParent.getFID();

      String errMsg = null;
      // - This shouldn't happen
      if( (xmlParent instanceof Group && ptrType!=GROUP_PTR) ||
          (xmlParent instanceof Datatype && ptrType!=NAMED_DATATYPE_PTR) ||
          (xmlParent instanceof Dataset &&
         (ptrType!=DATASET_PTR && ptrType!=NAMED_DATATYPE_PTR)) )
      {
          errMsg = "ERROR: H5gPointer xmlParent type error";
          throw new SAXException(errMsg);
      }

      // - If the pointer is NAMED_DATATYPE_PTR, and its xmlParent is DATASET,
      //   then we don't need to set its name and path.
      if( ptrType==NAMED_DATATYPE_PTR && xmlParent instanceof Dataset )
         return;

      try {
          setName(xmlParent.getName());
          setPath(xmlParent.getPath());
      } catch (Exception ex) {}
   }


   public void setPtrType( int type )
   {
      ptrType = type;
   }

   public HObject getObject()
   {
      return object;
   }

   public HObject getXMLParent()
   {
      return xmlParent;
   }

   public int getPtrType()
   {
      return ptrType;
   }

   public void close() throws Throwable {}

    // implement HObject
    public int open() { return -1;}

    // implement HObject
    public void close(int id) {;}

    // implement DataFromat
    public List getMetadata() throws Exception{ return null;}

    // implement DataFromat
    public void writeMetadata(Object info) throws Exception{;}

    // implement DataFromat
    public void removeMetadata(Object info) throws Exception{;}
}



