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


import java.util.Hashtable;

import ncsa.hdf.object.*;
import ncsa.hdf.object.h5.*;
import java.util.Hashtable;
import java.util.List;
import org.xml.sax.SAXException;


public class H5gAttribute extends HObject
{
    private HObject xmlParent = null;

    private H5gParseInfo pinf = null;
    private H5gObjRefList forwardRefs = null;

    private int nativeDatatype;

    protected Object data;
    protected int rank;
    protected long[] dims;
    protected long[] maxdims;

    public H5gAttribute(FileFormat fileFormat, String name, HObject parent)
    {
        super (fileFormat, name, null, null);
        xmlParent = parent;

        forwardRefs = new H5gObjRefList(name, true);
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


    public void setRank( int n )
    {
        rank = n;
        dims = new long[ rank ];
        maxdims = new long[ rank ];
    }

    public void setParseInfo( H5gParseInfo p )
    {
        pinf = p;
    }


    public H5gParseInfo getParseInfo()
    {
        return pinf;
    }

    // name has to be set before this function is called.
    public void setXMLParent( HObject p ) throws SAXException
    {
        xmlParent = p;
    }

    public HObject getXMLParent()
    {
        return xmlParent;
    }

}

