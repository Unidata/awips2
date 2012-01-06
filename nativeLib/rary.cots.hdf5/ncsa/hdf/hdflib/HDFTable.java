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

import java.lang.reflect.*;
/*
 *  This is a class for handling Vdata table records for
 *  HDF.
 *
 *  The purpose is to allow the storage and retrieval of
 *  tables containing arbitrary types of strings and numbers
 *
 *  The methods support the conversion of an array to and
 *  from Java to a one-dimensional array of bytes suitable
 *  for I/O by the C library.
 */

public class HDFTable {

    static {
        String v = HDFLibrary.getJHIVersion();
    }

    TabDescriptor _desc = null;

    public HDFTable() throws java.lang.IllegalAccessException, HDFException {
    }

    void init() throws java.lang.IllegalAccessException, HDFException {
        _desc = new TabDescriptor( this );
    }


public int size() throws HDFException, java.lang.IllegalAccessException
{
    if (_desc == null ) {
          init();
    }
    return(_desc.totalsize);
}

public byte[] byteify() throws java.lang.IllegalAccessException, HDFException
{
    int i;

    if (_desc == null ) {
          init();
    }

    byte [] array = new byte[_desc.totalsize];

    int pos = 0;
    for (i = 0; i < _desc.nfields; i++) {
        getfield( _desc.fldType[i], _desc.flds[i].get(this), array, pos);
        pos += _desc.fldLen[i];
    }

    return array;
}

void getfield( Class theType, Object obj, byte[] barray, int offset ) throws HDFException {

    String type = theType.getName();

    byte arow[];
    if (type.equals("int")) {
        arow = ncsa.hdf.hdflib.HDFNativeData.intToByte((Integer)obj);
        System.arraycopy(arow,0,barray,offset,4);
        return ;
    } else  if (type.equals("byte")) {
        byte f = ((Byte)obj).byteValue();
        barray[offset] = f;
        return ;
    } else  if (type.equals("short")) {
        arow = ncsa.hdf.hdflib.HDFNativeData.shortToByte((Short)obj);
        System.arraycopy(arow,0,barray,offset,2);
        return ;
    } else  if (type.equals("float")) {
        /* 32 bit float */
        arow = ncsa.hdf.hdflib.HDFNativeData.floatToByte((Float)obj);
        System.arraycopy(arow,0,barray,offset,4);
        return ;
    } else  if (type.equals("long")) {
        arow = ncsa.hdf.hdflib.HDFNativeData.longToByte((Long)obj);
        System.arraycopy(arow,0,barray,offset,8);
        return ;
    } else  if (type.equals("double")) {
        arow = ncsa.hdf.hdflib.HDFNativeData.doubleToByte((Double)obj);
        System.arraycopy(arow,0,barray,offset,8);
        return ;
    } else if (type.equals("java.lang.String")) {
        arow = ((String)obj).getBytes();
        System.arraycopy(arow,0,barray,offset,
            java.lang.reflect.Array.getLength(arow));
        return ;
    } else if (type.startsWith("[")) {
        if ((obj.getClass().isArray()) == false) {
            HDFException ex =
            new HDFJavaException("HDFTable: getfield not an array?: "+theType+" "+type);
            throw(ex);
        }
        HDFArray aa = new HDFArray(obj);
        arow = aa.byteify();
        System.arraycopy(arow,0,barray,offset,
            java.lang.reflect.Array.getLength(arow));

        return;
    } else {
        HDFException ex =
        new HDFJavaException("HDFTable: getfield unsupported type?: "+theType+" "+type);
        throw(ex);
    }
}


public void structify( byte [] theBytes) throws HDFException, java.lang.IllegalAccessException
{

    int i;

    if (_desc == null ) {
        init();
    }

    int pos = 0;
    for (i = 0; i < _desc.nfields; i++) {
        Object v = setfield( _desc.fldTypeName[i], _desc.flds[i].get(this), theBytes, pos,
            _desc.fldLen[i]);
        _desc.flds[i].set(this,v);
        pos += _desc.fldLen[i];
    }
}

Object setfield( String type, Object obj, byte[] barray, int offset, int maxlen ) throws HDFException {


    if (type.equals("int")) {
        int[] f = ncsa.hdf.hdflib.HDFNativeData.byteToInt(offset,1,barray);
        return new Integer(f[0]);
    } else  if (type.equals("byte")) {
        byte f[] = new byte[1];
        f[0] = ((Byte)obj).byteValue();
        return new Byte(f[0]) ;
    } else  if (type.equals("short")) {
        short[] f = ncsa.hdf.hdflib.HDFNativeData.byteToShort(offset,1,barray);
        return new Short(f[0]) ;
    } else  if (type.equals("float")) {
         /* 32 bit float */
        float[] f = ncsa.hdf.hdflib.HDFNativeData.byteToFloat(offset,1,barray);
        return new Float(f[0]) ;
    } else  if (type.equals("long")) {
        long[] f = ncsa.hdf.hdflib.HDFNativeData.byteToLong(offset,1,barray);
        return new Long(f[0]) ;
    } else  if (type.equals("double")) {
        double[] f = ncsa.hdf.hdflib.HDFNativeData.byteToDouble(offset,1,barray);
        return new Double(f[0] );
    } else if (type.equals("java.lang.String")) {
        if ((((String)obj).length()) > maxlen) {
            HDFException ex =
            new HDFJavaException("HDFTable: setfield string is too big?: "+obj+" "+type);
            throw(ex);
        }
        String ss = new String( barray, offset, ((String)obj).length());
        return ss;
    } else if (type.startsWith("[")) {
        if ((obj.getClass().isArray()) == false) {
            HDFException ex =
            new HDFJavaException("HDFTable: setfield type is not array?: "+obj+" "+type);
            throw(ex);
        }
        HDFArray aa = new HDFArray(obj);
        if (java.lang.reflect.Array.getLength(obj) > maxlen) {
            HDFException ex =
            new HDFJavaException("HDFTable: setfield array too big?: "+obj+" "+type);
            throw(ex);
        }
        byte b[] = new byte[maxlen];
        System.arraycopy(barray,offset,b,0,maxlen);
        Object o = aa.arrayify(b);
        return o;
    } else {
        /* exception: unsupprted type */
        HDFException ex =
        new HDFJavaException("HDFTable: setfield bad type: "+obj+" "+type);
        throw(ex);
    }
}
}


class TabDescriptor {

    String theType = "";
        Class theClass = null;
    int nfields;
        int totalsize = 0;
        Field [] flds;
        String [] fldTypeName;
        Class [] fldType;
        int [] fldLen;
        Object [] theFields;


    /*public*/ TabDescriptor(Object theStruct) throws HDFException,java.lang.IllegalAccessException
        {
        theClass = theStruct.getClass();
        theType = theClass.toString();
        int i = 0;
        flds = theClass.getFields();
        nfields = java.lang.reflect.Array.getLength(flds);
        fldType = new Class[nfields];
        fldTypeName = new String[nfields];
        fldLen = new int[nfields];
        theFields = new Object[nfields];
        for (i = 0; i < nfields; i++) {
            fldType[i] = flds[i].getType();
            fldTypeName[i] = fldType[i].getName();
            theFields[i] = flds[i].get(theStruct);
            if (theFields[i] == null) {
                HDFException ex =
                new HDFJavaException("HDFTable: Field not defined: "+flds[i].getName());
                throw(ex);
            }
            fldLen[i] = calcsize(fldType[i], flds[i].get(theStruct));
            totalsize += fldLen[i];
        }
    }

    int calcsize( Class type, Object obj ) throws HDFException {

        String s = type.toString();

        if (s.equals("int")) {
            return 4;
        } else  if (s.equals("byte")) {
            return 1;
        } else  if (s.equals("short")) {
            return 2;
        } else  if (s.equals("float")) {
            return 4;
        } else  if (s.equals("long")) {
            return 8;
        } else  if (s.equals("double")) {
            return 16;
        } else if (s.equals("class java.lang.String")) {
            return (((String)obj).length());
        } else if (s.startsWith("class [")) {
            if ((obj.getClass().isArray()) == false) {
                /* exception: not an array */
                System.out.println("Error:  byteify requires array");
                return -1;
            }

            int n = 6;
            int dims = 0;
            char c = ' ';
            while (n < s.length()) {
                c = s.charAt(n);
                n++;
                if (c == '[') {
                    dims++;
                }
            }

            /* To do:  extend to deal with Integer, Short, etc. */
            int NT = c;  /*  must be B,S,I,L,F,D, else error */
            int NTsize = 0;
            if (NT == 'B') {
                NTsize = 1;
            } else if (NT == 'S') {
                NTsize = 2;
            } else if ((NT == 'I') || (NT == 'F')) {
                NTsize = 4;
            } else if ((NT == 'J') || (NT == 'D')){
                NTsize = 8;
            } else {
                /* exception:  not a numeric type */
                System.out.println("Error:  array is not numeric?");
                HDFException ex =
                new HDFJavaException("HDFTable: Array is not numeric?: "+
                    s);
                                throw(ex);
            }

            /* fill in the table */

            Object o = obj;
            int dimlen = 1;
            int size = NTsize;
            int i;
            for ( i = 1; i <= dims; i++) {
                dimlen= java.lang.reflect.Array.getLength(o);
                size *= dimlen;
                o = java.lang.reflect.Array.get(o,0);
            }
            return(size);
        } else {
            HDFException ex =
            new HDFJavaException("HDFTable: Unsupported data type: "+
                s);
                                throw(ex);
        }
    }
}

