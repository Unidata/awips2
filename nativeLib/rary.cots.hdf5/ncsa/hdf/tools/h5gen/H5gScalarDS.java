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
import java.lang.reflect.Array;
import java.math.BigInteger;
import java.io.*;

import ncsa.hdf.object.*;
import ncsa.hdf.object.h5.*;
import ncsa.hdf.hdf5lib.*;
import ncsa.hdf.hdf5lib.exceptions.*;
import org.xml.sax.SAXException;


public class H5gScalarDS extends H5ScalarDS
{
   private static byte bZero = new String( "\0" ).getBytes()[0];
   private static byte bSpace = new String( " " ).getBytes()[0];

    private H5gParseInfo pinf = null;
    private H5gObjRefList forwardRefs = null;

    private String xid;  /* XML object identifier */
    private int sClass;

    private int did, tid, sid, plistID;
    private Hashtable xmlAttributes;
    private H5gGroup parent;
    private long[] maxdims;
    private int storageLayout;
    private long[] chunkDims;
    private int chunkIndex=0;

    public H5gScalarDS(FileFormat fileFormat, String name, String path)
    {
        this(fileFormat, name, path, null);
    }

    public H5gScalarDS(
        FileFormat fileFormat,
        String name,
        String path,
        long[] oid)
    {
        super (fileFormat, name, path, oid);

        did = tid = sid = -1;
        xmlAttributes = new Hashtable();
        sClass = HDF5Constants.H5S_SIMPLE;
        plistID = HDF5Constants.H5P_DEFAULT;
        storageLayout = HDF5Constants.H5D_CONTIGUOUS;
    }

    public int open()
    {
        if (did > 0)
            return did;

        try
        {
            did = H5.H5Dopen(getFID(), getPath()+getName());
        } catch (HDF5Exception ex)
        {
            did = -1;
        }

        return did;
    }

    public void close()
    {
        try { H5.H5Dclose(did); }
        catch (HDF5Exception ex) {;}

        did = -1;
    }

    public void setXID( String strID )
    {
        xid = strID;
    }

    public String getXID()
    {
        return xid;
    }

    public void setParseInfo( H5gParseInfo p )
    {
        pinf = p;
    }

    public H5gParseInfo getParseInfo() {
        return pinf;
    }

    //  Major Notes:
    //   1.  A new argument, 'lastToken'.  This is null
    //         for numbers, for strings it contains the
    //         partial string in case of overflow.
    //   2.  REFERENCE types are parsed the same as
    //       STRING types.  The code for REFs is merged
    //       with the code for strings.
    //   3.  REFERNCE types should be read as byte[], NOT
    //       long[].
    //   4.  Strings with '\0xx' (octal digits) are skipped.
    //   5.  Changes to detect and handle overflow of
    //       strings.

    public static int parseTextData( StringTokenizer st, Object dataArray, int start,
        String[] lastToken, H5gScalarDS dataset )
        throws SAXException, NumberFormatException, HDF5LibraryException
    {
        if ( st.countTokens() == 0 ) return 0; // nothing to parse

        int fid=-1, did=-1, tid=-1, sid=-1, i=start;
        H5gParseInfo pInfo = dataset.getParseInfo();
        H5gObjRefList orlist = dataset.getForwardRefs();
        String DSName = dataset.getPath();

        int totalSize = pInfo.getTotalElems();
        int tsize = pInfo.getElemSize();
        int mtid = pInfo.getMemTypeID();

        fid = dataset.getFID();

        short UNSGN_BYTE_MAX = 255;
        int UNSGN_SHORT_MAX = 65535;
        long UNSGN_INT_MAX = 4294967295L;
        boolean readStr = false;
        BigInteger UNSGN_LONG_MAX = new BigInteger( "18446744073709551615" );
        BigInteger BIG_ZERO = new BigInteger( "0" );

        int tclass = -1;
        boolean isSigned = false;
        try {
            did = dataset.open();
            tid = H5.H5Dget_type(did);
            tclass = H5.H5Tget_class( tid );
            if( tclass == HDF5Constants.H5T_INTEGER)
                isSigned = (H5.H5Tget_sign(tid) == HDF5Constants.H5T_SGN_2);
        } catch (HDF5LibraryException h5le) {
            closeAll(did, tid, sid);
            String errMsg = "ERROR: H5gDataset.parseTextData() with HDF5LibraryException";
            throw new SAXException( errMsg, h5le );
        }

        if( tclass == HDF5Constants.H5T_ARRAY ) {
            // special case:  use the base class of the array
            tclass = pInfo.getBaseTypeClass();
            tsize =  pInfo.getBaseTypeSize();
        }

        String strVal = null;
        boolean mustSwap = false;

        try {
        if( tclass == HDF5Constants.H5T_INTEGER ||
            tclass == HDF5Constants.H5T_BITFIELD )
        {
            mustSwap = ( tclass == HDF5Constants.H5T_BITFIELD );

            if( tsize == 1 ) // Byte
            {
                byte value = 0;
                while( st.hasMoreTokens() && i<totalSize )
                {
                    strVal = st.nextToken();
                    if (strVal.startsWith("0x")) {
                        Long Ll = Long.decode( strVal );
                        value = Ll.byteValue();
                    } else if( isSigned )
                        value = Byte.parseByte( strVal );
                    else
                        value = (byte)Short.parseShort( strVal );
                    Array.setByte( dataArray, i++, value );
                } // end of while
            } else if( tsize == 2 ) // Short
            {
                short value = 0;
                while( st.hasMoreTokens() && i<totalSize )
                {
                    strVal = st.nextToken();
                    if (strVal.startsWith("0x") )
                    {
                        Long Ll = Long.decode( strVal );
                        value = Ll.shortValue();
                        if (mustSwap)
                        {
                            String s1 = strVal.substring(2,4);
                            String s2 = strVal.substring(4,6);
                            Ll = Long.decode( "0x"+s2+s1 );
                            value = Ll.shortValue();
                        }
                    } else if( isSigned )
                        value = Short.parseShort( strVal );
                    else
                        value = (short)Integer.parseInt( strVal );
                    Array.setShort( dataArray, i++, value );
                } // end of while
            } else if( tsize == 4 ) // Int
            {
                int value = 0;

                while( st.hasMoreTokens() && i<totalSize )
                {
                    strVal = st.nextToken();
                    if (strVal.startsWith("0x"))
                    {
                        Long Ll = Long.decode( strVal );
                        value = Ll.intValue();
                        if (mustSwap)
                        {
                            String s1 = strVal.substring(2,4);
                            String s2 = strVal.substring(4,6);
                            String s3 = strVal.substring(6,8);
                            String s4 = strVal.substring(8,10);
                            Ll = Long.decode( "0x"+s4+s3+s2+s1 );
                            value = Ll.shortValue();
                        }
                    } else if( isSigned )
                        value = Integer.parseInt( strVal );
                    else
                        value = (int)Long.parseLong( strVal );

                    Array.setInt( dataArray, i++, value );
                } // end of while
            } else if( tsize == 8 ) // Long
            {
                long value = 0;
                while( st.hasMoreTokens() && i<totalSize )
                {
                    strVal = st.nextToken();
                    if (strVal.startsWith("0x"))
                    {
                        Long Ll = Long.decode( strVal );
                        value = Ll.longValue();
                        if (mustSwap)
                        {
                            // ?? check this
                            String s1 = strVal.substring(2,4);
                            String s2 = strVal.substring(4,6);
                            String s3 = strVal.substring(6,8);
                            String s4 = strVal.substring(8,10);
                            String s5 = strVal.substring(10,12);
                            String s6 = strVal.substring(12,14);
                            String s7 = strVal.substring(14,16);
                            String s8 = strVal.substring(16,18);
                            Ll = Long.decode( "0x"+s8+s7+s6+s5+s4+s3+s2+s1 );
                            value = Ll.shortValue();
                        }
                    } else
                        value = Long.parseLong( strVal );
                    Array.setLong( dataArray, i++, value );
                } // end of while
            }
        } else if( tclass == HDF5Constants.H5T_FLOAT )
        {
            if( tsize == 4 )
            {
                float value = 0.0f;
                while( st.hasMoreTokens() && i<totalSize )
                {
                    strVal = st.nextToken();
                    value = Float.parseFloat( strVal );
                    Array.setFloat( dataArray, i++, value );
                }
            } else if( tsize == 8 )
            {
                double value = 0.0;
                while( st.hasMoreTokens() && i<totalSize )
                {
                    strVal = st.nextToken();
                    value = Double.parseDouble( strVal );
                    Array.setDouble( dataArray, i++, value );
                }
            }
        }  else if( ( tclass == HDF5Constants.H5T_STRING ) ||
                    ( tclass == HDF5Constants.H5T_REFERENCE ))
        {
            int length = totalSize;
            String value = "";
            String thisToken = null;

            // - Find the first double quote
            while( st.hasMoreTokens() ) {
                if( (thisToken=st.nextToken()).equals( "\"" ) )
                    break;
            }

            if( !thisToken.equals( "\"" ) && !st.hasMoreTokens() )
            {
                // special case no quote but is white space
                String t = new String(thisToken);
                StringTokenizer stt = new StringTokenizer(t);
                if(stt.countTokens() == 0) {
                    closeAll(did, tid, sid);
                    return(i - start);
                }
            }

            if( thisToken.equals( "&" )
                ||  thisToken.equals( "<" )
                ||  thisToken.equals( ">" )
                ||  thisToken.equals( "'" ) )
            {
                // special character, keep going
            } else if( !thisToken.equals( "\"" ) )
            {
                closeAll(did, tid, sid);
                String errMsg = "ERROR: String data format error. " +
                                "Put each string in double quotes.";
                throw new SAXException( errMsg );
            }

            readStr = true;
            while( st.hasMoreTokens() && i<length )
            {
                if( readStr )
                {
                    thisToken = st.nextToken( "\"\\" );

                    // - Handle escaping
                    if( thisToken.equals( "\\" ) )
                    {
                        if( st.hasMoreTokens() )
                        {
                            thisToken = st.nextToken();
                            char c = thisToken.charAt( 0 );
                            if( c=='\\' || c=='\"' )
                                value = value.concat( thisToken );
                            else
                            {
                                switch( c )
                                {
                                    case 'n' :
                                        value = value.concat( "\n" + thisToken.substring(1) );
                                        break;
                                    case 't' :
                                        value = value.concat( "\t" + thisToken.substring(1) );
                                        break;
                                    case '0' :
                                        if (thisToken.length() > 4) {
                                            String stripped = thisToken.substring(4);
                                            value = value.concat( stripped );
                                        }
                                        // this is an octal number -- ignore it
                                        break;
                                    default:
                                        value = value.concat( thisToken );
                                    break;
                                }
                            }
                        }
                    } else if( thisToken.equals( "\"" ) )
                    {
                        // - Ending double quote.
                        if( tclass == HDF5Constants.H5T_STRING )
                        {
                            int j;
                            byte[] b = value.getBytes();
                            for( j = 0; j<tsize && j<b.length; j++ )
                                Array.setByte( dataArray, i*tsize+j, b[j] );
                            if (H5.H5Tget_strpad(tid) == HDF5Constants.H5T_STR_SPACEPAD ){
                                for( ; j < tsize; j++ )
                                    Array.setByte( dataArray, i*tsize+j, bSpace );
                            } else {
                                for( ; j < tsize; j++ )
                                    Array.setByte( dataArray, i*tsize+j, bZero );
                            }
                        }
                        else if( tclass == HDF5Constants.H5T_REFERENCE )
                        {
                            int refSize = H5.H5Tget_size(tid);;

                            byte []	refBuff = null;
                            try {
                               refBuff = H5.H5Rcreate( fid, value, HDF5Constants.H5R_OBJECT, -1 );
                            } catch( HDF5BtreeException bte )
                            {
                                if (bte.getMessage().equals("Object not found"))
                                {
                                    H5gObjectRefPatch patch = new H5gObjectRefPatch(DSName, value, i ,refSize);
                                    orlist.add(patch);
                                    refBuff = new byte[refSize]; // empty ref
                                } else {
                                    closeAll(did, tid, sid);
                                    throw new SAXException( bte );
                                }
                            } catch( HDF5SymbolTableException ste)
                            {
                                if (ste.getMessage().equals("Object not found"))
                                {
                                    H5gObjectRefPatch patch = new H5gObjectRefPatch(DSName, value, i ,refSize);
                                    orlist.add(patch);
                                    refBuff = new byte[refSize]; // empty ref
                                } else {
                                    closeAll(did, tid, sid);
                                    throw new SAXException( ste );
                                }
                            }

                            // Should be an array of bytes, NOT longs
                            // Repeat:  DO NOT USE LONG here.
                            for( int lCnt = 0; lCnt < refSize; lCnt++ )
                                Array.setByte( dataArray, ((i * refSize)+lCnt), refBuff[lCnt] );
                        }
                        i++;
                        readStr = false;
                    } else if( value.length() == 0 )
                    {
                        value = thisToken;
                    }
                    else
                    {
                        value = value.concat( thisToken );
                    }
                }  else
                {
                    thisToken = st.nextToken( "\"" );
                    if( thisToken.equals( "\"" ) )
                    {
                        readStr = true;
                        value = "";
                    }
                }
            } // end of while

            //  if we read here, then are done with string
            if (readStr)
            {
                // oops, incomplete string on end of buffer
                // return to caller to continue
                lastToken[0] = new String(value);
            }
        } else if( tclass == HDF5Constants.H5T_ENUM )
        {
            String name = null;
            //  can only handle 4 byte ints?
            int[] value = new int[ 1 ];
            while( st.hasMoreTokens() && i<totalSize )
            {
                name = st.nextToken();
                // - Currently only int type is supported for enum,
                //   so the value is stored in value[0]
                H5.H5Tenum_valueof( tid, name, value );

                Array.setInt( dataArray, i++, value[0] );
            }
        }
        else if( tclass == HDF5Constants.H5T_OPAQUE )
        {
            byte value = 0;
            short sValue = 0;
            while( st.hasMoreTokens() && i<totalSize )
            {
                strVal = st.nextToken();
                // each entry is tsize bytes of
                //  arbitrary data
                //  Parse each two characters as HEXADECIMAL byte
                if (strVal.startsWith("0x"))
                    strVal = strVal.substring(2);

                for( int nb = 0; nb < tsize; nb++)
                {
                    String s = strVal.substring(0,2);
                    Long Ll = Long.decode( "0x"+s );
                    value = Ll.byteValue();
                    Array.setByte( dataArray, (i * tsize)+nb, value );
                    strVal = strVal.substring(2);
                }
                i++;
            }
        } // end of else if( tclass == HDF5Constants.H5T_OPAQUE )
        if( i == totalSize && st.hasMoreTokens() )
        {
            if(( tclass == HDF5Constants.H5T_STRING ) ||
               (tclass == HDF5Constants.H5T_REFERENCE ))
            {
                // these types get all the separators
                // usually is one \n on the end
                // if so, everything is fine.
                strVal = st.nextToken();
                if ( !st.hasMoreTokens() )
                {
                    // one last CR -- OK
                    closeAll(did, tid, sid);
                    return i-start;
                }
            }

            closeAll(did, tid, sid);
            return -1;
        }
        } finally { closeAll(did, tid, sid); }

        closeAll(did, tid, sid);
        return i-start;
    }

   public H5gObjRefList getForwardRefs(){
    return( forwardRefs);
   }

    private static void closeAll(int did, int tid, int sid)
    {
        try {
            if (did > 0) H5.H5Dclose(did);
            if (tid > 0) H5.H5Tclose(tid);
            if (sid > 0) H5.H5Sclose(sid);
        } catch (Exception ex) {}
    }

    public Hashtable getAttributes() { return xmlAttributes; }

    public void setParent(H5gGroup pGroup) { parent = pGroup; };

    public void setDatatype(H5gDatatype dtype) { datatype = dtype; }

    public void setSpaceClass(int cls) { sClass = cls; }

    public void setMaxDims(long[] mdims)
    {
        if (mdims != null && maxdims != null && rank>0)
        {
            System.arraycopy(mdims, 0, maxdims, 0, rank);
        }
    }

    public void create() throws Exception
    {
        int tid, sid, did;

        tid = datatype.toNative();
        if (sClass == HDF5Constants.H5S_SCALAR)
            sid =H5.H5Screate( HDF5Constants.H5S_SCALAR );
        else
            sid = H5.H5Screate_simple( rank, dims, maxdims );

        did = H5.H5Dcreate(this.getFID(), this.getFullName(), tid, sid, plistID);

        try { H5.H5Dclose(did); } catch (Exception ex) {}
        try { H5.H5Tclose(tid); } catch (Exception ex) {}
        try { H5.H5Sclose(sid); } catch (Exception ex) {}
    }

    public void createPlist() throws Exception
    {
        plistID = H5.H5Pcreate( HDF5Constants.H5P_DATASET_CREATE );
    }

    public void closePlist() throws Exception
    {
        H5.H5Pclose(plistID);
    }

    public void setLayout(int layout) throws Exception
    {
        storageLayout = layout;

        if (plistID > 0)
            H5.H5Pset_layout( plistID, layout );
    }

    public void setChunkRank( int n )
    {
      chunkDims = new long[ n ];
    }

    public void setChunkDims( int size ) throws SAXException
    {
        if (chunkIndex<chunkDims.length && size > 0)
            chunkDims[chunkIndex++] = size;
    }


}
