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

import org.xml.sax.XMLReader;
import org.xml.sax.ContentHandler;
import org.xml.sax.helpers.XMLReaderFactory;
import org.xml.sax.SAXException;
import org.xml.sax.Attributes;
import org.xml.sax.Locator;
import org.xml.sax.InputSource;

import java.io.*;
import java.util.*;

import ncsa.hdf.object.*;
import ncsa.hdf.object.h5.*;
import ncsa.hdf.object.HObject;
import ncsa.hdf.hdf5lib.*;
import ncsa.hdf.hdf5lib.exceptions.*;
import ncsa.hdf.tools.h5gen.*;

/**
  *  This class is the main program.
  *
  *  h5gen processes XML that follows the HDF5 DTD.  See
  *    http://hdf.ncsa.uiuc.edu/HDF5/XML
  *
  *  h5gen implements the 'ContentHandler' interface for
  *  the SAXParser.  This includes all the 'Start' and 'End'
  *  call backs to process the XML document.
  *
  *  The main program initializes and calls the SAX Parser.
  *  This version uses the xerces parser.
  *
  *  The 'Start' and 'End' functions create and write HDF5
  *  objects as they are defined in the XML.
  */

public class H5gen implements ContentHandler
{
    /* Nothing to print out even if an error occurs */
    public static final int NO_MSG             = 0;

    /* Print out error message when it occurs */
    public static final int ERROR_MSG          = 1;

    /* Print out error message and HDF5 error stack */
    public static final int STACK_MSG          = 2;

    /* Print out error message and warning message and HDF5 error stack */
    public static final int STACK_AND_WARNING  = 3;

    /* Print out as STACK_AND_WARNING, and also print out the parsing information */
    public static final int ERR_STACK_WAR_PARS = 4;

    /* Verbose mode is only effective if "cmdLineTool" is true */
    private static int verbose                = ERROR_MSG;

    private static final String PARSER_NAME = "org.apache.xerces.parsers.SAXParser";


    private static boolean cmdLineTool        = false;
    protected static Stack h5Objects          = null;
    protected static Object currentObj         = null;

    private XMLReader reader          = null;

    private String h5FileName         = null;
    private String xmlFileName        = null;

    private Hashtable ptrTable        = null;

    private String lastLocalName  = null;
    private String errMsg = null;
    private boolean dataProcessed = false;

    private H5File h5file         = null;


    /* - NamedDataTypePtr_in_NamedDataType. It is set/unset according to the following scheme:
         <NamedDataType ...>
           <NamedDataTypePtr ...>	-- set the falg to be true
           </NamedDataTypePtr>
        </NamedDataType>		-- set the falg to be false
     */
    private boolean NDTPtrInNDT   = false;

    private int fid = -1;
    private int status = -1;

    // State of reading when data is > 116K block
    private boolean overflowed = false;
    private boolean mustInitRead = true;
    private int elemsRead  = 0;
    private String lastPart  = null;
    private LinkedList forwardobjRefs = null;

    /**
     * Constructs a new H5gen by given the input xml file and output HDF5 file name
     *
     * @param xmlName -- the name of input XML file
     * @param h5Name -- the name of output HDF5 file
     */
    public H5gen( String xmlName, String h5Name )
    {
        this( xmlName, h5Name, ERROR_MSG );
    }

    /**
     * Constructs a new H5gen by given the input xml file and output HDF5 file name
     *
     * @param xmlName -- the name of input XML file
     * @param h5Name -- the name of output HDF5 file
     * @param vMode -- verbose mode for error report
     */
    public H5gen( String xmlName, String h5Name, int vMode )
    {
        h5FileName = h5Name;
        xmlFileName = xmlName;
        h5Objects = new Stack();
        ptrTable = new Hashtable();
        verbose = vMode;
        forwardobjRefs = new LinkedList();
    }

    public static void setVerboseMode( int vMode )
    {
        verbose = vMode;
    }

    public static boolean isCmdLineTool()
    {
       return cmdLineTool;
    }

    public static void setIsCmdLineTool( boolean isCmdTool )
    {
       cmdLineTool = isCmdTool;
       if( !cmdLineTool )
          verbose = NO_MSG;
    }

    public int parseXML() throws SAXException, IOException
    {
        reader = XMLReaderFactory.createXMLReader( PARSER_NAME );
        reader.setContentHandler( this );

        InputSource ins = null;
        if (xmlFileName.equals("-")) {
            ins = new InputSource(System.in );
        } else {
            ins = new InputSource( xmlFileName );
        }

        try
        {
            reader.parse( ins );
        }
        catch( IOException ex )
        {
            cleanUp();
            throw ex;
         }
         catch( SAXException ex )
        {
            throw ex;
         }

         return 0;
    }

    /**
      *  Implementing ContentHandler
      */
    public void setDocumentLocator( Locator locator ) {  }

    /**
      *  Implementing ContentHandler
      */
    public void startDocument() throws SAXException
    {
        try
        {
            h5file = new H5File(h5FileName);
        } catch (Exception ex)  { throw new SAXException(ex); }
    }

    private void patchRefData( H5gObjRefList rl )
            throws HDF5Exception, HDF5LibraryException
    {
        if (rl.isAttribute())
            patchRefAttr( rl );
        else
            patchRefDataset( rl );
    }

    private void patchRefAttr( H5gObjRefList rl )
            throws HDF5Exception, HDF5LibraryException
    {
        String Dsetname = rl.getName();
        String attrName = rl.getAttributeName();
        int loc_id=-1, tid=-1, sid=-1, aid=-1, tsize=0, rank=0, status=-1;
        long []dims = null;
        long size = 0;

        if (rl.attrInDT())
            loc_id = H5.H5Topen(fid, Dsetname);
        else if (rl.attrInGroup())
            loc_id = H5.H5Gopen(fid, Dsetname);
        else
            loc_id = H5.H5Dopen(fid, Dsetname);

        aid = H5.H5Aopen_name(loc_id, attrName);
        sid = (int)H5.H5Aget_space(aid);
        rank = H5.H5Sget_simple_extent_ndims(sid);
        dims = new long[rank];
        status = H5.H5Sget_simple_extent_dims(sid,dims,null);
        size = dims[0];
        for (int j = 1; j < rank; j++)
           size *= dims[j];

       tid = (int)H5.H5Aget_type(aid);
       tsize = H5.H5Tget_size(tid);
       if (!H5.H5Tequal(tid, HDF5Constants.H5T_STD_REF_OBJ) )
           throw new HDF5Exception( "Not an object reference" );
       size *= tsize;

        byte [] buf = new byte[(int)size];

        /* no need to read here, -- Peter Cao
        H5.H5Aread(aid, tid, buf);
        */

        ListIterator lli = rl.getIterator();
        byte [] rb = new byte[tsize];
        String refpath = null;
        int offset = 0;
        while(lli.hasNext()) {
            H5gObjectRefPatch p = (H5gObjectRefPatch)lli.next();
            refpath = p.getRefPath();
            offset = p.getOffset();
            rb = H5.H5Rcreate( fid, refpath, HDF5Constants.H5R_OBJECT, -1 );
            offset *= tsize;
            for (int i = 0; i < tsize; i++) {
                buf[offset+i] = rb[i];
            }
        }

        H5.H5Awrite( aid, tid, buf);
        H5.H5Aclose(aid);

        try {
            H5.H5Tclose(tid);
            H5.H5Sclose(sid);
            if (rl.attrInDT()) H5.H5Tclose(loc_id);
            else if (rl.attrInGroup()) H5.H5Gclose(loc_id);
            else H5.H5Dclose(loc_id);
        } catch (Exception ex) {}
    }

    private void patchRefDataset( H5gObjRefList rl )
            throws HDF5Exception, HDF5LibraryException
    {
        String Dsetname = rl.getName();
        int did=-1, tid=-1;
        int  bsize = 0;
        int  tsize = -1;
        byte buf[] = null;
        int memt = -1;

        did = H5.H5Dopen(fid, Dsetname);
        bsize = (int)H5.H5Dget_storage_size(did);
        buf = new byte[bsize];
        tid = H5.H5Dget_type( did );
        tsize = H5.H5Tget_size(tid);

        H5.H5Dread(did, tid, HDF5Constants.H5S_ALL, HDF5Constants.H5S_ALL,
            HDF5Constants.H5P_DEFAULT, buf);

        byte[] ref = new byte[tsize];
        String refpath = null;
        int position = 0;
        ListIterator lli = rl.getIterator();
        while(lli.hasNext()) {
            H5gObjectRefPatch p = (H5gObjectRefPatch)lli.next();
            refpath = p.getRefPath();
            position = p.getOffset();
            ref = H5.H5Rcreate( fid, refpath, HDF5Constants.H5R_OBJECT, -1 );
            position *= tsize;
            for (int jj = 0; jj < tsize; jj++) {
                buf[position+jj] = ref[jj];
            }
        }

        H5.H5Dwrite(did, HDF5Constants.H5T_STD_REF_OBJ, HDF5Constants.H5S_ALL,
            HDF5Constants.H5S_ALL,HDF5Constants.H5P_DEFAULT, buf);
        H5.H5Tclose(tid);
        H5.H5Dclose(did);
    }

    public void endDocument() throws SAXException
    {
        ListIterator li = forwardobjRefs.listIterator();
        while(li.hasNext()) {
            H5gObjRefList rl = (H5gObjRefList)li.next();
            if (rl.getSize() > 0) {
                patchRefData( rl );
            }
        }

        // close all open objects associated to this file
        try {
            int n=0, type=-1, oids[];
            n = H5.H5Fget_obj_count(fid, HDF5Constants.H5F_OBJ_ALL);
            if ( n>0) {
                oids = new int[n];
                H5.H5Fget_obj_ids(fid, HDF5Constants.H5F_OBJ_ALL, n, oids);
                for (int i=0; i<n; i++) {
                    type = H5.H5Iget_type(oids[i]);
                    if (HDF5Constants.H5I_DATASET == type) {
                        try { H5.H5Dclose(oids[i]); } catch (Exception ex2) {}
                    } else if (HDF5Constants.H5I_GROUP == type) {
                        try { H5.H5Gclose(oids[i]); } catch (Exception ex2) {}
                    } else if (HDF5Constants.H5I_DATATYPE == type) {
                        try { H5.H5Tclose(oids[i]); } catch (Exception ex2) {}
                    } else if (HDF5Constants.H5I_ATTR == type) {
                        try { H5.H5Aclose(oids[i]); } catch (Exception ex2) {}
                    }
                } // for (int i=0; i<n; i++)
            } // if ( n>0)
        } catch (Exception ex) {}

        H5.H5Fclose( fid );
    }

    private void parseIntType( H5gDatatype dType, String byteOrder,
        String sign, String size ) throws SAXException, NumberFormatException
    {
        HObject p = dType.getXMLParent();

        int tsize = 4; // set default to 4 bytes */
        try {  tsize = Integer.parseInt( size ); }
        catch( NumberFormatException ne ) {}
        if (tsize<1 || tsize>8) tsize = 4;

        boolean signed = false;
        if( sign.equals( "true" ) )
            signed = true;
        else if( sign.equals( "false" ) )
            signed = false;

        if ( byteOrder.equals( "BE" ) && signed ) {
            switch (tsize) {
                case 1: dType.fromNative(HDF5Constants.H5T_STD_I8BE); break;
                case 2: dType.fromNative(HDF5Constants.H5T_STD_I16BE); break;
                case 4: dType.fromNative(HDF5Constants.H5T_STD_I32BE); break;
                case 8: dType.fromNative(HDF5Constants.H5T_STD_I64BE); break;
            }
        } else if( byteOrder.equals( "BE" ) && !signed ) {
            switch (tsize) {
                case 1: dType.fromNative(HDF5Constants.H5T_STD_U8BE); break;
                case 2: dType.fromNative(HDF5Constants.H5T_STD_U16BE); break;
                case 4: dType.fromNative(HDF5Constants.H5T_STD_U32BE); break;
                case 8: dType.fromNative(HDF5Constants.H5T_STD_U64BE); break;
            }
        } else if( byteOrder.equals( "LE" ) && signed ) {
            switch (tsize) {
                case 1: dType.fromNative(HDF5Constants.H5T_STD_I8LE); break;
                case 2: dType.fromNative(HDF5Constants.H5T_STD_I16LE); break;
                case 4: dType.fromNative(HDF5Constants.H5T_STD_I32LE); break;
                case 8: dType.fromNative(HDF5Constants.H5T_STD_I64LE); break;
            }
        } else if( byteOrder.equals( "LE" ) && !signed ) {
            switch (tsize) {
                case 1: dType.fromNative(HDF5Constants.H5T_STD_U8LE); break;
                case 2: dType.fromNative(HDF5Constants.H5T_STD_U16LE); break;
                case 4: dType.fromNative(HDF5Constants.H5T_STD_U32LE); break;
                case 8: dType.fromNative(HDF5Constants.H5T_STD_U64LE); break;
            }
        } else
           throwSAXException( "Unknown integer data type", null );
    }

    /**
     *  (Required by the ContentHandler interface. )
     *
     *  Called once for each open tag encountered.
     *
     *  This routine must determine which tag is being entered
     *  and take appropriate action.  Each HDF5 object has different
     *  required action.
     *
     *  Get the name of the tag (localName) and act accordingly.
     *
     *  Usually will push open objects onto the stack while processing
     *  children.  Some XML elements have no action required here.
     *
     *  The XML attributes need to be read and stored in the Java
     *  object as needed.
     *
     */
    public void startElement( String namespaceURI, String localName,
         String rawName, Attributes atts )
       throws SAXException
    {
        if( H5gConstants.ELEM_HDF5_FILE.equals(localName))
        {
            // h5file = new H5File(); already created at startDocument
            currentObj = h5file;
        } else if( H5gConstants.ELEM_ROOT_GROUP.equals(localName))
        {
            // root group is automatically created when file was created
            h5Objects.push( currentObj );
            H5gGroup rGroup;
            currentObj = rGroup = new H5gGroup(h5file, "/", null, null);
            Hashtable attrib = rGroup.getAttributes();
            attrib.put(H5gConstants.ATTR_XOID, atts.getValue(H5gConstants.ATTR_XOID));
            checkPtrTable( atts.getValue(H5gConstants.ATTR_XOID), rGroup );
        } else if( H5gConstants.ELEM_GROUP.equals(localName) )
        {
            if( !(currentObj instanceof Group ))
                throwSAXException( "Parent object is not a group", null );

            H5gGroup pGroup = (H5gGroup)currentObj;
            h5Objects.push( pGroup );

            String gname = atts.getValue(H5gConstants.ATTR_NAME);
            String path = atts.getValue(H5gConstants.ATTR_PATH);
            H5gGroup grp = new H5gGroup(h5file, gname, path, pGroup);
            grp.create();
            checkPtrTable( (String)grp.getAttributes().get( H5gConstants.ATTR_XOID ), grp );

            Hashtable attrib = grp.getAttributes();
            attrib.put( H5gConstants.ATTR_XOID,
            atts.getValue(H5gConstants.ATTR_XOID) );
            attrib.put( H5gConstants.ATTR_PARENTS, atts.getValue(H5gConstants.ATTR_PARENTS) );
            pGroup.addToMemberList(grp );
            currentObj = grp;
        } else if( H5gConstants.ELEM_GROUP_PTR.equals( localName ) )
        {
            H5gPointer gPtr = new H5gPointer();
            gPtr.setPtrType( H5gPointer.GROUP_PTR );
            gPtr.setXMLParent( (H5gGroup)currentObj );
            h5Objects.push( currentObj );
            currentObj = gPtr;
            checkPtrTable( atts.getValue( H5gConstants.ATTR_XOID ), gPtr );
        } else if( H5gConstants.ELEM_ATTRIBUTE.equals( localName ) )
        {
            h5Objects.push( currentObj );
            H5gAttribute attr = new H5gAttribute(h5file,
                    atts.getValue(H5gConstants.ATTR_NAME), (HObject)currentObj);
            currentObj = attr;
        } else if( H5gConstants.ELEM_DATASET.equals(localName) )
        {
            h5Objects.push( currentObj );

            // TODO: need to support compound datatype
            H5gScalarDS dataset = new H5gScalarDS(h5file,
                    atts.getValue(H5gConstants.ATTR_NAME),
                    atts.getValue(H5gConstants.ATTR_PATH));

            Hashtable attrib = dataset.getAttributes();
            attrib.put( H5gConstants.ATTR_XOID, atts.getValue(H5gConstants.ATTR_XOID) );
            attrib.put( H5gConstants.ATTR_PARENTS, atts.getValue(H5gConstants.ATTR_PARENTS) );
            dataset.setParent( (H5gGroup)currentObj );
            ((H5gGroup)currentObj).addToMemberList( dataset );

            // - Do not do checkPtrTable() here.
            // - checkPtrTable() for Dataset should be done
            //   when this dataset is created, ie, when the startElement of
            //   "NamedDataTypePtr" or the endElement of "DataType" is reached.
            currentObj = dataset;
        } else if( H5gConstants.ELEM_DATASET_PTR.equals( localName ) )
        {
            H5gPointer dPtr = new H5gPointer();
            dPtr.setPtrType( H5gPointer.DATASET_PTR );
            dPtr.setXMLParent( (HObject)currentObj );
            checkPtrTable( atts.getValue( H5gConstants.ATTR_XOID ), dPtr );
        } else if( H5gConstants.ELEM_NAMED_DATATYPE.equals( localName ) )
        {
            h5Objects.push( currentObj );
            H5gDatatype dType = new H5gDatatype(h5file,
                    atts.getValue(H5gConstants.ATTR_NAME),
                    atts.getValue(H5gConstants.ATTR_PATH), null);

            dType.setXMLParent( (H5gGroup)currentObj );
            Hashtable attrib = dType.getAttributes();
            attrib.put( H5gConstants.ATTR_XOID, atts.getValue(H5gConstants.ATTR_XOID) );
            attrib.put( H5gConstants.ATTR_PARENTS, atts.getValue(H5gConstants.ATTR_PARENTS) );
            ((H5gGroup)currentObj).addToMemberList( dType );

            // - Do not do checkPtrTable() for this DataType.
            //   Call checkPtrTable() when this datatype is actually commit()'ed.
            // - This is because under the following situation, it actually
            //   means a hardlink, so when line_1 is seen, it should not be
            //   put into the pointer table.
            // - checkPtrTable() will be called when line_2 is seen.
            //   <NamedDataType ...>		-- line_1
            //      <NamedDataTypePtr ...>		-- line_2
            //      </NamedDataTypePtr>
            //   </NamedDataType>
            currentObj = dType;
        } else if( H5gConstants.ELEM_NAMED_DATATYPE_PTR.equals( localName) )
        {
            H5gPointer dtPtr = new H5gPointer();
            dtPtr.setPtrType( H5gPointer.NAMED_DATATYPE_PTR );
            dtPtr.setXMLParent( (H5gGroup)currentObj );

            if( currentObj instanceof Dataset )
            {
                // we can now create the data in file since all the required are here.
                H5gScalarDS dset = (H5gScalarDS)currentObj;
                H5gDatatype dType = (H5gDatatype)getObjByPtr( atts.getValue( H5gConstants.ATTR_XOID ) );
                if (dType != null)
                {
                    dset.setDatatype( dType );
                    dset.create();
                    checkPtrTable( (String)dset.getAttributes().get( H5gConstants.ATTR_XOID ), dset);
                }
            } else
            {
                NDTPtrInNDT = true;
                checkPtrTable( atts.getValue( H5gConstants.ATTR_XOID ), dtPtr );
            }
        } else if (H5gConstants.ELEM_SOFT_LINK.equals(localName) )
        {
            String linkName = atts.getValue( H5gConstants.ATTR_LINK_NAME );
            String target = atts. getValue( H5gConstants.ATTR_TARGET_PATH );
            String targetObj = atts.getValue( H5gConstants.ATTR_TARGET_OBJ );
            String obj_Xid = atts.getValue( H5gConstants.ATTR_XOID );

            if( !(currentObj instanceof Group) )
                throwSAXException( "Error: the parent of a softlink is not a group", null );

            Group grp = (Group)currentObj;
            String pPath = grp.getFullName();
            if ( pPath.charAt( pPath.length()-1 ) != '/' )
                pPath += "/";

            // relative path
            if (!linkName.startsWith("/"))
                linkName = pPath + linkName;

            H5.H5Glink( grp.getFID(), HDF5Constants.H5G_LINK_SOFT, target, linkName );
        } else if( H5gConstants.ELEM_STORAGE_LAYOUT.equals( localName ) )
        {
            ((H5gScalarDS)currentObj).createPlist();
        }
        else if( H5gConstants.ELEM_CONTIGUOUS_LAYOUT.equals( localName ) )
        {
            // default layout is contigous, nothing more is needed;
        }
        else if( H5gConstants.ELEM_CHUNKED_LAYOUT.equals( localName ) )
        {
            ((H5gScalarDS)currentObj).setLayout( HDF5Constants.H5D_CHUNKED );
            int ndims = Integer.parseInt( atts.getValue(H5gConstants.ATTR_NDIMS) );
            ((H5gScalarDS)currentObj).setChunkRank( ndims );
        }
        else if( H5gConstants.ELEM_COMPACT_LAYOUT.equals( localName ) )
        {
            ((H5gScalarDS)currentObj).setLayout( HDF5Constants.H5D_COMPACT );
        }
        else if( H5gConstants.ELEM_EXTERNAL_LAYOUT.equals( localName ) )
        {
            throwSAXException( "ERROR: ExternalLayout is not supported.", null );
        }
        else if( H5gConstants.ELEM_COMPRESSION.equals( localName ) )
        {
            lastLocalName = localName;
        }
        else if( H5gConstants.ELEM_CHUNK_DIMENSION.equals( localName ) )
        {
            int dimSize = Integer.parseInt( atts.getValue(H5gConstants.ATTR_DIM_SIZE) );
            ((H5gScalarDS)currentObj).setChunkDims( dimSize );
        }
        else if( H5gConstants.ELEM_DATATYPE.equals( localName ) )
        {
            H5gDatatype dType = new H5gDatatype( false );
     dType.setXMLParent( currentObj );

     // - Consider such an example:
     //   <DataType>    			-- dt_1    	line_1
     //      <CompoundType>			--         	line_2
     //         <Field_1>			--		line_3
     //            <DataType>		-- dt_2		line_4
     //               <ArrayType>   	-- 		line_5
     //                  ...
     //                  <DataType>		-- dt_3         line_6
     //                     <AtomicType>    --		line_7
     //                        <IntegerType .../> -- 	line_8
     //                     </AtomicType>   --		line_9
     //                  </DataType>	-- dt_3		line_10
     //               </ArrayType>   	-- 		line_11
     //            </DataType>		-- dt_2		line_12
     //         </Field_1>			--		line_13
     //
     //         <Field_2>			--		line_14
     //            <DataType>		-- dt_4		line_15
     //         ...
     //            </DataType>		-- dt_4		line_16
     //         </Field_2>			--		line_17
     //      </CompoundType>	 	--         	line_18
     //   </DataType>    			-- dt_1    	line_19
     //
     // - A CompoundType should only cares about its children, and
     //   let its childern to care about their children. The same should hold
     //   for ArrayType and VLType. Because when the endElement of a child is
     //   seen, that child should have been created (unless the child is a
     //   forward pointer).
     //
     // - This is what the code does:
     //   The CompoundType dt_1 adds its child dt_2 to itself as
     //   a component, and this happens when the line_12 is seen, at which
     //   time dt_2 is done. And then dt_1 adds dt_4 to itself as a component,
     //   when line_16 is seen, by which time dt_4 is done.
     // - Similar analysis goes to ArrayType

     h5Objects.push( currentObj );
     currentObj = dType;
      }

      else if( localName.equals( "AtomicType" ) )
      {  ((H5gDatatype)currentObj).setCategory( H5gDatatype.ATOMIC_TYPE );
      }

      else if( localName.equals( "CompoundType" ) )
      {  ((H5gDatatype)currentObj).setCategory( H5gDatatype.COMPOUND_TYPE );
     H5Object p = ((H5gDatatype)currentObj).getXMLParent();
      }

      else if( localName.equals( "VLType" ) )
      {
        ((H5gDatatype)currentObj).setCategory( H5gDatatype.VL_TYPE );
      }

      else if( localName.equals( "IntegerType" ) )
      {  String byteOrder = atts.getValue( H5gDatatype.BYTE_ORDER );
     String sign = atts.getValue( H5gDatatype.SIGN );
     String size = atts.getValue( H5gDatatype.SIZE );

     H5gDatatype dType = (H5gDatatype)currentObj;
     // - dType will be modified/updated by the function, that is,
     //   dType holds the returned value.
         parseIntType( dType, byteOrder, sign , size );
      }

      else if( localName.equals( "FloatType" ) )
      {  String byteOrder = atts.getValue( H5gDatatype.BYTE_ORDER );
     String size = atts.getValue( H5gDatatype.SIZE );
     String signBitLoc = atts.getValue( H5gDatatype.SIGN_BIT_LOCATION );
     String expBit = atts.getValue( H5gDatatype.EXPONENT_BITS );
     String expLoc = atts.getValue( H5gDatatype.EXPONENT_LOCATION );
     String mtsBit = atts.getValue( H5gDatatype.MANTISSA_BITS );
     H5gDatatype dType = (H5gDatatype)currentObj;
     H5Object p = dType.getXMLParent();

     int s = -1;

     // - Since the size has to match the previous bits and
     //   bit location values, we can't set a default value
     //   FloatType size. So, the program should be terminated
     //   if a NumberFormatException is caught or if the size
     //   is not 4 or 8
     try
     {  s = Integer.parseInt( size );
     }
     catch( NumberFormatException ne )
     {  errMsg = "ERROR: NumberFormatException with FloatType size: "
             + size;
        if( isCmdLineTool() )
        {  printErrMsg( errMsg );
           exitOnError();
        }
        else
           throwSAXException( errMsg, ne );
     }

     if( !(s==4 || s==8) )
     {  errMsg = "ERROR: Invalid float size " + s + " for object: " +
             currentObj.getPath();
        if( isCmdLineTool() )
        {  printErrMsg( errMsg );
           exitOnError();
        }
        else
           throwSAXException( errMsg, null );
     }

     // - We should set the size this way instead of only using
     //   H5gDatatype.setSize(), which uses H5.H5Tset_size().
     //   Using H5.H5Tset_size() to set the size as 8 doesn't
     //   work for JH5T_NATIVE_FLOAT.
     if( s == 4 )
        dType.setClassType( HDF5Constants.H5T_NATIVE_FLOAT );
     else if( s == 8 )
        dType.setClassType( HDF5Constants.H5T_NATIVE_DOUBLE );

     dType.create();
     dType.setSize( s );

     if( byteOrder.equals( "BE" ) )
        dType.setOrder( HDF5Constants.H5T_ORDER_BE );
     else if( byteOrder.equals( "LE" ) )
        dType.setOrder( HDF5Constants.H5T_ORDER_LE );
     else
     {  errMsg = "ERROR: The value for ByteOrder should be either " +
             "\"BE\" or \"LE\".";
        if( isCmdLineTool() )
        {  printErrMsg( errMsg );
           exitOnError();
        }
        else
           throwSAXException( errMsg, null );
     }

     int mpos = -1;
     try
     {  mpos = Integer.parseInt(
              atts.getValue( H5gDatatype.MANTISSA_LOCATION ) );
     }
     catch( NumberFormatException ne )
     {  errMsg = "ERROR: NumberFormatException with FloatType " +
             "MantissaLocation: " +
             atts.getValue(H5gDatatype.MANTISSA_LOCATION);
        if( isCmdLineTool() )
        {  printErrMsg( errMsg );
           exitOnError();
        }
        else
           throwSAXException( errMsg, null );
     }

     if( mpos < 0 )
     {  errMsg = "ERROR: MantissaLocation is negative: " + mpos;
        if( isCmdLineTool() )
        {  printErrMsg( errMsg );
           exitOnError();
        }
        else
           throwSAXException( errMsg, null );
     }

     int signbitloc = -1;
     int exploc = -1;
     int expbit = -1;
     int mtsbit = -1;
     try
     {  signbitloc = Integer.parseInt( signBitLoc );
     }
     catch( NumberFormatException ne )
     {  errMsg = "ERROR: NumberFormatException with FloatType " +
             "SignBigLocation: " + signBitLoc;
        if( isCmdLineTool() )
        {  printErrMsg( errMsg );
           exitOnError();
        }
        else
           throwSAXException( errMsg, null );
     }

     try
     {  exploc = Integer.parseInt( expLoc );
     }
     catch( NumberFormatException ne )
     {  errMsg = "ERROR: NumberFormatException with FloatType " +
             "ExponentLocation: " + expLoc;
        if( isCmdLineTool() )
        {  printErrMsg( errMsg );
           exitOnError();
        }
        else
           throwSAXException( errMsg, null );
     }

     try
     {  expbit = Integer.parseInt( expBit );
     }
     catch( NumberFormatException ne )
     {  errMsg = "ERROR: NumberFormatException with FloatType " +
             "ExponentBits: " + expBit;
        if( isCmdLineTool() )
        {  printErrMsg( errMsg );
           exitOnError();
        }
        else
           throwSAXException( errMsg, null );
     }

     try
     {  mtsbit = Integer.parseInt( mtsBit );
     }
     catch( NumberFormatException ne )
     {  errMsg = "ERROR: NumberFormatException with FloatType " +
             "MantissaBits: " + mtsBit;
        if( isCmdLineTool() )
        {  printErrMsg( errMsg );
           exitOnError();
        }
        else
           throwSAXException( errMsg, null );
     }

     dType.setFields( signbitloc, exploc, expbit, mpos, mtsbit );
      }

      else if( localName.equals( "StringType" ) )
      {  H5gDatatype dType = (H5gDatatype)currentObj;
     dType.setClassType( HDF5Constants.H5T_C_S1 );
     dType.setH5ClassType( HDF5Constants.H5T_STRING );

     dType.create();
     if( !atts.getValue("Cset").equals( "H5T_CSET_ASCII") )
     {  if( isCmdLineTool() )
        {  printWarningMsg(
             "WARNING: " + atts.getValue("Cset")
             + " currently is not supported."
             + "\n         Only \"H5T_CSET_ASCII\" is supported "
             + "for \"Cset\" field."
             + "\n         Set to H5T_CSET_ASCII"
             + "\n         Associated object is: "
             + currentObj.getPath() );
        }
     }
     dType.setCSet( HDF5Constants.H5T_CSET_ASCII );
     int s = -1;
     try
     {  s = Integer.parseInt( atts.getValue("StrSize") );
     }
     catch( NumberFormatException ne )
     {  if( isCmdLineTool() )
        {  printWarningMsg(
             "WARNING: NumberFormatException with StringType StrSize: "
             + "\n         " + atts.getValue( "StrSize" )
             + "\n         Set to default size 10"
             + "\n         Associated object is: " + currentObj.getPath() );
        }
        s = 10;
     }

     if( s <= 0 )
     {  if( isCmdLineTool() )
        {  printWarningMsg(
             "WARNING: Invalid string size " + s + " for object:"
             + "\n         " + currentObj.getPath()
             + "\n         Set to default size 10"
             + "\n         Associated object is: "
             + currentObj.getPath() );
        }
        s = 10;
     }
     dType.setSize( s );

     String spad = atts.getValue( "StrPad" );
     if( spad.equals( "H5T_STR_NULLTERM" ) )
        dType.setStrPad( HDF5Constants.H5T_STR_NULLTERM );
     else if( spad.equals( "H5T_STR_NULLPAD" ) )
        dType.setStrPad( HDF5Constants.H5T_STR_NULLPAD );
     else if( spad.equals( "H5T_STR_SPACEPAD" ) )
        dType.setStrPad( HDF5Constants.H5T_STR_SPACEPAD );
     else
     {  if( isCmdLineTool() )
        {  printWarningMsg(
             "WARNING: \"StrPad\" field has to be one of:"
             + "\n         \"H5T_STR_NULLTERM\", \"H5T_STR_NULLPAD\", "
             + "or \"H5T_STR_SPACEPAD\"."
             + "\n         Set to default \"H5T_STR_NULLTERM\""
             + "\n         Associated object is: "
             + currentObj.getPath() );
        }
        dType.setStrPad( HDF5Constants.H5T_STR_NULLTERM );
     }
      }

      else if( localName.equals( "TimeType" ) )
      {  H5gDatatype dType = (H5gDatatype)currentObj;
      }

      else if( localName.equals( "BitfieldType" ) )
      {  String byteOrder = atts.getValue( H5gDatatype.BYTE_ORDER );
     String size = atts.getValue( H5gDatatype.SIZE );

     H5gDatatype dType = (H5gDatatype)currentObj;
     H5Object p = dType.getXMLParent();

     if( !byteOrder.equals( "BE" ) && !byteOrder.equals( "LE" ) )
     {  errMsg = "ERROR: The value for ByteOrder should be either " +
             "\"BE\" or \"LE\".";
        if( isCmdLineTool() )
        {  printErrMsg( errMsg );
           exitOnError();
        }
        else
           throwSAXException( errMsg, null );
     }

     int s = -1;
     try
     {  s = Integer.parseInt( size );
     }
     catch( NumberFormatException ne )
     {  if( isCmdLineTool() )
        {  printWarningMsg(
             "WARNING: NumberFormatException with BitFieldType size: "
             + size + "\n         Set to default value 4."
             + "\n         Associated object is: "
             + currentObj.getPath() );
        }
        s = 4;
     }

     if( !(s==1 || s==2 || s==4 || s==8) )
     {  if( isCmdLineTool() )
        {  printWarningMsg(
             "WARNING: Invalid integer size " + s + " for object: "
             + "\n         Set to default size 4"
             + "\n         Associated object is: "
             + currentObj.getPath() );
        }
        s = 4;
     }

    if( s == 1 )
    {  if (byteOrder.equals("LE"))
          dType.setClassType( HDF5Constants.H5T_STD_B8LE );
       else if (byteOrder.equals("BE"))
          dType.setClassType( HDF5Constants.H5T_STD_I8BE );
    }
    else if( s == 2 )
    {  if (byteOrder.equals("LE"))
          dType.setClassType( HDF5Constants.H5T_STD_B16LE );
       else if (byteOrder.equals("BE"))
          dType.setClassType( HDF5Constants.H5T_STD_B16BE );
    }
    else if( s == 4 )
    {  if (byteOrder.equals("LE"))
          dType.setClassType(  HDF5Constants.H5T_STD_B32LE );
       else if (byteOrder.equals("BE"))
          dType.setClassType( HDF5Constants.H5T_STD_B32BE );
    }
    else if ( s == 8 )
    {  if (byteOrder.equals("LE"))
          dType.setClassType( HDF5Constants.H5T_STD_B64LE );
       else if (byteOrder.equals("BE"))
          dType.setClassType( HDF5Constants.H5T_STD_B64BE );
    }

    dType.create();
    dType.setSize( s );
      }

      else if( localName.equals( "OpaqueType" ) )
      {  H5gDatatype dType = (H5gDatatype)currentObj;
     String size = atts.getValue( H5gDatatype.SIZE );
     String tag = atts.getValue( H5gDatatype.TAG );
     int s = -1;
     try
     {  s = Integer.parseInt( size );
     }
     catch( NumberFormatException ne )
     {  if( isCmdLineTool() )
        {  printWarningMsg(
             "WARNING: NumberFormatException with OpaqueType size: "
             + size + "\n         Set to default value 4."
             + "\n         Associated object is: "
             + currentObj.getPath() );
        }
        s = 1;
     }
     dType.setClassType( HDF5Constants.H5T_OPAQUE );
     dType.create();
     dType.setOpaqueType( s );
     dType.setTag( tag );
      }
      else if( localName.equals( "ReferenceType" ) )
      {  H5gDatatype dType = (H5gDatatype)currentObj;
      }
      else if( localName.equals( "EnumType" ) )
      {  H5gDatatype dType = (H5gDatatype)currentObj;

     dType.createEnum( HDF5Constants.H5T_NATIVE_INT );
     h5Objects.push( dType );
     currentObj = dType.new EnumType();
      }

      else if( localName.equals( "ObjectReferenceType" ) )
      {  ((H5gDatatype)currentObj).setRefType( HDF5Constants.H5T_STD_REF_OBJ );
      }
      else if( localName.equals( "RegionReferenceType" ) )
      {  ((H5gDatatype)currentObj).setRefType( HDF5Constants.H5T_STD_REF_DSETREG );
      }


      // - "Field" for CompoundType
      else if( localName.equals( "Field" ) )
      {  ((H5gDatatype)currentObj).addFieldName( atts.getValue("FieldName") );
      }

      else if( localName.equals( "ArrayType" ) )
      {
    int  ndims = -1;
    try {
     ndims = Integer.parseInt(atts.getValue("Ndims"));
     } catch( NumberFormatException ne )
     {  if( isCmdLineTool() )
        {  printWarningMsg(
             "WARNING: NumberFormatException with Array Ndims: "
             + atts.getValue( "Ndims" )
             + "\n         Set to default value 1"
             + "\n         Associated object is: "
             + currentObj.getPath() );
        }
        ndims = 1;
     }

     if( ndims <= 0 )
     {  if( isCmdLineTool() )
        {  printWarningMsg(
             "WARNING: \"ndims\" for Array is non-positive."
             + "\n         "
             + "\n         Associated object is: "
             + currentObj.getPath() );
        }
        ndims = 1;
     }

        H5gDatatype thedt = (H5gDatatype)currentObj;
        thedt.setCategory( H5gDatatype.ARRAY_TYPE );
        thedt.setArrayDimsRank( ndims );
      }

      else if( localName.equals( "ArrayDimension" ) )
      {  int dimSize = -1;
         try
     {  dimSize = Integer.parseInt( atts.getValue("DimSize"));
     }
     catch( NumberFormatException ne )
     {  if( isCmdLineTool() )
        {  printWarningMsg(
             "WARNING: NumberFormatException with ArrayDimension: "
             + "\n         " +atts.getValue( "DimSize" )
             + "\n         Set to default size 1"
             + "\n         Associated object is: "
             + currentObj.getPath() );
        }
        dimSize = 1;
     }
     if( dimSize < 0 )
     {  if( isCmdLineTool() )
        {  printWarningMsg(
             "WARNING: ArrayDimension " + dimSize + " is negative."
             + "\n         Set to default value 1."
             + "\n         Associated object is: "
             + currentObj.getPath() );
        }
        dimSize = 1;
     }
     if (dimSize == 0) { // scalar == 1???
        dimSize = 1;
     }
         ((H5gDatatype)currentObj).setArrayDims( dimSize );
      }

      else if( localName.equals( "EnumElement" ) )
      {  if( !((H5gDatatype.EnumType)currentObj).shouldSetName() )
         {  errMsg = "ERROR: EnumElement and EnumValue should come in pairs.";
        if( isCmdLineTool() )
        {  printErrMsg( errMsg );
           exitOnError();
        }
        else
           throwSAXException( errMsg, null );
     }
      }
      else if( localName.equals( "EnumValue" ) )
      {  if( ((H5gDatatype.EnumType)currentObj).shouldSetName() )
         {  errMsg = "ERROR: EnumElement and EnumValue should come in pairs.";
        if( isCmdLineTool() )
        {  printErrMsg( errMsg );
           exitOnError();
        }
        else
           throwSAXException( errMsg, null );
     }
      }

      else if( localName.equals( "Dataspace" ) )
      {  // - This shouldn't happen if the XML is valid.
         if( currentObj.getType()!=H5Object.DATASET &&
             currentObj.getType()!=H5Object.ATTRIBUTE)
         {  errMsg = "\nERROR: Current object type error in Dataspace!";
        if( isCmdLineTool() )
        {  printErrMsg( errMsg );
           exitOnError();
        }
        else
           throwSAXException( errMsg, null );
         }
      }

      else if( localName.equals( "SimpleDataspace" ) )
      {  int ndims = -1;
         try
     {  ndims = Integer.parseInt(atts.getValue("Ndims"));
     }
     catch( NumberFormatException ne )
     {  if( isCmdLineTool() )
        {  printWarningMsg(
             "WARNING: NumberFormatException with SimpleDataspace: "
             + atts.getValue( "Ndims" )
             + "\n         Set to default value 1"
             + "\n         Associated object is: "
             + currentObj.getPath() );
        }
        ndims = 1;
     }

     if( ndims <= 0 )
     {  if( isCmdLineTool() )
        {  printWarningMsg(
             "WARNING: \"ndims\" for SimpleDataspace is non-positive."
             + "\n         "
             + "\n         Associated object is: "
             + currentObj.getPath() );
        }
        ndims = 1;
     }

         if( currentObj instanceof DATASET )
            ((H5gScalarDS)currentObj).setRank( ndims );
         else if( currentObj instanceof ATTRIBUTE )
            ((H5gAttribute)currentObj).setRank( ndims );

     // - This shouldn't happen.
         else
     {  errMsg = "ERROR: Current object type error for SimpleDataspace.";
        if( isCmdLineTool() )
        {  printErrMsg( errMsg );
           exitOnError();
        }
        else
           throwSAXException( errMsg, null );
     }
      }

      else if( localName.equals( "Dimension" ) )
      {  int maxDim = -1;
         int dim = -1;
     try
     {  dim = Integer.parseInt( atts.getValue( "DimSize" ) );
     }
     catch( NumberFormatException ne )
     {  if( isCmdLineTool() )
        {  printWarningMsg(
             "WARNING: NumberFormatException with Dimension DimSize: "
             + atts.getValue( "DimSize" )
             + "\n         Set to default value 1"
             + "\n         Associated object is: "
             + currentObj.getPath() );
        }
        dim = 1;
     }

     if( dim <= 0 )
     {  if( isCmdLineTool() )
        {  printWarningMsg(
             "WARNING: Dimension " + dim + " is non-positive." +
             "\n         Set to default value 1."
             + "\n         Associated object is: "
             + currentObj.getPath() );
        }
        dim = 1;
     }

     String md = atts.getValue( "MaxDimSize" );
     if( md == null )
        maxDim = dim;
     else if( md.equals( "UNLIMITED" ) )
     {  maxDim = HDF5Constants.H5S_UNLIMITED;
     }
     else
         {  try
        {  maxDim = Integer.parseInt( md );
        }
        catch( NumberFormatException ne )
        {  if( isCmdLineTool() )
           {  printWarningMsg(
            "WARNING: NumberFormatException with Dimension DimSize: "
            + md + "\n         Set to DimSize " + dim
            + "\n         Associated object is: "
            + currentObj.getPath() );
           }
           maxDim = dim;
        }
     }

         if( currentObj instanceof DATASET )
            ((H5gScalarDS)currentObj).setDims( dim, maxDim );
         else if( currentObj instanceof ATTRIBUTE )
            ((H5gAttribute)currentObj).setDims( dim, maxDim );

     // - This shouldn't happen.
         else
     {  errMsg = "ERROR: Current object type error for Dimension.\n" +
             "\n       Current object is: " + currentObj.getPath();
        if( isCmdLineTool() )
        {  printErrMsg( errMsg );
           exitOnError();
        }
        else
           throwSAXException( errMsg, null );
     }
      }

      else if( localName.equals( "ScalarDataspace" ) )
      {  if( currentObj instanceof DATASET )
             ((H5gScalarDS)currentObj).createDSpace( HDF5Constants.H5S_SCALAR );
          else if( currentObj instanceof ATTRIBUTE )
             ((H5gAttribute)currentObj).createDSpace( HDF5Constants.H5S_SCALAR );

     // - This shouldn't happen.
         else
     {  errMsg = "ERROR: Current object type error for ScalarDataspace."
             + "\n       Current object is: " + currentObj.getPath();
        if( isCmdLineTool() )
        {  printErrMsg( errMsg );
           exitOnError();
        }
        else
           throwSAXException( errMsg, null );
     }
      }

      else if( localName.equals( "Data" ) )
      {
      }
      else if( localName.equals( "DataFromFile" ) )
      {  lastLocalName = localName;
         mustInitRead=true;  // ready for first read
      }
      else if( localName.equals( "NoData" ) )
      {  lastLocalName = localName;
      }
      else if( localName.equals( "NativeHDF5" ) )
      {  lastLocalName = localName;
     if( isCmdLineTool() )
     {  printWarningMsg(
          "WARNING: NativeHDF5 is not currently supported. "
          + "It is ignored"
          + "\n         Associated object is: " + currentObj.getPath() );
         }
      }
      else
      {  errMsg = "ERROR: un-supported or undefined entity type: "+localName;
         if( isCmdLineTool() )
     {  printErrMsg( errMsg );
        exitOnError();
     }
     else
        throwSAXException( errMsg, null );
      }
   }

     /**
       *  (Required by the ContentHandler interface. )
       *
       *  Called once for each close tag encountered.
       *
       *  This routine must determine which tag is being left
       *  and take appropriate action.  Each HDF5 object has different
       *  required action.
       *
       *  Get the name of the tag (localName) and act accordingly.
       *
       *  Usually will create an HDF5 object and pop the stack.
       *  Some XML elements have no action required here.
       *
       *  The java object should have all the information from the
       *  XML, so an appropriate HDF5 object should be created.
       *
       */

   public void endElement( String namespaceURI, String localName,
         String rawName ) throws SAXException
   {
      // DEBUG
      // - Keep these print statements, because if any error happens,
      //   these print results can help to locate the problem.
      if( verbose >= ERR_STACK_WAR_PARS )
      {  System.err.print( "\nEnter H5gen::endElement()\n" );
     System.err.print( "   namespaceURI = " + namespaceURI +
               ",   localName = " + localName +
               ",   rawName = " + rawName + "\n" );
      }


      if( localName.equals( "HDF5-File" ) )
      {
      }
      else if( localName.equals( "RootGroup" ) )
      {  currentObj = (H5Object)(h5Objects.pop());
      }
      else if( localName.equals( "Group" ) )
      {  // - If currentObj is a GroupPtr, then we pop twice to pop the Group
     //   object from the stack. Refer to the startElement and endElement
     //   of "GroupPtr" implementations.
         if( currentObj instanceof H5gPointer &&
           ((H5gPointer)currentObj).getPtrType() == H5gPointer.GROUP_PTR )
     {  h5Objects.pop();
        currentObj = (H5Object)h5Objects.pop();
     }

     // - Generally speaking, a group is created when its first child is
     //   seen by the parser. But if this group contains nothing, it has
     //   to be created when the endElement of this "Group" is seen.
     // - Note that the implementation of createGroup(), multiple invocations
     //   on the same group won't hurt.
     else
     {  H5gGroup group = (H5gGroup)currentObj;
        createGroup( group );
        currentObj = (H5Object)(h5Objects.pop());
     }
      }

      else if( localName.equals( "GroupPtr" ) )
      {
         // - Don't pop here, so when the endElement of "Group" is reached,
     //   we now that this group is actually a link, therefore we don't
     //   create this group.
      }

      else if( localName.equals( "Attribute" ) )
      {
     H5gObjRefList dslist = ((H5gAttribute)currentObj).getForwardRefs();
     if ((dslist != null) && (dslist.getSize() > 0)) {
        forwardobjRefs.add(dslist);
     }

         try{ ((H5gAttribute)currentObj).close(); }
     catch( Throwable t ) { throw (SAXException)t; }
     currentObj = (H5Object)(h5Objects.pop());
      }

      else if( localName.equals( "Dataset" ) )
      {  ((H5gScalarDS)currentObj).closeStorePrpty();
    H5gObjRefList dslist = ((H5gScalarDS)currentObj).getForwardRefs();
    if ((dslist != null) && (dslist.getSize() > 0)) {
        forwardobjRefs.add(dslist);
    }

         try{ ((H5gScalarDS)currentObj).close(); }
     catch( Throwable t ) { throw (SAXException)t; }
         currentObj = (H5Object)(h5Objects.pop());
      }

      else if( localName.equals( "DatasetPtr" ) )
      {
      }

      else if( localName.equals( "Dataspace" ) )
      {  // - This shouldn't happen
         int t = currentObj.getType();
         if( t!=H5Object.DATASET && t!=H5Object.ATTRIBUTE )
         {  errMsg = "\nDataspace parent error!";
        if( isCmdLineTool() )
        {  printErrMsg( errMsg );
           exitOnError();
        }
        else
           throwSAXException( errMsg, null );
         }
      }

      else if( localName.equals( "SimpleDataspace" ) )
      {  if( currentObj instanceof DATASET )
         {  H5gScalarDS dset = (H5gScalarDS)currentObj;
        if( !dset.allDimsSet() )
        {  errMsg = "ERROR: Number of dimensions does not match " +
            "Ndims' value for: " + dset.getPath();
           if( isCmdLineTool() )
           {  printErrMsg( errMsg );
          exitOnError();
           }
           else
              throwSAXException( errMsg, null );
        }
        dset.createDSpace( HDF5Constants.H5S_SIMPLE );
     }

         else if( currentObj instanceof ATTRIBUTE )
         {  H5gAttribute h5Attrib = (H5gAttribute)currentObj;
        if( !h5Attrib.allDimsSet() )
        {  errMsg = "ERROR: Number of dimensions set in XML file does not match " +
            "Ndims' value for:\n       " + h5Attrib.getPath();
           if( isCmdLineTool() )
           {  printErrMsg( errMsg );
          exitOnError();
           }
           else
              throwSAXException( errMsg, null );
        }
        h5Attrib.createDSpace( HDF5Constants.H5S_SIMPLE );
     }
      }

      else if( localName.equals( "Dimension" ) )
      {

      }

      else if( localName.equals( "ScalarDataspace" ) )
      {
      }

      else if( localName.equals( "NamedDataType" ) )
      {  H5gDatatype dType = (H5gDatatype)currentObj;

     // NOTE: IMPORTANT:
     // - Read this comment before making any changes in this
     //   if() block, and update the comment (it would be better to
     //   mark where changes will be made and add new comment
     //   after such marks).
     // - TRY NOT TO MIDIFY THIS PART OF CODE BEFORE UNDERSTANDING
     //   BOTH THE COMMENT AND THE CODE.

     // - For both SITUATION_1:
     //
     //      <NamedDataType ...>
     //         <DataType>
     //         ...
     //         </DataType>
     //      </NamedDataType> 	-- this is the current "localName"
     //
     //   and SITUATION_2:
     //
     //      <NamedDataType ...>
     //         <NamedDataTypePtr ...>
     //         </NamedDataTypePtr>
     //      </NamedDataType>	-- this is the current "localName"
     //
     //   dType.isNamedType() is true. But the two situations require
     //   different handling.
     // - For SITUATION_1, H5gDataType.commit() is called to when the
     //   endElement of "NamedDataType" is seen.
     // - For SITUATION_2, H5.H5Glink() is called() to create a hard link
     //   when the startElement of "NamedDataTypePtr" is seen.
     //   (Note that H5.H5Glink() is actually done through checkPtrTable() )
     //   And for SITUATION_2, when we seen the endElement of "NamedDataType",
     //   we do nothing except poping the stack.
     //
     // - A flag is used: *NDTPtrInNDT*, which is a shorthand of
     //   *NamedDataTypePtr_in_NamedDataType*, and is set/unset according
     //   to the following diagram (default to be false):
     //   <NamedDataType ...>
     //      <NamedDataTypePtr .../>	-- set to be true
     //   </NamedDataType>			-- set to be false

     if( NDTPtrInNDT )
     {  currentObj = (H5Object)h5Objects.pop();
        NDTPtrInNDT = false;
        // - SHOULD not call commit() and checkPtrTable() here.
        return;
     }

     dType.commit();
     checkPtrTable( (String)dType.getAttributes().get(dType.OBJ_XID), dType );
     try{ dType.close(); }
     catch( Throwable t ) { throw (SAXException)t; }
     currentObj = (H5Object)h5Objects.pop();
     return;
      }

      else if( localName.equals( "NamedDataTypePtr" ) )
      {
      }
      else if( localName.equals( "StorageLayout" ) )
      {
      }
      else if( localName.equals( "ContiguousLayout" ) )
      {
      }
      else if( localName.equals( "ChunkedLayout" ) )
      {  ((H5gScalarDS)currentObj).setChunk();
      }
      else if( localName.equals( "CompactLayout" ) )
      {
      }
      else if( localName.equals( "ExternalLayout" ) )
      {
      }

      else if( localName.equals( "Compression" ) )
      {  lastLocalName = null;
         if( !dataProcessed )
     {  if( isCmdLineTool() )
        {  printWarningMsg(
             "WARNING: Compression value not provided for dataset: "
             + "\n         set to default value."
             + "\n         Associated object is: " + currentObj.getPath() );
        }
     }
     dataProcessed = false;
      }
      else if( localName.equals( "ChunkDimension" ) )
      {
      }


      else if( localName.equals( "DataType" ) )
      {
        H5gDatatype dType = (H5gDatatype)currentObj;

     // - When we pop the stack, if the parent of the current
     //   datatype object is Dataset or Attribute, we should
     //   pop the stack under this situation.
     //   Otherwise, the parent is NamedDataType, ArrayType,
     //   CompoundType or VLType, then we don't want to pop and close
     //   the datatype at this time.
     H5Object p = dType.getXMLParent();
     if( p.getType() == H5Object.DATASET )
     {
        ((H5gScalarDS)p).createDSet( dType );
        checkPtrTable( (String)((H5gScalarDS)p).getAttributes().
          get( ((H5gScalarDS)p).OBJ_XID ), p );
     }
     else if( p.getType() == H5Object.ATTRIBUTE )
     {  ((H5gAttribute)p).createAttrib( dType.getFileTid(),
              HDF5Constants.H5P_DEFAULT );
     }
     else if( p.getType() == H5Object.GROUP )
     {  errMsg = "ERROR: DataType parent error. DataType object is:"
                 + "       " + dType.getPath();
        if( isCmdLineTool() )
        {  printErrMsg( errMsg );
           exitOnError();
        }
        else
          throwSAXException( errMsg, null );
     }
     else if( p.getType() == H5Object.DATATYPE )
     {
        // NOTE: IMPORTANT:
        // - Read this comment before making any changes in this
        //   if() block, and update the comment (it would be better to
        //   mark where changes will be made and add new comment
        //   after such marks).
        // - TRY NOT TO MIDIFY THIS PART OF CODE BEFORE UNDERSTANDING
        //   BOTH THE COMMENT AND THE CODE.

        // - we do not want to close dType if we goes into this if() block,
        //   because we need the *ftid* (see H5gDatatype.java for *ftid*) of
        //   this datatype for its parent datatype, and its parent datatype
        //   can be a NamedDataType, a CompoundType, an ArrayType, or a VLType.
        H5gDatatype pType = (H5gDatatype)p;

        // - For such a situation:
        //   <NamedDataType ...>	-- dt_1		line_1
        //      <DataType>		-- dt_2		line_2
        //      ...
        //      </DataType>		-- dt_2		line_3
        //   </NamedDataType>	-- dt_1		line_4
        //
        // - Now line_3 is seen by the parser.
        //
        // - dt_2 (DataType) is inside dt_1 (NamedDataType),
        //   shouldn't close this dt_2 now, because its
        //   *ftid* (see H5gDatatype.java for *ftid*) is needed when
        //   commit() dt_1 when line_4 is seen.
        //   block, this DataType is not closed.
        //
        //   dt_2 is is closed after the call of commit() for dt_1.
        //   Either call close() on dt_2 or on dt_1, but not on both.
        //   In this implementation, close() is called on dt_1 when
        //   line_4 is seen by the parser.

        if( pType.isNamedType() )
           pType.setDTypeInNamedDType( dType );
        else if( pType.getCategory() == H5gDatatype.COMPOUND_TYPE )
           pType.addComponent( dType );
        else if( pType.getCategory() == H5gDatatype.ARRAY_TYPE )
        {
           pType.setDTypeBaseType( dType );

           // - initTotalSize() should not be called. When the code reachs here,
           //   the parser has seen all the ArrayDimension for this ArrayType,
           // - When it sees ArrayDimension, the parser calculates the
           //   total number of elements in the array, so after the parser
           //   has seen all the ArrayDimension for this ArrayType,
           //   the total size is set to be the number of elements in the array.
           //   Calling initTotalSize() now will set the total size to be
           //   its base type total size, which makes the parser loose
           //   the stored information of numbe of elements in the array.
        }
        else if( pType.getCategory() == H5gDatatype.VL_TYPE )
        {  pType.setDTypeBaseType( dType );
           // - initTotalSize() should not be called. Refer to
           //   the comments for ARRAY_TYPE.
        }

        currentObj = (H5Object)h5Objects.pop();
        // - Note: Should not close dType. Just return.
        // - See the beginning of this if() block for comments
        return;
     }

     // - This shouldn't happen.
     else
     {  errMsg = "ERROR: DataType parent type error. Datatype object is:" +
             "\n       " + dType.getPath();
        if( isCmdLineTool() )
        {  printErrMsg( errMsg );
           exitOnError();
        }
        else
          throwSAXException( errMsg, null );
     }


     currentObj = (H5Object)h5Objects.pop();
     try{ dType.close(); }
     catch( Throwable t ) { throw (SAXException)t; }
      }

      else if( localName.equals( "AtomicType" ) )
      {
      }

      else if( localName.equals( "CompoundType" ) )
      {  ((H5gDatatype)currentObj).create();
      }

      else if( localName.equals( "VLType" ) )
      {
    H5gDatatype dt = ((H5gDatatype)currentObj);
        dt.createVLType( );
      }

      else if( localName.equals( "ArrayType" ) )
      {
    H5gDatatype dt = ((H5gDatatype)currentObj);
        dt.createArrayType( );
      }
      else if( localName.equals( "ArrayDimension" ) )
      {
      }

      else if( localName.equals( "IntegerType" ) )
      {
      }
      else if( localName.equals( "FloatType" ) )
      {
      }
      else if( localName.equals( "StringType" ) )
      {
      }
      else if( localName.equals( "TimeType" ) )
      {
      }
      else if( localName.equals( "BitfieldType" ) )
      {
      }
      else if( localName.equals( "OpaqueType" ) )
      {
      }
      else if( localName.equals( "ReferenceType" ) )
      {
      }
      else if( localName.equals( "EnumType" ) )
      {  currentObj = (H5Object)h5Objects.pop();
      }
      else if( localName.equals( "ObjectReferenceType" ) )
      {
      }
      else if( localName.equals( "RegionReferenceType" ) )
      {
      }

      else if( localName.equals( "Field" ) )
      {
      }

      else if( localName.equals( "EnumElement" ) )
      {  if( !dataProcessed )
         {  errMsg = "ERROR: EnumElement value not provided.";
        if( isCmdLineTool() )
        {  printErrMsg( errMsg );
           exitOnError();
        }
        else
          throwSAXException( errMsg, null );
     }
     dataProcessed = false;
      }

      else if( localName.equals( "EnumValue" ) )
      {  if( !dataProcessed )
         {  errMsg ="ERROR: EnumValue not provided.";
        if( isCmdLineTool() )
        {  printErrMsg( errMsg );
           exitOnError();
        }
        else
           throwSAXException( errMsg, null );
     }
     dataProcessed = false;
      }
      else if( localName.equals( "Data" ) )
      {
      }
      else if( localName.equals( "DataFromFile" ) )
      {  lastLocalName = null;
         dataProcessed = false;
      }
      else if( localName.equals( "NoData" ) )
      {  lastLocalName = null;
         dataProcessed = false;
      }
      else if( localName.equals( "NativeHDF5" ) )
      {  lastLocalName = null;
         dataProcessed = false;
      }
      else if( localName.equals( "SoftLink" ) )
      {
      }
      else
      {  errMsg = "ERROR: Un-supported or undefined entity type: "+localName;
     if( isCmdLineTool() )
     {  printErrMsg( errMsg );
        exitOnError();
     }
     else
        throwSAXException( errMsg, null );
      }
   }

     /**
       *  (Required by the ContentHandler interface.)
       *
       *  This is called for CDATA elements.  The input
       *  array contains characters representing the content
       *  of the CDATA.
       *
       *  Note that the current version of xerces uses 16KB buffers.
       *
       *  This routine must parse the character array to pull
       *  out any data. E.g., data values of an HDF5 attribute
       *  or dataset will be in a CDATA element.
       */

   public void characters( char[] ch, int start, int length )
     throws SAXException
   {

      if( lastLocalName!=null && lastLocalName.equals("NoData") )
      {  lastLocalName = null;
     return;
      }

      dataProcessed = true;

      if( currentObj instanceof H5gDatatype.EnumType )
      {  H5gDatatype.EnumType eType = (H5gDatatype.EnumType)currentObj;

     String str = getOneToken( ch, start, length );
         if( eType.shouldSetName() )
     {  if( str == null )
        {  errMsg = "ERROR: no data for EnumElement" +
            "\n       " + ((H5Object)h5Objects.peek()).getPath();
           if( isCmdLineTool() )
           {  printErrMsg( errMsg );
          exitOnError();
           }
           else
              throwSAXException( errMsg, null );
        }
        eType.setName( str );
     }
         else
     {  if( str == null )
        {  errMsg = "ERROR: enum value is empty.";
           if( isCmdLineTool() )
           {  printErrMsg( "ERROR: enum value is empty." );
          exitOnError();
           }
           else
              throwSAXException( errMsg, null);
        }
        eType.setValue( str );
        ((H5gDatatype)h5Objects.peek()).enumInsert( eType );
         }

     return;
      }


      String str = new String( ch, start, length );
    if (overflowed) {
            // previous read overflowed,
            // need to tack on last bit of
            // previous block.
            if (lastPart != null) {
            String patched = new String(lastPart+str);
            str = patched;
            }
    }
      String lastBit = str.substring((str.length() - 1));
      if (lastBit.equals(",")
      || lastBit.equals(" ")
      || lastBit.equals("\t")
      || lastBit.equals("\n") ) {
    // OK, last token is complete
        lastPart = null;
        overflowed = false;
    } else {
            // OK, last token may not be complete
            // clip it off and add to the next batch
            int isp = str.lastIndexOf(" ");
            if (isp < 0) {
                lastPart = str;
                str = new String("");
            } else {
                lastPart = str.substring(isp);
                String clipped = str.substring(0,isp);
                str  = clipped;
            }
            overflowed = true;
    }
      str.trim();
      StringTokenizer sToken = null;

      if( currentObj instanceof DATASET )
      {  H5gScalarDS dset = (H5gScalarDS)currentObj;

    if (lastLocalName == null) return;
     if( lastLocalName.equals( "Compression" ) )
     {  sToken = new StringTokenizer( str, ", \t\n" );
        if( !sToken.hasMoreTokens() )
        {  if( isCmdLineTool() )
           {  printWarningMsg(
            "WARNING: Compression value not provided for dataset"
            + "\n         set to default value."
            + "\n         Associated object is: " + currentObj.getPath() );
           }
        }
        else
        {  int level = -1;
           String comp = sToken.nextToken();
           try
           {  level = Integer.parseInt( comp );
              if( level > 9 || level < 0 )
              {  if( isCmdLineTool() )
             {  printWarningMsg(
                  "WARNING: Compression ratio has to be within "
                  + "[0, 9] (inclusive)" + "\n         Set to default value."
                  + "\n         Associated object is: " + currentObj.getPath() );
             }
              }
              else
                     dset.setCompression( level );
           }
           catch( NumberFormatException ne )
           {  if( isCmdLineTool() )
              {  printWarningMsg(
               "WARNING: NumberFormatException with "
               + "Compression ratio " + comp + "\n         Set to default value"
               + "\n         Associated object is: " + currentObj.getPath() );
              }
           }
        }
         }

     else if( lastLocalName.equals( "DataFromFile" ) )
     {
        String [] lastTok = new String[1];
        lastTok[0] = null;
        boolean doString =false;
        int totalElems = 0;
                H5gParseInfo pInfo = dset.getParseInfo();
        if (mustInitRead == true) {
            //  the first block of this data
            //  will save params for subsequent
            //  reads if needed
            dset.initReadData();
            pInfo = dset.getParseInfo();
            totalElems = pInfo.getTotalElems();
            //overflowed = false;
            mustInitRead = false;
        }
                int fileTypeID = dset.getFileTid();
                int classType = -1;
                try
                {  classType = H5.H5Tget_class( fileTypeID );
        } catch (HDF5LibraryException h5ex) {
        errMsg = "ERROR: H5.H5tget_class with HDF5LibraryException:\n"
              + h5ex.getMessage();
         if( isCmdLineTool() )
         {  printErrMsg( errMsg );
            exitOnError();
         }
         else
            throwSAXException( errMsg, h5ex );
        }
        if( classType == HDF5Constants.H5T_VLEN ) {
        boolean justWarn = true;
            if (justWarn) {
            errMsg = "WARNING: VLEN data is not supported";
            } else {
            errMsg = "ERROR: VLEN data is not supported";
            }
             if( isCmdLineTool() )
             {  printErrMsg( errMsg );
            if (!justWarn) {
            exitOnError();
            }
             }
             else {
            if (!justWarn) {
            throwSAXException( errMsg, null );
            }
            }
            return;
        }
        totalElems = pInfo.getTotalElems();
        int elems = 0;
        if( classType == HDF5Constants.H5T_STRING ) {
            doString = true;
        } else if( classType ==  HDF5Constants.H5T_REFERENCE ) {
            doString = true;
        }

//??		str = str.trim();
        if( doString ) {
            sToken = new StringTokenizer( str, "\"", true );
        } else {
            sToken = new StringTokenizer( str, ", \t\n", false );
//			sToken = new StringTokenizer( str, ", \t\n" );
        }

        //  Note
        //   1.  pass in the next element to be read (elems)
        //   2.  returns the number of _elements_ parsed
        //   3.  returns the 'lastToken' (only for strings)
        //       which is the partial string broken by the
        //       end of a character data block.
        elems = dset.parseTextData( sToken, dset.getData(),
            elemsRead, lastTok, currentObj );

// should always hold the last part...
        if (doString && lastTok[0] != null) {
            //  String ran over the end
            //lastPart = new String("\""+lastTok[0]);
            if (lastPart != null) {
                lastPart = new String("\""+lastTok[0]+lastPart);
            } else {
                lastPart = new String("\""+lastTok[0]);
            }
            overflowed = true;
        }

        if (elems >= 0) {
            elemsRead += elems;
        }

        if ((elems < 0) || (elemsRead >= totalElems)) {
            //  elems = -1 => error
            //  elemsRead >= totalElems --> all read
            // either way, are done, so must write out
            // data now.
            dset.writeData( pInfo.getMemTypeID() );
            elemsRead = 0;
            overflowed = false;
            mustInitRead = true;
        }
       }

     else if( lastLocalName.equals( "NativeHDF5" ) )
     {
     }

     return;
      }

      if( currentObj instanceof ATTRIBUTE )
      {
         boolean doString = false;
         int totalElems = 0;
         H5gAttribute h5Attrib= (H5gAttribute)currentObj;
         String [] lastTok = new String[1];
         lastTok[0] = null;
         if( lastLocalName!=null && lastLocalName.equals("DataFromFile") )
         {
        H5gParseInfo pInfo = h5Attrib.getParseInfo();
        if (mustInitRead == true) {
            //  the first block of this data
            //  will save params for subsequent
            //  reads if needed
            h5Attrib.initReadData();
            pInfo = h5Attrib.getParseInfo();
            totalElems = pInfo.getTotalElems();
            //overflowed = false;
                        mustInitRead = false;
        }
        totalElems = pInfo.getTotalElems();
                int elems = 0;

                int fileTypeID = h5Attrib.getFileTid();
                int classType = -1;
                try
                {  classType = H5.H5Tget_class( fileTypeID );
        } catch (HDF5LibraryException h5ex) {
            errMsg = "ERROR: H5.H5Tget_class() with HDF5LibraryException:\n       "
              + h5ex.getMessage();
         if( isCmdLineTool() )
         {  printErrMsg( errMsg );
            exitOnError();
         }
         else
            throwSAXException( errMsg, h5ex );
        }
        if( classType == HDF5Constants.H5T_STRING ) {
            doString = true;
        } else if( classType ==  HDF5Constants.H5T_REFERENCE ) {
            doString = true;
        }

        if( doString ) {
            sToken = new StringTokenizer( str, "\"", true );
        } else {
            sToken = new StringTokenizer( str, ", \t\n" );
        }
        elems = H5gScalarDS.parseTextData( sToken,
            h5Attrib.getData(), elemsRead,
            lastTok, currentObj );

        if (doString && lastTok[0] != null) {
            //  String ran over the end
            if (lastPart != null) {
                lastPart = new String("\""+lastTok[0]+lastPart);
            } else {
                lastPart = new String("\""+lastTok[0]);
            }
            overflowed = true;
        }

        if (elems >= 0) {
            elemsRead += elems;
        }

        if ((elems < 0) || (elemsRead >= totalElems)) {
            //  elems = -1 => error
            //  elemsRead >= totalElems --> all read
            // either way, are done, so must write out
            // data now.
            h5Attrib.writeData( pInfo.getMemTypeID() );
            elemsRead = 0;
            overflowed = false;
                        mustInitRead = true;
        }
     }

     return;
      }
   }

   public void startPrefixMapping( String prefix, String uri )
   {
   }

   public void endPrefixMapping( String prefix )
   {
   }

   public void ignorableWhitespace( char[] ch, int start, int length )
   {
   }

   public void processingInstruction( String target, String data )
   {
   }

   public void skippedEntity( String name )
   {
   }

   /**** end of ContentHandler implementation ****/



   private void checkPtrTable( String xidKey, HObject valChecked )
     throws SAXException
   {
       Object valReferred;
       if( valChecked instanceof H5gPointer )
       {
          valReferred = ptrTable.get( xidKey );

      if( valReferred == null )
      {  // - Create a linked list, which should contain all
         //   forward pointers pointing to the same object this
         //   valChecked points to.
         LinkedList list = new LinkedList();
         list.add( valChecked );
         ptrTable.put( xidKey, list );
         return;
      }

      // - If valReferred is indeed an object that the pointer should
      //   point to, create the link.
      if( valReferred instanceof HObject )
      {  if( ((HObject)valReferred).getType()==HObject.GROUP ||
           ((HObject)valReferred).getType()==HObject.DATASET ||
           ((HObject)valReferred).getType()==HObject.DATATYPE )
         {  ((H5gPointer)valChecked).setObject( (HObject)valReferred );
        makeHardLink( (H5gPointer)valChecked, (HObject)valReferred );
        return;
         }
         else
         {  errMsg = "ERROR: a pointer points to a wrong type.";
        if( isCmdLineTool() )
        {  printErrMsg( errMsg );
           exitOnError();
        }
        else
           throwSAXException( errMsg, null );
         }
      }
      else if( valReferred instanceof LinkedList )
      {  // - valReferred is a list of unsolved pointers.
         ((LinkedList)valReferred).add( valChecked );
         return;
      }
      else
      {  errMsg = "ERROR: reference object type error in ptrTable!";
         if( isCmdLineTool() )
         {  printErrMsg( errMsg );
        exitOnError();
         }
         else
            throwSAXException( errMsg, null );
      }
      }

      // - valChecked is a solid object.
      valReferred = ptrTable.put( xidKey, valChecked );

      // - If valReferred == null, object *valChecked* has not been referenced
      //   by anyther pointers, so simply return (valChecked has been put
      //   into the table now).
      if( valReferred == null )
         return;

      // - valReferred != null, object *valChecked* has been referenced
      //   by anyther pointers, so we need to resolve all those pointers.
      // - If valChecked is a solid object, valReferred can only be
      //   either null or a linked list, because no two objects can have
      //   the same xidKey.
      ListIterator iter = ((LinkedList)valReferred).listIterator();
      while( iter.hasNext() )
         makeHardLink( (H5gPointer)iter.next(), (HObject)valChecked );
   }


   private void makeHardLink( H5gPointer objPtr, HObject obj )
         throws SAXException
   {
      objPtr.setObject( obj );
      try
      {  H5.H5Glink( objPtr.getFID(), HDF5Constants.H5G_LINK_HARD,
           obj.getPath(), objPtr.getPath() );
      }

      catch( HDF5LibraryException le )
      {  errMsg = "ERROR: H5.H5Glink() with HDF5LibraryException: "+le.getMessage();
         if( isCmdLineTool() )
         {  printErrMsg( errMsg );
        exitOnError();
     }
     else
        throwSAXException( errMsg, null );
      }
      catch( NullPointerException npe )
      {  errMsg = "ERROR: H5.H5Glink() with NullPointerException: "+npe.getMessage();
     if( isCmdLineTool() )
     {  printErrMsg( errMsg );
        exitOnError();
     }
     else
        throwSAXException( errMsg, null );
      }
   }

   private HObject getObjByPtr( String objXid )
   {
      return (HObject)ptrTable.get( objXid );
   }




   public String getOneToken( char[] ch, int start, int length )
   {
      StringTokenizer st = new StringTokenizer(
            new String(ch, start, length), ", \t\n" );
      if( st.hasMoreTokens() )
         return st.nextToken();
      else
         return null;
   }

   public static void exitOnError()
   {
       cleanUp();
       System.exit( -1 );
   }

   public static void printErrMsg( String msg )
   {
      if( verbose >= ERROR_MSG )
         System.err.println( msg );
   }

   public static void printWarningMsg( String msg )
   {  if( verbose == STACK_AND_WARNING )
         System.err.println( msg );
   }

   public static void cleanUp()
   {
      if( verbose >= STACK_MSG )
      {  System.err.println( "HDF5 objects stack:" );
         if( currentObj==null && HObjects.empty() )
            System.err.println( "   The stack is empty" );
      }

      if( currentObj != null )
      {
     // - We need to set currentObj to be null to avoid possible infinite
     //   recursive call close() and cleanUp().
     // - Suppose currentObj.close() raises an exception,
     //   it calls H5gen.exitOnError(). If we don't set currentObj
     //   to be null, close() will be called on the same object again,
     //   and this never stop.
     HObject tmpObj = currentObj;
     currentObj = null;
     try
     {  tmpObj.close(); }
     catch( Throwable t ) { System.err.println( "Catch here" ); }
     if( verbose >= STACK_MSG )
        System.err.println( "   " + tmpObj.getPath() );
      }

      if( HObjects == null ) return;
      while( !HObjects.empty() )
      {  HObject obj = (HObject)HObjects.pop();
     if( verbose > STACK_MSG )
            System.err.println( "   " + obj.getPath() );
         try
     {  obj.close(); }
     catch( Throwable t ) {}
      }
   }

   /*  only used for debugging the stack */
   public void printStack()
   {
      System.err.println( "HDF5 objects stack:" );
      if( currentObj==null && ((HObjects == null)|| HObjects.empty()) ) {
            System.err.println( "   The stack is empty" );
      }

      if( currentObj != null )
      {
        System.err.println( "   " + currentObj.getPath() );
      }

      Enumeration e = HObjects.elements();
      while( e.hasMoreElements() )
      {  HObject obj = (HObject)e.nextElement();
         System.err.println( "   " + obj.getPath() );
      }
   }

   /**
    *  Throw the SAXException, if the exception is only a SAXException, or throw
    *  the exception which is passed to the SAXException. Because the underlying
    *  ContenHandler declares its functions only throw SAXException, if we want to
    *  throw a HDF5LibraryException, a NullPointerException, a H5gException or whatever,
    *  we have to generate a new SAXException using the constructor
    *  SAXException( String message, Exception e ). And this function throws the real
    *  exception accordingly.
    *
    *  @throws 	 	HDF5LibrarayException
    *  @throws 	 	NullPointerException
    *  @throws		HDF5Exception
    *  @throws 	 	IllegalArgumentException
    *  @throws 		H5gLibFailureException
    *  @throws 	 	H5gException
    */
   public static void throwSAXException( String error, Exception e )
     throws SAXException
   {
      H5gException ge = new H5gException( error, currentObj, HObjects, e );
      cleanUp();
      throw new SAXException( error, ge );
   }

   public static void throwSAXExcpThrowable( String error, Exception e )
     throws Throwable
   {
      H5gException ge = new H5gException( error, currentObj, HObjects, e );
      cleanUp();
      throw (Throwable)new SAXException( error, ge );
   }

   private static void Usage()
   {
      System.err.println(
            "\nUsage: java ncsa.hdf.tools.h5gen.H5gen xml_filename "
        + "hdf5_filename [verbose_mode]\n"
        + "   verbose_mode:\n"
        + "      0 -- no error or warning message at all.\n"
        + "      1 -- error message only.\n"
        + "      2 -- error message and stack information.\n"
        + "      3 -- error message, stack information and "
        + "warning messages.\n"
        );

      System.exit( -1 );
   }

   public static void getSAXExceptionMsg( SAXException se )
   {
      H5gException ge = (H5gException)se.getException();
      if( ge == null )
      {  System.err.println( "A SAXException." + se.getMessage() );
      }
      else if( ge.getException() == null )
      {  System.err.println( "A H5gEXception: " + ge.getMessage() );
         ge.printNamesStack();
      }
      else
      {  System.err.println( "Another exception: " + ge.getException().getMessage() );
         ge.printNamesStack();
      }
   }
   public static void main( String[] args )
   {
      if( args.length < 2 )
      {  Usage();
      }

    if (args[0].equals("-")) {
    // will use stdin
    } else {

      File inf = new File(args[0]);
      if (inf != null) {
         if (!inf.exists()) {
            System.err.println( "\nInput file: "+args[0]+" not found\n");
            Usage();
    } else if (!inf.canRead()) {
            System.err.println( "\nInput file: "+args[0]+" Can't read\n");
            Usage();
        }
      } else {
            System.err.println( "\nInput file: "+args[0]+" can't open\n");
            Usage();
      }
    }

      File of = new File(args[1]);
      if (of != null ) {
    if (of.exists()) {
            System.err.println( "\nOutput file: "+args[1]+" already exists\n");
            Usage();
    }
      } else {
            System.err.println( "\nInput file: "+args[1]+" can't open\n");
            Usage();
      }

      H5gen hg = new H5gen( args[0], args[1] );
      hg.setIsCmdLineTool( true );
      if( args.length >= 3 )
      {  try
     {  int vMode = Integer.parseInt( args[2] );
        hg.setVerboseMode( vMode );
     }
     catch( NumberFormatException ne ) {}
      }
      try
      {  if( hg.parseXML() < 0 )
        exitOnError();
      }
      catch( SAXException se )
      {  System.err.println( "In main(), a SAXException is caught." );
         hg.getSAXExceptionMsg( se );
         exitOnError();
      }
      catch( IOException ioe )
      {  exitOnError();
      }
   }
}