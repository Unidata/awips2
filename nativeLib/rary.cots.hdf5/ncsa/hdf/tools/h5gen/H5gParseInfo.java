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

public class H5gParseInfo 
{
   protected int totalElems;
   protected int elemClass;
   protected int elemSize;
   protected boolean signed;
   protected int memTypeID;
   protected int fileTypeID;
   protected int baseTypeID;
   protected int baseTypeSize;
   protected int baseTypeClass;

   public H5gParseInfo()
   {
	totalElems = 0;
	elemClass = -1;
	elemSize = 0;
	signed = false;
        memTypeID = -1;
        fileTypeID = -1;
	baseTypeID = -1;
	baseTypeSize = 0;
	baseTypeClass = -1;
   }

   public void setTotalElems( int te ) {
	totalElems = te;
   }

   public int getTotalElems( ) {
	return( totalElems );
   }

   public void setElemClass( int ecl ) {
	elemClass = ecl;
   }

   public int getElemClass( ) {
	return( elemClass );
   }

   public void setElemSize( int es ) {
	elemSize = es;
   }

   public int getElemSize( ) {
	return( elemSize );
   }

   public void setSigned( boolean s ) {
	signed = s;
   }

   public boolean getSigned( ) {
	return( signed );
   }

   public void setMemTypeID( int mtid ) {
	memTypeID = mtid;
   }

   public int getMemTypeID( ) {
	return( memTypeID );
   }

   public void setFileTypeID( int ftid ) {
	fileTypeID = ftid;
   }

   public int getFileTypeID( ) {
	return( fileTypeID );
   }

   public void setBaseTypeID( int btid ) {
	baseTypeID = btid;
   }

   public int getBaseTypeID( ) {
	return( baseTypeID );
   }

   public void setBaseTypeSize( int bts ) {
	baseTypeSize = bts;
   }

   public int getBaseTypeSize( ) {
	return( baseTypeSize );
   }

   public void setBaseTypeClass( int btcl ) {
	baseTypeClass = btcl;
   }

   public int getBaseTypeClass( ) {
	return( baseTypeClass );
   }
}
