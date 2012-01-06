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

public class H5gObjectRefPatch
{
   protected String datasetPath;
   protected String refPath;
   protected int offset;  // position in the dataset data
   protected int size;   // should be 8 or 12 bytes

   public H5gObjectRefPatch()
   {
	   datasetPath = null;
	   refPath = null;
	   offset = -1;  // position in the dataset data
	   size = 0;   // should be 8 or 12 bytes
   }

   public H5gObjectRefPatch( String dsp,
	   String rp, int off, int sz)
   {
	   datasetPath = dsp;
	   refPath = rp;
	   offset = off;  // position in the dataset data
	   size = sz;   // should be 8 or 12 bytes
   }

   public void setDatasetPath( String p ) {
	datasetPath = p;
   }

   public String getDatasetPath( ) {
	return( datasetPath );
   }

   public void setRefPath( String rp ) {
	refPath = rp;
   }

   public String getRefPath( ) {
	return( refPath );
   }

   public void setOffset( int o ) {
	offset = o;
   }

   public int getOffset( ) {
	return( offset );
   }

   public void setSize( int s ) {
	size = s;
   }

   public int getSize( ) {
	return( size );
   }

}
