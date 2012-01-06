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

//  refs for a single dataset or attribute
public class H5gObjRefList
{
   protected String datasetPath;
   protected boolean isAttribute;
   protected boolean attrInGroup;
   protected boolean attrInDT;
   protected String attributeName;
   protected LinkedList refPath;

   public H5gObjRefList()
   {
	   isAttribute = false;
	   attributeName = null;
	   attrInGroup = false;
	   attrInDT = false;
	   datasetPath = null;
	   refPath = new LinkedList();
   }

   public H5gObjRefList( String dsp )
   {
	   isAttribute = false;
	   attrInGroup = false;
	   attrInDT = false;
	   attributeName = null;
	   datasetPath = dsp;
	   refPath = new LinkedList();
   }
   public H5gObjRefList( String dsp, boolean isA )
   {
	   isAttribute = isA;
	   attrInGroup = false;
	   attrInDT = false;
	   setName(dsp);
	   refPath = new LinkedList();
   }

   public void setName( String p ) {
	if (isAttribute) {
	int i = p.lastIndexOf("/");
	  if (i < 0) {
		// attr name with no leading slash
		//  probably an error
		datasetPath = new String("/");
	        attributeName = p;
	  }  else {
	   String pth = p.substring(0,i+1);
	   datasetPath = pth;
	   attributeName = p.substring(i+1);
	  }
	} else {
	   attributeName = null;
	   datasetPath = p;
	}
   }
   public void setAttrInGroup( ) {
	attrInGroup = true;
   }
   public boolean attrInGroup( ) {
	return (attrInGroup);
   }
   public void setAttrInDT( ) {
	attrInDT = true;
   }
   public boolean attrInDT( ) {
	return (attrInDT);
   }
   public void setDatasetPath( String p ) {
	setName(p);
   }

public boolean isAttribute() { return isAttribute; }

   public String getName( ) {
	return( datasetPath );
   }
   public String getAttributeName( ) {
	return( attributeName );
   }

   public String getDatasetPath( ) {
	return( datasetPath );
   }

   public void add( H5gObjectRefPatch rp ) {
	refPath.add((Object)rp);
   }

   public H5gObjectRefPatch getFirst( ) {
	return( (H5gObjectRefPatch)refPath.getFirst() );
   }

   public ListIterator getIterator( ) {
	return ( refPath.listIterator() );
   }

   public int getSize( ) {
	return ( refPath.size() );
   }
}
