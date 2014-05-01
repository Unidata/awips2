/*
 * gov.noaa.nws.ncep.ui.pgen.rsc.PgenSinglePointDrawingTool
 * 
 * 20 June 2009
 *
 * This code has been developed by the NCEP/SIB for use in the AWIPS2 system.
 */

package gov.noaa.nws.ncep.ui.pgen.elements;

import java.util.Iterator;


/**
 * Implementation of an iterator of DrawableElement in order to
 * treat DEs and DECollections the same way.
 * 
 * <pre>
 * SOFTWARE HISTORY
 * Date       	Ticket#		Engineer	Description
 * ------------	----------	-----------	--------------------------
 * 06/09		#116			B. Yin   	Initial Creation.
 * 
 * </pre>
 * 
 * @author	B. Yin
 */
public class SelfIterator implements Iterator<DrawableElement>{
	
	private DrawableElement de;
	boolean nextFlag;

	public SelfIterator(DrawableElement de){
		this.de = de;
		if ( de != null ) nextFlag = true;
	}
	
	@Override
	public boolean hasNext() {
		return nextFlag;
	}

	@Override
	public DrawableElement next() {
		if ( hasNext()){
			nextFlag = false;
			return de;
		}
		else {
			return null;
		}
	}

	@Override
	public void remove() {
		// TODO Auto-generated method stub
		
	}

}
