/*
 * PgenClipboard
 * 
 * Date created: 01/02/2013
 *
 * This code has been developed by the NCEP/SIB for use in the AWIPS2 system.
 */
package gov.noaa.nws.ncep.ui.pgen;

import java.util.ArrayList;
import java.util.List;

import gov.noaa.nws.ncep.ui.pgen.elements.AbstractDrawableComponent;

/**
 * This singleton is intended to hold copy/cut/paste PGEN elements.
 *
 * <pre>
 * SOFTWARE HISTORY
 * Date       	Ticket#		Engineer	Description
 * ------------	----------	-----------	--------------------------
 * 01/2013		#967		Jun Wu  	Initial Creation.
 * 
 * </pre>
 * 
 * @author	J. Wu
 *
 */
public class PgenClipboard {

	/*
	 * The singleton instance
	 */
	private static PgenClipboard instance = null;
			
    /*
     * List of elements that be selected for cut/paste
     */
    private List<AbstractDrawableComponent> elSelected = null;

    /*
	 * Hide default constructor
	 */
	private PgenClipboard() {
			
	}
	
	/**
	 * Static method to get THE PgenSession instance
	 * @return PgenSession reference
	 */
	public static synchronized PgenClipboard getInstance() {
		
		if ( instance == null ) instance = new PgenClipboard();
		return instance;
	}
	
	/**
	 * @param elSelected the elSelected to set
	 */
	public void setElSelected(List<AbstractDrawableComponent> elSelected) {
		this.elSelected = elSelected;		
	}

	/**
	 * @return the elSelected
	 */
	public List<AbstractDrawableComponent> getElSelected() {
		return elSelected;
	}

	/**
	 * @return none
	 */
	public void clear() {
		elSelected.clear();
	}

	/**
	 * Remove the existing elements and copy the input elements.
	 * 
	 * @return none
	 */
	public void copy( List<AbstractDrawableComponent> elSelected ) {
		if ( this.elSelected == null ) {
			this.elSelected = new ArrayList<AbstractDrawableComponent>();
		}		
		
		clear();
		for ( AbstractDrawableComponent el : elSelected ) {
			this.elSelected.add( el.copy() );
		}
		
	}
			
}
