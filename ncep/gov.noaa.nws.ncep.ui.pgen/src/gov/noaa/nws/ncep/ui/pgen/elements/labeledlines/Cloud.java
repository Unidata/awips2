/*
 * gov.noaa.nws.ncep.ui.pgen.elements.labeledLines.Cloud
 * 
 * 5 September 2010
 *
 * This code has been developed by the NCEP/SIB for use in the AWIPS2 system.
 */

package gov.noaa.nws.ncep.ui.pgen.elements.labeledlines;

import gov.noaa.nws.ncep.ui.pgen.annotation.ElementOperations;
import gov.noaa.nws.ncep.ui.pgen.annotation.Operation;
import gov.noaa.nws.ncep.ui.pgen.elements.AbstractDrawableComponent;

import java.util.Iterator;

/**
 * Implements a class for PGEN Cloud elements.
 * 
 * <pre>
 * SOFTWARE HISTORY
 * Date       	Ticket#		Engineer	Description
 * ------------	----------	-----------	--------------------------
 * 09/10		#305			B. Yin   	Initial Creation.
 * 
 * </pre>
 * 
 * @author	B. Yin
 */

@ElementOperations ( {Operation.COPY_MOVE,  Operation.FLIP, Operation.EXTRAPOLATE,
    Operation.INTERPOLATE} )
    
public class Cloud extends LabeledLine {

	/**
	 * Public Constructor
	 * @param name
	 */
	public Cloud(String name) {
		super(name);
	}
	
	/**
	 * Deep copy
	 */
	@Override
	public LabeledLine copy(){
		Cloud ll = new Cloud( this.getName());
		ll.setParent( this.parent);
		ll.setPgenCategory(pgenCategory);
		ll.setPgenType(pgenType);
		
		Iterator<AbstractDrawableComponent> it = this.getComponentIterator();
		while( it.hasNext() ){
			ll.add(it.next().copy());
		}
		
		return ll;
	}
}
