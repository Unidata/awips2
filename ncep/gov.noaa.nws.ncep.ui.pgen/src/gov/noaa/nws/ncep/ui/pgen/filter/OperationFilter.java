/*
 * Operation
 * 
 * Date created 03 FEBRUARY 2010
 *
 * This code has been developed by the NCEP/SIB for use in the AWIPS2 system.
 */
package gov.noaa.nws.ncep.ui.pgen.filter;

import gov.noaa.nws.ncep.ui.pgen.annotation.ElementOperations;
import gov.noaa.nws.ncep.ui.pgen.annotation.Operation;
import gov.noaa.nws.ncep.ui.pgen.elements.AbstractDrawableComponent;

/**
 * Filter used to accept only AbstractDrawableComponents whose class declaration contains an
 * annotation for the given PGEN Operation.
 * @author sgilbert
 *
 */
public class OperationFilter implements ElementFilter {

	Operation oper;
	
	/**
	 * @param oper
	 */
	public OperationFilter(Operation oper) {
		this.oper = oper;
	}

	@Override
	public boolean accept(AbstractDrawableComponent adc) {
		
		if ( adc.getClass().isAnnotationPresent(ElementOperations.class)) {
			ElementOperations ops = adc.getClass().getAnnotation(ElementOperations.class);
			for ( Operation op :  ops.value() ) {
				if ( op == oper ) return true;
			}
		}
		
		return false;
	}

}
