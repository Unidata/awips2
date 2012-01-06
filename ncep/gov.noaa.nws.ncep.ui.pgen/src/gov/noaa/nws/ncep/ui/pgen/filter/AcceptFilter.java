/*
 * AcceptFilter
 * 
 * Date created 03 FEBRUARY 2010
 *
 * This code has been developed by the NCEP/SIB for use in the AWIPS2 system.
 */

package gov.noaa.nws.ncep.ui.pgen.filter;

import gov.noaa.nws.ncep.ui.pgen.elements.AbstractDrawableComponent;

/**
 * Filter used to accept ALL AbstractDrawableComponents
 * @author sgilbert
 *
 */
public class AcceptFilter implements ElementFilter {

	/* (non-Javadoc)
	 * @see gov.noaa.nws.ncep.ui.pgen.filter.ElementFilter#accept(gov.noaa.nws.ncep.ui.pgen.elements.AbstractDrawableComponent)
	 */
	@Override
	public boolean accept(AbstractDrawableComponent adc) {
		return true;
	}

}
