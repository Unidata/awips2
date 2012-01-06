/*
 * ElementFilter
 * 
 * Date created 03 FEBRUARY 2010
 *
 * This code has been developed by the NCEP/SIB for use in the AWIPS2 system.
 */

package gov.noaa.nws.ncep.ui.pgen.filter;

import gov.noaa.nws.ncep.ui.pgen.elements.AbstractDrawableComponent;

/**
 * Interface used by PGEN AbstractDrawableComponent Filters
 * @author sgilbert
 *
 */
public interface ElementFilter {

	public boolean accept( AbstractDrawableComponent adc );
	
}
