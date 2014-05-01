/*
 * ITcmWindQuarter
 * 
 * Date created: 6 September 2011
 *
 * This code has been developed by the NCEP/SIB for use in the AWIPS2 system.
 */

package gov.noaa.nws.ncep.ui.pgen.elements.tcm;

import gov.noaa.nws.ncep.ui.pgen.display.ISinglePoint;

/**
 * Interface for TCM wind quarters 
 * 
 * <pre>
 * SOFTWARE HISTORY
 * Date       	Ticket#		Engineer	Description
 * ------------	----------	-----------	--------------------------
 * 09/11					B. Yin   	Initial Creation.
 * </pre>
 * 
 * @author	B. Yin
 */

public interface ITcmWindQuarter extends ISinglePoint{
	public double[] getQuarters();
	public int getWindSpeed();
}
