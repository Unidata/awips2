/*
 * gov.noaa.nws.ncep.ui.pgen.sigmet.ISigmet
 * 
 * December 2009
 *
 * This code has been developed by the NCEP/SIB for use in the AWIPS2 system.
 */

package gov.noaa.nws.ncep.ui.pgen.sigmet;

import gov.noaa.nws.ncep.ui.pgen.display.ILine;

/**
 * Marker Interface for Sigmet,Conv/non-Conv Sigmet, Airmet, and Outlook.
 * 
 * <pre>
 * SOFTWARE HISTORY
 * Date       	Ticket#		Engineer	Description
 * ------------	----------	-----------	--------------------------
 * 12/09		160			Gang Zhang 	Initial Creation. 
 * 04/11		#?			B. Yin		Re-factor IAttribute
 *
 * </pre>
 * 
 * @author	gzhang
 */

public interface ISigmet extends ILine{
	public String getLineType();
	public double getWidth();
}
