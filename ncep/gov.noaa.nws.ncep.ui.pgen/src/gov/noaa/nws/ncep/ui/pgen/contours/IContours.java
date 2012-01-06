/*
 * gov.noaa.nws.ncep.ui.pgen.contours.IContours
 * 
 * october 2009
 *
 * This code has been developed by the NCEP/SIB for use in the AWIPS2 system.
 */
package gov.noaa.nws.ncep.ui.pgen.contours;

import gov.noaa.nws.ncep.ui.pgen.display.IAttribute;

import java.util.Calendar;

/**
 * Interface for Contours element.
 * 
 * <pre>
 * SOFTWARE HISTORY
 * Date       	Ticket#		Engineer	Description
 * ------------	----------	-----------	--------------------------
 * 10/09		#167		J. Wu   	Initial Creation.
 * 
 * </pre>
 * 
 * @author	J. Wu
 */
public interface IContours {
    
	public String getParm();
	
	public String getLevel();

	public String getForecastHour();
	
	public Calendar getTime1();

	public Calendar getTime2();

	public String getCint();	
		
}
