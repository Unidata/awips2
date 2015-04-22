/*
 * ITca
 * 
 * Date created 03 NOVEMBER 2009
 *
 * This code has been developed by the NCEP/SIB for use in the AWIPS2 system.
 */

package gov.noaa.nws.ncep.ui.pgen.tca;

import java.util.ArrayList;
import java.util.Calendar;

import com.vividsolutions.jts.geom.Coordinate;

import gov.noaa.nws.ncep.ui.pgen.display.IAttribute;
import gov.noaa.nws.ncep.ui.pgen.tca.TropicalCycloneAdvisory;

/**
 * This interface defines the methods used to query the attributes of a PGEN Tropical Cyclone
 * Advisory element ( TCAElement ).
 * @author sgilbert
 *
 */
public interface ITca extends IAttribute {

	/**
	 * Returns the name of the Tropical Cyclone
	 * @return
	 */
	public String getStormName();

	/**
	 * Returns the type of storm indicating its strength
	 * @return
	 */
	public String getStormType();
	
	/**
	 * Returns the storm basin
	 * @return
	 */
	public String getBasin();
	
	/**
	 * Returns the number assigned to the cyclone
	 * @return
	 */
	public int getStormNumber();
	
	/**
	 * Gets the advisory number
	 * @return
	 */
	public String getAdvisoryNumber();

	/**
	 * Returns the time the advisory is issued
	 * @return
	 */
	public Calendar getAdvisoryTime();
	
	/**
	 * Returns the local time zone near the storm
	 * @return
	 */
	public String getTimeZone();
	
	/**
	 * Returns the list of current watches/warnings.
	 * @return
	 */
	public ArrayList<TropicalCycloneAdvisory> getAdvisories();
	
	/**
	 * Returns the issuing status of the advisory
	 * @return
	 */
	public String getIssuingStatus();
	
	/**
	 * Returns a lat/lon location to display a text summary when there are 
	 * no current watches/warnings
	 * @return
	 */
	public Coordinate getTextLocation();
	
}
