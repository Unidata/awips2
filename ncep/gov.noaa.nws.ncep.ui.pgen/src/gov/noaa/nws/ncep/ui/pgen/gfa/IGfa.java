/*
 * gov.noaa.nws.ncep.ui.pgen.gfa.IGfa
 * 
 * February 2010
 *
 * This code has been developed by the NCEP/SIB for use in the AWIPS2 system.
 */

package gov.noaa.nws.ncep.ui.pgen.gfa;

import java.util.HashMap;

import gov.noaa.nws.ncep.ui.pgen.display.ILine;

/**
 * Marker Interface for Gfa.
 * 
 * <pre>
 * SOFTWARE HISTORY
 * Date       	Ticket#		Engineer	Description
 * ------------	----------	-----------	--------------------------
 * 03/2010		#223		M.Laryukhin	Initial Creation.
 * 04/11		#?			B. Yin		Re-factor IAttribute
 * 
 * </pre>
 * 
 * @author mlaryukhin
 */

public interface IGfa extends ILine {
	public String getGfaHazard();
	public String getGfaFcstHr();
	public String getGfaTag();
	public String getGfaDesk();
	public String getGfaIssueType();
	public String getGfaType();
	public String getSymbolType();
	public String getGfaArea();
	public String getGfaBeginning();
	public String getGfaEnding();
	public String getGfaStates();
	public int getGfaCycleDay();
	public int getGfaCycleHour();
	public HashMap<String, String> getGfaValues();

}
