/*
 * IWatchBox
 * 
 * Date created: 26 SEPTEMBER 2009
 *
 * This code has been developed by the NCEP/SIB for use in the AWIPS2 system.
 */
package gov.noaa.nws.ncep.ui.pgen.display;

/**
 * Interface for PGEN watch box.
 * 
 * <pre>
 * SOFTWARE HISTORY
 * Date       	Ticket#		Engineer	Description
 * ------------	----------	-----------	--------------------------
 * 09/09		#159		B. Yin   	Initial Creation.
 * 03/10		#159		B. Yin   	Added getIssueFlag and getWatchNumber.
 * 04/11		#?			B. Yin		Re-factor IAttribute. Extends from IMultiPoint.
 *
 * </pre>
 * 
 * @author	B. Yin
 */


import gov.noaa.nws.ncep.edex.common.stationTables.Station;
import gov.noaa.nws.ncep.ui.pgen.elements.WatchBox.WatchShape;
import gov.noaa.nws.ncep.ui.pgen.maps.County;

import java.awt.Color;
import java.util.List;

public interface IWatchBox extends IMultiPoint{
	
	public Color[] getColors();
	public Color getFillColor();
	public WatchShape getWatchBoxShape();
	public Station[] getAnchors();
	public List<County> getCountyList();
	public String getWatchSymbolType();
	public float getWatchSymbolWidth();
	public double getWatchSymbolSize();
	public boolean getFillFlag();
	public int getWatchNumber();
	public int getIssueFlag();

}
