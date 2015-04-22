/*
 * ITCM
 * 
 * Date created: 6 September 2011
 *
 * This code has been developed by the NCEP/SIB for use in the AWIPS2 system.
 */

package gov.noaa.nws.ncep.ui.pgen.elements.tcm;

import java.util.Calendar;
import java.util.List;

import gov.noaa.nws.ncep.ui.pgen.display.IAttribute;


/**
 * Interface for TCM 
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

public interface ITcm extends IAttribute{
	public double[][] getWindRadius();
	public TcmWindQuarters getWaveQuarters();
	public List<TcmFcst> getTcmFcst();
	public String getStormName();
	public Calendar getAdvisoryTime();
	public int getCentralPressure();
	public int getFcstHr();
	public String getStormType();
	public int getStormNumber();
	public int getAdvisoryNumber();
	public String getBasin();
	public int getEyeSize();
	public int getPositionAccuracy();
	public boolean isCorrection();
	
}
