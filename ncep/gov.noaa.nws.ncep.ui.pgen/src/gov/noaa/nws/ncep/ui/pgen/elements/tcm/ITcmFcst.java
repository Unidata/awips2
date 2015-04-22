/*
 * ITcmFcst
 * 
 * Date created: 6 September 2011
 *
 * This code has been developed by the NCEP/SIB for use in the AWIPS2 system.
 */

package gov.noaa.nws.ncep.ui.pgen.elements.tcm;

/**
 * Interface for TcmFcst
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

public interface ITcmFcst {
	public ITcmWindQuarter[] getQuarters();
}
