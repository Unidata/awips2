/*
 * IAttribute
 * 
 * Date created: 20 April 2011
 *
 * This code has been developed by the NCEP/SIB for use in the AWIPS2 system.
 */

package gov.noaa.nws.ncep.ui.pgen.display;

import java.awt.Color;

/**
 * Common interface for all drawable elements.
 * 
 * <pre>
 * SOFTWARE HISTORY
 * Date       	Ticket#		Engineer	Description
 * ------------	----------	-----------	--------------------------
 * 04/11					B. Yin   	Initial Creation.
 * </pre>
 * 
 * @author	B. Yin
 */

public interface IAttribute {
	public Color[]		getColors();
    public float		getLineWidth();
    public double		getSizeScale();
}
