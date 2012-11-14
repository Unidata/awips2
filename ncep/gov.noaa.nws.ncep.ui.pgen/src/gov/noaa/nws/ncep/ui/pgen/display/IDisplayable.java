/*
 * IDisplayable
 * 
 * Date created: 20 NOVEMBER 2008
 *
 * This code has been developed by the NCEP/SIB for use in the AWIPS2 system.
 */
package gov.noaa.nws.ncep.ui.pgen.display;

import com.raytheon.uf.viz.core.IGraphicsTarget;
import com.raytheon.uf.viz.core.drawables.PaintProperties;

/**
 * Interface used for all graphic objects in PGEN.  
 * <P>
 * Its intended use is for the PGEN Resource to be able to draw and get rid of 
 * graphic objects without needing to know the details of "how".
 * @author sgilbert
 */
public interface IDisplayable {

	/**
	 * Draws graphic objects to the specified graphics target with given paint properties.
	 * @param target
	 * @param paintProps
	 */
	public void draw(IGraphicsTarget target, PaintProperties paintProps);
	
	/**
	 * Disposes of any resources held by the graphic objects
	 */
	public void dispose();
	
}
