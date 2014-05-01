/*
 * ElementContainerFactory
 * 
 * Date created 08 DECEMBER 2009
 *
 * This code has been developed by the NCEP/SIB for use in the AWIPS2 system.
 */

package gov.noaa.nws.ncep.ui.pgen.display;

import com.raytheon.uf.viz.core.IGraphicsTarget;
import com.raytheon.uf.viz.core.map.MapDescriptor;

import gov.noaa.nws.ncep.ui.pgen.elements.DrawableElement;
import gov.noaa.nws.ncep.ui.pgen.elements.Symbol;

/**
 * @author sgilbert
 *
 */
public class ElementContainerFactory {

	/**
	 * Creates an ElementContainer based on the type of DrawableElement.
	 * @param el
	 * @param descriptor
	 * @param target
	 * @return
	 */
	public static AbstractElementContainer createContainer( DrawableElement el, MapDescriptor descriptor, IGraphicsTarget target) {

		if ( (el instanceof Symbol) )
			return new RasterElementContainer(el, descriptor, target );
		else
			return new DefaultElementContainer(el, descriptor, target );
	
	}

}
