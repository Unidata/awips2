/*
 * RasterElementContainer
 * 
 * Date created: 08 DECEMBER 2009
 *
 * This code has been developed by the NCEP/SIB for use in the AWIPS2 system.
 */
package gov.noaa.nws.ncep.ui.pgen.display;

import gov.noaa.nws.ncep.ui.pgen.elements.DrawableElement;

import com.raytheon.uf.viz.core.IGraphicsTarget;
import com.raytheon.uf.viz.core.drawables.PaintProperties;
import com.raytheon.uf.viz.core.map.IMapDescriptor;

/**
 * An Element Container that can be used for most Symbol/Marker Elements.
 * Recreation of the IDisplayable objects is only done if the layer DisplayProperties change.
 * The raster images do not need to be recreated when panning or zooming.
 * @author sgilbert
 *
 */
public class RasterElementContainer extends AbstractElementContainer {

	private DisplayProperties saveProps = null;
	
	/**
	 * @param element
	 * @param mapDescriptor
	 * @param target
	 */
	public RasterElementContainer(DrawableElement element,
			IMapDescriptor mapDescriptor, IGraphicsTarget target) {
		super(element, mapDescriptor, target);
		// 
	}

	/* 	 
 	 * Draws to the given graphics target.  Recreates the IDisplayable objects  
	 * if the Layer properties change.
	 * @see gov.noaa.nws.ncep.ui.pgen.display.AbstractTBNL#draw(com.raytheon.uf.viz.core.IGraphicsTarget, com.raytheon.uf.viz.core.drawables.PaintProperties)
	 */
	@Override
	public void draw(IGraphicsTarget target, PaintProperties paintProps,
				DisplayProperties dprops) {
		draw(target, paintProps, dprops, false);
	}
		
	/* 	 
 	 * Draws to the given graphics target.  Recreates the IDisplayable objects  
	 * if the Layer properties change.
	 * @see gov.noaa.nws.ncep.ui.pgen.display.AbstractTBNL#draw(com.raytheon.uf.viz.core.IGraphicsTarget, com.raytheon.uf.viz.core.drawables.PaintProperties, boolean)
	 */
	@Override
	public void draw(IGraphicsTarget target, PaintProperties paintProps,
				DisplayProperties dprops, boolean needsCreate) {
		
		if ( displayEls == null ) needsCreate = true;
		
		if ( (dprops != null) &&  ! dprops.equals(saveProps) ) {
			def.setLayerDisplayAttr(dprops.getLayerMonoColor(), dprops.getLayerColor(), dprops.getLayerFilled());
			needsCreate = true;
		}
		
		if ( needsCreate ) createDisplayables(paintProps);
		saveProps = dprops;
		
		for ( IDisplayable each : displayEls ) {
	         each.draw(target, paintProps);
		}
	}

}
