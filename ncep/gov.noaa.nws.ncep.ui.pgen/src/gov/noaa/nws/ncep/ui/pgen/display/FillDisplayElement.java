/*
 * FillDisplayElement
 * 
 * Date created: 19 NOVEMBER 2008
 *
 * This code has been developed by the NCEP/SIB for use in the AWIPS2 system.
 */
package gov.noaa.nws.ncep.ui.pgen.display;

import com.raytheon.uf.viz.core.IGraphicsTarget;
import com.raytheon.uf.viz.core.drawables.IShadedShape;
import com.raytheon.uf.viz.core.drawables.PaintProperties;
import com.raytheon.uf.viz.core.exception.VizException;

/**
 * Contains a set of filled graphic shapes that can readily be displayed to a graphics target.
 * <P>
 * Objects of this class are typically created from PGEN "drawable elements" using the DisplayElementFactory 
 * class.
 * @author sgilbert
 *
 */
public class FillDisplayElement implements IDisplayable {

	/**
	 * The filled shapes to be displayed.
	 */
	private IShadedShape shape;

	/**
	 * Transparency of filled shapes.  Values should range between 0.0 and 1.0
	 */
	private float alpha;
	
	/**
	 * Constructor used to set the filled shapes and transparency value
	 * @param shape Filled shapes to be displayed
	 * @param alpha Transparency of the filled shapes
	 */
	public FillDisplayElement(IShadedShape shape, float alpha ) {
		
		this.shape = shape;
		this.alpha = alpha;
	}

	/**
	 * Disposes any graphic resources held by this object.
	 * @see gov.noaa.nws.ncep.ui.pgen.display.IDisplayable#dispose()
	 */
	@Override
	public void dispose() {

		shape.dispose();
	}

	/**
	 * Draws the filled shapes to the specified graphics target
	 * @param target Destination graphics target
	 * @param paintProps PaintProperties
	 * @see gov.noaa.nws.ncep.ui.pgen.display.IDisplayable#draw(com.raytheon.viz.core.IGraphicsTarget)
	 */
	@Override
	public void draw(IGraphicsTarget target, PaintProperties paintProps) {

		try {
		   target.drawShadedShape(shape, alpha);
		}
		catch (VizException ve) {
			System.out.println("Shaded Shape not displayable  :(");
		}
	}

}
