/*
 * LineDisplayElement
 * 
 * Date created: 20 NOVEMBER 2008
 *
 * This code has been developed by the NCEP/SIB for use in the AWIPS2 system.
 */
package gov.noaa.nws.ncep.ui.pgen.display;

import java.awt.Color;

import org.eclipse.swt.graphics.RGB;

import com.raytheon.uf.viz.core.IGraphicsTarget;
import com.raytheon.uf.viz.core.drawables.IWireframeShape;
import com.raytheon.uf.viz.core.drawables.PaintProperties;
import com.raytheon.uf.viz.core.exception.VizException;

/**
 * Contains a set of line segments that can readily be displayed to a graphics target.
 * <P>
 * Objects of this class are typically created from PGEN "drawable elements" using the DisplayElementFactory 
 * class.
 * @author sgilbert
 */
public class LineDisplayElement implements IDisplayable {

	/**
	 * The line segments to be displayed.
	 */
	private IWireframeShape shape;
	
	/**
	 * Color of the line segments.
	 */
	private Color color;
	
	/**
	 * Thickness of the line segments.
	 */
	private float lineWidth;
	
	/**
	 * Constructor used to set the line segemnts, and their corresponding color and thickness.
	 * @param shape - The set of line segements.
	 * @param color - The color in which to display the line segments.
	 * @param lineWidth - The desired line thickness.
	 */
	public LineDisplayElement(IWireframeShape shape, Color color,
			float lineWidth) {

		this.shape = shape;
		this.color = color;
		this.lineWidth = lineWidth;
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
	 * Draws the line segments to the specified graphics target.
	 * @param target Destination graphics target.
	 * @see gov.noaa.nws.ncep.ui.pgen.display.IDisplayable#draw()
	 */
	@Override
	public void draw(IGraphicsTarget target, PaintProperties paintProps) {
		
		RGB shapeColor = new RGB(color.getRed(), color.getGreen(), color.getBlue());
		try {
            target.drawWireframeShape(shape, shapeColor, lineWidth);
		}
		catch (VizException ve) {
			System.out.println("Wireframe Shape not displayable  :(");
		}
	}

}
