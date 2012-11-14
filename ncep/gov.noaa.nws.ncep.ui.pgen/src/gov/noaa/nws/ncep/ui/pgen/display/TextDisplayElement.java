/*
 * TextDisplayElement
 * 
 * Date created: 23 JANUARY 2009
 *
 * This code has been developed by the NCEP/SIB for use in the AWIPS2 system.
 */
package gov.noaa.nws.ncep.ui.pgen.display;

import java.awt.geom.Rectangle2D;

import gov.noaa.nws.ncep.ui.pgen.display.IText.DisplayType;

import org.eclipse.swt.graphics.RGB;

import com.raytheon.uf.viz.core.DrawableString;
import com.raytheon.uf.viz.core.IExtent;
import com.raytheon.uf.viz.core.IGraphicsTarget;
import com.raytheon.uf.viz.core.IGraphicsTarget.HorizontalAlignment;
import com.raytheon.uf.viz.core.IGraphicsTarget.TextStyle;
import com.raytheon.uf.viz.core.IGraphicsTarget.VerticalAlignment;
import com.raytheon.uf.viz.core.PixelExtent;
import com.raytheon.uf.viz.core.drawables.IFont;
import com.raytheon.uf.viz.core.drawables.PaintProperties;
import com.raytheon.uf.viz.core.exception.VizException;
import com.raytheon.viz.ui.color.BackgroundColor;
import com.raytheon.viz.ui.color.IBackgroundColorChangedListener.BGColorMode;

/**
 * Contains information needed to readily display "text" information to a graphics target.
 * <P>
 * Objects of this class are typically created from PGEN "drawable elements" using the DisplayElementFactory 
 * class.
 * @author sgilbert
 *
 */
public class TextDisplayElement implements IDisplayable {

	/**
	 * The text to be displayed.  Each element of the String array is displayed on a separate line.
	private String[] text;
	 */
	
	/**
	 * The font used to display the text
	private IFont font;
	 */
	
	/**
	 * Screen coordinates for the text loation
	private double xpos, ypos;
	 */
	
	/**
	 * Indicates whether any additional style should be applied to the text display
	 * such as a background mask, outline box, etc...
	private TextStyle textStyle;
	 */

	private boolean mask;
	//private RGB color;
	
	/**
	 * specifies the text justification to use.
	private HorizontalAlignment horizontalAlignment;
	 */
	
	/**
	 * The rotation angle (relative to the screen) at which to display the text.
	private Double rotation;
	 */
	private IExtent box;
	private DisplayType displayType;
	private DrawableString dstring;
	
	/**
	 */
	public TextDisplayElement(DrawableString dstring, boolean mask, DisplayType dType,
			IExtent box) {

		this.mask = mask;
		this.box = box;
		//this.xpos = xpos;
		//this.ypos = ypos;
		this.displayType = dType;
		this.dstring = dstring;
		//this.horizontalAlignment = horizontalAlignment;
		//this.rotation = rotation;

		/*
		dstring = new DrawableString( text, color);
		dstring.font =  font;
		dstring.setCoordinates(xpos, ypos);
		dstring.textStyle = TextStyle.NORMAL;
		dstring.horizontalAlignment = horizontalAlignment;
		dstring.verticallAlignment = VerticalAlignment.MIDDLE;
		dstring.rotation = rotation;
		 */
		 
		//if ( mask || (dType != DisplayType.NORMAL) )
		//	box = createBox(bounds);
	}

	/*
	private IExtent createBox(Rectangle2D bounds) {
        
		System.out.println("bounds: "+bounds);

        PixelExtent box = new PixelExtent(bounds.getMinX(),bounds.getMaxX(),
        		                          bounds.getMinY(), bounds.getMaxY());

		return box;
	}
*/

	/**
	 * Disposes any graphic resources held by this object.
	 * @see gov.noaa.nws.ncep.ui.pgen.display.IDisplayable#dispose()
	 */
	@Override
	public void dispose() {

		dstring.font.dispose();

	}

	/**
	 * Draws the text strings to the specified graphics target
	 * @param target Destination graphics target
	 * @see gov.noaa.nws.ncep.ui.pgen.display.IDisplayable#draw(com.raytheon.viz.core.IGraphicsTarget)
	 */
	@Override
	public void draw(IGraphicsTarget target, PaintProperties paintProps) {

		//target.setRotateTextAroundPoint(true);

		try {
			if ( mask ) {
				RGB bg = BackgroundColor.getActivePerspectiveInstance().getColor(BGColorMode.EDITOR);
				target.drawShadedRect(box, bg, 1.0, null);
			}
				
			switch (displayType) {
			
			case BOX:
				target.drawRect(box, dstring.getColors()[0], 1.0f, 1.0);
				break;
				
			case OVERLINE:
				target.drawLine(box.getMinX(), box.getMinY(), 0.0, box.getMaxX(), box.getMinY(), 
						0.0, dstring.getColors()[0], 1.0f);
				break;
				
			case UNDERLINE:
				target.drawLine(box.getMinX(), box.getMaxY(), 0.0, box.getMaxX(), box.getMaxY(), 
						0.0, dstring.getColors()[0], 1.0f);
				break;
				
			}
			
			target.drawStrings(dstring);
			/*
			if ( text.length == 1 ) {
				target.drawString(font, text[0], xpos, ypos, 0.0, textStyle, color, horizontalAlignment, VerticalAlignment.MIDDLE, rotation);
			}
			else {
				RGB[] colors = new RGB[text.length];
				for ( int j=0; j<text.length; j++ ) {
					colors[j] = color;
				}
				target.drawStrings1Box(font, text, xpos, ypos, 0.0, textStyle, colors, horizontalAlignment, rotation);
			}
			*/
		}
		catch (VizException ve) {
			ve.printStackTrace();
		}
		//finally {
		//	target.setRotateTextAroundPoint(false);
		//}

	}

}
