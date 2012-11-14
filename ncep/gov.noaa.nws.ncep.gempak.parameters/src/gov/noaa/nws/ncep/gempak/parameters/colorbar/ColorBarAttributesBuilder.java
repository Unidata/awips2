/**
 * 
 */
package gov.noaa.nws.ncep.gempak.parameters.colorbar;

import gov.noaa.nws.ncep.viz.common.ui.color.GempakColor;

import org.eclipse.swt.graphics.RGB;

/**
 * Presents the color bar attributes as parsed from CLRBAR (as well as IMCBAR)
 * *<pre>
 * SOFTWARE HISTORY
 *     Date       Ticket#     Engineer       Description
 * ------------ ---------- ----------- --------------------------
 *  11-Jun-2012    743        Archana.S    Initial Creation
 * </pre>
 * @author Archana.S
 * @version 1 
 * @see $GEMPAK/help/hlx/clrbar.hl2
 */

public class ColorBarAttributesBuilder {
   
	private ColorBarAnchorLocation anchorLocation;
   
   private ColorBarOrientation colorBarOrientation;
   
   private double length;
   
   private double width;
   
   /**fraction of PixelCoordinate. Values 0-1*/
   private double x; 
   
   /**fraction of PixelCoordinate. Values 0-1*/
   private double y;
   
   private boolean drawColorBar;
   
   private boolean drawBoxAroundColorBar; 
   
   private RGB color;
  
   /**
 * @return the drawColorBar
 */
public final boolean isDrawColorBar() {
	return drawColorBar;
}

/**
 * @param drawColorBar the drawColorBar to set
 */
public final void setDrawColorBar(boolean drawColorBar) {
	this.drawColorBar = drawColorBar;
}

/**
 * @return the drawBoxAroundColorBar
 */
public final boolean isDrawBoxAroundColorBar() {
	return drawBoxAroundColorBar;
}

/**
 * @param drawBoxAroundColorBar the drawBoxAroundColorBar to set
 */
public final void setDrawBoxAroundColorBar(boolean drawBoxAroundColorBar) {
	this.drawBoxAroundColorBar = drawBoxAroundColorBar;
}

/**
 * @return the color
 */
public final RGB getColor() {
	return color;
}

/**
 * @param color the color to set
 */
public final void setColor(RGB color) {
	this.color = color;
}

/**
 * @return the anchorLocation
 */
public final ColorBarAnchorLocation getAnchorLocation() {
	return anchorLocation;
}

/**
 * @param anchorLocation the anchorLocation to set
 */
public final void setAnchorLocation(ColorBarAnchorLocation anchorLocation) {
	this.anchorLocation = anchorLocation;
}

/**
 * @return the colorBarOrientation
 */
public final ColorBarOrientation getColorBarOrientation() {
	return colorBarOrientation;
}

/**
 * @param colorBarOrientation the colorBarOrientation to set
 */
public final void setColorBarOrientation(ColorBarOrientation colorBarOrientation) {
	this.colorBarOrientation = colorBarOrientation;
}

/**
 * @return the length
 */
public final double getLength() {
	return length;
}

/**
 * @param length the length to set
 */
public final void setLength(double length) {
	this.length = length;
}

/**
 * @return the x
 */
public final double getX() {
	return x;
}

/**
 * @param x the x to set
 */
public final void setX(double x) {
	this.x = x;
}

/**
 * @return the y
 */
public final double getY() {
	return y;
}

/**
 * @param y the y to set
 */
public final void setY(double y) {
	this.y = y;
}

/**
 * @param width the width to set
 */
public void setWidth(double width) {
	this.width = width;
}

/**
 * @return the width
 */
public double getWidth() {
	return width;
}


   
   public ColorBarAttributesBuilder() {
	          initializedefaults();
   }
   
   private void initializedefaults(){
	  length = 0.5;
	  width  = 0.01;
	  x      = 0.005;
	  y      = 0.05;
	  anchorLocation        = ColorBarAnchorLocation.LowerLeft;
	  colorBarOrientation   = ColorBarOrientation.Vertical;
	  drawColorBar          = false;
	  drawBoxAroundColorBar = false;
	  color = GempakColor.VANILLA.getRGB();
	  
   } 
   
}
