/*
 * IText
 * 
 * Date created: 23 JANUARY 2009
 *
 * This code has been developed by the NCEP/SIB for use in the AWIPS2 system.
 */
package gov.noaa.nws.ncep.ui.pgen.display;

import java.awt.Color;

import com.vividsolutions.jts.geom.Coordinate;

/**
 * Interface used to get specific attributes of a PGEN text drawable object
 * @author sgilbert
 *
 */
public interface IText extends ISinglePoint{

	/**
	 * Defines whether the text rotation is relative to the screen or geographic north
	 * @author sgilbert
	 *
	 */
    public static enum TextRotation {
        SCREEN_RELATIVE, NORTH_RELATIVE
    }
    
    /**
     * Defines the text justification options
     * @author sgilbert
     *
     */
    public static enum TextJustification {
        LEFT_JUSTIFY, CENTER, RIGHT_JUSTIFY
    }
    
    /**
     * Defines available font styles
     * @author sgilbert
     *
     */
    public static enum FontStyle {
    	REGULAR, BOLD, ITALIC, BOLD_ITALIC
    }
    
    
    public static enum DisplayType {
    	NORMAL, BOX, UNDERLINE, OVERLINE 
    }
    
    /**
     * Gets the text to draw
     * @return Array of text strings
     */
	public String[] getString();

	/**
	 * Gets the name of the font to use
	 * @return font name
	 */
	public String getFontName();
	
	/**
	 * Gets the size of the font
	 * @return font size
	 */
	public float getFontSize();
	
	/**
	 * Gets the font style to use
	 * @return font style
	 */
	public FontStyle getStyle();
	
	/**
	 * Gets the lat/lon refernce position for the text display
	 * @return lat/lon coordinate
	 */
	public Coordinate getPosition();
	
	/**
	 * Gets the color to use for the text
	 * @return text color
	 */
	public Color getTextColor();
	
	/**
	 * Gets the specified justification
	 * @return the text justification
	 */
	public TextJustification getJustification();
	
	/**
	 * Gets the rotation angle to use
	 * @return rotation angle
	 */
	public double getRotation();
	
	/**
	 * Gets how the rotation angle is applied
	 * @return the rotation relativity
	 */
	public TextRotation getRotationRelativity();
	
    /**
     * Determines whether the text should be displayed with an outline box
     * @return true, if outline box should be displayed
     */
	public DisplayType getDisplayType();
	
	/**
	 * Determines whether text background should be masked out.
	 * @return true, if background is to be masked
	 */
	public Boolean maskText();
	
	/**
	 * Gets the offset in the x direction for the text location.  
	 * The offset is specified in half-characters.
	 * @return The x direction offset
	 */
	public int getXOffset();

	/**
	 * Gets the offset in the y direction for the text location.  
	 * The offset is specified in half-characters.
	 * @return The y direction offset
	 */
	public int getYOffset();
	
	public Boolean getHide();
	public Boolean getAuto();

}
