/*
 * ITrack
 * 
 * Date created: 06 April 2009
 *
 * This code has been developed by the NCEP/SIB for use in the AWIPS2 system.
 */
package gov.noaa.nws.ncep.ui.pgen.display;

import java.awt.Color;
import java.util.Calendar;

import gov.noaa.nws.ncep.ui.pgen.display.IText.FontStyle;

/**
 * Interface used to get specific attributes of a PGEN Storm track object.
 * @author sgilbert
 *
 */
public interface ITrack extends ILine{

	/**
	 * this enum stores the time display options for extra points
	 */
	public static enum ExtraPointTimeDisplayOption { SKIP_FACTOR, SHOW_FIRST_LAST, ON_ONE_HOUR, ON_HALF_HOUR };

//    /**
//     * Defines available font styles
//     * @return font style
//     */
//    public static enum FontStyle {
//    	REGULAR, BOLD, ITALIC, BOLD_ITALIC
//    }
    
    /**
     * Gets a font style for text of track points
     */
    public FontStyle getFontStyle() ;
    
	/**
	 * Fets a boolean array that indicates if a time text of a extra point should be skipped
	 */
	public boolean[] getExtraPointTimeTextDisplayIndicator(); 

	/**
	 * Gets extra point time display option
	 */
	public ExtraPointTimeDisplayOption getExtraPointTimeDisplayOption(); 
	
	/**
	 * Gets color to plot the initial storm points
	 * @return color
	 */
	public Color getInitialColor();
	
	/**
	 * Gets the line pattern used to display the initial storm points
	 * @return line pattern
	 */
	public String getInitialLinePattern();
	
	/**
	 * Gets the marker used to display the initial storm points
	 * @return marker type
	 */
	public String getInitialMarker();
	
	/**
	 * Gets the initial storm points and associated date/times
	 * @return track points
	 */
	public TrackPoint[] getInitialPoints();
	
	/**
	 * Gets color to plot the extrapolated storm points
	 * @return color
	 */
	public Color getExtrapColor();
	
	/**
	 * Gets the line pattern used to display the extrapolated storm points
	 * @return line pattern
	 */
	public String getExtrapLinePattern();
	
	/**
	 * Gets the marker used to display the extrapolated storm points
	 * @return marker type
	 */
	public String getExtrapMarker();
	
	/**
	 * Gets the extrapolated storm points and associated date/times
	 * @return track points
	 */
	public TrackPoint[] getExtrapPoints();
	
	/**
	 * Gets the font used to display the location times
	 * @return font name
	 */
	public String getFontName();
	
	/** 
	 * Gets the size of the font
	 * @return font size
	 */
	public float getFontSize();
	
	public Calendar getFirstTimeCalendar();
	public Calendar getSecondTimeCalendar();
	public boolean isSetTimeButtonSelected();
	public int getExtraDrawingPointNumber();
	public FontStyle getStyle();
	public String getSkipFactorText();
	public int getFontNameComboSelectedIndex();
	public int getFontSizeComboSelectedIndex();
	public int getFontStyleComboSelectedIndex();
	public int getUnitComboSelectedIndex();
	public int getRoundComboSelectedIndex();
	public int getRoundDirComboSelectedIndex();
	public String getIntervalTimeString();

	}
