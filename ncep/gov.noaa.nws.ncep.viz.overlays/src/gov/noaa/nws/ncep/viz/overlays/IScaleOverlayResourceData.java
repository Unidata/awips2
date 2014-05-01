/**
 * 
 */
package gov.noaa.nws.ncep.viz.overlays;

import javax.measure.quantity.Length;
import javax.measure.unit.NonSI;
import javax.measure.unit.SI;
import javax.measure.unit.Unit;

import org.eclipse.swt.graphics.RGB;

import com.raytheon.uf.viz.core.drawables.IFont;
import com.raytheon.uf.viz.core.drawables.IFont.Style;

/**
 * <pre>
 *
 * SOFTWARE HISTORY
 *
 * Date         Ticket#     Engineer    Description
 * ------------ ----------  ----------- --------------------------
 * 12 Oct 2010  311         bhebbard    Initial Creation (from IPointOverlayResourceData).                                     
 * 
 * </pre>
 *
 * @author bhebbard
 * @version 1
 */
public interface IScaleOverlayResourceData {

    /** Defines marker symbol choice */
	
    public enum ScaleModel {
    	DEFAULT          ("Default"),        //  like AWIPS II D2D
    	OTHER            ("NMAP2");          //  like legacy NMAP2    	
    	private String displayName;    	
    	// Constructor    	
    	ScaleModel (String displayName)
    	{
    		this.displayName = displayName;
    	}    	
    	public String getDisplayName()
    	{
    		return this.displayName;
    	}    	
    };
    
    /** Where to draw the scale */
    
    public enum ScalePosition {
        UPPER_LEFT    ( "Upper Left" ),
        UPPER_CENTER  ( "Upper Center" ),
        UPPER_RIGHT   ( "Upper Right" ),
        LOWER_LEFT    ( "Lower Left" ),
        LOWER_CENTER  ( "Lower Center" ),
        LOWER_RIGHT   ( "Lower Right" );
    	private String displayName;
    	// Constructor
    	ScalePosition (String displayName) {
    		this.displayName = displayName;
    	}
    	public String getDisplayName() {
    		return displayName;
    	}
    };
    
    /** Units of distance for the scale */
    
    public enum ScaleUnit {
    	SM         ( NonSI.MILE ),
    	NM         ( NonSI.NAUTICAL_MILE ),
    	KM         ( SI.KILOMETER );
    	private Unit<Length> unit;
    	// Constructor
    	ScaleUnit (Unit<Length> unit) {
    		this.unit = unit;
    	}
    	public Unit<Length> getUnit() {
    		return unit;
    	}
    };
    
    /** Defines how to select scale intervals */
    
    public enum ScaleIntervalMode {
    	AUTO       ( "Auto" ),
    	MANUAL     ( "Manual" );
    	private String displayName;
    	// Constructor
    	ScaleIntervalMode (String displayName) {
    		this.displayName = displayName;
    	}
    	public String getDisplayName() {
    		return displayName;
    	}
    };
    
    /** Defines how to select scale latitude */
    
    public enum ScaleLatMode {
    	AUTO_AT_BAR      ( "Auto - At Bar" ),
    	AUTO_AT_CENTER   ( "Auto - At Center" ),
    	MANUAL           ( "Manual" );
    	private String displayName;
    	// Constructor
    	ScaleLatMode (String displayName) {
    		this.displayName = displayName;
    	}
    	public String getDisplayName() {
    		return displayName;
    	}
    };
    
    /** Defines text font choice */
    
    public enum ScaleTextFont {
    	COURIER       ( "Courier",   "Courier" ),
    	HELVETICA     ( "Helvetica", "Helvetica" ),
    	TIMES         ( "Times",     "Times"     );
    	private String displayName;
    	private String fontName;
    	// Constructor
    	ScaleTextFont (String displayName, String fontName) {
    		this.displayName = displayName;
    		this.fontName = fontName;
    	}
    	public String getDisplayName() {
    		return displayName;
    	}
    	public String getFontName() {
    		return fontName;
    	}
    };
    
    /** Defines text size choice */
    
    public static enum ScaleTextSize {          //  GEMPAK/NMAP Table Code  //TODO - applies here, or no...?
    	TINY       ( 0.714f , 10, "Tiny" ),      //   0
    	SMALL      ( 0.857f , 12, "Small" ),     //   1
    	MEDIUM     ( 1.000f , 14, "Medium" ),    //   2
    	LARGE      ( 1.286f , 18, "Large" ),     //   3
    	HUGE       ( 1.714f , 24, "Huge" ),      //   4
    	GIANT      ( 2.429f , 34, "Giant" );     //   5
        private float  softwareSize ;
        private int    hardwareSize ;
        private String displayName ;        
        // Constructor        
        ScaleTextSize (float softwareSize, int hardwareSize, String displayName) {
        	this.softwareSize = softwareSize ;
        	this.hardwareSize = hardwareSize ;
        	this.displayName  = displayName  ;
        }        
        public float getSoftwareSize() {
        	return softwareSize ;
        }        
        public int getHardwareSize() {
        	return hardwareSize ;
        }        
        public String getDisplayName() {
        	return displayName ;
        }
    };
    
    /** Defines text style choice */
    
    public enum ScaleTextStyle {
    	REGULAR       ( "Regular",     new Style[] { } ),
    	ITALIC        ( "Italic",      new Style[] { IFont.Style.ITALIC } ),
    	BOLD          ( "Bold",        new Style[] { IFont.Style.BOLD } ),
    	BOLD_ITALIC   ( "Bold Italic", new Style[] { IFont.Style.BOLD, IFont.Style.ITALIC } );
    	private String displayName;
    	private Style[] styles;
    	// Constructor
    	ScaleTextStyle (String displayName, Style[] styles) {
    		this.displayName = displayName;
    		this.styles = styles;
    	}
    	public String getDisplayName() {
    		return displayName;
    	}
    	public Style[] getStyles() {
    		return styles;
    	}
    };

	/**
	 * @return the ScaleModel
	 */
	public abstract ScaleModel getScaleModel();

	/**
	 * @param ScaleModel the ScaleModel to set
	 */
	public abstract void setScaleModel(ScaleModel ScaleModel);

	/**
	 * @return the ScaleTextSize
	 */
	public abstract ScaleTextSize getScaleTextSize();

	/**
	 * @param ScaleTextSize the ScaleTextSize to set
	 */
	public abstract void setScaleTextSize(ScaleTextSize ScaleTextSize);

	/**
	 * The color to set
	 * 
	 * @param color
	 *            the color
	 */
	public abstract void setColor(RGB color);

	/**
	 * Get the currently set color
	 * 
	 * @return the color
	 */
	public abstract RGB getColor();
}
