package gov.noaa.nws.ncep.viz.ui.display;


import java.util.ArrayList;
import javax.measure.unit.Unit;
import javax.xml.bind.annotation.XmlAccessType;
import javax.xml.bind.annotation.XmlAccessorType;
import javax.xml.bind.annotation.XmlElement;
import javax.xml.bind.annotation.adapters.XmlJavaTypeAdapter;

import org.eclipse.swt.graphics.Color;
import org.eclipse.swt.graphics.RGB;
import org.eclipse.swt.widgets.Display;

import com.raytheon.uf.common.serialization.ISerializableObject;

import gov.noaa.nws.ncep.gempak.parameters.colorbar.ColorBarAttributesBuilder;
import gov.noaa.nws.ncep.viz.common.RGBColorAdapter;
import gov.noaa.nws.ncep.gempak.parameters.colorbar.ColorBarOrientation;
import gov.noaa.nws.ncep.gempak.parameters.colorbar.ColorBarAnchorLocation;
/**
 *  An ColorBar for use by FFG, LTNG and other resources which assign colors to 
 *  defined interval ranges. 
 * 
 * <pre>
 * SOFTWARE HISTORY
 * Date         Ticket#     Engineer    Description
 * ------------ ----------  ----------- --------------------------
 * 04/04/10      #259        Greg Hull    Initial Creation.
 * 10/25/11      #463        qzhou        Added equals()
 * 12/06/11      #572        qzhou        Modified equals and added hashCode
 * 06/07/12      #794       Archana       Added a Boolean flag called reverseOrder to enable/disable
 *                                        reversing the order of colors in the color-bar.
 * 06/07/12      #717       Archana       Added the overridden methods getDisplayUnitStr() and getNumPixelsToReAlignLabel()                                                                           
 * 06/18/12      #743       Archana       Added attributes to implement GEMPAK's CLRBAR parameter:
 *                                                                       xPixelCoordFraction, yPixelCoordFraction,drawColorBar,
 *                                                                       isDrawBoxAroundColorBar. added the corresponding setter/getter methods
 *                                                                       Added setAttibutesFromColorBarAttributesBuilder()                                      
 *07/18/12       #717       Archana       Refactored numPixelsToReAlignLabel to alignLabelInTheMiddleOfInterval
 *                                                                        and added the corresponding setter/getter methods 
 *09/11/12        #743  Archana           Minor update in the method setAttributesFromColorBarAttributesBuilder() to set
 *                                                                       the pixel width correctly.
 * 
 * </pre>
 * 
 * @author ghull
 * @version 1
 */

@XmlAccessorType(XmlAccessType.NONE)
public class ColorBar implements IColorBar, ISerializableObject {
	
    @XmlElement
    @XmlJavaTypeAdapter(RGBColorAdapter.class)
    private RGB labelColor = new RGB(255,255,255) ;

	@XmlElement(name = "IntervalValue")
    private final ArrayList<Float> intervals = new ArrayList<Float>();
    
    @XmlElement(name = "IntervalColor")
    @XmlJavaTypeAdapter(RGBColorAdapter.class)
    private final ArrayList<RGB> intervalRGBs = new ArrayList<RGB>();

    private final ArrayList<Color> colors = new ArrayList<Color>();
    
    private Unit<?> dataUnits;
    //private Unit<?> displayUnits;

    @XmlElement
    private Boolean showLabels = true;
    
    @XmlElement
    private Boolean reverseOrder = true;    
    
	@XmlElement 
    private Boolean drawToScale = true;
    
	@XmlElement
	private ColorBarOrientation orientation = ColorBarOrientation.Vertical;
	
    @XmlElement
    private ColorBarAnchorLocation anchorLoc = ColorBarAnchorLocation.LowerLeft;
    
	private static final Float defaultLength = .5f;  
	private static final int defaultWidth = 15; //change to 0.01
	
    @XmlElement
    private Float lengthAsRatio = defaultLength;  // as a ratio of the screen size

    @XmlElement
    private Integer widthInPixels = defaultWidth;  // in pixels ?
    
    @XmlElement
    private int numDecimals=0;
    
    @XmlElement
    private Boolean drawColorBar = true;
    
    @XmlElement
    private Boolean drawBoxAroundColorBar = true;
    
    private double xPixelCoordFraction = 0.005;
    
    private double yPixelCoordFraction = 0.05;
    
    private boolean alignLabelInTheMiddleOfInterval = false;
    /**
     * 
	 * @return the drawColorBar
	 */
	public final Boolean isDrawColorBar() {
		return drawColorBar;
	}

	/**
	 * @param drawColorBar the drawColorBar to set
	 */
	public final void setDrawColorBar(Boolean drawColorBar) {
		this.drawColorBar = drawColorBar;
	}

	/**
	 * @return the drawBoxAroundColorBar
	 */
	public final Boolean isDrawBoxAroundColorBar() {
		return drawBoxAroundColorBar;
	}

	/**
	 * @param drawBoxAroundColorBar the drawBoxAroundColorBar to set
	 */
	public final void setDrawBoxAroundColorBar(Boolean drawBoxAroundColorBar) {
		this.drawBoxAroundColorBar = drawBoxAroundColorBar;
	}

	private  ColorBarAttributesBuilder colorBarAttributesBuilder = null;
    
	private Display display=null; // the Display used to create the Colors in the intervals

	public ColorBar() {	
	}

	// create from a list of intervals and colors.
	// There should be 1 more interval than colors since this will start with the
	// first interval as the lower end of the first interval. Float.NEGATIVE_INFINITY and 
	// POSITIVE_INFINITY can be used to create intervals below/above a given value. 
	// 
	
	public ColorBar( ColorBar cbar ) {
		if( cbar == null ) {
			return;
		}
		anchorLoc =   cbar.anchorLoc;
		orientation = cbar.orientation;
		drawToScale = cbar.drawToScale;
		labelColor  = cbar.labelColor;
		dataUnits   = cbar.dataUnits;
		showLabels  = cbar.showLabels;
		lengthAsRatio = cbar.lengthAsRatio;
		widthInPixels = cbar.widthInPixels;
		numDecimals = cbar.numDecimals;
		reverseOrder = cbar.reverseOrder;
		colorBarAttributesBuilder = cbar.colorBarAttributesBuilder;
		drawColorBar              = cbar.drawColorBar;
		drawBoxAroundColorBar     = cbar.drawBoxAroundColorBar;
		xPixelCoordFraction       = cbar.xPixelCoordFraction;
		yPixelCoordFraction       = cbar.yPixelCoordFraction;
		alignLabelInTheMiddleOfInterval = cbar.alignLabelInTheMiddleOfInterval;
		for( int i=0 ; i<cbar.getNumIntervals() ; i++ ) {
			addColorBarInterval( cbar.getIntervalMin(i),
							     cbar.getIntervalMax(i), cbar.getRGB(i) );
		}
	}

//	public boolean equals(ColorBar cbar) {
//		if( (anchorLoc == null && cbar.anchorLoc != null ) ||
//	            (anchorLoc != null && cbar.anchorLoc == null ) ) 
//	            	return false;	        
//	    if( !anchorLoc.equals( cbar.anchorLoc ) ) 
//	        	return false;
//	    
//	    if( (orientation == null && cbar.orientation != null ) ||
//	            (orientation != null && cbar.orientation == null ) ) 
//	            	return false;	        
//	    if( !orientation.equals( cbar.orientation ) ) 
//	        	return false;
//	        
//	    if( (drawToScale == null && cbar.drawToScale != null ) ||
//	            (drawToScale != null && cbar.drawToScale == null ) ) 
//	            	return false;	        
//	    if( !drawToScale.equals( cbar.drawToScale ) ) 
//	        	return false;
//	    
//	    if( (labelColor == null && cbar.labelColor != null ) ||
//	            (labelColor != null && cbar.labelColor == null ) ) 
//	            	return false;	        
//	    if( !labelColor.equals( cbar.labelColor ) ) 
//	        	return false;
//	    
//	    if( (dataUnits == null && cbar.dataUnits != null ) ||
//	            (dataUnits != null && cbar.dataUnits == null ) ) 
//	            	return false;	        
//	    if( !dataUnits.equals( cbar.dataUnits ) ) 
//	        	return false;
//	    
//	    if( (showLabels == null && cbar.showLabels != null ) ||
//	            (showLabels != null && cbar.showLabels == null ) ) 
//	            	return false;	        
//	    if( !showLabels.equals( cbar.showLabels ) ) 
//	        	return false;
//	    
//	    if( (lengthAsRatio == null && cbar.lengthAsRatio != null ) ||
//	            (lengthAsRatio != null && cbar.lengthAsRatio == null ) ) 
//	            	return false;	        
//	    if( !lengthAsRatio.equals( cbar.lengthAsRatio ) ) 
//	        	return false;
//	    
//	    if( (widthInPixels == null && cbar.widthInPixels != null ) ||
//	            (widthInPixels != null && cbar.widthInPixels == null ) ) 
//	            	return false;	        
//	    if( !widthInPixels.equals( cbar.widthInPixels ) ) 
//	        	return false;
//	    
//	    if( numDecimals != cbar.numDecimals ) 
//	        	return false;
//	    
//		return true;
//	}
	 
	public int scaleCmapValue( float cmapVal ) {
		int rgbVal = (int)(cmapVal *256);
		return (rgbVal < 0 ? 0 : (rgbVal > 255 ? 255 : rgbVal) );
	}
	
	public int getNumIntervals() {
		return (intervals.isEmpty() ? 0 : intervals.size()-1);
	}
	
	public void addColorBarInterval( Float min, Float max, RGB rgb ) {
		if( min >= max ) {
			System.out.println("addColorBarInterval : min >= max, ");
			return;
		}
		
		// if this is the first interval then create it
		if( getNumIntervals() == 0 ) {
			intervals.add( min );
			intervals.add( max );
			intervalRGBs.add( rgb );
		}		
		// if this interval is outside the current range then add one or more
		// new intervals 
		else if( max <= intervals.get(0) ) {
			if( max < intervals.get(0) ) {
				intervals.add(0, max);
				intervalRGBs.add( 0, rgb );
			}
			intervals.add(0, min);
			intervalRGBs.add( 0, rgb ); 
		}
		else if( min >= intervals.get( getNumIntervals() ) ) {
			if( min > intervals.get( getNumIntervals() ) ) {
				intervals.add( min);
				intervalRGBs.add( rgb );				
			}
			intervals.add( max);
			intervalRGBs.add( rgb );
		}
		else {
		// first find the current interval that the new interval
		// begins in and add an entry for it. If the new max equals
		// an 
			int newIntIndx = -1;

			for( int i=0 ; newIntIndx == -1 && i<getNumIntervals() ; i++ ) {
				if( min > intervals.get(i) && min <= intervals.get(i+1) ) {

					newIntIndx = i+1;

					if( min < intervals.get(newIntIndx) ) {
						intervals.add( newIntIndx, min );
						intervalRGBs.add( newIntIndx, rgb );
					}
					// if the new max interval doesn't match any of the existing
					// intervals then add a new interval. Also if the max is 
					// greater than any of the current intervals then we will
					// remove them.
					newIntIndx++;

					while( newIntIndx <= getNumIntervals() &&
							max > intervals.get(newIntIndx) ) {
						intervals.remove(newIntIndx);
						intervalRGBs.remove(newIntIndx);
					}

					if( max < intervals.get(newIntIndx) ) {
						intervals.add( newIntIndx, min );
						intervalRGBs.add( newIntIndx, rgb );				
					}
				}
			}
		}

		recreateColorsFromRGB();
	}
	
    public RGB getLabelColor() {
		return labelColor;
	}

	public void setLabelColor(RGB labelColor) {
		this.labelColor = labelColor;
	}

    public Boolean getShowLabels() {
		return showLabels;
	}

	public void setShowLabels(Boolean showLabels) {
		this.showLabels = showLabels;
	}

	/**
	 * @return the reverseOrder
	 */
	@Override
	public final Boolean getReverseOrder() {
		return reverseOrder;
	}

	/**
	 * @param reverseOrder the reverseOrder to set
	 */
	@Override
	public final void setReverseOrder(Boolean reverseOrder) {
		this.reverseOrder = reverseOrder;
	}

	public Boolean getDrawToScale() {
		return drawToScale;
	}

	public void setDrawToScale(Boolean drawToScale) {
		this.drawToScale = drawToScale;
	}

	public Unit<?> getDataUnits() {
		return dataUnits;
	}

	public void setDataUnits(Unit<?> dataUnits) {
		this.dataUnits = dataUnits;
	}
	
	public ColorBarOrientation getOrientation() {
		return orientation;
	}

	public void setOrientation(ColorBarOrientation orientation) {
		this.orientation = orientation;
	}
	
    public ColorBarAnchorLocation getAnchorLoc() {
		return anchorLoc;
	}

	public void setAnchorLoc(ColorBarAnchorLocation anchorLoc) {
		this.anchorLoc = anchorLoc;
	}

	public int getNumDecimals() {
		//DecimalFormat nf = new NumberFormat();
		return numDecimals;
	}

	public void setNumDecimals(int numDecimals) {
		this.numDecimals = numDecimals;
	}

	// 
	// Methods to get/set values for the colorBar intervals and colors
	//
	public boolean isValueInInterval( int c, Float value, Unit<?> units ) {
		if( c < 0 || c >= getNumIntervals() ) {
			return false;
		}
		if( c == 0 && 
			value.equals( intervals.get(c) ) ) {
			return true;
		}
		else {
		// TODO : add support for units
			return ( value >  intervals.get(c) &&
					 value <= intervals.get(c+1) );
		}
	}
	
	public RGB getRGB( int c ) {
		return ( c < getNumIntervals() ? intervalRGBs.get(c) : null );
	}

	public void setRGB( int c, RGB rgb ) {
		if( c < 0 || c >= getNumIntervals() ) {
			return;
		}
		intervalRGBs.set(c,rgb);
		
		if( display != null ) {
			colors.get(c).dispose();
			colors.set(c, new Color( display, rgb ) );
		}
	}
	
	public Float getIntervalMin( int c ) {
		return ( c < getNumIntervals() ? intervals.get(c) : null );
	}
	
	public Float getIntervalMax( int c ) {
		return ( c < getNumIntervals() ? intervals.get(c+1) : null );
	}

	// when setting the min and max interval values this will also update
	// the min or max values of adjacent intervals
	//
	public void setIntervalMin( int c, Float min ) {
		if( c >= getNumIntervals() ) {
			return;
		}
		else if( min == Float.NEGATIVE_INFINITY && c != 0 ) {
			System.out.println("Error setting min interval to -Inf.");
			return;
		}		
		else if( c != 0 && min <= getIntervalMin(c-1) ) {
			System.out.println("Error setting min interval < prev min.");
			return;
		}
		else if( min >= getIntervalMax(c) ) { 
			System.out.println("Error setting min interval > max.");
			return;
		}
		else {
			intervals.set(c,min );		
		}
	}
	
	// It is assumed that range checking was done before this method was called.
	public void setIntervalMax( int c, Float max ) {
		if( c >= getNumIntervals() ) {
			return;
		}
		else if( max == Float.POSITIVE_INFINITY && c != 0 ) {
			System.out.println("Error setting max interval to -Inf.");
			return;
		}		
		else if( max <= getIntervalMin(c) ) {
			System.out.println("Error setting max interval < min.");
			return;
		}
		else if( c != getNumIntervals()-1 &&
				max >= getIntervalMax(c+1) ) { 
			System.out.println("Error setting max interval > next max.");
			return;
		}
		else {
			intervals.set(c+1,max );		
		}
	}

	// if numIntervals is passed in we will return the max value of the last interval
	//
	public String getLabelString(int i) {		
		if( !showLabels || i > getNumIntervals() ) {
			return null;
		}

		double mult = Math.pow(10,numDecimals);
		float intVal = intervals.get(i);
		if( intVal == Float.NEGATIVE_INFINITY ) {
			return "-Inf";
		}
		else if( intVal == Float.POSITIVE_INFINITY ) {
			return "Inf";
		}
		else { 		    
//			Float incrValue = (float) (1/Math.pow(10, numDecimals));
			String lblStr = Float.toString( (float)(Math.round(intervals.get(i) * mult) / mult) );
			// really should use a Formatter here but since i'm not, clean this up for the case numDecimals = 0 and
			// the Float keeps an '.0' at the end.
			if( numDecimals == 0 && lblStr.endsWith(".0") ) {
				lblStr = lblStr.substring(0, lblStr.length()-2);
			}
			return lblStr;
		}		
	}
	
	public void createNewInterval( int c ) {
		if( c >= getNumIntervals() ) {
			return;
		}
		
		// add another element to the lists
		// copy the last interval to the ennd of the lists
		intervals.add( null ); //intervals.get( intervals.size()-1 ) );
		intervalRGBs.add( null ); //intervalRGBs.get( intervalRGBs.size()-1 ) );
		
//		if( display != null && !colors.isEmpty() ) {
//			colors.add( null ); // colors.get( colors.size()-1 ) );
//		}
		
		for( int i=getNumIntervals()-1 ; i>c ; i-- ) {

			intervalRGBs.set( i, intervalRGBs.get(i-1) );

			intervals.set( i+1, intervals.get(i) );
			
//			if( display != null && !colors.isEmpty() ) {
//				if( i == c+1 ) {
//					colors.set( i, new Color( display, intervalRGBs.get( i) ) );
//				}
//				else {
//					colors.set( i, colors.get( i-1 ) );
//				}
//			}
		}
			
		recreateColorsFromRGB();
		
		// now split the new interval. If the min or max is Inf then we will use the 
		// size of the adjacent interval as a default size.
		if( intervals.get(c) == Float.NEGATIVE_INFINITY && 
			intervals.get(c+1) == Float.POSITIVE_INFINITY ) {
			intervals.set( c+1, 0f );			
		}
		else if( intervals.get(c) == Float.NEGATIVE_INFINITY ) {
//			if( getNumIntervals() == 2 ) {
//				intervals.set( c+1, 
//			}
//			else { // default to 1.0 for new width.
				intervals.set( c+1, intervals.get(c+1)-1.0f );
//			}
		}
		else if( intervals.get(c+1) == Float.POSITIVE_INFINITY ) {
			intervals.set( c+1, intervals.get(c)+1.0f );
		}
		else {
		    Float mult = (float) (Math.pow(10, numDecimals));
		    
		    if( intervals.get(c+1) - intervals.get(c) < 2*1/mult ) {
				System.out.println("Interval is too small to split");
				return;
			}
			Float newIntVal = (intervals.get(c+1)+intervals.get(c))/2f;
			newIntVal = (float)(Math.round(newIntVal * mult) / mult); 	

			intervals.set( c+1, newIntVal );
		}		
	}

	public void removeInterval( int c ) {
		if( c >= getNumIntervals() ) {
			return;
		}
		// if removing the last interval 
		intervals.remove( c == getNumIntervals()-1 ? c : c+1);
		intervalRGBs.remove(c);
		
		recreateColorsFromRGB();
		
//		if( display != null && !colors.isEmpty() ) {
//			colors.remove(c);
//		}
	}

	// need to set the Device before the colors are created.
	public Color getColor( int c ) {
		if( display == null ) {
			return null;
		}
		
		if( c < getNumIntervals() ) {
			// if for some reason there are fewer colors than intervals then fill in the colors
			// array from the rgb array
			while( colors.size() < getNumIntervals() ) {
				colors.add( new Color( display, getRGB( colors.size() ) ) );
			}
			return colors.get(c);
		}
		else return null;
	}
		
	// all the Colors in the intervals list will have this same device 
	public void setColorDevice( Display disp ) {
		if( display != disp ) {
			display = disp;

			recreateColorsFromRGB();
		}
	}
	
	private void recreateColorsFromRGB() {
		for( Color c : colors ) {
			c.dispose();
		}
		colors.clear();

		if( display == null ) {
			return;
		}

		for( int i=0 ; i<getNumIntervals() ; i++ ) {
			colors.add( new Color( display, intervalRGBs.get(i) ) );
		}
	}
	
	public void dispose() {
		if( display != null ) {
			display = null;
			
			for( Color c : colors ) {
				c.dispose();
//				colors.remove(c);
			}
			colors.clear();
		}
	}
			
	public float getDiscreteRange() {
		int numIntrvls = getNumIntervals();
		Float rangeMin = ( getIntervalMin(0) == Float.NEGATIVE_INFINITY ?
	            getIntervalMax(0) : getIntervalMin(0) );
		Float rangeMax = ( getIntervalMax(numIntrvls-1) == Float.POSITIVE_INFINITY ?
				getIntervalMin(numIntrvls-1) : getIntervalMax(numIntrvls-1) );
		return rangeMax-rangeMin;
	}

	public Float getLengthAsRatio() {
		return lengthAsRatio;
	}
	
	public void setLengthAsRatio(Float l) {
		lengthAsRatio = l;
	}

	public Integer getWidthInPixels() {
		return widthInPixels;
	}
	
	public void setWidthInPixels(Integer w) {
		widthInPixels = w;
	}

	// Ignore these methods. All intervals are labeled with
	// 
	public void labelInterval(int p, String lblStr) {
		
	}

	public void removeAllLabels() {
		
	}

	public boolean isIntervalLabeled( int intrvl ) {
		return showLabels;
	}

	@Override
	public int hashCode() {
		final int prime = 31;
		int result = 1;
		result = prime * result
				+ ((anchorLoc == null) ? 0 : anchorLoc.hashCode());
		result = prime * result + ((colors == null) ? 0 : colors.hashCode());
		result = prime * result
				+ ((dataUnits == null) ? 0 : dataUnits.hashCode());
//		result = prime * result + ((display == null) ? 0 : display.hashCode());
		result = prime * result
				+ ((drawToScale == null) ? 0 : drawToScale.hashCode());
		result = prime * result
				+ ((intervalRGBs == null) ? 0 : intervalRGBs.hashCode());
		result = prime * result
				+ ((intervals == null) ? 0 : intervals.hashCode());
		result = prime * result
				+ ((labelColor == null) ? 0 : labelColor.hashCode());
		result = prime * result
				+ ((lengthAsRatio == null) ? 0 : lengthAsRatio.hashCode());
		result = prime * result + numDecimals;
		result = prime * result
				+ ((orientation == null) ? 0 : orientation.hashCode());
		result = prime * result
				+ ((showLabels == null) ? 0 : showLabels.hashCode());
		result = prime * result
				+ ((widthInPixels == null) ? 0 : widthInPixels.hashCode());

		result = prime * result
		+ ((reverseOrder == null) ? 0 : reverseOrder.hashCode());		
		
		result = prime * result
		+ ((colorBarAttributesBuilder == null) ? 0 : colorBarAttributesBuilder.hashCode());

		result = prime * result
		+ ((drawColorBar == null) ? 0 : drawColorBar.hashCode());		
		
		result = prime * result
		+ ((drawBoxAroundColorBar == null) ? 0 : drawBoxAroundColorBar.hashCode());
		
		return result;
	}

	@Override
	public boolean equals(Object obj) {
		if (this == obj)
			return true;
		if (obj == null)
			return false;
		if (!(obj instanceof ColorBar))
			return false;
		
		ColorBar other = (ColorBar) obj;
		if (anchorLoc != other.anchorLoc)
			return false;
		if (colors == null) {
			if (other.colors != null)
				return false;
		} else if (!colors.equals(other.colors))
			return false;
		if (dataUnits == null) {
			if (other.dataUnits != null)
				return false;
		} else if (!dataUnits.equals(other.dataUnits))
			return false;
//		if (display == null) {
//		if (other.display != null)
//			return false;
//		} else if (!display.equals(other.display))
//			return false;
		if (drawToScale == null) {
			if (other.drawToScale != null)
				return false;
		} else if (!drawToScale.equals(other.drawToScale))
			return false;
		if (intervalRGBs == null) {
			if (other.intervalRGBs != null)
				return false;
		} else if (!intervalRGBs.equals(other.intervalRGBs))
			return false;
		if (intervals == null) {
			if (other.intervals != null)
				return false;
		} else if (!intervals.equals(other.intervals))
			return false;
		if (labelColor == null) {
			if (other.labelColor != null)
				return false;
		} else if (!labelColor.equals(other.labelColor))
			return false;
		if (lengthAsRatio == null) {
			if (other.lengthAsRatio != null)
				return false;
		} else if (!lengthAsRatio.equals(other.lengthAsRatio))
			return false;
		if (numDecimals != other.numDecimals)
			return false;
		if (orientation != other.orientation)
			return false;
		if (showLabels == null) {
			if (other.showLabels != null)
				return false;
		} else if (!showLabels.equals(other.showLabels))
			return false;
		if (widthInPixels == null) {
			if (other.widthInPixels != null)
				return false;
		} else if (!widthInPixels.equals(other.widthInPixels))
			return false;
		
		if (reverseOrder == null) {
			if (other.reverseOrder != null)
				return false;
		} else if (!reverseOrder.equals(other.reverseOrder))
			return false;
		
		if (colorBarAttributesBuilder == null) {
			if (other.colorBarAttributesBuilder != null)
				return false;
		} else if (!colorBarAttributesBuilder.equals(other.colorBarAttributesBuilder))
			return false;		
		
		if (drawColorBar == null) {
			if (other.drawColorBar != null)
				return false;
		} else if (!drawColorBar.equals(other.drawColorBar))
			return false;
		
		if (drawBoxAroundColorBar == null) {
			if (other.drawBoxAroundColorBar != null)
				return false;
		} else if (!drawBoxAroundColorBar.equals(other.drawBoxAroundColorBar))
			return false;		
		
        if (xPixelCoordFraction != other.xPixelCoordFraction )   		
		    return false;
        
        if (yPixelCoordFraction != other.yPixelCoordFraction )
        	return false;
        
		return true;
	}

	@Override
	public String getDisplayUnitStr(){
		return null;
	}
	
	@Override
	public boolean isAlignLabelInTheMiddleOfInterval(){
		return alignLabelInTheMiddleOfInterval;
	}
	
	@Override
	public void setAlignLabelInTheMiddleOfInterval(boolean b){
		alignLabelInTheMiddleOfInterval = b;
	}

	/**
	 * @return the colorBarAttributesBuilder
	 */
	public final ColorBarAttributesBuilder getColorBarAttributesBuilder() {
		return colorBarAttributesBuilder;
	}

	/**
	 * @param colorBarAttributesBuilder the colorBarAttributesBuilder to set
	 */
	public final void setColorBarAttributesBuilder(
			ColorBarAttributesBuilder colorBarAttributesBuilder) {
		this.colorBarAttributesBuilder = colorBarAttributesBuilder;
	}

	@Override
	public void setAttributesFromColorBarAttributesBuilder(ColorBarAttributesBuilder colorBarAttributesBuilder) {
		     if (colorBarAttributesBuilder != null ){
		    	 
		    	 setLengthAsRatio( (float) colorBarAttributesBuilder.getLength());
		    	 double tempWidth =  Math.abs( colorBarAttributesBuilder.getWidth() * 1000);
		    	 setWidthInPixels((int)tempWidth);
		    	 setAnchorLoc(colorBarAttributesBuilder.getAnchorLocation());
		    	 setOrientation(colorBarAttributesBuilder.getColorBarOrientation());
		    	 
		    	 setXPixelCoordFraction(colorBarAttributesBuilder.getX());
		    	 setYPixelCoordFraction(colorBarAttributesBuilder.getY());
		    	 setLabelColor(colorBarAttributesBuilder.getColor());
		    	 setDrawBoxAroundColorBar(colorBarAttributesBuilder.isDrawBoxAroundColorBar());
		    	 setDrawColorBar(colorBarAttributesBuilder.isDrawColorBar() );
		     }
		
	}

	/**
	 * @param yPixelCoordFraction the yPixelCoordFraction to set
	 */
	public void setYPixelCoordFraction(double yPixelCoordFraction) {
		this.yPixelCoordFraction = yPixelCoordFraction;
	}

	/**
	 * @return the yPixelCoordFraction
	 */
	public double getYPixelCoordFraction() {
		return yPixelCoordFraction;
	}

	/**
	 * @param xPixelCoordFraction the xPixelCoordFraction to set
	 */
	public void setXPixelCoordFraction(double xPixelCoordFraction) {
		this.xPixelCoordFraction = xPixelCoordFraction;
	}

	/**
	 * @return the xPixelCoordFraction
	 */
	public double getXPixelCoordFraction() {
		return xPixelCoordFraction;
	}
}

