package gov.noaa.nws.ncep.viz.rsc.plotdata.advanced;


import java.util.ArrayList;
import javax.measure.unit.Unit;
import javax.xml.bind.annotation.XmlAccessType;
import javax.xml.bind.annotation.XmlAccessorType;
import javax.xml.bind.annotation.XmlElement;
import javax.xml.bind.annotation.XmlRootElement;
import javax.xml.bind.annotation.adapters.XmlJavaTypeAdapter;

import org.eclipse.swt.graphics.Color;
import org.eclipse.swt.graphics.RGB;
import org.eclipse.swt.widgets.Display;

import gov.noaa.nws.ncep.viz.common.RGBColorAdapter;

/**
 *  A ColorBar for use by Plot resources to specify conditional coloring (assigns colors to 
 *  defined interval ranges). 
 * 
 * <pre>
 * SOFTWARE HISTORY
 * Date         Ticket#     Engineer    Description
 * ------------ ----------  ----------- --------------------------
 * 07/23/12     #431        S. Gurung   Initial Creation.
 * 
 * </pre>
 * 
 * @author sgurung
 * @version 1
 */

@XmlAccessorType(XmlAccessType.NONE)
@XmlRootElement(name = "conditionalColorBar")
public class ConditionalColorBar {
		
	@XmlElement(name = "label_Color")
    @XmlJavaTypeAdapter(RGBColorAdapter.class)
    private RGB labelColor = new RGB(255,255,255) ;

	@XmlElement(name = "Interval_Value")
    private final ArrayList<Float> intervals = new ArrayList<Float>();
    
    @XmlElement(name = "Interval_Color")
    @XmlJavaTypeAdapter(RGBColorAdapter.class)
    private final ArrayList<RGB> intervalRGBs = new ArrayList<RGB>();

    private final ArrayList<Color> colors = new ArrayList<Color>();
    
    private Unit<?> dataUnits;

    @XmlElement(name = "show_Labels")
    private Boolean showLabels = true;  
    
	@XmlElement (name = "draw_ToScale")
    private Boolean drawToScale = false;
    	
	private static final Float defaultLength = .5f;  
	private static final int defaultWidth = 15;
	
    @XmlElement(name = "lengthRatio")
    private Float lengthAsRatio = defaultLength;  // as a ratio of the screen size

    @XmlElement(name = "widthPx")
    private Integer widthInPixels = defaultWidth;  // in pixels ?
    
    @XmlElement(name = "num_Decimals")
    private int numDecimals=0;
    
	private Display display=null; // the Display used to create the Colors in the intervals

	public ConditionalColorBar() {	
	}

	// create from a list of intervals and colors.
	// There should be 1 more interval than colors since this will start with the
	// first interval as the lower end of the first interval. Float.NEGATIVE_INFINITY and 
	// POSITIVE_INFINITY can be used to create intervals below/above a given value. 
	// 
	
	public ConditionalColorBar( ConditionalColorBar cbar ) {
		if( cbar == null ) {
			return;
		}
		drawToScale = cbar.drawToScale;
		labelColor  = cbar.labelColor;
		dataUnits   = cbar.dataUnits;
		showLabels  = cbar.showLabels;
		lengthAsRatio = cbar.lengthAsRatio;
		widthInPixels = cbar.widthInPixels;
		numDecimals = cbar.numDecimals;
		for( int i=0 ; i<cbar.getNumIntervals() ; i++ ) {
			addColorBarInterval( cbar.getIntervalMin(i),
							     cbar.getIntervalMax(i), cbar.getRGB(i) );
		}
	}
	 
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

	public int getNumDecimals() {
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
		
		for( int i=getNumIntervals()-1 ; i>c ; i-- ) {

			intervalRGBs.set( i, intervalRGBs.get(i-1) );

			intervals.set( i+1, intervals.get(i) );			
		}
			
		recreateColorsFromRGB();
		
		// now split the new interval. If the min or max is Inf then we will use the 
		// size of the adjacent interval as a default size.
		if( intervals.get(c) == Float.NEGATIVE_INFINITY && 
			intervals.get(c+1) == Float.POSITIVE_INFINITY ) {
			intervals.set( c+1, 0f );			
		}
		else if( intervals.get(c) == Float.NEGATIVE_INFINITY ) {
				intervals.set( c+1, intervals.get(c+1)-1.0f );
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
		result = prime * result + ((colors == null) ? 0 : colors.hashCode());
		result = prime * result
				+ ((dataUnits == null) ? 0 : dataUnits.hashCode());
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
				+ ((showLabels == null) ? 0 : showLabels.hashCode());
		result = prime * result
				+ ((widthInPixels == null) ? 0 : widthInPixels.hashCode());
		return result;
	}

	@Override
	public boolean equals(Object obj) {
		if (this == obj)
			return true;
		if (obj == null)
			return false;
		if (!(obj instanceof ConditionalColorBar))
			return false;
		
		ConditionalColorBar other = (ConditionalColorBar) obj;
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
				
		return true;
	}

	//@Override
	public String getDisplayUnitStr(){
		return null;
	}
	
	//@Override
	public int getNumPixelsToReAlignLabel(){
		return 0;
	}
	
	public RGB getRGBForInterval(float c ) {
		
		for (int i=0; i<intervals.size(); i++) {
			if (c <= intervals.get(i))
				return getRGB(i-1);
		}
		
		return null;
	}
	
}

