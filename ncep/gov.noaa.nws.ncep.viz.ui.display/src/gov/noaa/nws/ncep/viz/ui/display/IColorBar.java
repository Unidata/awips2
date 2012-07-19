package gov.noaa.nws.ncep.viz.ui.display;



import javax.measure.unit.Unit;

import org.eclipse.swt.graphics.Color;
import org.eclipse.swt.graphics.RGB;
import org.eclipse.swt.widgets.Display;


/**
 *  An Interface to define the methods needed to edit and paint ColorBars. 
 * 
 * <pre>
 * SOFTWARE HISTORY
 * Date         Ticket#     Engineer    Description
 * ------------ ----------  ----------- --------------------------
 * 04/04/10      #259        Greg Hull    Initial Creation.
 * 06/07/12      #794         Archana     Added the method
 *                                        getReverseOrder()
 * 06/07/12      #717         Archana     Added the methods
 *                                        getDisplayUnitStr() and 
 *                                        getNumPixelsToReAlignLabel()
 * 
 * </pre>
 * 
 * @author ghull
 * @version 1
 */

public interface IColorBar {
	
	public static enum ColorBarOrientation {
		Vertical, Horizontal
	}
	
	public static enum ColorBarAnchorLocation {
		UpperLeft, /*UpperCenter,*/ UpperRight,
		/*CenterLeft, CenterCenter, CenterRight,*/
		LowerLeft, /* LowerCenter,*/ LowerRight
	}
		
	public abstract int getNumIntervals();
	
	public abstract void addColorBarInterval( Float min, Float max, RGB rgb ) ;
	
    public abstract RGB getLabelColor();

	public abstract void setLabelColor(RGB labelColor);

    public abstract Boolean getShowLabels();

	public abstract void setShowLabels(Boolean showLabels);

	public abstract Boolean getDrawToScale();

	public abstract void setDrawToScale(Boolean drawToScale);

	public abstract Unit<?> getDataUnits();

	public abstract void setDataUnits(Unit<?> dataUnits);
	
	public abstract Integer getWidthInPixels();

	public abstract void setWidthInPixels(Integer w);
	
	public abstract ColorBarOrientation getOrientation();

	public abstract void setOrientation(ColorBarOrientation orientation);
	
    public abstract ColorBarAnchorLocation getAnchorLoc();

	public abstract void setAnchorLoc(ColorBarAnchorLocation anchorLoc);

	public abstract Float getLengthAsRatio();
	
	public abstract void setLengthAsRatio(Float l);
	
	public abstract int getNumDecimals();
	
	public abstract void setNumDecimals(int numDecimals);

	public abstract Boolean getReverseOrder();
	public abstract void setReverseOrder(Boolean b);
	
	public abstract String getDisplayUnitStr();
	
	public abstract int getNumPixelsToReAlignLabel();
	
	// Methods to get/set values for the colorBar intervals and colors
	//
	public abstract boolean isValueInInterval( int c, Float value, Unit<?> units );
	
	public abstract RGB getRGB( int intrvl );

	public abstract void setRGB( int intrvl, RGB rgb );
	
	public abstract Float getIntervalMin( int intrvl );
	
	public abstract Float getIntervalMax( int intrvl );

	// when setting the min and max interval values this will also update
	// the min or max values of adjacent intervals and may even remove the adjacent interval
	// in the case where it is eaten up by the first.
	//
	// TODO : add code to check that the new min is in range. Currently this logic is 
	// done in the colorBarEditor.
	public abstract void setIntervalMin( int intrvl, Float min );
	
	public abstract void setIntervalMax( int intrvl, Float max );

	// if numIntervals is passed in we will return the max value of the last interval
	public abstract String getLabelString(int intrvl );
	
	public abstract void labelInterval( int p, String lblStr );
	
	public abstract boolean isIntervalLabeled( int intrvl );

	public abstract void createNewInterval( int intrvl );

	public abstract void removeInterval( int intrvl );
		
	public void removeAllLabels();

	public abstract float getDiscreteRange();
	
	// need to set the Device before the colors are created.
	public abstract Color getColor( int intrvl );
		
	// all the Colors in the intervals list will have this same device 
	// 
	public abstract void setColorDevice( Display disp );
	
	public abstract void dispose();
}

