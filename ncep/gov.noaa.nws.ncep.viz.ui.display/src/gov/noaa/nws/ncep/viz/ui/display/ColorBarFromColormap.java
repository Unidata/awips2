package gov.noaa.nws.ncep.viz.ui.display;

import java.io.File;
import java.text.DecimalFormat;
import java.text.NumberFormat;
import java.util.ArrayList;

import javax.measure.unit.Unit;
import javax.xml.bind.annotation.XmlAccessType;
import javax.xml.bind.annotation.XmlAccessorType;
import javax.xml.bind.annotation.XmlElement;
import javax.xml.bind.annotation.adapters.XmlAdapter;
import javax.xml.bind.annotation.adapters.XmlJavaTypeAdapter;

import org.eclipse.swt.graphics.Color;
import org.eclipse.swt.graphics.RGB;
import org.eclipse.swt.graphics.Rectangle;
import org.eclipse.swt.widgets.Display;

import com.raytheon.uf.common.colormap.ColorMap;
import com.raytheon.uf.common.serialization.ISerializableObject;
import com.raytheon.uf.viz.core.exception.VizException;

import gov.noaa.nws.ncep.viz.common.ColorMapUtil;
import gov.noaa.nws.ncep.viz.common.RGBColorAdapter;
import gov.noaa.nws.ncep.viz.common.IntegerListAdapter;

/**
 *  An ColorBar initialized from a ColorMap. 
 * 
 * <pre>
 * SOFTWARE HISTORY
 * Date         Ticket#     Engineer    Description
 * ------------ ----------  ----------- --------------------------
 * 04/14/10      #259        Greg Hull    Initial Creation.
 * 
 * </pre>
 * 
 * @author ghull
 * @version 1
 */

@XmlAccessorType(XmlAccessType.NONE)
public class ColorBarFromColormap implements IColorBar, ISerializableObject {

	@XmlElement
	private ColorBarOrientation orientation = ColorBarOrientation.Vertical;

	@XmlElement
	private ColorBarAnchorLocation anchorLoc = ColorBarAnchorLocation.UpperLeft;

//	@XmlElement
//	private String colorMapName = null;

	// @XmlElement
	private ColorMap colorMap = null;

	// if we let the user edit the colormap then the IntervalColors will
	// be used to create a new colormap instead of reading the colorMapName
	//
	// @XmlElement
	private Boolean colorMapModified = false;

	@XmlElement
	@XmlJavaTypeAdapter(RGBColorAdapter.class)
	private RGB labelColor = new RGB(255, 255, 255);

	@XmlElement(name = "pixelRGBs")
	@XmlJavaTypeAdapter(RGBColorAdapter.class)
	private final ArrayList<RGB> pixelRGBs = new ArrayList<RGB>();

	private final ArrayList<Color> colors = new ArrayList<Color>();

	private Unit<?> dataUnits;
	// private Unit<?> displayUnits;

	@XmlElement
	private Boolean showLabels = true;

	@XmlElement
	@XmlJavaTypeAdapter(IntegerListAdapter.class)
	private ArrayList<Integer> labeledPixels = new ArrayList<Integer>();

	private static final Float defaultLength = .5f;
	private static final int defaultWidth = 10;

	@XmlElement
	private Float lengthAsRatio = defaultLength; // as a ratio of the screen

	@XmlElement
	private Integer widthInPixels = defaultWidth; // in pixels ?

	private Display display = null; // the Display used to create the Colors in
									// the intervals

	public ColorBarFromColormap() {
	}

	public ColorBarFromColormap(ColorBarFromColormap cbar) {
		if (cbar == null) {
			return;
		}
//		colorMapName = (cbar.colorMapName == null ? null : new String(
//				cbar.colorMapName));
		anchorLoc = cbar.anchorLoc;
		orientation = cbar.orientation;
		labelColor = cbar.labelColor;
		dataUnits = cbar.dataUnits;
		showLabels = cbar.showLabels;
		lengthAsRatio = cbar.lengthAsRatio;
		widthInPixels = cbar.widthInPixels;

		colorMap = null;
		if( cbar.getColorMap() != null ) {
			// ?we may need a deep copy 
			colorMap = cbar.getColorMap();
		}
		for( int p=0 ; p<cbar.getNumIntervals() ; p++ ) {
			if( cbar.isPixelLabeled(p) ) {
				labelPixel(p);
			}
		}
	}

//	public String getColorMapName() {
//		return colorMapName;
//	}
//
//	// if a colormap is specified then the intervals & colors will
//	// be defined by the colormap
//	// The colormap name must also have the
//	public void setColorMapName(String colorMapName) {
//		this.colorMapName = colorMapName;
//		// initFromColorMap( );
//	}

	public ColorMap getColorMap() {
//		if (colorMapName != null && colorMap == null) {
//			// initFromColorMap();
//		}
		return colorMap;
	}

	public boolean setColorMap(ColorMap cm) {
		if (colorMap != null) {
			// cleanup.....
			pixelRGBs.clear();
			
			for( Color clr : colors) {
				clr.dispose();
			}
			colors.clear();
		}
		colorMap = cm;
		if( colorMap == null || colorMap.getSize() == 0 ) {
			return false;
		}

		float[] reds = colorMap.getRed();
		float[] greens = colorMap.getGreen();
		float[] blues = colorMap.getBlue();

		// interval units are pixels
		dataUnits = null;

		for (int c = 0; c < reds.length; c++) {

			RGB rgb = new RGB( scaleCmapValue(reds[c]),
					           scaleCmapValue(greens[c]), scaleCmapValue(blues[c]));
			pixelRGBs.add( rgb );
			
			if (display != null) {
				colors.add(new Color(display, rgb));
			}
		}

		return true;
	}

	public int scaleCmapValue(float cmapVal) {
		int rgbVal = (int) (cmapVal * 256);
		return (rgbVal < 0 ? 0 : (rgbVal > 255 ? 255 : rgbVal));
	}

	public int getNumIntervals() {
		return (colorMap != null ? colorMap.getSize() : 0);
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
		return false;
	}

	public void setDrawToScale(Boolean drawToScale) {
//		System.out.println("drawToScale not applicable for Image ColorBars");
	}

	// public Unit<?> getDataUnits() {
	// return dataUnits;
	// }
	//
	// public void setDataUnits(Unit<?> dataUnits) {
	// this.dataUnits = dataUnits;
	// }
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
		return 0;
	}

	public void setNumDecimals(int numDecimals) {
//		System.out.println("numDecimals not applicable for Image ColorBars");
	}

	// This doesn't really have a meaning for image colorbars
	// 
	public void addColorBarInterval(Float min, Float max, RGB rgb) {
		// colorMapModified = true;
	}

	// 
	// Methods to get/set values for the colorBar pixels/intervals and colors
	//
	public RGB getRGB(int p) {
		return (p < pixelRGBs.size() ? pixelRGBs.get(p) : null);
	}

	public void setRGB(int c, RGB rgb) {
		if (c < 0 || c >= getNumIntervals()) {
			return;
		}
		colorMapModified = true;
		
		pixelRGBs.set(c, rgb);

		if( display != null ) {
			colors.get(c).dispose();
			colors.set(c, new Color(display, rgb));
		}
	}

	public Float getIntervalMin(int c) {
		return (float)c;
	}

	public Float getIntervalMax(int c) {
		return (float) (c + 1);
	}

	public boolean isValueInInterval(int c, Float value, Unit<?> units) {
		return false; // no-op for images
	}

	public void setIntervalMin(int c, Float min) {
	}

	public void setIntervalMax(int c, Float max) {
	}

	// if numIntervals is passed in we will return the max value of the last
	// interval
	public String getLabelString(int i) {
		if (!showLabels || i >= getNumIntervals()) {
			return null;
		}
		// if applying to a colorMap then use the labeledPixels map
		// otherwise get the value of the interval
		else {
			return (isPixelLabeled(i) ? Integer.toString(i) : null);
		}
	}

	public void createNewInterval(int c) {
		// currently do not support adding entries in the colormap
		//colorMapModified = true;
	}

	public void removeInterval(int c) {
		// currently do not support removing entries from the colormap
		//colorMapModified = true;
	}

	// all the Colors in the intervals list will have this same device
	public void setColorDevice(Display disp) {
		if (display != disp) {
			display = disp;

			for (Color c : colors) {
				c.dispose();
			}
			colors.clear();

			for (int i = 0; i < getNumIntervals(); i++) {
				colors.add(new Color(display, pixelRGBs.get(i)));
			}
		}
	}

	public void dispose() {
		if (display != null) {
			display = null;

			for (Color c : colors) {
				c.dispose();
				// colors.remove(c);
			}
			colors.clear();
		}
	}

	public boolean unlabelPixel(int p) {
		for (Integer pix : labeledPixels) {
			if (p == pix) {
				labeledPixels.remove(pix);
				return true;
			}
		}
		return false;
	}

	public boolean isPixelLabeled(int p) {
		return labeledPixels.contains(new Integer(p));
	}

	public void removeAllLabels() {
		labeledPixels.clear();
	}

	public float getDiscreteRange() {
		return getNumIntervals();
	}

	public Color getColor(int intrvl) {
		if( display == null ) {
			return null;
		}
		
		if( intrvl < getNumIntervals() ) {
			// if for some reason there are fewer colors than intervals then fill in the colors
			// array from the rgb array
			while( colors.size() < getNumIntervals() ) {
				colors.add( new Color( display, getRGB( colors.size() ) ) );
			}
			return colors.get(intrvl);
		}
		else return null;
	}

	public Unit<?> getDataUnits() {
		return null;
	}

	public Float getLengthAsRatio() {
		return lengthAsRatio;
	}

	public Integer getWidthInPixels() {
		return widthInPixels;
	}

	public void labelInterval(int p, String lblStr) {
		if (lblStr != null) {
			labelPixel(p);
		} else {
			unlabelPixel(p);
		}
	}

	public void labelPixel(int p) { // , String lblStr ) {
		for (Integer pix : labeledPixels) {
			if (p == pix) {
				return; // already labeled
			}
		}
		labeledPixels.add(p);
	}

	public boolean isIntervalLabeled(int intrvl) {
		return isPixelLabeled(intrvl);
	}

	public void setDataUnits(Unit<?> dataUnits) {

	}

	public void setLengthAsRatio(Float l) {
		lengthAsRatio = l;
	}

	public void setWidthInPixels(Integer w) {
		widthInPixels = w;
	}
}
