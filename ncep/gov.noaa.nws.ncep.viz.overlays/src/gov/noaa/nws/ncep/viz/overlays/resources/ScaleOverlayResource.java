package gov.noaa.nws.ncep.viz.overlays.resources;

import gov.noaa.nws.ncep.viz.overlays.IScaleOverlayResourceData.ScaleIntervalMode;
import gov.noaa.nws.ncep.viz.overlays.IScaleOverlayResourceData.ScaleLatMode;
import gov.noaa.nws.ncep.viz.overlays.IScaleOverlayResourceData.ScaleModel;
import gov.noaa.nws.ncep.viz.overlays.IScaleOverlayResourceData.ScalePosition;
import gov.noaa.nws.ncep.viz.overlays.IScaleOverlayResourceData.ScaleTextFont;
import gov.noaa.nws.ncep.viz.overlays.IScaleOverlayResourceData.ScaleTextSize;
import gov.noaa.nws.ncep.viz.overlays.IScaleOverlayResourceData.ScaleTextStyle;
import gov.noaa.nws.ncep.viz.overlays.IScaleOverlayResourceData.ScaleUnit;
import gov.noaa.nws.ncep.viz.resources.INatlCntrsResource;

import java.text.DecimalFormat;

import javax.measure.converter.UnitConverter;
import javax.measure.quantity.Length;
import javax.measure.unit.NonSI;
import javax.measure.unit.SI;
import javax.measure.unit.Unit;
import javax.xml.bind.annotation.XmlElement;

import org.eclipse.swt.graphics.RGB;
import org.geotools.referencing.GeodeticCalculator;
import org.opengis.referencing.crs.CoordinateReferenceSystem;

import com.raytheon.uf.common.status.UFStatus;
import com.raytheon.uf.common.status.UFStatus.Priority;
import com.raytheon.uf.viz.core.IExtent;
import com.raytheon.uf.viz.core.IGraphicsTarget;
import com.raytheon.uf.viz.core.PixelExtent;
import com.raytheon.uf.viz.core.IGraphicsTarget.HorizontalAlignment;
import com.raytheon.uf.viz.core.IGraphicsTarget.TextStyle;
import com.raytheon.uf.viz.core.IGraphicsTarget.VerticalAlignment;
import com.raytheon.uf.viz.core.drawables.IFont;
import com.raytheon.uf.viz.core.drawables.PaintProperties;
import com.raytheon.uf.viz.core.drawables.IFont.Style;
import com.raytheon.uf.viz.core.exception.VizException;
import com.raytheon.uf.viz.core.map.IMapDescriptor;
import com.raytheon.uf.viz.core.rsc.AbstractResourceData;
import com.raytheon.uf.viz.core.rsc.AbstractVizResource;
import com.raytheon.uf.viz.core.rsc.LoadProperties;
import com.raytheon.uf.viz.core.rsc.capabilities.ColorableCapability;
import com.raytheon.uf.viz.core.status.StatusConstants;
//import com.raytheon.viz.awipstools.Activator;
//import com.raytheon.viz.awipstools.ui.display.AwipsToolsResourceData;

/**
 * Modified from DistanceTool class.
 * Tool to display the distance scale on the bottom left of the screen<br>
 * Note that the distance tool is only accurate in the area in which it is
 * displayed due to the warping inherent in different projections
 * 
 * <pre>
 * 
 *    SOFTWARE HISTORY
 *   
 *    Date          Ticket#     Engineer    Description
 *    ------------  ----------  ----------- --------------------------
 *    06 Oct 2010   311         B. Hebbard  Initial creation (derived from DistanceTool)
 *    19 Oct 2012   898         sgurung     Fix for fuzzy fonts
 * 
 * </pre>
 * 
 * @author bhebbard
 * 
 */
public class ScaleOverlayResource extends AbstractVizResource<ScaleOverlayResourceData, IMapDescriptor> 
                                implements INatlCntrsResource  {

    private static final double[] scales = new double[] { 0.1, 0.2, 0.4, 1, 2,
            4, 10, 20, 40, 100, 200, 400, 1000, 2000, 4000, 10000 };

    private static Unit<Length> displayUnit = NonSI.MILE;

    private static UnitConverter toDisplay = SI.METER
            .getConverterTo(displayUnit);

    private GeodeticCalculator gc;

    public ScaleOverlayResource(ScaleOverlayResourceData scaleRscdata,
            LoadProperties props) {
        super(scaleRscdata, props);
    }

    @Override
    protected void initInternal(IGraphicsTarget target) throws VizException {
        gc = new GeodeticCalculator(getDescriptor().getCRS());
    }

    /*
     * (non-Javadoc)
     * 
     * @see
     * com.raytheon.uf.viz.core.rsc.AbstractVizResource#project(org.opengis.
     * referencing.crs.CoordinateReferenceSystem)
     */
    @Override
    public void project(CoordinateReferenceSystem crs) throws VizException {
        gc = new GeodeticCalculator(crs);
    }

    @Override
    protected void paintInternal(IGraphicsTarget target,
            PaintProperties paintProps) throws VizException {
    	
    	//resourceData.setColor (new RGB(0,255,0));
        RGB color = resourceData.getColor();
        ScaleModel scaleModel = ScaleModel.values()[resourceData.getScaleModel()];
        ScalePosition scalePosition = ScalePosition.values()[resourceData.getScalePosition()];
        ScaleUnit scaleUnit = ScaleUnit.values()[resourceData.getScaleUnit()];
        ScaleIntervalMode scaleIntervalMode = ScaleIntervalMode.values()[resourceData.getScaleIntervalMode()]; 
        Integer scaleIntervalValue = resourceData.getScaleIntervalValue(); 
        ScaleLatMode scaleLatMode = ScaleLatMode.values()[resourceData.getScaleLatMode()]; 
        Integer scaleEffectiveLatitudeValue = resourceData.getScaleEffectiveLatitudeValue(); 
        ScaleTextFont scaleTextFont = ScaleTextFont.values()[resourceData.getScaleTextFont()];
        ScaleTextSize scaleTextSize = ScaleTextSize.values()[resourceData.getScaleTextSize()];
        ScaleTextStyle scaleTextStyle = ScaleTextStyle.values()[resourceData.getScaleTextStyle()];
        
        displayUnit = scaleUnit.getUnit();
        toDisplay = SI.METER.getConverterTo(displayUnit);

		IFont font = target.initializeFont(scaleTextFont.getFontName(),
				(float) (12 * scaleTextSize.getSoftwareSize()), scaleTextStyle.getStyles());
		font.setSmoothing(false);
		font.setScaleFont(false);

        IExtent screenExtent = paintProps.getView().getExtent();
        IExtent mapExtent = new PixelExtent(descriptor.getGridGeometry().getGridRange());

        double x0 = 0.0;
        double y0 = 0.0;

        switch (scalePosition) {
            case UPPER_LEFT:
            case LOWER_LEFT: {
                x0 = Math.max(mapExtent.getMinX(), screenExtent.getMinX()
                        + (screenExtent.getWidth() * .020));
        	    break;
            }
            case UPPER_CENTER:
            case LOWER_CENTER: {
                x0 = Math.max(mapExtent.getMinX(), screenExtent.getMinX()
                        + (screenExtent.getWidth() * .275));
        	    break;
            }
            case UPPER_RIGHT:
            case LOWER_RIGHT: {        	
                x0 = Math.min(mapExtent.getMaxX(), screenExtent.getMaxX()
                        - (screenExtent.getWidth() * .435));
        	    break;
            }
        }

        switch (scalePosition) {
            case UPPER_LEFT:
            case UPPER_CENTER:
            case UPPER_RIGHT: {
                y0 = Math.max(mapExtent.getMinY(), screenExtent.getMinY()
                        + (screenExtent.getHeight() * .035));
        	    break;
            }
            case LOWER_LEFT:
            case LOWER_CENTER:
            case LOWER_RIGHT: {
                y0 = Math.min(mapExtent.getMaxY(), screenExtent.getMaxY()
                        - (screenExtent.getHeight() * .020));
        	    break;
            }            
        }

        double[] startPixel = new double[] { x0, y0 };

        double x1 = Math.min(mapExtent.getMaxX(), x0 + (screenExtent.getWidth() * .45));
        double y1 = y0;
        
        double[] endPixel = new double[] { x1, y1 };

        // if the distance scale origin is off map then return
        if (!mapExtent.contains(startPixel) ||
        	!mapExtent.contains(endPixel)) {
            return;
        }

        double[] startLatLon = null;
        double[] endLatLon = null;

        if (scaleLatMode == ScaleLatMode.AUTO_AT_BAR) {
            startLatLon = getDescriptor().pixelToWorld(startPixel);
            endLatLon = getDescriptor().pixelToWorld(endPixel);
        }
        else if (scaleLatMode == ScaleLatMode.AUTO_AT_CENTER) {
            double[] center = screenExtent.getCenter();
            double lineWidth = screenExtent.getWidth() * 0.01;
            double[] left = new double[] { center[0]-lineWidth/2, center[1] };
            double[] right = new double[] { center[0]+lineWidth/2, center[1] };
            startLatLon = getDescriptor().pixelToWorld(left);
            endLatLon = getDescriptor().pixelToWorld(right);
        }      
        else if (scaleLatMode == ScaleLatMode.MANUAL) {
        	//scaleEffectiveLatitudeValue = 45;
            double[] center = screenExtent.getCenter();
            double[] centerLL = getDescriptor().pixelToWorld(center);
            centerLL[1] = scaleEffectiveLatitudeValue;
            center = getDescriptor().worldToPixel(centerLL);
            double lineWidth = screenExtent.getWidth() * 0.01;
            double[] left = new double[] { center[0]-lineWidth/2, center[1] };
            double[] right = new double[] { center[0]+lineWidth/2, center[1] };
            startLatLon = getDescriptor().pixelToWorld(left);
            endLatLon = getDescriptor().pixelToWorld(right);
        }      

        float zoom = paintProps.getZoomLevel();

        double distMeters = 0;
        try {
            gc.setStartingGeographicPoint(startLatLon[0], startLatLon[1]);
            gc.setDestinationGeographicPoint(endLatLon[0], endLatLon[1]);
            distMeters = gc.getOrthodromicDistance();
            if (scaleLatMode != ScaleLatMode.AUTO_AT_BAR) {
            	distMeters *= 100.0 * 0.45;
            }
        } catch (Exception e) {
            // error computing distance
            // don't paint
            //UFStatus.handle(Priority.PROBLEM, Activator.PLUGIN_ID,
            //        StatusConstants.CATEGORY_WORKSTATION, null,
            //        "Unexpected error in distance tool", e);
            return;
        }

        double distDisplay = toDisplay.convert(distMeters);

        double length = endPixel[0] - startPixel[0];

        int selectedIndex = 0;
        for (int i = 0; i < scales.length; i++) {
            if (scales[i] < distDisplay) {
                selectedIndex = i;
            }
        }

        //scaleIntervalValue = 100;

        length = length * scales[selectedIndex] / distDisplay;

        double segLength = 0;
        if (scaleIntervalMode == ScaleIntervalMode.MANUAL) {
            segLength = length * scaleIntervalValue / distDisplay;
        }

        target.clearClippingPlane();

        double yOff = 75 * zoom;

//        RGB color = getCapability(ColorableCapability.class).getColor();
        DecimalFormat df3 = new DecimalFormat("0.###");
        DecimalFormat df2 = new DecimalFormat("0.##");
//        IFont font = target.getDefaultFont();

        target.drawLine(x0, y0 - yOff, 0.0, x0, y0 + yOff, 0.0, color, 1);
        target.drawString(font, "0", x0, y0 - yOff, 0.0, TextStyle.NORMAL,
                color, HorizontalAlignment.CENTER, VerticalAlignment.BOTTOM,
                null);
        double yLegend = 0.0;
        switch (scalePosition) {
            case UPPER_LEFT:
            case UPPER_CENTER:
            case UPPER_RIGHT: {
                yLegend = y0 + 3.6*yOff*scaleTextSize.getSoftwareSize();
                break;
            }
            case LOWER_LEFT:
            case LOWER_CENTER:
            case LOWER_RIGHT: {
                yLegend = y0 - 3.2*yOff*scaleTextSize.getSoftwareSize();
                break;
            }
        }
        if (scaleLatMode != ScaleLatMode.AUTO_AT_BAR) {
        	double meanLat = (startLatLon[1]+endLatLon[1])/2.0;
        	String s = df2.format(Math.abs(meanLat));
            target.drawString(font, "True at "+s+(meanLat>=-0.001?"N":"S"), x0-0.75*yOff, yLegend, 0.0, TextStyle.NORMAL,
        		    color, HorizontalAlignment.LEFT, VerticalAlignment.BOTTOM,
        		    null);
        }
        if (scaleIntervalMode == ScaleIntervalMode.MANUAL) {
        	double l = segLength;
        	int i;
        	for (i = 1; l < 0.45*length ; i++) {
        		l = segLength * i;
        		String s = df3.format(scaleIntervalValue * i);
        		target.drawLine(x0 + l, y0 - yOff, 0.0, x0 + l, y0 + yOff, 0.0,
        				color, 1);
        		target.drawString(font, s, x0 + l, y0 - yOff*scaleTextSize.getSoftwareSize(), 0.0,
        				TextStyle.NORMAL, color, HorizontalAlignment.CENTER,
        				VerticalAlignment.BOTTOM, null);
        	}
        	l = segLength * i;
            target.drawLine(x0 + l, y0 - yOff, 0.0, x0 + l, y0 + yOff,
                    0.0, color, 1);
            target.drawString(font, df3.format(scaleIntervalValue * i) + " "
                    + displayUnit.toString(), x0 + l, y0 - yOff*scaleTextSize.getSoftwareSize(), 0.0,
                    TextStyle.NORMAL, color, HorizontalAlignment.CENTER,
                    VerticalAlignment.BOTTOM, null);
            target.drawLine(x0, y0, 0.0, x0 + l, y0, 0.0, color, 1);
        }
        else {
        	for (int i = Math.max(0, selectedIndex - 3); i < selectedIndex; i++) {
        		double l = length * scales[i] / scales[selectedIndex];
        		String s = df3.format(scales[i]);
        		target.drawLine(x0 + l, y0 - yOff, 0.0, x0 + l, y0 + yOff, 0.0,
        				color, 1);
        		target.drawString(font, s, x0 + l, y0 - yOff, 0.0,
        				TextStyle.NORMAL, color, HorizontalAlignment.CENTER,
        				VerticalAlignment.BOTTOM, null);
        	}
            target.drawLine(x0 + length, y0 - yOff, 0.0, x0 + length, y0 + yOff,
                    0.0, color, 1);
            target.drawString(font, df3.format(scales[selectedIndex]) + " "
                    + displayUnit.toString(), x0 + length, y0 - yOff, 0.0,
                    TextStyle.NORMAL, color, HorizontalAlignment.CENTER,
                    VerticalAlignment.BOTTOM, null);
            target.drawLine(x0, y0, 0.0, x0 + length, y0, 0.0, color, 1);
        }

        target.setupClippingPlane(screenExtent);

    }

//  @Override
  public void resourceAttrsModified() {
  	//needsUpdate = true; 
  }


    @Override
    protected void disposeInternal() {
        // TODO Auto-generated method stub
    }
}
