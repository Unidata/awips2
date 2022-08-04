/**
 * This software was developed and / or modified by Raytheon Company,
 * pursuant to Contract DG133W-05-CQ-1067 with the US Government.
 * 
 * U.S. EXPORT CONTROLLED TECHNICAL DATA
 * This software product contains export-restricted data whose
 * export/transfer/disclosure is restricted by U.S. law. Dissemination
 * to non-U.S. persons whether in the United States or abroad requires
 * an export license or other authorization.
 * 
 * Contractor Name:        Raytheon Company
 * Contractor Address:     6825 Pine Street, Suite 340
 *                         Mail Stop B8
 *                         Omaha, NE 68106
 *                         402.291.0100
 * 
 * See the AWIPS II Master Rights File ("Master Rights File.pdf") for
 * further licensing information.
 **/

package com.raytheon.viz.awipstools.common;

import java.text.DecimalFormat;
import java.util.ArrayList;
import java.util.List;

import javax.measure.Unit;
import javax.measure.UnitConverter;
import javax.measure.quantity.Length;

import org.eclipse.swt.graphics.RGB;
import org.geotools.referencing.GeodeticCalculator;
import org.opengis.referencing.crs.CoordinateReferenceSystem;

import com.raytheon.uf.common.status.IUFStatusHandler;
import com.raytheon.uf.common.status.UFStatus;
import com.raytheon.uf.common.status.UFStatus.Priority;
import com.raytheon.uf.viz.core.DrawableLine;
import com.raytheon.uf.viz.core.DrawableString;
import com.raytheon.uf.viz.core.IExtent;
import com.raytheon.uf.viz.core.IGraphicsTarget;
import com.raytheon.uf.viz.core.IGraphicsTarget.HorizontalAlignment;
import com.raytheon.uf.viz.core.PixelExtent;
import com.raytheon.uf.viz.core.drawables.IFont;
import com.raytheon.uf.viz.core.drawables.PaintProperties;
import com.raytheon.uf.viz.core.exception.VizException;
import com.raytheon.uf.viz.core.map.MapDescriptor;
import com.raytheon.uf.viz.core.rsc.AbstractResourceData;
import com.raytheon.uf.viz.core.rsc.AbstractVizResource;
import com.raytheon.uf.viz.core.rsc.LoadProperties;
import com.raytheon.uf.viz.core.rsc.capabilities.ColorableCapability;
import com.raytheon.uf.viz.core.rsc.tools.GenericToolsResourceData;
import com.raytheon.viz.awipstools.capabilities.DistanceScaleUnitsCapability;

import si.uom.SI;
import systems.uom.common.USCustomary;
import tec.uom.se.unit.MetricPrefix;

/**
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
 *    1/10/08       562         bphillip    Initial Creation.
 *    7/23/14       3429        mapeters    Updated deprecated drawLine() calls.
 *    7/29/14       3465        mapeters    Updated deprecated drawString() calls.
 *    10/24/18      56957       edebebe     Added capability for choosing Distance Scale Unit.
 * </pre>
 * 
 * @author bphillip
 * 
 */
public class DistanceTool extends
        AbstractVizResource<AbstractResourceData, MapDescriptor> {

    private static final transient IUFStatusHandler statusHandler = UFStatus
            .getHandler(DistanceTool.class);

    private static final double[] scales = new double[] { 0.1, 0.2, 0.4, 1, 2,
            4, 10, 20, 40, 100, 200, 400, 1000, 2000, 4000, 10000 };

    private static final Unit<Length> displayUnit = USCustomary.MILE;

    private static final UnitConverter toDisplay = SI.METRE
            .getConverterTo(displayUnit);

    private GeodeticCalculator gc;

    public DistanceTool(GenericToolsResourceData<DistanceTool> data,
            LoadProperties props) {
        super(data, props);
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

        Unit<Length> displayUnit = USCustomary.MILE;

        UnitConverter toDisplay;

        /**
         * Determine the Distance Scale Unit to display
         */
        String distanceScaleUnit = getCapability(DistanceScaleUnitsCapability.class).getDistanceScaleUnit();

        switch (distanceScaleUnit) {

            case "SM":
                displayUnit = USCustomary.MILE;
                break;
            case "NM":
                displayUnit = USCustomary.NAUTICAL_MILE;
                break;
            case "KM":
                displayUnit = MetricPrefix.KILO(SI.METRE);
                break;
            default:
                displayUnit = USCustomary.MILE;
                statusHandler.handle(Priority.PROBLEM,
                        "Invalid Distance Scale Unit: " + distanceScaleUnit);
        }
        toDisplay = SI.METRE.getConverterTo(displayUnit);

        IExtent screenExtent = paintProps.getView().getExtent();
        IExtent mapExtent = new PixelExtent(descriptor.getGridGeometry()
                .getGridRange());

        double x0 = Math.max(mapExtent.getMinX(), screenExtent.getMinX()
                + (screenExtent.getWidth() * .02));
        double y0 = Math.min(mapExtent.getMaxY(), screenExtent.getMaxY()
                - (screenExtent.getHeight() * .02));
        double[] startPixel = new double[] { x0, y0 };

        // if the distance scale origin is off map then return
        if (!mapExtent.contains(startPixel)) {
            return;
        }

        double[] startLatLon = getDescriptor().pixelToWorld(startPixel);

        double x1 = Math.min(mapExtent.getMaxX(), x0
                + (screenExtent.getWidth() * .45));
        double y1 = y0;
        double[] endPixel = new double[] { x1, y1 };

        double[] tempLatLon = getDescriptor().pixelToWorld(endPixel);

        float zoom = paintProps.getZoomLevel();

        double distMeters = 0;
        try {
            gc.setStartingGeographicPoint(startLatLon[0], startLatLon[1]);
            gc.setDestinationGeographicPoint(tempLatLon[0], tempLatLon[1]);
            distMeters = gc.getOrthodromicDistance();

        } catch (Exception e) {
            // error computing distance
            // don't paint
            statusHandler.handle(Priority.PROBLEM,
                    "Unexpected error in distance tool", e);
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

        length = length * scales[selectedIndex] / distDisplay;

        target.clearClippingPlane();

        double yOff = 75 * zoom;

        RGB color = getCapability(ColorableCapability.class).getColor();
        DecimalFormat df = new DecimalFormat("0.###");
        IFont font = target.getDefaultFont();

        int max = Math.max(0, selectedIndex - 3);
        List<DrawableLine> lines = new ArrayList<DrawableLine>(selectedIndex
                - max + 3);
        
        DrawableLine line1 = new DrawableLine();
        line1.setCoordinates(x0, y0 - yOff);
        line1.addPoint(x0, y0 + yOff);
        line1.basics.color = color;
        lines.add(line1);
        
        List<DrawableString> strings = new ArrayList<DrawableString>(
                selectedIndex - max + 2);

        DrawableString string1 = new DrawableString("0", color);
        string1.font = font;
        string1.setCoordinates(x0, y0 - yOff);
        string1.horizontalAlignment = HorizontalAlignment.CENTER;
        strings.add(string1);
        
        for (int i = max; i < selectedIndex; i++) {
            double l = length * scales[i] / scales[selectedIndex];
            String s = df.format(scales[i]);
            
            DrawableLine line2 = new DrawableLine();
            line2.setCoordinates(x0 + l, y0 - yOff);
            line2.addPoint(x0 + l, y0 + yOff);
            line2.basics.color = color;
            lines.add(line2);

            DrawableString string2 = new DrawableString(s, color);
            string2.font = font;
            string2.setCoordinates(x0 + l, y0 - yOff);
            string2.horizontalAlignment = HorizontalAlignment.CENTER;
            strings.add(string2);
        }

        DrawableLine line3 = new DrawableLine();
        line3.setCoordinates(x0 + length, y0 - yOff);
        line3.addPoint(x0 + length, y0 + yOff);
        line3.basics.color = color;
        lines.add(line3);

        DrawableString string3 = new DrawableString(
                df.format(scales[selectedIndex])
                + displayUnit.toString(), color);
        string3.font = font;
        string3.setCoordinates(x0 + length, y0 - yOff);
        string3.horizontalAlignment = HorizontalAlignment.CENTER;
        strings.add(string3);

        DrawableLine line4 = new DrawableLine();
        line4.setCoordinates(x0, y0);
        line4.addPoint(x0 + length, y0);
        line4.basics.color = color;
        lines.add(line4);

        target.drawLine(lines.toArray(new DrawableLine[0]));
        target.drawStrings(strings);

        target.setupClippingPlane(screenExtent);
    }

    @Override
    protected void disposeInternal() {
        // TODO Auto-generated method stub
    }
}