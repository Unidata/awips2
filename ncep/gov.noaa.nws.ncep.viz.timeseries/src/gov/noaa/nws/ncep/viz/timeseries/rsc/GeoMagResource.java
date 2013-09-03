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
package gov.noaa.nws.ncep.viz.timeseries.rsc;

import java.text.SimpleDateFormat;
import java.util.List;
import java.util.TimeZone;

import javax.measure.converter.AddConverter;
import javax.measure.converter.UnitConverter;

import org.eclipse.swt.graphics.RGB;

import com.raytheon.uf.common.time.DataTime;
import com.raytheon.uf.viz.core.IExtent;
import com.raytheon.uf.viz.core.IGraphicsTarget;
import com.raytheon.uf.viz.core.PixelCoverage;
import com.raytheon.uf.viz.core.PixelExtent;
import com.raytheon.uf.viz.core.drawables.PaintProperties;
import com.raytheon.uf.viz.core.exception.VizException;
import com.raytheon.uf.viz.core.rsc.AbstractVizResource;
import com.raytheon.uf.viz.core.rsc.LoadProperties;
import com.raytheon.uf.viz.core.rsc.capabilities.ColorableCapability;
import com.raytheon.uf.viz.core.rsc.capabilities.MagnificationCapability;
import com.raytheon.uf.viz.core.rsc.capabilities.OutlineCapability;
import com.raytheon.uf.viz.xy.timeseries.adapter.AbstractTimeSeriesAdapter;
import com.raytheon.uf.viz.xy.timeseries.rsc.TimeSeriesResource;
import com.raytheon.viz.core.graphing.xy.XYData;
import com.raytheon.viz.core.graphing.xy.XYImageData;
import com.raytheon.viz.core.rsc.ICombinedResourceData.CombineOperation;
import com.vividsolutions.jts.geom.Coordinate;

/**
 * TODO Add Description
 * 
 * <pre>
 * 
 * SOFTWARE HISTORY
 * 
 * Date         Ticket#    Engineer    Description
 * ------------ ---------- ----------- --------------------------
 * Jul 30, 2013            sgilbert     Initial creation
 * 
 * </pre>
 * 
 * @author sgilbert
 * @version 1.0
 */

public class GeoMagResource extends TimeSeriesResource {

    public static SimpleDateFormat sdf = new SimpleDateFormat(
            "HHmm 'UTC' dd MMM yyyy");

    private UnitConverter toDeltanT;

    /**
     * @param data
     * @param props
     * @param adapter
     */
    public GeoMagResource(GeoMagResourceData data, LoadProperties props,
            AbstractTimeSeriesAdapter<?> adapter) {
        super(data, props, adapter);
    }

    @Override
    public Object getGraphKey() {
        return resourceData.getSource();
    }

    public double getDelta() {
        double delta = getMaxDataValue() - getMinDataValue();
        if (delta <= 30)
            return 50.;
        else if (delta <= 90)
            return 100.;
        else if (delta <= 200)
            return 200.;
        else if (delta <= 400)
            return 400.;
        else if (delta <= 800)
            return 800.;
        else if (delta <= 1600)
            return 1600.;
        else
            return 3200.;
    }

    public double getMedian() {
        // simple for now.... not really median value
        return (getMaxDataValue() + getMinDataValue()) / 2.0;
    }

    @Override
    protected void paintInternal(IGraphicsTarget target,
            PaintProperties paintProps) throws VizException {
        Double magnification = getCapability(MagnificationCapability.class)
                .getMagnification();

        if (data == null) {
            return;
        }

        if (secondaryResource != null) {
            secondaryResource.paint(target, paintProps);
        }

        if (combineOperation == CombineOperation.NONE) {
            return;
        }

        if (graph == null) {
            graph = descriptor.getGraph(this);
        }
        // Wait for graph to initialize before plotting to it, TODO: do better
        if (graph.isReady() == false) {
            return;
        }

        if (toDeltanT == null) {
            toDeltanT = createDataConverter();
        }

        graph.setCurrentMagnification(magnification);

        target.setupClippingPlane(graph.getExtent());
        double[] prevScreen = null;
        for (int i = 0; i < data.getData().size(); i++) {

            XYData point = data.getData().get(i);

            Double newY = toDeltanT.convert(((Number) point.getY())
                    .doubleValue());
            double[] screen = getScreenPosition(point.getX(), newY);

            RGB color = getCapability(ColorableCapability.class).getColor();
            // Draws shapes for each data point

            // draw wind Data
            if (point instanceof XYImageData) {
                // Draw all images in a striaight line. Move the line to be able
                // to accomodate multiple resources.
                List<AbstractVizResource<?, ?>> tsrs = descriptor
                        .getResourceList().getResourcesByType(
                                TimeSeriesResource.class);
                int index = tsrs.indexOf(this);
                IExtent extent = graph.getExtent();
                screen[1] = extent.getMinY() + (index + 1)
                        * (extent.getHeight() / (tsrs.size() + 1));
                XYImageData imageData = (XYImageData) point;
                imageData.setColor(color);
                imageData.setTarget(target);
                PaintProperties imagePaintProperties = new PaintProperties(
                        paintProps);
                imagePaintProperties.setAlpha(1.0f);
                double ratio = paintProps.getView().getExtent().getWidth()
                        / paintProps.getCanvasBounds().width;

                int[] dims = imageData.getDefaultSize();
                double adjDims[] = new double[2];
                adjDims[0] = (dims[0] * 0.5 * ratio) * magnification;
                adjDims[1] = (dims[1] * 0.5 * ratio) * magnification;

                Coordinate ul = new Coordinate(screen[0] - adjDims[0],
                        screen[1] - adjDims[1]);
                Coordinate ur = new Coordinate(screen[0] + adjDims[0],
                        screen[1] - adjDims[1]);
                Coordinate lr = new Coordinate(screen[0] + adjDims[0],
                        screen[1] + adjDims[1]);
                Coordinate ll = new Coordinate(screen[0] - adjDims[0],
                        screen[1] + adjDims[1]);
                PixelCoverage coverage = new PixelCoverage(ul, ur, lr, ll);

                target.drawRaster(imageData.getImage(), coverage,
                        imagePaintProperties);
                continue;
            }

            if (shapesVisible) {
                target.drawRect(new PixelExtent(screen[0] - 3, screen[0] + 3,
                        screen[1] - 3, screen[1] + 3), color, 1.0f, 1.0);
            }

            // Connects adjacent data points with a line
            if (prevScreen != null) {
                OutlineCapability lineCap = getCapability(OutlineCapability.class);
                target.drawLine(screen[0], screen[1], 0.0, prevScreen[0],
                        prevScreen[1], 0.0, color, lineCap.getOutlineWidth(),
                        lineCap.getLineStyle());
            }

            prevScreen = screen;
        }
        target.clearClippingPlane();
    }

    private UnitConverter createDataConverter() {
        // double min = getMinDataValue();
        // double max = getMaxDataValue();
        // double delta = getDelta();
        // double factor = delta / (max - min);
        UnitConverter conv = new AddConverter(-1.0 * getMedian());
        // UnitConverter conv = new AddConverter(-0.5 * delta);
        // UnitConverter conv2 = conv.concatenate(new
        // MultiplyConverter(factor));
        // UnitConverter conv3 = conv2.concatenate(new AddConverter(-1 * min));
        return conv;
    }

    private double[] getScreenPosition(Object x, Object y) {
        double valY = ((Number) y).doubleValue();
        double valX = ((DataTime) x).getValidTime().getTimeInMillis();
        return graph.getGridLocation(valX, valY);
    }

    public DataTime getStartTime() {
        return ((GeoMagResourceData) resourceData).getStartTime();
    }

    public DataTime getEndTime() {
        return ((GeoMagResourceData) resourceData).getEndTime();
    }

    @Override
    public String getName() {
        sdf.setTimeZone(TimeZone.getTimeZone("GMT"));
        // XYData point = data.getData().get(0);
        // DataTime time = (DataTime) point.getX();
        DataTime time = getStartTime();
        StringBuilder sb = new StringBuilder(
                ((GeoMagResourceData) resourceData).getStation());
        sb.append(" - ");
        sb.append(resourceData.getSource());
        sb.append(" - Begin: ");
        sb.append(sdf.format(time.getRefTimeAsCalendar().getTime()));
        if (data.getData().size() == 0)
            sb.append(" - NO DATA");
        return sb.toString();
    }

    @Override
    public String[] getTitles() {
        String baseline = "Baseline = " + String.format("%.1f", getMedian());
        String unit = String.format("%.0f", getDelta()) + " nT Delta";
        return new String[] { baseline, unit };
    }

}
