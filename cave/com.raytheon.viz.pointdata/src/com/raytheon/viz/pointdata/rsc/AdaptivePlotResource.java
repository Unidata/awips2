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
package com.raytheon.viz.pointdata.rsc;

import java.util.ArrayList;
import java.util.Collections;
import java.util.Comparator;
import java.util.HashSet;
import java.util.List;
import java.util.Set;

import org.eclipse.swt.graphics.RGB;

import com.raytheon.uf.common.geospatial.ReferencedCoordinate;
import com.raytheon.uf.common.status.UFStatus.Priority;
import com.raytheon.uf.viz.core.IGraphicsTarget;
import com.raytheon.uf.viz.core.IGraphicsTarget.PointStyle;
import com.raytheon.uf.viz.core.drawables.IDescriptor;
import com.raytheon.uf.viz.core.drawables.PaintProperties;
import com.raytheon.uf.viz.core.exception.VizException;
import com.raytheon.uf.viz.core.rsc.AbstractVizResource;
import com.raytheon.uf.viz.core.rsc.IResourceDataChanged.ChangeType;
import com.raytheon.uf.viz.core.rsc.LoadProperties;
import com.raytheon.uf.viz.core.rsc.capabilities.ColorableCapability;
import com.raytheon.uf.viz.core.rsc.capabilities.MagnificationCapability;
import com.raytheon.uf.viz.core.rsc.capabilities.PointCapability;
import com.raytheon.viz.pointdata.rsc.AdaptivePlotResourceData.PlotObject;
import org.locationtech.jts.geom.Coordinate;

/**
 * Adaptive plot resource. Used for displaying spotters readout, etc.
 * 
 * <pre>
 * 
 * SOFTWARE HISTORY
 * 
 * Date         Ticket#    Engineer    Description
 * ------------ ---------- ----------- --------------------------
 * Aug 03, 2011            mschenke    Initial creation
 * Apr 30, 2014 3092       njensen     Sped up paintInternal()
 * Nov 01, 2017 6271       bsteffen    Override resourceDataChanged for updates.
 * 
 * </pre>
 * 
 * @author mschenke
 */
public class AdaptivePlotResource
        extends AbstractVizResource<AdaptivePlotResourceData, IDescriptor> {

    private static final int PIXEL_THRESHOLD = 25;

    private double pixelRatio;

    private Set<PlotObject> plots = new HashSet<>();

    protected AdaptivePlotResource(AdaptivePlotResourceData resourceData,
            LoadProperties loadProperties) {
        super(resourceData, loadProperties);
    }

    @Override
    protected void resourceDataChanged(ChangeType type, Object updateObject) {
        if (updateObject instanceof PlotObject[]) {
            for (PlotObject obj : (PlotObject[]) updateObject) {
                if (type == ChangeType.DATA_UPDATE) {
                    addPlotObject(obj);
                } else {
                    plots.remove(obj);
                }
            }
        }
    }

    public void addPlotObject(PlotObject obj) {
        plots.add(obj);
    }

    @Override
    protected void initInternal(IGraphicsTarget target) throws VizException {
        getCapability(PointCapability.class).setPointStyle(PointStyle.STAR);
    }

    @Override
    protected void paintInternal(IGraphicsTarget target,
            PaintProperties paintProps) throws VizException {
        pixelRatio = paintProps.getView().getExtent().getWidth()
                / paintProps.getCanvasBounds().width;

        RGB color = getCapability(ColorableCapability.class).getColor();
        float mag = getCapability(MagnificationCapability.class)
                .getMagnification().floatValue();
        PointStyle style = getCapability(PointCapability.class).getPointStyle();
        List<double[]> points = new ArrayList<>(plots.size());
        for (PlotObject object : plots) {
            points.add(descriptor.worldToPixel(
                    new double[] { object.longitude, object.latitude }));
        }
        target.drawPoints(points, color, style, mag);
    }

    @Override
    public String inspect(ReferencedCoordinate coord) throws VizException {
        try {
            Coordinate latLon = coord.asLatLon();
            double[] pixelLoc = descriptor
                    .worldToPixel(new double[] { latLon.x, latLon.y });
            double minDist = Double.MAX_VALUE;
            PlotObject closest = null;
            for (PlotObject plot : plots) {
                double[] spotLoc = descriptor.worldToPixel(
                        new double[] { plot.longitude, plot.latitude });
                double dist = Math.sqrt(
                        Math.pow(Math.abs(spotLoc[0] - pixelLoc[0]), 2) + Math
                                .pow(Math.abs(spotLoc[1] - pixelLoc[1]), 2));
                if (dist < minDist) {
                    minDist = dist;
                    closest = plot;
                }
            }

            if (closest != null && (minDist / pixelRatio) <= PIXEL_THRESHOLD) {
                List<PlotObject> objs = new ArrayList<>();
                objs.add(closest);
                StringBuilder rval = new StringBuilder();
                for (PlotObject plot : plots) {
                    if (plot != closest) {
                        double[] spotLoc = descriptor.worldToPixel(
                                new double[] { plot.longitude, plot.latitude });
                        double dist = Math.sqrt(Math
                                .pow(Math.abs(spotLoc[0] - pixelLoc[0]), 2)
                                + Math.pow(Math.abs(spotLoc[1] - pixelLoc[1]),
                                        2));
                        if (Math.abs(dist - minDist) / pixelRatio < 1) {
                            objs.add(plot);
                        }
                    }
                }

                if (objs.size() > 1) {
                    Collections.sort(objs, new Comparator<PlotObject>() {
                        @Override
                        public int compare(PlotObject o1, PlotObject o2) {
                            return o2.id.compareTo(o1.id);
                        }
                    });
                }

                for (int i = 0; i < objs.size(); ++i) {
                    if (i > 0) {
                        rval.append('\n');
                    }
                    rval.append(objs.get(i));
                }

                return rval.toString();
            }
        } catch (Exception e) {
            AdaptivePlotResourceData.statusHandler.handle(Priority.PROBLEM,
                    "Error inspecting plots", e);
        }

        return "NO DATA";
    }

    @Override
    protected void disposeInternal() {
        // Nothing to do
    }

}
