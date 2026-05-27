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
package com.raytheon.viz.pointdata.rsc.wind;

import java.util.Collection;

import org.eclipse.swt.graphics.RGB;
import org.geotools.referencing.GeodeticCalculator;
import org.locationtech.jts.geom.Coordinate;

import com.raytheon.uf.viz.core.IExtent;
import com.raytheon.uf.viz.core.IGraphicsTarget;
import com.raytheon.uf.viz.core.drawables.IDescriptor;
import com.raytheon.uf.viz.core.drawables.PaintProperties;
import com.raytheon.uf.viz.core.exception.VizException;
import com.raytheon.uf.viz.core.point.display.VectorGraphicsConfig;
import com.raytheon.uf.viz.core.point.display.VectorGraphicsRenderable;
import com.raytheon.uf.viz.core.rsc.AbstractVizResource;
import com.raytheon.uf.viz.core.rsc.IResourceDataChanged;
import com.raytheon.uf.viz.core.rsc.capabilities.ColorableCapability;
import com.raytheon.uf.viz.core.rsc.capabilities.DensityCapability;
import com.raytheon.uf.viz.core.rsc.capabilities.MagnificationCapability;
import com.raytheon.viz.pointdata.rsc.progdisc.GenericProgressiveDisclosure;
import com.raytheon.viz.pointdata.rsc.progdisc.GenericProgressiveDisclosure.PlotItem;

/**
 *
 * Combines a {@link VectorGraphicsRenderable} with a
 * {@link GenericProgressiveDisclosure} to generate progressively disclosed wind
 * displays.
 *
 * <pre>
 *
 * SOFTWARE HISTORY
 *
 * Date          Ticket#  Engineer  Description
 * ------------- -------- --------- --------------------------------------------
 * Nov 13, 2015  4903     bsteffen  Initial creation
 * Jun 18, 2018  6601     bsteffen  Inspect full barb instead of just text.
 * Nov 15, 2018  58492    edebebe   Enabled configurable 'Wind Barb' properties
 * Apr 22, 2019  62861    ksunil    don't draw wind barb when pixelToWorld
 *                                  returns a Double.NaN.
 * Apr 15, 2020  8126     randerso  Fix race between reproject and paint
 *
 * </pre>
 *
 * @author bsteffen
 */
public class WindPlotRenderable {

    protected final AbstractVizResource<?, ?> resource;

    private final IResourceDataChanged changeListener = new IResourceDataChanged() {

        @Override
        public void resourceChanged(ChangeType type, Object object) {
            if (type == ChangeType.CAPABILITY) {
                lastPaintedExtent = null;
            }
        }
    };

    // Parameters used to construct 'VectorGraphicsConfig'
    private static final String PLUGIN_NAME = "PointDataPlugin";

    private static final String CLASS_NAME = "WindPlotRenderable";

    protected VectorGraphicsConfig config = new VectorGraphicsConfig(
            PLUGIN_NAME, CLASS_NAME);

    protected double baseDensity = 1.0;

    protected GenericProgressiveDisclosure<Barb> barbs = new GenericProgressiveDisclosure<>();

    private VectorGraphicsRenderable renderable;

    private double lastPaintedDistance = Double.POSITIVE_INFINITY;

    private IExtent lastPaintedExtent;

    public WindPlotRenderable(AbstractVizResource<?, ?> resource) {
        this.resource = resource;

        resource.getResourceData().addChangeListener(changeListener);
    }

    public VectorGraphicsConfig getConfig() {
        return config;
    }

    public void reconfigure() {
        if (lastPaintedExtent != null) {
            lastPaintedExtent = null;
            resource.issueRefresh();
        }
    }

    public void dispose() {
        if (renderable != null) {
            renderable.dispose();
            renderable = null;
        }
        resource.getResourceData().removeChangeListener(changeListener);
    }

    public void paint(IGraphicsTarget target, PaintProperties paintProps)
            throws VizException {
        IExtent extent = paintProps.getView().getExtent();
        if (renderable != null && (lastPaintedExtent == null
                || !lastPaintedExtent.equals(extent))) {
            renderable.dispose();
            renderable = null;
        }
        if (renderable == null) {

            double magnification = resource
                    .getCapability(MagnificationCapability.class)
                    .getMagnification();
            double ratio = magnification * extent.getWidth()
                    / paintProps.getCanvasBounds().width;
            config.setSizeScaler(ratio);
            renderable = new VectorGraphicsRenderable(getDescriptor(), target,
                    config);
            lastPaintedExtent = extent.clone();
            double density = resource.getCapability(DensityCapability.class)
                    .getDensity();
            double minDist = config.getScaledSize() / baseDensity / density;
            lastPaintedDistance = minDist;
            Collection<Barb> barbs = this.barbs.runDisclosure(extent, minDist);
            GeodeticCalculator gc = new GeodeticCalculator();
            for (Barb barb : barbs) {
                double dir = barb.getDirection();
                Coordinate lonLat = barb.getLonLat();
                Coordinate plotLoc = barb.getLocation();
                gc.setStartingGeographicPoint(lonLat.x, lonLat.y);
                double[] world = getDescriptor().pixelToWorld(
                        new double[] { plotLoc.x, plotLoc.y - 1 });
                if (Double.isNaN(world[0]) || Double.isNaN(world[1])) {
                    // In some projections, pixelToWorld may return NaN. Don't
                    // paint the barb
                    continue;
                }
                gc.setDestinationGeographicPoint(world[0], world[1]);
                dir -= gc.getAzimuth();
                dir = Math.toRadians(dir);
                renderable.paintBarb(plotLoc, barb.getMagnitude(), dir);
            }
        }
        renderable.setColor(getColor());
        renderable.paint(target);
    }

    public Barb getClosestBarb(Coordinate pixelLoc) {
        Barb result = null;
        double bestDist = 100;
        if (renderable != null) {
            bestDist = renderable.getConfig().getScaledSize() * 2;
        }
        for (Barb barb : barbs.getAll()) {
            double dist = barb.getLocation().distance(pixelLoc);
            if (dist < bestDist) {
                bestDist = dist;
                result = barb;
            }
        }
        return result;
    }

    protected RGB getColor() {
        return resource.getCapability(ColorableCapability.class).getColor();
    }

    public synchronized boolean optimizeDisclosure() {
        return barbs.calculateStaticDistances();
    }

    public void addBarb(Coordinate lonLat, double magnitude, double direction) {
        addBarb(lonLat, magnitude, direction, null, false);
    }

    public void addBarb(Coordinate lonLat, double magnitude, double direction,
            boolean checkDuplicate) {
        addBarb(lonLat, magnitude, direction, null, checkDuplicate);
    }

    public void addBarb(Coordinate lonLat, double magnitude, double direction,
            String text) {
        addBarb(lonLat, magnitude, direction, text, false);
    }

    public synchronized void addBarb(Coordinate lonLat, double magnitude,
            double direction, String text, boolean checkDuplicate) {
        double[] pixel = getDescriptor()
                .worldToPixel(new double[] { lonLat.x, lonLat.y });
        Coordinate plotLoc = new Coordinate(pixel[0], pixel[1]);
        double distance = barbs.add(
                new Barb(lonLat, plotLoc, magnitude, direction, text),
                checkDuplicate);
        if (Double.isNaN(distance) || distance > lastPaintedDistance) {
            IExtent extent = lastPaintedExtent;
            if (extent != null && extent.contains(pixel)) {
                resource.issueRefresh();
                lastPaintedExtent = null;
            }
        }
    }

    public synchronized void reproject() {
        GenericProgressiveDisclosure<Barb> barbs = this.barbs;
        this.barbs = new GenericProgressiveDisclosure<>();
        for (Barb barb : barbs.getAll()) {
            addBarb(barb.getLonLat(), barb.getMagnitude(), barb.getDirection(),
                    barb.getText());
        }
        renderable.dispose();
        renderable = null;
    }

    protected IDescriptor getDescriptor() {
        return resource.getDescriptor();
    }

    public class Barb implements PlotItem {

        public final Coordinate lonLat;

        public final Coordinate plotLoc;

        public final double magnitude;

        public final double direction;

        public final String text;

        public Barb(Coordinate lonLat, Coordinate plotLoc, double magnitude,
                double direction, String text) {
            this.lonLat = lonLat;
            this.plotLoc = plotLoc;
            this.magnitude = magnitude;
            this.direction = direction;
            this.text = text;
        }

        public Coordinate getLonLat() {
            return lonLat;
        }

        @Override
        public Coordinate getLocation() {
            return plotLoc;
        }

        public double getMagnitude() {
            return magnitude;
        }

        public double getDirection() {
            return direction;
        }

        public String getText() {
            return text;
        }

    }

    public void setBaseDensity(double baseDensity) {
        this.baseDensity = baseDensity;
    }
}
