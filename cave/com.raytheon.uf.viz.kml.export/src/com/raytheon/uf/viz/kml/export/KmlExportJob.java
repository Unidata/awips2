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
package com.raytheon.uf.viz.kml.export;

import java.awt.Graphics;
import java.awt.geom.Rectangle2D;
import java.awt.image.BufferedImage;
import java.io.IOException;
import java.text.SimpleDateFormat;
import java.util.ArrayList;
import java.util.Arrays;
import java.util.Date;
import java.util.Iterator;
import java.util.List;
import java.util.TimeZone;

import org.eclipse.core.runtime.IProgressMonitor;
import org.eclipse.core.runtime.IStatus;
import org.eclipse.core.runtime.Status;
import org.eclipse.core.runtime.SubMonitor;
import org.eclipse.core.runtime.jobs.Job;
import org.eclipse.swt.graphics.RGB;
import org.geotools.geometry.DirectPosition2D;
import org.geotools.referencing.GeodeticCalculator;
import org.opengis.referencing.FactoryException;
import org.opengis.referencing.datum.PixelInCell;
import org.opengis.referencing.operation.MathTransform;
import org.opengis.referencing.operation.TransformException;

import com.raytheon.uf.common.colormap.Color;
import com.raytheon.uf.common.colormap.IColorMap;
import com.raytheon.uf.common.colormap.prefs.ColorMapParameters;
import com.raytheon.uf.common.colormap.prefs.ColorMapParameters.LabelEntry;
import com.raytheon.uf.common.geospatial.MapUtil;
import com.raytheon.uf.common.geospatial.TransformFactory;
import com.raytheon.uf.common.status.IUFStatusHandler;
import com.raytheon.uf.common.status.UFStatus;
import com.raytheon.uf.common.status.UFStatus.Priority;
import com.raytheon.uf.common.time.DataTime;
import com.raytheon.uf.viz.core.IExtent;
import com.raytheon.uf.viz.core.drawables.AbstractRenderableDisplay;
import com.raytheon.uf.viz.core.drawables.IDescriptor;
import com.raytheon.uf.viz.core.drawables.IDescriptor.FramesInfo;
import com.raytheon.uf.viz.core.drawables.PaintProperties;
import com.raytheon.uf.viz.core.drawables.PaintStatus;
import com.raytheon.uf.viz.core.drawables.ResourcePair;
import com.raytheon.uf.viz.core.exception.VizException;
import com.raytheon.uf.viz.core.jobs.JobPool;
import com.raytheon.uf.viz.core.rsc.AbstractVizResource;
import com.raytheon.uf.viz.core.rsc.IResourceGroup;
import com.raytheon.uf.viz.core.rsc.ResourceList;
import com.raytheon.uf.viz.core.rsc.capabilities.BlendableCapability;
import com.raytheon.uf.viz.core.rsc.capabilities.BlendedCapability;
import com.raytheon.uf.viz.core.rsc.capabilities.ColorMapCapability;
import com.raytheon.uf.viz.kml.export.graphics.KmlGraphicsFactoryAdapter;
import com.raytheon.uf.viz.kml.export.graphics.KmlGraphicsTarget;
import com.raytheon.uf.viz.kml.export.io.KmlOutputManager;
import com.raytheon.uf.viz.kml.export.io.KmlRootOutputManager;

import de.micromata.opengis.kml.v_2_2_0.AbstractObject;
import de.micromata.opengis.kml.v_2_2_0.Document;
import de.micromata.opengis.kml.v_2_2_0.Feature;
import de.micromata.opengis.kml.v_2_2_0.Folder;
import de.micromata.opengis.kml.v_2_2_0.LinearRing;
import de.micromata.opengis.kml.v_2_2_0.LookAt;
import de.micromata.opengis.kml.v_2_2_0.MultiGeometry;
import de.micromata.opengis.kml.v_2_2_0.Placemark;
import de.micromata.opengis.kml.v_2_2_0.PolyStyle;
import de.micromata.opengis.kml.v_2_2_0.ScreenOverlay;
import de.micromata.opengis.kml.v_2_2_0.Style;
import de.micromata.opengis.kml.v_2_2_0.TimePrimitive;
import de.micromata.opengis.kml.v_2_2_0.TimeSpan;
import de.micromata.opengis.kml.v_2_2_0.TimeStamp;
import de.micromata.opengis.kml.v_2_2_0.Units;
import de.micromata.opengis.kml.v_2_2_0.Vec2;

/**
 * The main Job for exporting KML in a background thread
 * 
 * <pre>
 * 
 * SOFTWARE HISTORY
 * 
 * Date          Ticket#  Engineer    Description
 * ------------- -------- ----------- --------------------------
 * Jun0 6, 2012           bsteffen    Initial creation
 * Jan 23, 2014  2703     bsteffen    Use framesInfo for frame count.
 * 
 * 
 * </pre>
 * 
 * @author bsteffen
 * @version 1.0
 */

public class KmlExportJob extends Job {
    private static final transient IUFStatusHandler statusHandler = UFStatus
            .getHandler(KmlExportJob.class);

    private static final SimpleDateFormat KML_TIME_FORMAT = new SimpleDateFormat(
            "yyyy-MM-dd'T'HH:mm:ss.SSS'Z'");

    private final KmlExportOptions options;

    private final JobPool backgroundPool = new JobPool("Exporting KML", 4,
            true, Job.INTERACTIVE);

    public KmlExportJob(KmlExportOptions options) {
        super("Generating Kml");
        setUser(true);
        this.options = options;
        KML_TIME_FORMAT.setTimeZone(TimeZone.getTimeZone("GMT"));
    }

    @Override
    protected IStatus run(IProgressMonitor monitor) {
        SubMonitor smonitor = SubMonitor
                .convert(monitor, "Generating KML", 800);

        try {
            copyPanes(smonitor.newChild(20, SubMonitor.SUPPRESS_NONE));

            initPanes(smonitor.newChild(80, SubMonitor.SUPPRESS_NONE));

            KmlRootOutputManager out = new KmlRootOutputManager(
                    options.getKmzFileLocation());

            exportPanes(smonitor.newChild(500, SubMonitor.SUPPRESS_NONE), out);
            joinBackground(smonitor.newChild(150, SubMonitor.SUPPRESS_NONE));
            // Do not dispose until all background processes are done
            for (KmlPane pane : options.getPanes()) {
                pane.getDisplay().dispose();
            }
            recursiveInvisibility(out.getContainer(), true);
            out.close();
            smonitor.worked(50);
        } catch (IOException e) {
            statusHandler.handle(Priority.PROBLEM, "Error writing KML", e);
        }
        smonitor.done();
        return Status.OK_STATUS;
    }

    /**
     * Copy each pane and remove any panes that aren't being exported.
     * 
     * @param monitor
     */
    private void copyPanes(IProgressMonitor monitor) {
        // Keep this code as fast as possible, if the user runs kml export in
        // the background and modifies the main display it will affect kml if
        // copy is not done.
        List<KmlPane> panes = options.getPanes();
        monitor.beginTask("Copying Displays", panes.size());
        Iterator<KmlPane> paneIt = panes.iterator();
        while (paneIt.hasNext()) {
            KmlPane pane = paneIt.next();
            List<ResourcePair> exports = pane.getResourcesToExport();
            if (exports == null || exports.isEmpty()) {
                paneIt.remove();
            } else {
                try {
                    AbstractRenderableDisplay display = pane.getDisplay();
                    // copy the current time before clone
                    FramesInfo fi = display.getDescriptor().getFramesInfo();
                    if (fi.getFrameTimes() != null) {
                        int index = fi.getFrameIndex();
                        if (index > options.getFirstFrameIndex()
                                && index < options.getLastFrameIndex()) {
                            pane.setDisplayedTime(fi.getFrameTimes()[fi
                                    .getFrameIndex()]);
                        }
                    }
                    display = display.cloneDisplay();
                    pane.setDisplay(display);
                    KmlGraphicsFactoryAdapter graphicsAdapter = new KmlGraphicsFactoryAdapter(
                            display.getView().getExtent(), pane.getBounds());
                    display.setGraphicsAdapter(graphicsAdapter);
                    KmlGraphicsTarget target = graphicsAdapter.constructTarget(
                            null, 0.0f, 0.0f);
                    target.setBackgroundColor(display.getBackgroundColor());
                    display.setup(target);
                    pane.setTarget(target);
                } catch (VizException e) {
                    statusHandler.handle(Priority.PROBLEM,
                            e.getLocalizedMessage(), e);
                    paneIt.remove();
                }
            }
            monitor.worked(1);
        }
        monitor.done();
    }

    private void initPanes(IProgressMonitor monitor) {
        List<KmlPane> panes = options.getPanes();
        monitor.beginTask("Initializing Displays", panes.size());
        Iterator<KmlPane> paneIt = panes.iterator();
        while (paneIt.hasNext()) {
            KmlPane pane = paneIt.next();
            try {
                AbstractRenderableDisplay display = pane.getDisplay();

                IDescriptor descriptor = display.getDescriptor();
                descriptor.setRenderableDisplay(display);
                descriptor.getResourceList().instantiateResources(descriptor,
                        true);
                for (ResourcePair rp : descriptor.getResourceList()) {
                    rp.getResource().init(pane.getTarget());
                    monitor.worked(1);
                }
            } catch (VizException e) {
                statusHandler.handle(Priority.PROBLEM, e.getLocalizedMessage(),
                        e);
                paneIt.remove();
            }
            monitor.worked(1);
        }
        monitor.done();
    }

    private void exportPanes(IProgressMonitor monitor, KmlOutputManager out)
            throws IOException {
        List<KmlPane> panes = options.getPanes();
        SubMonitor smonitor = SubMonitor.convert(monitor, "Exporting Displays",
                panes.size() * 100);
        int paneNumber = 1;
        for (KmlPane pane : options.getPanes()) {
            KmlOutputManager displayOut = out;
            if (!options.isSinglePane()) {
                displayOut = out.createFolder("Pane " + (paneNumber++));
            }
            AbstractRenderableDisplay display = pane.getDisplay();
            setView(pane, displayOut);
            if (options.isShadeEarth()) {
                shadeEarth(displayOut, display.getBackgroundColor());
            }
            IDescriptor descriptor = display.getDescriptor();
            List<ResourcePair> exports = new ArrayList<ResourcePair>();
            for (ResourcePair rp : descriptor.getResourceList()) {
                if (pane.getResourcesToExport().contains(rp)) {
                    exports.add(rp);
                } else {
                    rp.getProperties().setVisible(false);
                }
            }
            exportResources(smonitor.newChild(100, SubMonitor.SUPPRESS_NONE),
                    displayOut, exports, pane);
            if (smonitor.isCanceled()) {
                break;
            }
        }
        smonitor.done();
    }

    private void exportResources(IProgressMonitor monitor,
            KmlOutputManager out, List<ResourcePair> exports, KmlPane pane)
            throws IOException {
        SubMonitor smonitor = SubMonitor.convert(monitor, "Exporting Products",
                exports.size() * 100);
        KmlGraphicsTarget target = pane.getTarget();
        AbstractRenderableDisplay display = pane.getDisplay();
        IDescriptor descriptor = display.getDescriptor();
        List<Boolean> visibility = new ArrayList<Boolean>();
        for (ResourcePair rp : exports) {
            visibility.add(rp.getProperties().isVisible());
            rp.getProperties().setVisible(false);
        }
        for (int c = 0; c < exports.size(); c++) {
            ResourcePair rp = exports.get(c);
            AbstractVizResource<?, ?> rsc = rp.getResource();
            rp.getProperties().setVisible(true);
            String name = rp.getResource().getName();
            if (name == null) {
                name = rp.getResource().getClass().getSimpleName();
            }
            KmlOutputManager resourceOut = out.createFolder(name.trim());
            SubMonitor rscmonitor = smonitor.newChild(100,
                    SubMonitor.SUPPRESS_NONE);
            if (rsc.hasCapability(BlendableCapability.class)) {
                ResourceList list = rsc
                        .getCapability(BlendableCapability.class)
                        .getResourceList();
                exportResources(rscmonitor, resourceOut, list, pane);
            } else {
                int startIndex = options.getFirstFrameIndex();
                startIndex = Math.max(startIndex, 0);
                int lastIndex = options.getLastFrameIndex();
                lastIndex = Math.min(lastIndex, descriptor.getFramesInfo()
                        .getFrameCount());
                rscmonitor.beginTask("Saving " + rsc.getName(), lastIndex
                        - startIndex);
                DataTime[] times = descriptor.getFramesInfo().getTimeMap()
                        .get(rsc);
                if ((times == null || times.length == 0)
                        && rsc instanceof IResourceGroup) {
                    ResourceList list = ((IResourceGroup) rsc)
                            .getResourceList();
                    for (ResourcePair pair : list) {
                        times = descriptor.getFramesInfo().getTimeMap()
                                .get(pair.getResource());
                        if (times != null && times.length > 0) {
                            break;
                        }
                    }
                }
                List<DataTime> pastFrames = new ArrayList<DataTime>();
                for (int i = startIndex; i < lastIndex; i += 1) {
                    descriptor.setFramesInfo(new FramesInfo(i));
                    KmlOutputManager timeOut = resourceOut;
                    if (rsc.isTimeAgnostic()
                            && (times == null || times.length == 0)) {
                        i = lastIndex - 1;
                    } else {
                        if (i < 0 || times == null || i >= times.length) {
                            rscmonitor.worked(1);
                            continue;
                        }
                        DataTime time = times[i];
                        if (time == null || pastFrames.contains(time)) {
                            rscmonitor.worked(1);
                            continue;
                        }
                        timeOut = resourceOut.createFolder(time
                                .getLegendString());
                        timeOut.getContainer().setTimePrimitive(
                                getTimePrimitive(times, i));
                        pastFrames.add(time);
                    }
                    PaintProperties paintProps = new PaintProperties(1.0f,
                            (float) display.getZoom(), display.getView(),
                            pane.getBounds(), false, descriptor.getFramesInfo());
                    paintResource(rscmonitor, timeOut, display, target, rsc,
                            paintProps);
                    rscmonitor.worked(1);
                    if (rscmonitor.isCanceled()) {
                        return;
                    }
                }
                addColorMap(resourceOut, display.getBackgroundColor(), rsc);
            }
            rp.getProperties().setVisible(false);
            if (options.isPreserveVisibility() && !visibility.get(c)) {
                resourceOut.getContainer().setVisibility(false);
            }
            rscmonitor.done();
        }
    }

    private void setView(KmlPane pane, KmlOutputManager out) {
        IExtent extent = pane.getDisplay().getView().getExtent();
        try {
            DirectPosition2D center = new DirectPosition2D(
                    extent.getCenter()[0], extent.getCenter()[1]);
            DirectPosition2D corner = new DirectPosition2D(extent.getMaxX(),
                    extent.getMinX());
            MathTransform gridToLatLon = TransformFactory.gridToLatLon(pane
                    .getDisplay().getDescriptor().getGridGeometry(),
                    PixelInCell.CELL_CENTER);
            gridToLatLon.transform(center, center);
            gridToLatLon.transform(corner, corner);

            GeodeticCalculator gc = new GeodeticCalculator();
            gc.setStartingGeographicPoint(MapUtil.correctLon(center.x),
                    center.y);
            gc.setDestinationGeographicPoint(MapUtil.correctLon(corner.x),
                    corner.y);
            LookAt lookAt = out.getContainer().createAndSetLookAt();
            lookAt.setLongitude(center.x);
            lookAt.setLatitude(center.y);
            lookAt.setRange(gc.getOrthodromicDistance());
            if (pane.getDisplayedTime() != null) {
                DataTime time = pane.getDisplayedTime();
                TimeStamp ts = new TimeStamp();
                ts.setWhen(KML_TIME_FORMAT.format(new Date(time.getMatchValid())));
                // At the time of this writing the current api doesn't allow
                // setting time primitive for AbstractView
                lookAt.setAbstractViewObjectExtension(Arrays
                        .asList((AbstractObject) ts));
            }
        } catch (TransformException e) {
            statusHandler.handle(Priority.PROBLEM, e.getLocalizedMessage(), e);
        } catch (FactoryException e) {
            statusHandler.handle(Priority.PROBLEM, e.getLocalizedMessage(), e);
        }
    }

    /**
     * Given a list of times for a resource and the index of the current time,
     * generate a KML TimePrimitive for a resource. When the time mode is SPAN
     * the times in the array are used to calculate a span such that the spans
     * for every time create a continuos time line.
     * 
     * @param times
     * @param index
     * @return
     */
    private TimePrimitive getTimePrimitive(DataTime[] times, int index) {
        long validTime = times[index].getMatchValid();
        switch (options.getTimeMode()) {
        case TIME_STAMP: {
            TimeStamp ts = new TimeStamp();
            ts.setWhen(KML_TIME_FORMAT.format(new Date(validTime)));
            return ts;
        }
        case TIME_SPAN: {
            long prevValid = 0;
            long nextValid = 0;
            for (DataTime t : times) {
                if (t == null) {
                    continue;
                }
                long valid = t.getMatchValid();
                if (valid < validTime) {
                    if (prevValid == 0 || prevValid < valid) {
                        prevValid = valid;
                    }
                } else if (valid > validTime) {
                    if (nextValid == 0 || nextValid > valid) {
                        nextValid = valid;
                    }
                }
            }
            long prevDist = 0;
            long nextDist = 0;
            if (prevValid != 0) {
                nextDist = prevDist = (validTime - prevValid) / 2;
            }
            if (nextValid != 0) {
                nextDist = (nextValid - validTime) / 2;
                if (prevDist == 0) {
                    prevDist = nextDist;
                }
            }
            TimeSpan span = new TimeSpan();
            span.setBegin(KML_TIME_FORMAT
                    .format(new Date(validTime - prevDist)));
            span.setEnd(KML_TIME_FORMAT.format(new Date(validTime + nextDist)));
            return span;
        }
        default:
            return null;
        }
    }

    /**
     * KML reference documentation from google clearly states that a feature is
     * visible only if all of it's ancestors are also visible. Google
     * Earth(tested on version 6.2) ignores this and displays everything as
     * visible unless that item is specifically set to invisible even when
     * ancestors are invisible. This function makes google earth work properly
     * by finding invisible features and making all their children invisible.
     * 
     * @param feature
     * @param parentVisibility
     */
    private void recursiveInvisibility(Feature feature, boolean parentVisibility) {
        if (!parentVisibility) {
            feature.setVisibility(false);
        }
        List<Feature> features = null;
        if (feature instanceof Folder) {
            features = ((Folder) feature).getFeature();
        } else if (feature instanceof Document) {
            features = ((Document) feature).getFeature();
        }
        if (features == null) {
            return;
        }
        for (Feature f : features) {
            if (f == null) {
                continue;
            }
            recursiveInvisibility(f,
                    !Boolean.FALSE.equals(feature.isVisibility()));
        }
    }

    private void paintResource(IProgressMonitor monitor, KmlOutputManager out,
            AbstractRenderableDisplay display, KmlGraphicsTarget target,
            AbstractVizResource<?, ?> resource, PaintProperties paintProps) {
        target.setNeedsRefresh(true);
        long startTime = System.currentTimeMillis();
        while (target.isNeedsRefresh()
                || resource.getPaintStatus() != PaintStatus.PAINTED) {
            if (target.isNeedsRefresh()) {
                target.beginFrame(paintProps.getView(), false);
                try {
                    display.paint(target, paintProps);
                } catch (VizException e) {
                    statusHandler.handle(Priority.PROBLEM,
                            e.getLocalizedMessage(), e);
                    return;
                }
                target.endFrame();
            }
            if (System.currentTimeMillis() - startTime > options
                    .getMaxRefreshSeconds() * 1000) {
                statusHandler.handle(Priority.PROBLEM, resource.getName()
                        + " took more than " + options.getMaxRefreshSeconds()
                        + " seconds to paint, KML may be incomplete.");
                break;
            }
            try {
                Thread.sleep(options.getPaintSleepMillis());
            } catch (InterruptedException e) {
                /* When interupted try again right away. */
            }
            if (monitor.isCanceled()) {
                break;
            }
        }
        List<KmlFeatureGenerator> generators = new ArrayList<KmlFeatureGenerator>(
                target.getGenerators());
        for (KmlFeatureGenerator generator : generators) {
            generator
                    .setGridGeometry(display.getDescriptor().getGridGeometry());
            generator.setBackgroundColor(display.getBackgroundColor());
            generator.setOptions(options);
        }
        backgroundPool.schedule(new GenerateRunnable(generators, out));
    }

    private void addColorMap(KmlOutputManager out, RGB backcolor,
            AbstractVizResource<?, ?> rsc) {
        ColorMapParameters parameters = null;
        if (rsc.hasCapability(ColorMapCapability.class)) {
            ColorMapCapability cap = rsc
                    .getCapability(ColorMapCapability.class);
            parameters = cap.getColorMapParameters();
        } else {
            return;
        }
        double xAnchor = 0;
        if (rsc.hasCapability(BlendedCapability.class)) {
            BlendedCapability cap = rsc.getCapability(BlendedCapability.class);
            xAnchor = Math.min(1, cap.getResourceIndex());
        }
        IColorMap colorMap = parameters.getColorMap();
        if (colorMap == null) {
            // the resource isn't actually using it's color bar.
            return;
        }
        BufferedImage bi = new BufferedImage(colorMap.getSize() * 2, 25,
                BufferedImage.TYPE_INT_RGB);
        Graphics graphics = bi.getGraphics();
        graphics.setColor(new java.awt.Color(backcolor.red, backcolor.green,
                backcolor.blue));
        graphics.fillRect(0, 0, bi.getWidth(), 25);
        int x = 0;
        for (Color color : colorMap.getColors()) {
            graphics.setColor(new java.awt.Color(color.getRed(), color
                    .getGreen(), color.getBlue(), color.getAlpha()));
            graphics.drawLine(x, 0, x, 25);
            x += 1;
            graphics.drawLine(x, 0, x, 25);
            x += 1;
        }
        for (LabelEntry label : parameters.getLabels()) {
            if (label.getText().isEmpty()) {
                continue;
            }
            Rectangle2D bounds = graphics.getFontMetrics().getStringBounds(
                    label.getText(), graphics);
            int centerX = (int) (bi.getWidth() * label.getLocation());
            int leftX = (int) (centerX - bounds.getWidth() / 2);
            if (leftX < 0) {
                leftX = 0;
            } else if (leftX + bounds.getWidth() > bi.getWidth()) {
                leftX = (int) (bi.getWidth() - bounds.getWidth());
            }
            graphics.setColor(java.awt.Color.BLACK);
            graphics.fillRect(leftX - 1, 2, (int) bounds.getWidth() + 2,
                    (int) bounds.getHeight() + 2);
            graphics.setColor(java.awt.Color.WHITE);
            graphics.drawString(label.getText(), leftX,
                    (int) bounds.getHeight() + 1);
        }
        graphics.dispose();
        ScreenOverlay overlay = new ScreenOverlay();
        overlay.setName("ColorMap");
        Vec2 overlayxy = overlay.createAndSetOverlayXY();
        overlayxy.withX(xAnchor).withXunits(Units.FRACTION);
        overlayxy.withY(1).withYunits(Units.FRACTION);
        Vec2 screenxy = overlay.createAndSetScreenXY();
        screenxy.withX(xAnchor).withXunits(Units.FRACTION);
        screenxy.withY(1).withYunits(Units.FRACTION);
        overlay.createAndSetIcon().setHref(
                out.addImage(bi, "colormap" + xAnchor + ".png"));
        out.addFeature(overlay);
    }

    private void shadeEarth(KmlOutputManager out, RGB color) {
        Placemark placemark = new Placemark();
        placemark.setName("Background Color");
        Style style = new Style();
        style.createAndSetIconStyle().setScale(0.0);
        PolyStyle polyStyle = style.createAndSetPolyStyle();
        polyStyle.setFill(true);
        polyStyle.setOutline(false);
        polyStyle.setColor(KmlFeatureGenerator.toColorStr(1.0, color));
        placemark.setStyleUrl(out.getStyleUrl(style));
        // Google earth seems to do a weird things with one big polygon when you
        // zoom way out, specifically there is lots of flickering and it misses
        // big pieces towards the back of the sphere, lots of smaller polygons
        // helps avoid the missing hunks but I still see a lot of flickering.
        MultiGeometry multi = placemark.createAndSetMultiGeometry();
        for (int i = -180; i < 180; i += 10) {
            for (int j = -90; j < 90; j += 10) {
                LinearRing ring = multi.createAndAddPolygon()
                        .createAndSetOuterBoundaryIs().createAndSetLinearRing();
                ring.addToCoordinates(i, j);
                ring.addToCoordinates(i, j + 10);
                ring.addToCoordinates(i + 10, j + 10);
                ring.addToCoordinates(i + 10, j);
                ring.addToCoordinates(i, j);
            }

        }
        out.addFeature(placemark);
    }

    private void joinBackground(IProgressMonitor monitor) {
        // some tasks(like radar mosaic) can take a very long time to finish the
        // background task, so this waits for those to finish and makes an
        // attempt to let the user know how it is going.
        int remaining = backgroundPool.getWorkRemaining();
        monitor.beginTask("Finalizing KML", remaining);
        while (remaining > 0) {
            try {
                Thread.sleep(300);
            } catch (InterruptedException e) {
                /* When interupted move on right away. */
            }
            int r = backgroundPool.getWorkRemaining();
            monitor.worked(remaining - r);
            remaining = r;
            if (monitor.isCanceled()) {
                monitor.subTask("Canceling");
                backgroundPool.cancel();
                break;
            }
        }
        backgroundPool.join();
    }

    private static class GenerateRunnable implements Runnable {

        private final List<KmlFeatureGenerator> generators;

        private final KmlOutputManager outputManager;

        public GenerateRunnable(List<KmlFeatureGenerator> generators,
                KmlOutputManager outputManager) {
            this.generators = generators;
            this.outputManager = outputManager;
        }

        @Override
        public void run() {
            for (KmlFeatureGenerator generator : generators) {
                generator.addFeature(outputManager);
            }
        }

    }

}
