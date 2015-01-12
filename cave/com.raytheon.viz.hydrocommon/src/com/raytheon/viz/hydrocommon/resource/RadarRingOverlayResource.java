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
package com.raytheon.viz.hydrocommon.resource;

import java.util.HashMap;
import java.util.Map;
import java.util.Timer;
import java.util.TimerTask;

import org.eclipse.swt.graphics.RGB;
import org.eclipse.ui.IWorkbenchPage;
import org.eclipse.ui.PlatformUI;
import org.geotools.referencing.CRS;
import org.opengis.referencing.FactoryException;
import org.opengis.referencing.crs.CoordinateReferenceSystem;
import org.opengis.referencing.crs.ProjectedCRS;
import org.opengis.referencing.operation.MathTransform;
import org.opengis.referencing.operation.TransformException;

import com.raytheon.uf.common.geospatial.MapUtil;
import com.raytheon.uf.common.status.IUFStatusHandler;
import com.raytheon.uf.common.status.UFStatus;
import com.raytheon.uf.common.status.UFStatus.Priority;
import com.raytheon.uf.viz.core.IGraphicsTarget;
import com.raytheon.uf.viz.core.drawables.IWireframeShape;
import com.raytheon.uf.viz.core.drawables.PaintProperties;
import com.raytheon.uf.viz.core.exception.VizException;
import com.raytheon.uf.viz.core.map.MapDescriptor;
import com.raytheon.uf.viz.core.rsc.AbstractResourceData;
import com.raytheon.uf.viz.core.rsc.AbstractVizResource;
import com.raytheon.uf.viz.core.rsc.LoadProperties;
import com.raytheon.uf.viz.core.rsc.capabilities.OutlineCapability;
import com.raytheon.viz.hydrocommon.radaroverlay.RadarRingOverlayDAO;
import com.raytheon.viz.hydrocommon.radaroverlay.RadarRingOverlayData;

/**
 * HydroView Radar Rings Overlay Resource.
 * 
 * <pre>
 * 
 * SOFTWARE HISTORY
 * Date         Ticket#    Engineer    Description
 * ------------ ---------- ----------- --------------------------
 * Jan 18, 2010 1783       mpduff      Initial creation.
 * Apr 04, 2011 8934       mnash       Fix memory leaks, added timer to retrieve data
 * May 27, 2014 3133       njensen     Organized imports
 * 
 * </pre>
 * 
 * @author mpduff
 * @version 1.0
 */

public class RadarRingOverlayResource extends
        AbstractVizResource<AbstractResourceData, MapDescriptor> {
    private static final transient IUFStatusHandler statusHandler = UFStatus
            .getHandler(RadarRingOverlayResource.class);

    /** Number of segments in the circle */
    private static final int RANGE_CIRCLE_PTS = 360;

    protected static final RGB GREEN = new RGB(0, 255, 0);

    /** Radar ring range */
    protected static final int RANGE = 230000;

    protected Map<String, IWireframeShape> shapes = null;

    protected Timer timer;

    protected TimerTask timerTask;

    protected RGB displayColor = GREEN;

    protected boolean loadDone = false;

    // holds the data
    protected Map<String, RadarRingOverlayData> dataMap;

    /** Data access object */
    protected RadarRingOverlayDAO dao = new RadarRingOverlayDAO();

    /**
     * Constructor.
     * 
     * @param rscData
     *            The RadarRingOverlayResourceData
     * @param loadProps
     *            The LoadProperties
     */
    public RadarRingOverlayResource(RadarRingOverlayResourceData rscData,
            LoadProperties loadProps) {
        super(rscData, loadProps);
    }

    /**
     * Compute the range circle. Reused from RadarResource.java.
     * 
     * @param target
     *            The graphics target
     * @param crs
     *            The CoordinateReferenceSystem
     * @param range
     *            The Range in m
     * @return The IWireframeShape
     */
    protected void computeRangeCircle(IGraphicsTarget target,
            CoordinateReferenceSystem crs, String id) {
        IWireframeShape rangeCircle = target.createWireframeShape(true,
                descriptor);

        try {
            MathTransform mt = CRS.findMathTransform(crs, descriptor.getCRS());

            double[][] pts = new double[RANGE_CIRCLE_PTS + 1][2];
            double azDelta = 2 * Math.PI / RANGE_CIRCLE_PTS;
            double az = 0.0;
            double[] input = new double[2];
            double[] output = new double[2];
            for (int i = 0; i < pts.length; i++) {
                input[0] = RANGE * Math.cos(az);
                input[1] = RANGE * Math.sin(az);
                mt.transform(input, 0, output, 0, 1);
                pts[i] = descriptor.worldToPixel(output, descriptor.getCRS());
                az += azDelta;
            }
            pts[RANGE_CIRCLE_PTS] = pts[0];

            rangeCircle.addLineSegment(pts);
        } catch (TransformException e) {
            e.printStackTrace();
        } catch (FactoryException e) {
            e.printStackTrace();
        }
        shapes.put(id, rangeCircle);
    }

    /**
     * Get the Coordinate Reference System.
     * 
     * @param lat
     *            The latitude
     * @param lon
     *            The longitude
     * @return The Coordinate Reference System
     */
    protected ProjectedCRS getCRS(double lat, double lon) {
        ProjectedCRS crs = MapUtil.constructStereographic(
                MapUtil.AWIPS_EARTH_RADIUS, MapUtil.AWIPS_EARTH_RADIUS, lat,
                lon);

        return crs;
    }

    /**
     * Paint the rings.
     * 
     * @param target
     *            The IGraphicsTarget
     */
    protected void paintRings(IGraphicsTarget target) {
        IWorkbenchPage page = PlatformUI.getWorkbench()
                .getActiveWorkbenchWindow().getActivePage();
        if (page == null) {
            return;
        }

        try {
            if (dataMap != null && loadDone) {
                for (RadarRingOverlayData rdata : dataMap.values()) {
                    CoordinateReferenceSystem crs = getCRS(rdata.getLat(),
                            rdata.getLon());
                    if (!shapes.containsKey(rdata.getRadId())) {
                        computeRangeCircle(target, crs, rdata.getRadId());
                    }

                    if ((shapes.get(rdata.getRadId()) != null)
                            && (getCapability(OutlineCapability.class)
                                    .isOutlineOn())) {
                        if (rdata.getColor() == null) {
                            rdata.setColor(GREEN);
                        }
                        target.drawWireframeShape(shapes.get(rdata.getRadId()),
                                rdata.getColor(),
                                getCapability(OutlineCapability.class)
                                        .getOutlineWidth(),
                                getCapability(OutlineCapability.class)
                                        .getLineStyle());
                    }
                }
            }
        } catch (VizException e1) {
            statusHandler.handle(Priority.ERROR, "Could not draw radar rings",
                    e1);
        }
    }

    @Override
    protected void disposeInternal() {
        for (IWireframeShape shape : shapes.values()) {
            shape.dispose();
        }
        timer.cancel();
        timerTask.cancel();
    }

    @Override
    protected void initInternal(IGraphicsTarget target) throws VizException {
        shapes = new HashMap<String, IWireframeShape>();
        dataMap = dao.getData();
        constructDataTimer();
    }

    @Override
    protected void paintInternal(IGraphicsTarget target,
            PaintProperties paintProps) throws VizException {
        paintRings(target);
    }

    protected void constructDataTimer() {
        timer = new Timer("hydroradarRingRetrieve");
        timerTask = new TimerTask() {
            @Override
            public void run() {
                try {
                    dataMap = dao.getData();
                    loadDone = true;
                    issueRefresh();
                } catch (VizException e) {
                    statusHandler.handle(Priority.PROBLEM,
                            "Unable to retrieve the data", e);

                }
            }
        };
        // update data every 30 seconds
        timer.schedule(timerTask, 0, 30000);
    }

    /*
     * (non-Javadoc)
     * 
     * @see com.raytheon.uf.viz.core.rsc.AbstractVizResource#getName()
     */
    @Override
    public String getName() {
        return ((RadarRingOverlayResourceData) resourceData).getMapName();
    }
}
