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
/**
 * 
 */
package com.raytheon.viz.gfe.rsc.zones;

import java.util.ArrayList;
import java.util.Arrays;
import java.util.HashMap;
import java.util.List;
import java.util.Random;
import java.util.concurrent.ArrayBlockingQueue;

import org.eclipse.core.runtime.IProgressMonitor;
import org.eclipse.core.runtime.IStatus;
import org.eclipse.core.runtime.Status;
import org.eclipse.core.runtime.jobs.Job;
import org.eclipse.swt.graphics.RGB;

import com.raytheon.uf.common.status.IUFStatusHandler;
import com.raytheon.uf.common.status.UFStatus;
import com.raytheon.uf.common.status.UFStatus.Priority;
import com.raytheon.uf.viz.core.drawables.IDescriptor;
import com.raytheon.uf.viz.core.drawables.IShadedShape;
import com.raytheon.uf.viz.core.exception.VizException;
import com.raytheon.viz.core.rsc.jts.JTSCompiler;
import com.raytheon.viz.core.rsc.jts.JTSCompiler.PointStyle;
import com.vividsolutions.jts.geom.Geometry;
import com.vividsolutions.jts.io.ParseException;
import com.vividsolutions.jts.io.WKBReader;

/**
 * TODO Add Description
 * 
 * <pre>
 * 
 * SOFTWARE HISTORY
 * Date         Ticket#    Engineer    Description
 * ------------ ---------- ----------- --------------------------
 *                         wldougher   Initial creation.
 * Aug 01, 2014 3471       mapeters    Updated deprecated createShadedShape() calls.
 * Aug 13, 2014 3492       mapeters    Updated deprecated createWireframeShape() calls.
 * 
 * </pre>
 * 
 * @author wldougher
 * 
 */
public class ShapeBuilderJob extends Job {
    private static final transient IUFStatusHandler statusHandler = UFStatus
            .getHandler(ShapeBuilderJob.class);

    private static final int QUEUE_SIZE = 1;

    protected ArrayBlockingQueue<ShapeBuilderRequest> requestQueue;

    protected ArrayBlockingQueue<ShapeBuilderResponse> responseQueue;

    protected final WKBReader wkbReader;

    protected String cwaID;

    protected IDescriptor descriptor;

    protected TabNameComp tabNameComp;

    protected ZoneNameComp zoneNameComp;

    protected Random rand;

    public ShapeBuilderJob(String cwaID, IDescriptor descriptor) {
        super("Building shapes...");
        this.cwaID = cwaID;
        this.descriptor = descriptor;
        requestQueue = new ArrayBlockingQueue<ShapeBuilderRequest>(QUEUE_SIZE);
        responseQueue = new ArrayBlockingQueue<ShapeBuilderResponse>(QUEUE_SIZE);
        tabNameComp = new TabNameComp();
        wkbReader = new WKBReader();
        zoneNameComp = new ZoneNameComp();
        rand = new Random();
    }

    /**
     * 
     * @see org.eclipse.core.runtime.jobs.Job#run(org.eclipse.core.runtime.IProgressMonitor)
     */
    @Override
    protected IStatus run(IProgressMonitor monitor) {
        if (monitor.isCanceled()) {
            return Status.CANCEL_STATUS;
        }
        // long startTime = System.currentTimeMillis();

        ShapeBuilderRequest request = requestQueue.poll();
        if (request != null) {

            ShapeBuilderResponse response = process(request);

            // Put the response on the queue
            long millis = 0L;
            boolean interrupted = false;
            while (!responseQueue.offer(response) && !interrupted
                    && !monitor.isCanceled()) {
                responseQueue.poll();
                try {
                    Thread.sleep(millis);
                } catch (InterruptedException e) {
                    interrupted = true;
                }
                millis = 100L;
            }
            // Trigger another paint() call; the original one probably
            // finished long ago.
            request.target.setNeedsRefresh(true);
        }

        // statusHandler.handle(Priority.VERBOSE,
        // "ShapeBuilderJob completed in "
        // + (System.currentTimeMillis() - startTime) + " ms");
        return Status.OK_STATUS;
    }

    /**
     * @param request
     * @return
     */
    protected ShapeBuilderResponse process(ShapeBuilderRequest request) {
        ShapeBuilderResponse response = new ShapeBuilderResponse();
        response.id = request.id;
        response.groups = request.groups;
        // response.groupShapes = request.groupShapes;
        response.colorMap = request.colorMap;
        response.tables = request.tables;

        // Nothing can be done if the first database query hasn't completed
        if (request.dbData != null && (request.shaded || request.outlined)) {
            // statusHandler.handle(Priority.VERBOSE,
            // "Processing shapebuilder request " + request.id
            // + " because " + request.reason);
            buildBasicMap(request, response);
            if (request.shaded) {
                buildGroupShapes(request, response);
            }
        }
        return response;
    }

    /**
     * Build the background and wireframe shapes outline and cwaOutline.
     * 
     * @param request
     *            The request for new shapes.
     * @param response
     *            The response to fill in.
     * @return response, with modifications.
     */
    protected ShapeBuilderResponse buildBasicMap(ShapeBuilderRequest request,
            ShapeBuilderResponse response) {

        boolean needBackground = request.shaded
                && (request.background == null || !request.background
                        .isDrawable());
        boolean needOutlines = request.outlined;
        needOutlines &= (request.outline == null
                || !request.outline.isDrawable() || request.cwaOutline == null || !request.cwaOutline
                .isDrawable());
        if (needBackground || needOutlines) {
            // When request.shaded==false or request.outlined==false, this
            // does more work than it needs to. Currently, all the users of this
            // class are both shaded and outlined, so the extra logic has not
            // been fully implemented.
            response.background = request.target.createShadedShape(false,
                    descriptor.getGridGeometry(), true);
            response.outline = request.target.createWireframeShape(false,
                    descriptor);
            response.cwaOutline = request.target.createWireframeShape(false,
                    descriptor);
            JTSCompiler compiler = new JTSCompiler(response.background,
                    response.outline, descriptor, PointStyle.CROSS);
            JTSCompiler cwaCompiler = new JTSCompiler(null,
                    response.cwaOutline, descriptor, PointStyle.CROSS);

            RGB backgroundColor = getColor(request,
                    Integer.valueOf(ZoneDbResource.NO_GROUP));
            byte[] emptyByteArray = new byte[0];
            for (String table : request.tables) {
                // Use binary search to find the start and end indices for table
                DbData key = new DbData(emptyByteArray, table, "", "");
                DbData afterKey = new DbData(emptyByteArray, table + " ", "",
                        "");
                int tstart = Arrays.binarySearch(request.dbData, key,
                        tabNameComp);
                int tend = Arrays.binarySearch(request.dbData, afterKey,
                        tabNameComp);
                tstart = (tstart < 0) ? -(tstart + 1) : tstart;
                tend = (tend < 0) ? -(tend + 1) : tend;

                for (int dbIdx = tstart; dbIdx < tend; dbIdx++) {
                    DbData rec = request.dbData[dbIdx];
                    try {
                        // add the zone geometry to outline
                        Geometry geom = wkbReader.read(rec.wkb);
                        compiler.handle(geom, backgroundColor);
                        if (rec.cwa.startsWith(cwaID)) {
                            // add the zone geometry to the cwa outline
                            geom = wkbReader.read(rec.wkb);
                            cwaCompiler.handle(geom);
                        }
                    } catch (ParseException e) {
                        statusHandler.handle(Priority.PROBLEM,
                                "Error parsing geometry for zone " + rec.zone,
                                e);
                    } catch (VizException e) {
                        statusHandler.handle(Priority.PROBLEM,
                                "Error handling geometry for zone " + rec.zone,
                                e);
                    }
                }
            }

            if (request.shaded) {
                response.background.compile();
            } else {
                response.background = null;
            }

            if (request.outlined) {
                response.outline.compile();
                response.cwaOutline.compile();
            } else {
                response.outline = null;
                response.cwaOutline = null;
            }
        }

        return response;
    }

    /**
     * Build the shaded shapes for the colored groups.
     * 
     * @param request
     *            The request for new shapes
     * @param response
     */
    protected ShapeBuilderResponse buildGroupShapes(
            ShapeBuilderRequest request, ShapeBuilderResponse response) {
        int groupCount = request.groups.size();
        if (request.dbData != null && request.shaded) {

            // Make sure the response has enough room for the new shapes.
            if (request.groupShapes == null) {
                response.groupShapes = new ArrayList<IShadedShape>(groupCount);
            } else {
                // Keep known group shapes
                response.groupShapes = request.groupShapes;
                response.groupShapes.ensureCapacity(groupCount);
            }

            int groupNum = 0;
            for (List<String> group : request.groups) {
                RGB color = getColor(request, Integer.valueOf(groupNum));

                IShadedShape groupShape = request.target.createShadedShape(
                        false, descriptor.getGridGeometry(), true);
                JTSCompiler groupCompiler = new JTSCompiler(groupShape, null,
                        descriptor, PointStyle.CROSS);

                if (group != null) {
                    for (String groupZone : group) {
                        DbData[] dataA = findZoneData(request, groupZone);
                        for (DbData data : dataA) {
                            // create a geometry from it
                            try {
                                Geometry geom = wkbReader.read(data.wkb);
                                groupCompiler.handle(geom, color);
                            } catch (ParseException e) {
                                statusHandler.handle(Priority.PROBLEM,
                                        "Error parsing geometry for zone "
                                                + groupZone, e);
                            } catch (VizException e) {
                                statusHandler.handle(Priority.PROBLEM,
                                        "Error handling geometry for zone "
                                                + groupZone, e);
                            }
                        }
                    }
                }

                groupShape.compile();
                if (groupNum >= request.groupShapes.size()) {
                    response.groupShapes.add(groupShape);
                } else {
                    response.groupShapes.set(groupNum, groupShape);
                }
                groupNum++;
            }

        }
        return response;
    }

    /**
     * Find the database records for the zone with the given ID. Uses the
     * zoneData array, which must be sorted by zone.
     * 
     * 2010-11-18: Allow multiple records for the same zone ID.
     * 
     * @param zoneID
     *            The ID of the zone to look up.
     * @return the DbData for the zone
     */
    protected DbData[] findZoneData(ShapeBuilderRequest request, String zoneID) {
        if (request.zoneData != null) {
            int zdIndex = -1;
            int from = -1;
            int to = -1;
            // If there are multiple records, binarySearch gives no guarantee
            // it will find the first. Look for the first entry AFTER zoneID
            DbData key = new DbData(new byte[0], "", "", zoneID + "\t");
            zdIndex = Arrays.binarySearch(request.zoneData, key, zoneNameComp);
            zdIndex = (zdIndex < 0) ? -(zdIndex + 1) : zdIndex;
            from = zdIndex;
            to = zdIndex;
            // Now walk the list backwards starting at to-1 until we
            // find an entry that doesn't have the right zoneID
            // (there will usually only be one or two)
            for (zdIndex--; zdIndex >= 0
                    && zoneID.equals(request.zoneData[zdIndex].zone); zdIndex--) {
                from = zdIndex;
            }
            return Arrays.copyOfRange(request.zoneData, from, to);
        } else {
            return new DbData[0];
        }
    }

    /**
     * Get the color that has been chosen for the designated object. If no color
     * has been set, return a random color.
     * 
     * @param key
     *            The object to look up
     * @return The color in which the object should be drawn.
     */
    protected RGB getColor(ShapeBuilderRequest request, Object key) {
        if (request.colorMap == null) {
            request.colorMap = new HashMap<Object, RGB>();
        }
        RGB color = request.colorMap.get(key);
        if (color == null) {
            color = new RGB(rand.nextInt(206) + 50, rand.nextInt(206) + 50,
                    rand.nextInt(206) + 50);
            request.colorMap.put(key, color);
        }

        return color;
    }

    /**
     * @return the cwaID
     */
    public String getCwaID() {
        return cwaID;
    }

    /**
     * @param cwaID
     *            the cwaID to set
     */
    public void setCwaID(String cwaID) {
        this.cwaID = cwaID;
    }

}
