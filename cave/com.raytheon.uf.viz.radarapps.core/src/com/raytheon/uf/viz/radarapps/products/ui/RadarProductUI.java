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
package com.raytheon.uf.viz.radarapps.products.ui;

import java.util.ArrayList;
import java.util.Collections;
import java.util.HashMap;

import org.eclipse.core.commands.Command;
import org.eclipse.core.commands.ExecutionEvent;
import org.eclipse.core.commands.common.CommandException;
import org.eclipse.ui.PlatformUI;
import org.eclipse.ui.commands.ICommandService;
import org.geotools.referencing.GeodeticCalculator;

import com.raytheon.uf.viz.core.VizApp;
import com.raytheon.uf.viz.points.IPointChangedListener;
import com.raytheon.uf.viz.points.PointsDataManager;
import com.raytheon.uf.viz.radarapps.core.RadarApps;
import com.raytheon.viz.awipstools.IToolChangedListener;
import com.raytheon.viz.awipstools.ToolsDataManager;
import com.vividsolutions.jts.geom.Coordinate;

public class RadarProductUI extends BaseRadarProductUI {

    private IToolChangedListener baselineListener;

    private IPointChangedListener pointListener;

    @Override
    protected String[] getBaselineList() {
        ArrayList<String> baselineNames = new ArrayList<String>(
                ToolsDataManager.getInstance().getBaselineNames());
        Collections.sort(baselineNames);
        return baselineNames.toArray(new String[baselineNames.size()]);
    }

    @Override
    protected String[] getPointList() {
        ArrayList<String> pointNames = new ArrayList<String>(PointsDataManager
                .getInstance().getPointNames());
        Collections.sort(pointNames);
        return pointNames.toArray(new String[pointNames.size()]);
    }

    @Override
    protected float[] getBaselineLatLon(String which) {
        maybeAddPrefListener();
        Coordinate[] c = ToolsDataManager.getInstance().getBaseline(which)
                .getCoordinates();
        if (c.length >= 2) {
            int last = c.length - 1;
            float[] result = { (float) c[0].y, (float) c[0].x,
                    (float) c[last].y, (float) c[last].x };
            return result;
        }
        return null;
    }

    private void maybeAddPrefListener() {
        if (baselineListener == null) {
            final Runnable runnable = new Runnable() {
                @Override
                public void run() {
                    if (geomCombo != null && desiredGeom.length() > 0)
                        onGeomSelected(desiredGeom);
                }
            };
            baselineListener = new IToolChangedListener() {
                @Override
                public void toolChanged() {
                    VizApp.runAsync(runnable);
                }
            };
            pointListener = new IPointChangedListener() {
                @Override
                public void pointChanged() {
                    VizApp.runAsync(runnable);
                }
            };

            ToolsDataManager.getInstance().addBaselinesChangedListener(
                    baselineListener);
            PointsDataManager.getInstance().addPointsChangedListener(
                    pointListener);
        }
    }

    @Override
    protected float[] getPointLatLon(String which) {
        maybeAddPrefListener();
        Coordinate c = PointsDataManager.getInstance().getPoint(which);
        if (c != null) {
            float[] result = { (float) c.y, (float) c.x };
            return result;
        }
        return null;
    }

    @Override
    protected float[] getRadarLocation(String radar) {
        return RadarApps.getRadarLocation(radar);
    }

    @Override
    protected int[] getBaseline(String which) {
        String radarID = getRadarID();
        if (radarID != null) {
            float[] radarLoc = getRadarLocation(radarID);
            float[] baseline = getBaselineLatLon(which);

            if (radarLoc != null && baseline != null) {
                /*
                 * This is what AWIPS 1 does, but it is wrong(?) Should be
                 * projecting lat/lon onto the xy plane of crs centered on the
                 * station.
                 */
                GeodeticCalculator gc = new GeodeticCalculator();

                gc.setStartingGeographicPoint(radarLoc[1], radarLoc[0]);
                gc.setDestinationGeographicPoint(baseline[1], baseline[0]);

                int[] result = new int[4];
                result[0] = (int) (gc.getAzimuth() * 10);
                if (result[0] < 0)
                    result[0] += 360 * 10;
                // convert meters to nmi and multiple by 10
                result[1] = (int) (gc.getOrthodromicDistance() / 1852 * 10);

                gc.setDestinationGeographicPoint(baseline[3], baseline[2]);
                result[2] = (int) (gc.getAzimuth() * 10);
                if (result[2] < 0)
                    result[2] += 360 * 10;
                result[3] = (int) (gc.getOrthodromicDistance() / 1852 * 10);

                return result;
            }
        }
        return null;
    }

    @Override
    protected int[] getPoint(String which) {
        String radarID = getRadarID();
        if (radarID != null) {
            float[] radarLoc = getRadarLocation(radarID);
            float[] baseline = getPointLatLon(which);

            if (radarLoc != null && baseline != null) {
                /*
                 * This is what AWIPS 1 does, but it is wrong! Should be
                 * projecting the lat lon onto the plane containing the
                 * stations.
                 */
                GeodeticCalculator gc = new GeodeticCalculator();

                gc.setStartingGeographicPoint(radarLoc[1], radarLoc[0]);
                gc.setDestinationGeographicPoint(baseline[1], baseline[0]);

                int[] result = new int[2];
                result[0] = (int) (gc.getAzimuth() * 10);
                if (result[0] < 0)
                    result[0] += 360 * 10;
                // convert meters to nmi and multiple by 10
                result[1] = (int) (gc.getOrthodromicDistance() / 1852 * 10);

                return result;
            }
        }
        return null;
    }

    @Override
    protected void onDispose() {
        if (baselineListener != null)
            ToolsDataManager.getInstance().removeBaselinesChangedListener(
                    baselineListener);
        if (pointListener != null)
            PointsDataManager.getInstance().removePointsChangedListener(
                    pointListener);
    }

    @Override
    protected void onLoadBaselines() {
        runCommand("com.raytheon.viz.awipstools.baselines");
    }

    @Override
    protected void onLoadPoints() {
        runCommand("com.raytheon.viz.awipstools.points");
    }

    private void runCommand(String commandName) {
        ICommandService ics = (ICommandService) PlatformUI.getWorkbench()
                .getService(ICommandService.class);
        Command command = ics.getCommand(commandName);
        try {
            command.executeWithChecks(new ExecutionEvent(command,
                    new HashMap<Object, Object>(), null, null));
        } catch (CommandException e) {
            // nothing
        }
    }

}
