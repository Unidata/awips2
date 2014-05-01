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
package com.raytheon.uf.viz.monitor.safeseas.resources;

import java.util.HashMap;
import java.util.Map;

import org.eclipse.swt.widgets.Display;
import org.opengis.referencing.FactoryException;
import org.opengis.referencing.operation.TransformException;

import com.raytheon.uf.common.dataplugin.fog.FogRecord;
import com.raytheon.uf.common.dataplugin.fog.FogRecord.FOG_THREAT;
import com.raytheon.uf.common.geospatial.ReferencedCoordinate;
import com.raytheon.uf.common.geospatial.ReferencedObject.Type;
import com.raytheon.uf.common.status.IUFStatusHandler;
import com.raytheon.uf.common.status.UFStatus;
import com.raytheon.uf.common.status.UFStatus.Priority;
import com.raytheon.uf.viz.monitor.fog.FogCommonThreat;
import com.raytheon.uf.viz.monitor.fog.xml.FogMonitorAlgorithmXML;
import com.raytheon.uf.viz.monitor.safeseas.SafeSeasMonitor;
import com.vividsolutions.jts.geom.Coordinate;
import com.vividsolutions.jts.geom.Geometry;

/**
 * FogThreat for SafeSeas monitor
 * 
 * <pre>
 * 
 * SOFTWARE HISTORY
 * 
 * Date         Ticket#    Engineer    Description
 * ------------ ---------- ----------- -----------------
 * Apr 30, 2011 #4981      skorolev     Initial creation
 * Oct.30, 2012 #1297      skorolev     Changed HashMap to Map
 * 
 * </pre>
 * 
 * @author skorolev
 * @version 1.0
 */
public class SSFogThreat extends FogCommonThreat {

    private final IUFStatusHandler statusHandler = UFStatus
            .getHandler(SSFogThreat.class);

    /** SafeSeasMonitor monitor **/
    private SafeSeasMonitor ssmonitor = SafeSeasMonitor.getInstance();

    /**
     * serialization constructor
     */
    public SSFogThreat() {

    }

    /**
     * used constructor
     */
    public SSFogThreat(FogMonitorAlgorithmXML fogAlgXML) {
        this.fogAlgXML = fogAlgXML;
        setThresholdArrays();
        zoneGeos = ssmonitor.getMonitoringAreaGeometries();
        geoAdjAreas = ssmonitor.getGeoAdjAreas();
    }

    /**
     * Gets the zone threats (ALG) with worst case for zone area
     * 
     * @return Map<String, FOG_THREAT> zoneThreats
     */
    public void setMonitorAreaThreats() {

        final FogRecord ffogRec = getFog();
        // TODO: async or sync ????
        Display.getDefault().asyncExec(new Runnable() {

            public void run() {
                long start = System.currentTimeMillis();
                Map<String, Geometry> zoneGeos = ssmonitor
                        .getMonitoringAreaGeometries();
                // set to lowest for default
                Map<String, FOG_THREAT> zoneThreats = new HashMap<String, FOG_THREAT>();
                for (String zone : zoneGeos.keySet()) {
                    zoneThreats.put(zone, FOG_THREAT.BLACK);
                }

                for (int i = 0; i < ffogRec.getNx(); i++) {
                    for (int j = 0; j < ffogRec.getNy(); j++) {

                        ReferencedCoordinate rc = new ReferencedCoordinate(
                                new Coordinate(i, j),
                                ffogRec.getGridGeometry(), Type.GRID_CENTER);
                        Coordinate coor = null;
                        try {
                            coor = rc.asLatLon();
                        } catch (TransformException e) {
                            statusHandler.handle(Priority.ERROR, e.getMessage());
                        } catch (FactoryException e) {
                            statusHandler.handle(Priority.ERROR, e.getMessage());
                        }

                        for (String zone : zoneGeos.keySet()) {
                            if (zoneGeos.get(zone) != null) {
                                if (zoneGeos.get(zone).contains(
                                        geoFactory.createPoint(coor))) {
                                    try {
                                        if (getThreatValue(getThreat(i, j)) > getThreatValue(zoneThreats
                                                .get(zone))) {
                                            zoneThreats.put(zone,
                                                    getThreat(i, j));
                                        }
                                    } catch (Exception e) {
                                        statusHandler.handle(Priority.ERROR,
                                                e.getMessage());
                                    }
                                }
                            }
                        } // end zone loop
                    } // end j loop
                } // end i loop

                ssmonitor.setAlgorithmData(ffogRec.getRefHour().getTime(),
                        zoneThreats);
                if (statusHandler.isPriorityEnabled(Priority.DEBUG)) {
                    statusHandler.handle(Priority.DEBUG,
                            "FogThreat for SS: Set algorithm zone values..."
                                    + (System.currentTimeMillis() - start));
                }
            }
        });
    }

}
