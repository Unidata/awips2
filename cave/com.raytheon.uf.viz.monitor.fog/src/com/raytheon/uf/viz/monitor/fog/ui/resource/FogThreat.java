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
package com.raytheon.uf.viz.monitor.fog.ui.resource;

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
import com.raytheon.uf.viz.monitor.fog.FogMonitor;
import com.raytheon.uf.viz.monitor.fog.xml.FogMonitorAlgorithmXML;
import com.vividsolutions.jts.geom.Coordinate;

/**
 * Fog monitor threat calculation
 * 
 * <pre>
 * 
 * SOFTWARE HISTORY
 * 
 * Date         Ticket#    Engineer    Description
 * ------------ ---------- ----------- --------------------------
 * Nov  1, 2012            skorolev     Changed HashMap to Map and cleaned code.
 * Nov 13, 2012 1297       skorolev     Made abstract class FogCommonThreat
 * 
 * </pre>
 * 
 * @author skorolev
 * @version 1.0
 */
public class FogThreat extends FogCommonThreat {
    private final IUFStatusHandler statusHandler = UFStatus
            .getHandler(FogThreat.class);

    /** Fog monitor **/
    private FogMonitor fmonitor = FogMonitor.getInstance();

    /**
     * serialization constructor
     */
    public FogThreat() {

    }

    /**
     * used constructor
     * 
     * @param fogAlgXML
     */
    public FogThreat(FogMonitorAlgorithmXML fogAlgXML) {
        this.fogAlgXML = fogAlgXML;
        setThresholdArrays();
        zoneGeos = fmonitor.getMonitoringAreaGeometries();
        geoAdjAreas = fmonitor.getGeoAdjAreas();
    }

    /*
     * (non-Javadoc)
     * 
     * @see
     * com.raytheon.uf.viz.monitor.fog.FogCommonThreat#setMonitorAreaThreats()
     */
    @Override
    protected void setMonitorAreaThreats() {
        final FogRecord ffogRec = getFog();
        // TODO: async or sync ????
        Display.getDefault().asyncExec(new Runnable() {

            public void run() {
                long start = System.currentTimeMillis();
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

                fmonitor.setAlgorithmData(ffogRec.getRefHour().getTime(),
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
