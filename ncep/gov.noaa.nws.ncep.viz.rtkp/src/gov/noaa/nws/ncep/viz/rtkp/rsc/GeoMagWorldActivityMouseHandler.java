/**
 * This code has unlimited rights, and is provided "as is" by the National Centers 
 * for Environmental Prediction, without warranty of any kind, either expressed or implied, 
 * including but not limited to the implied warranties of merchantability and/or fitness 
 * for a particular purpose.
 * 
 * This code has been developed by the NCEP-SIB for use in the AWIPS2 system.
 * 
 **/
package gov.noaa.nws.ncep.viz.rtkp.rsc;

import gov.noaa.nws.ncep.common.dataplugin.geomag.table.GeoMagStation;
import gov.noaa.nws.ncep.ui.pgen.tools.InputHandlerDefaultImpl;
import gov.noaa.nws.ncep.viz.rtkp.util.RTKpUtil;
import gov.noaa.nws.ncep.viz.rtkp.util.RTKpUtil.GeoMagStationType;
import gov.noaa.nws.ncep.viz.ui.display.NatlCntrsEditor;

import java.util.ArrayList;
import java.util.Calendar;
import java.util.Date;
import java.util.List;
import java.util.Map;
import java.util.TimeZone;

import org.geotools.referencing.GeodeticCalculator;

import com.raytheon.uf.common.geospatial.ReferencedCoordinate;
import com.raytheon.uf.common.status.IUFStatusHandler;
import com.raytheon.uf.common.status.UFStatus;
import com.raytheon.uf.common.status.UFStatus.Priority;
import com.raytheon.uf.viz.core.exception.VizException;
import com.raytheon.uf.viz.core.map.IMapDescriptor;
import com.vividsolutions.jts.geom.Coordinate;

/**
 * Mouse handler for Geomag World Activity Map.
 * 
 * <pre>
 * 
 * SOFTWARE HISTORY
 * 
 * Date          Ticket#    Engineer    Description
 * ------------  ---------- ----------- --------------------------
 * May 5, 2014 1122       sgurung     Initial creation
 * 
 * </pre>
 * 
 * @author sgurung
 * @version 1.0
 */

public class GeoMagWorldActivityMouseHandler extends InputHandlerDefaultImpl {

    private static final transient IUFStatusHandler statusHandler = UFStatus
            .getHandler(GeoMagWorldActivityMouseHandler.class);

    private static final double geomagStnPointMinDistance = 75000;

    static int textDispIndex = 0;

    private List<GeoMagStation> allStations = null;

    private GeoMagWorldActivityResource resource;

    /**
     * Index of the selected point.
     */
    protected int ptIndex = 0;

    /*
     * (non-Javadoc)
     * 
     * @see com.raytheon.viz.ui.input.IInputHandler#handleMouseDown(int, int,
     * int)
     */
    @Override
    public boolean handleMouseDown(int x, int y, int button) {
        return false;
    }

    public GeoMagWorldActivityMouseHandler(GeoMagWorldActivityResource resource) {
        this.resource = resource;
        this.allStations = RTKpUtil.getGeoMagStations(GeoMagStationType.ALL);
    }

    /*
     * (non-Javadoc)
     * 
     * @see com.raytheon.viz.ui.input.IInputHandler#handleMouseUp(int, int, int)
     * handle left button, so user be able to pick stn and print text report
     */
    @Override
    public boolean handleMouseUp(int x, int y, int button) {

        // button 1 is left mouse button
        if (button == 1) {
            NatlCntrsEditor mapEditor = GeoMagWorldActivityResource
                    .getMapEditor();
            if (mapEditor != null) {
                // Check if mouse is in geographic extent
                Coordinate loc = mapEditor.translateClick(x, y);
                if (loc == null)
                    return false;

                if (resource != null) {
                    // get the stn (point) list
                    List<GeoMagStation> points = allStations;// resource.getPoints();

                    if (points.isEmpty() == false) {

                        // get the stn close to loc "enough" and retrieve text
                        // report for it
                        GeoMagStation stnPt = getPtWithinMinDist(points, loc);

                        if (stnPt != null) {

                            Date endTime = new Date();

                            List<String> stnCodes = new ArrayList<String>();
                            stnCodes.add(stnPt.getStationCode());
                            try {
                                List<Map<String, Object>> latestStnKLst = RTKpUtil
                                        .getLatestEstKIndex(stnCodes, null,
                                                null);
                                if (latestStnKLst != null
                                        && latestStnKLst.size() == 1) {
                                    endTime = (Date) latestStnKLst.get(0).get(
                                            "reftime");
                                }
                            } catch (VizException e) {
                                statusHandler.handle(Priority.PROBLEM,
                                        "Error while retrieving latest est k index: "
                                                + e.getLocalizedMessage(), e);
                            }

                            Calendar endTimeCal = Calendar.getInstance(TimeZone
                                    .getTimeZone("GMT"));
                            endTimeCal.setTime(endTime);
                            endTimeCal.set(Calendar.MINUTE, 0);
                            endTimeCal.set(Calendar.SECOND, 0);
                            endTimeCal.set(Calendar.MILLISECOND, 0);
                            int currentHour = endTimeCal
                                    .get(Calendar.HOUR_OF_DAY);
                            // find current synoptic end time
                            int hoursToAdd = 3 - (currentHour % 3);
                            endTimeCal.add(Calendar.HOUR_OF_DAY, hoursToAdd);
                            // subtract -12 to get start time

                            Calendar startTimeCal = Calendar
                                    .getInstance(TimeZone.getTimeZone("GMT"));
                            startTimeCal.setTime(endTimeCal.getTime());
                            startTimeCal.add(Calendar.HOUR_OF_DAY, -12);

                            try {
                                RTKpUtil.loadHDWithQdcPlots(stnPt
                                        .getStationCode(), RTKpUtil
                                        .getMostAvailableDataBasedOnSourceId(
                                                stnPt.getStationCode(),
                                                startTimeCal.getTime(),
                                                endTimeCal.getTime()));
                            } catch (Exception e) {
                                statusHandler.handle(
                                        Priority.PROBLEM,
                                        "Error while loading H & D plot for "
                                                + stnPt.getStationCode()
                                                + " in a new window: "
                                                + e.getLocalizedMessage(), e);
                            }
                            return true;
                        }

                    }
                }

            }
        }
        return false;
    }

    @Override
    public boolean handleMouseMove(int x, int y) {
        NatlCntrsEditor mapEditor = GeoMagWorldActivityResource.getMapEditor();
        if (mapEditor != null) {
            // Check if mouse is in geographic extent
            Coordinate loc = mapEditor.translateClick(x, y);
            if (loc == null)
                return false;

            if (resource != null) {
                // get the stn (point) list
                List<GeoMagStation> points = allStations;

                if (points.isEmpty() == false) {

                    // get the stn close to loc "enough" and retrieve text
                    // report for it
                    GeoMagStation stnPt = getPtWithinMinDist(points, loc);

                    if (stnPt != null) {
                        resource.sampleCoord = new ReferencedCoordinate(loc);
                        resource.sampleString = stnPt.getStationCode();
                    } else {
                        resource.sampleCoord = null;
                        resource.sampleString = null;
                    }

                }
            }
        }

        resource.issueRefresh();

        return false;
    }

    /**
     * Gets the nearest point of an selected element to the input point
     * 
     * @param el
     *            element
     * @param pt
     *            input point
     * @return
     */
    protected GeoMagStation getPtWithinMinDist(List<GeoMagStation> points,
            Coordinate pt) {

        GeoMagStation thePoint = null;
        double minDistance = geomagStnPointMinDistance;
        GeodeticCalculator gc;
        NatlCntrsEditor mapEditor = GeoMagWorldActivityResource.getMapEditor();
        if (mapEditor != null) {
            IMapDescriptor desc;
            desc = (IMapDescriptor) mapEditor.getActiveDisplayPane()
                    .getRenderableDisplay().getDescriptor();

            gc = new GeodeticCalculator(desc.getCRS());
            gc.setStartingGeographicPoint(pt.x, pt.y);

            for (GeoMagStation point : points) {

                gc.setDestinationGeographicPoint(point.getLocation()
                        .getLongitude(), point.getLocation().getLatitude());
                double dist;
                try {
                    dist = gc.getOrthodromicDistance();

                    if (dist < minDistance) {

                        minDistance = dist;
                        thePoint = point;
                    }
                } catch (Exception e) {
                    // statusHandler.handle(Priority.PROBLEM,
                    // e.getLocalizedMessage(), e);
                }

            }
        }
        return thePoint;

    }
}
