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
package com.raytheon.viz.warngen.gui;

import java.util.Map;
import java.util.Set;
import java.util.concurrent.ConcurrentHashMap;

import org.locationtech.jts.geom.Coordinate;
import org.locationtech.jts.geom.Geometry;
import org.locationtech.jts.geom.Polygon;

/**
 * Object that holds UI state information for warngen
 * 
 * <pre>
 * 
 * SOFTWARE HISTORY
 * Date         Ticket#    Engineer    Description
 * ------------ ---------- ----------- --------------------------
 * May 7, 2010            mschenke     Initial creation
 * 03/14/2012   DR 14690  Qinglu Lin   Add clear2().
 * 10/26/2012   DR 15479  Qinglu Lin   Added removeDuplicateCoordinate().
 * 12/06/2012   DR 15559  Qinglu Lin   Added computeSlope(), computeCoordinate(), 
 *                                     and adjustPolygon().
 * Feb 15, 2013    1624   jsanchez     Fix NullPointerException in removeDuplicateCoordinate.
 * 03/28/2013   DR 15974  D. Friedman  Track marked areas outside polygon.
 * 04/12/2013   DR 16045  Qinglu Lin   Updated setWarningPolygon() by removing the call to 
 *                                     removeDuplicateCoordinate() and updated removeDuplicateCoordinate()
 *                                     by changing access control level from private to public.
 *                                     Moved removeDuplicateCoordinate(), computeSlope(),computeCoordinate(),
 *                                     and adjustPolygon to PolygonUtil.
 * 02/09/2015      3954   dlovely      Store only the string location and county.
 * 
 * </pre>
 * 
 * @author mschenke
 * @version 1.0
 */

public class WarngenUIState {

    /** The hatched warning area (native is local space) */
    private Geometry warningArea;

    /** The user defined polygon (native is lat/lon space) */
    private Polygon warningPolygon;

    private Polygon markedWarningPolygon;

    private Geometry markedWarningArea;

    private Polygon oldWarningPolygon;

    private Geometry oldWarningArea;

    /**
     * Location of the text rendering in a given county warning area.
     */
    public Map<Coordinate, Geometry> warningTextLocations = new ConcurrentHashMap<Coordinate, Geometry>();

    public boolean snappedToArea = false;

    public boolean rightClickSelected = false;

    public boolean geometryChanged = false;

    public FollowupData followupData = null;

    private Set<String> fipsOutsidePolygon = null;

    /**
     * Get the warning area in lat/lon projection
     * 
     * @return the warningArea
     */
    public Geometry getWarningArea() {
        return warningArea;
    }

    /**
     * Set the warning area in lat/lon space, will be converted to local
     * 
     * @param warningAreaLatLon
     */
    public void setWarningArea(Geometry warningArea) {
        this.warningArea = warningArea;
    }

    /**
     * Get the old warning polygon in lat/lon projection
     * 
     * @return the oldWarningPolygon
     */
    public Polygon getOldWarningPolygon() {
        return oldWarningPolygon;
    }

    /**
     * Set the old warning polygon in lat/lon projection
     * 
     * @param oldWarningPolygon
     *            the oldWarningPolygon to set
     */
    public void setOldWarningPolygon(Polygon oldWarningPolygon) {
        this.oldWarningPolygon = oldWarningPolygon;
    }

    /**
     * The old warning area in lat/lon projection
     * 
     * @return the oldWarningArea
     */
    public Geometry getOldWarningArea() {
        return oldWarningArea;
    }

    /**
     * Set the old warning area in lat/lon projection. Will be converted to
     * local
     * 
     * @param oldWarningAreaLatLon
     */
    public void setOldWarningArea(Geometry oldWarningArea) {
        this.oldWarningArea = oldWarningArea;
    }

    /**
     * get the warning polygon in lat/lon projection
     * 
     * @return the warningPolygon
     */
    public Polygon getWarningPolygon() {
        return warningPolygon;
    }

    /**
     * Set the warning polygon in lat/lon projection
     * 
     * @param warningPolygon
     *            the warningPolygon to set
     */
    public void setWarningPolygon(Polygon warningPolygon) {
        this.warningPolygon = warningPolygon;
    }

	public Set<String> getFipsOutsidePolygon() {
		return fipsOutsidePolygon;
	}

	public void setFipsOutsidePolygon(Set<String> gidsOutsidePolygon) {
		this.fipsOutsidePolygon = gidsOutsidePolygon;
	}

    public void clear() {
        warningPolygon = null;
        clear2();
    }

    /**
     * clear2 Same as clear(), except for not assigning null to warningPolygon.
     * History 03-16-2012 Qinglu Lin DR14690 Created.
     */
    public void clear2() {
        oldWarningArea = null;
        oldWarningPolygon = null;
        oldWarningArea = null;
        oldWarningPolygon = null;
        warningTextLocations.clear();
        warningArea = null;
        markedWarningArea = null;
        markedWarningPolygon = null;
        fipsOutsidePolygon = null;
    }

    /**
     * @return the markedWarningPolygon
     */
    public Polygon getMarkedWarningPolygon() {
        return markedWarningPolygon;
    }

    /**
     * @return the markedWarningArea
     */
    public Geometry getMarkedWarningArea() {
        return markedWarningArea;
    }

    public void resetMarked() {
        this.markedWarningArea = null;
        this.markedWarningPolygon = null;
    }

    public void mark(Geometry newHatchedArea) {
        this.markedWarningPolygon = (Polygon) warningPolygon.clone();
        this.markedWarningArea = (Geometry) newHatchedArea.clone();
    }

    public boolean isMarked() {
        return (this.markedWarningArea != null && this.markedWarningPolygon != null);
    }

}