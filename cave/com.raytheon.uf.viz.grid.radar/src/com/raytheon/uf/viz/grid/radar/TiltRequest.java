/**
 * This software was developed andimport com.raytheon.uf.common.datastorage.Request;
Contract DG133W-05-CQ-1067 with the US Government.
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
package com.raytheon.uf.viz.grid.radar;

import org.locationtech.jts.geom.Coordinate;

import com.raytheon.uf.common.datastorage.Request;

/**
 *
 * This class provides a way for resources to pass a point to the GribDataCube
 * adapter that will be used as the center of all TILT levels.
 *
 * TODO I dont like this class because it causes problems if one of these
 * Requests gets inadvertently sent over to PyPies/HDF5. We need a better way to
 * communicate with the GribDataCubeAdapter
 *
 * <pre>
 *
 * SOFTWARE HISTORY
 *
 * Date          Ticket#  Engineer  Description
 * ------------- -------- --------- --------------------------------------------
 * Nov 09, 2011           bsteffen  Initial creation
 * Aug 15, 2017  6332     bsteffen  Move to viz.grid.radar plugin
 * Jul 17, 2020  17574    smoorthy  added true elevation angle to extract later
 *                                  for radar height calculations
 * Mar 29, 2021  8374     randerso  Updated for changes to Request constructors
 *
 * </pre>
 *
 * @author bsteffen
 */
public class TiltRequest extends Request {

    private Coordinate tiltLocation;

    private double trueElevationAngle;

    public TiltRequest() {
        super(Request.Type.ALL);
    }

    public Coordinate getTiltLocation() {
        return tiltLocation;
    }

    public void setTiltLocation(Coordinate tiltLocation) {
        this.tiltLocation = tiltLocation;
    }

    public double getTrueElevationAngle() {
        return trueElevationAngle;
    }

    public void setTrueElevationAngle(double trueElevationAngle) {
        this.trueElevationAngle = trueElevationAngle;
    }
}
