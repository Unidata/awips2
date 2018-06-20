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
package com.raytheon.uf.edex.plugin.mpe.fieldgen.hpe;

import com.raytheon.uf.edex.plugin.mpe.geo.BinaryGeoDataFile;
import com.raytheon.uf.edex.plugin.mpe.geo.MpeGageLocationsGeoDataAsciiFile;
import com.raytheon.uf.edex.plugin.mpe.geo.TownGeoDataAsciiFile;

/**
 * POJO containing the geo data used by HPE Field Gen.
 * 
 * <pre>
 * 
 * SOFTWARE HISTORY
 * 
 * Date         Ticket#    Engineer    Description
 * ------------ ---------- ----------- --------------------------
 * Aug 30, 2016 5631       bkowal      Initial creation
 * 
 * </pre>
 * 
 * @author bkowal
 */

public class GeoData {

    private final BinaryGeoDataFile rfcBoundaryOverlay;

    private final BinaryGeoDataFile stateOverlay;

    private final TownGeoDataAsciiFile townGeoData;

    private final MpeGageLocationsGeoDataAsciiFile gageLocationsGeoData;

    public GeoData(final BinaryGeoDataFile rfcBoundaryOverlay,
            final BinaryGeoDataFile stateOverlay,
            final TownGeoDataAsciiFile townGeoData,
            final MpeGageLocationsGeoDataAsciiFile gageLocationsGeoData) {
        this.rfcBoundaryOverlay = rfcBoundaryOverlay;
        this.stateOverlay = stateOverlay;
        this.townGeoData = townGeoData;
        this.gageLocationsGeoData = gageLocationsGeoData;
    }

    public BinaryGeoDataFile getRfcBoundaryOverlay() {
        return rfcBoundaryOverlay;
    }

    public BinaryGeoDataFile getStateOverlay() {
        return stateOverlay;
    }

    public TownGeoDataAsciiFile getTownGeoData() {
        return townGeoData;
    }

    public MpeGageLocationsGeoDataAsciiFile getGageLocationsGeoData() {
        return gageLocationsGeoData;
    }
}