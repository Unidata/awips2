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

import com.raytheon.uf.common.dataplugin.shef.tables.Radarloc;

/**
 * POJO representative of the radarLoc_record_struct in:
 * hpe_fieldgen/TEXT/empe_db_tables.h.
 * 
 * <pre>
 *
 * SOFTWARE HISTORY
 *
 * Date         Ticket#    Engineer    Description
 * ------------ ---------- ----------- --------------------------
 * Sep 15, 2016 5631       bkowal      Initial creation
 *
 * </pre>
 *
 * @author bkowal
 */

public class RadarLocRecord {

    private static final double ELEV_MULTIPLIER = 0.3048;

    private final String id;

    private final double latitude;

    private final double longitude;

    private final double elevation;

    public RadarLocRecord(final Radarloc radarloc) {
        if (radarloc == null) {
            throw new IllegalArgumentException(
                    "Required argument 'radarloc' cannot be NULL.");
        }

        this.id = radarloc.getRadid();
        this.latitude = radarloc.getLat();
        this.longitude = radarloc.getLon();
        this.elevation = (radarloc.getElev() + radarloc.getTowerHt())
                * ELEV_MULTIPLIER;
    }

    public String getId() {
        return id;
    }

    public double getLatitude() {
        return latitude;
    }

    public double getLongitude() {
        return longitude;
    }

    public double getElevation() {
        return elevation;
    }
}