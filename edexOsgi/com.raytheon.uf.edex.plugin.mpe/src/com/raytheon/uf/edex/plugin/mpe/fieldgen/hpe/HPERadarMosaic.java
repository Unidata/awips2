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

import java.util.Map;
import java.util.HashMap;

/**
 * Enumeration identifying the different types of radar mosaics recognized and
 * used by HPE Field Generator.
 * 
 * <pre>
 * 
 * SOFTWARE HISTORY
 * 
 * Date         Ticket#    Engineer    Description
 * ------------ ---------- ----------- --------------------------
 * Aug 22, 2016 5631       bkowal      Initial creation
 * 
 * </pre>
 * 
 * @author bkowal
 */

public enum HPERadarMosaic {
    DHRMOSAIC, BDHRMOSAIC(new HPERadarMosaic[] { DHRMOSAIC }), ERMOSAIC(true), AVGERMOSAIC(
            true), MAXERMOSAIC(true), P3LMOSAIC(
            new HPERadarMosaic[] { ERMOSAIC }), EBMOSAIC(
            new HPERadarMosaic[] { ERMOSAIC }), MMOSAIC(new HPERadarMosaic[] {
            ERMOSAIC, EBMOSAIC }), LMOSAIC(new HPERadarMosaic[] { ERMOSAIC }), MLMOSAIC(
            new HPERadarMosaic[] { ERMOSAIC, LMOSAIC }), GAGEONLY(
            new HPERadarMosaic[] { ERMOSAIC }), SATPRE, LSATPRE(
            new HPERadarMosaic[] { ERMOSAIC, SATPRE });

    private static Map<String, HPERadarMosaic> radarMosaicNameLookupMap;

    /**
     * boolean flag indicating if this mosaic is recognized as a base mosaic.
     */
    private final boolean base;

    /**
     * Identifies the {@link HPERadarMosaic} that this mosaic is dependent on.
     */
    private final HPERadarMosaic[] dependencies;

    private HPERadarMosaic() {
        this(false);
    }

    private HPERadarMosaic(final boolean base) {
        this(base, new HPERadarMosaic[0]);
    }

    private HPERadarMosaic(final HPERadarMosaic[] dependencies) {
        this(false, dependencies);
    }

    private HPERadarMosaic(final boolean base,
            final HPERadarMosaic[] dependencies) {
        this.base = base;
        this.dependencies = dependencies;
    }

    public static synchronized HPERadarMosaic lookupRadarMosaic(
            final String radarMosaic) {
        if (radarMosaicNameLookupMap == null) {
            radarMosaicNameLookupMap = new HashMap<>(
                    HPERadarMosaic.values().length, 1.0f);
            for (HPERadarMosaic mosaic : HPERadarMosaic.values()) {
                radarMosaicNameLookupMap.put(mosaic.name().toLowerCase(),
                        mosaic);
            }
        }
        return radarMosaicNameLookupMap.get(radarMosaic.toLowerCase());
    }

    public boolean isBase() {
        return base;
    }

    public HPERadarMosaic[] getDependencies() {
        return dependencies;
    }
}