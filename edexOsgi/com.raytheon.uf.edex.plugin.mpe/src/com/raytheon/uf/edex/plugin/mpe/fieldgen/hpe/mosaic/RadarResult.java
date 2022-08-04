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
package com.raytheon.uf.edex.plugin.mpe.fieldgen.hpe.mosaic;

import com.raytheon.uf.common.dataplugin.shef.tables.DAARadarResult;
import com.raytheon.uf.common.dataplugin.shef.tables.Rwradarresult;

/**
 * Generic POJO used to store information available in {@link DAARadarResult}
 * and {@link Rwradarresult} records required for mosaic generation.
 * 
 * <pre>
 *
 * SOFTWARE HISTORY
 *
 * Date         Ticket#    Engineer    Description
 * ------------ ---------- ----------- --------------------------
 * Sep 22, 2016 5631       bkowal      Initial creation
 *
 * </pre>
 *
 * @author bkowal
 */

public class RadarResult {

    private static final String RADAR_RESULT_YES = "y";

    private final String radarId;

    private final boolean editBias;

    private final boolean ignoreRadar;

    private final double bias;

    public RadarResult(DAARadarResult radarResult) {
        this.radarId = radarResult.getId().getRadid();
        this.editBias = RADAR_RESULT_YES.equals(radarResult.getEditBias());
        this.ignoreRadar = RADAR_RESULT_YES
                .equals(radarResult.getIgnoreRadar());
        this.bias = radarResult.getRwBiasValUsed();
    }

    public RadarResult(Rwradarresult radarResult) {
        this.radarId = radarResult.getId().getRadid();
        this.editBias = RADAR_RESULT_YES.equals(radarResult.getEditBias());
        this.ignoreRadar = RADAR_RESULT_YES
                .equals(radarResult.getIgnoreRadar());
        this.bias = radarResult.getRwBiasValUsed();
    }

    public String getRadarId() {
        return radarId;
    }

    public boolean isEditBias() {
        return editBias;
    }

    public boolean isIgnoreRadar() {
        return ignoreRadar;
    }

    public double getBias() {
        return bias;
    }
}