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
package com.raytheon.viz.radar.textcontributors;

import com.raytheon.uf.common.dataplugin.radar.RadarRecord;

/**
 * 
 * Format the radar resolution in km
 * 
 * <pre>
 * 
 * SOFTWARE HISTORY
 * Date         Ticket#    Engineer    Description
 * ------------ ---------- ----------- --------------------------
 * Sep 7, 2010            bsteffen     Initial creation
 * 
 * </pre>
 * 
 * @author bsteffen
 * @version 1.0
 */
public class ResolutionTextContributor implements IRadarTextContributor {

    @Override
    public String contributeText(RadarRecord record) {
        String res = "";
        Double resKm = record.getGateResolution() / 1000.0;
        if (resKm.intValue() == resKm.doubleValue()) {
            res = resKm.intValue() + " km";
        } else {
            res = resKm.doubleValue() + " km";
        }
        // Add this to Super Res, Num radials should be around 720
        if (record.getNumRadials() > 700) {
            res += ", 0.5 dAz";
        }
        return res;
    }
}