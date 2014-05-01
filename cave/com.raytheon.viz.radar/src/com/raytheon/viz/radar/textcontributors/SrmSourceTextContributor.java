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

import java.text.DecimalFormat;
import java.text.NumberFormat;

import com.raytheon.uf.common.dataplugin.radar.RadarRecord;
import com.raytheon.uf.common.dataplugin.radar.util.RadarRecordUtil;

/**
 * 
 * If the record has SRM then return a formatted string for the source
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
public class SrmSourceTextContributor implements IRadarTextContributor {
    private static final NumberFormat azFormat = new DecimalFormat("##0");

    @Override
    public String contributeText(RadarRecord record) {

        if (RadarRecordUtil.hasSRM(record)) {
            StringBuilder sourceBuff = new StringBuilder(
                    RadarRecordUtil.getSRMSourceName(record));
            sourceBuff.append(":  ");

            if (RadarRecordUtil.getSRMSpeed(record) == 0) {
                sourceBuff.append("STATIONARY");
            } else {
                sourceBuff
                        .append(azFormat.format(RadarRecordUtil
                                .getSRMDirection(record))).append("°  ")
                        .append(RadarRecordUtil.getSRMSpeed(record))
                        .append("kt");
            }
            return sourceBuff.toString();
        }
        return "";
    }
}