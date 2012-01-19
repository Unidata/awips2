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
 * Format user selectable precip max based on a scaling flag
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
public class USPMaxTextContributor implements IRadarTextContributor {

    @Override
    public String contributeText(RadarRecord record) {
        if (record.getProductDependentValues() == null) {
            return "Loading";
        }
        int p = record.getProductDependentValue(2) & 0xFF00; /*
                                                              * high scale flag
                                                              */
        if (record.getProductCode() == 31 || record.getProductCode() == 151
                && p != 0)
            return String.format("MX: %3.1fin",
                    record.getProductDependentValue(3) * 0.1);
        else if (record.getProductCode() == 150 && p == 0)
            return String.format("MX: %3.2fin",
                    record.getProductDependentValue(3) * 0.001);
        else
            return String.format("MX: %3.2fin",
                    record.getProductDependentValue(3) * 0.01);
    }
}