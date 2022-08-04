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
 * Format User Selectable Accumulation(DUA) Duration.
 * 
 * <pre>
 * 
 * SOFTWARE HISTORY
 * 
 * Date          Ticket#  Engineer  Description
 * ------------- -------- --------- -----------------
 * Jun 04, 2018  6725     bsteffen  Initial creation
 * 
 * </pre>
 * 
 * @author bsteffen
 */
public class DUADurationTextContributor implements IRadarTextContributor {

    @Override
    public String contributeText(RadarRecord record) {
        if (record.getProductDependentValues() == null) {
            return "Loading";
        }
        int duration = record.getProductDependentValue(1);
        int hours = duration / 60;
        int minutes = duration % 60;
        if (minutes == 0) {
            return String.format("DUR: %dhr", hours);
        } else {
            return String.format("DUR: %d:%d", hours, minutes);
        }
    }
}