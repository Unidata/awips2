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

import java.text.SimpleDateFormat;
import java.util.Calendar;
import java.util.Date;
import java.util.TimeZone;

import com.raytheon.uf.common.dataplugin.radar.RadarRecord;
import com.raytheon.uf.common.dataplugin.radar.util.RadarRecordUtil;

/**
 * 
 * If the record has SRM then return a formatted string for the movement
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
public class SrmMovementTextContributor implements IRadarTextContributor {
    private static final SimpleDateFormat hhmmFormat = new SimpleDateFormat(
            "HH:mm");

    @Override
    public String contributeText(RadarRecord record) {

        if (RadarRecordUtil.hasSRM(record)) {
            Date movementTime = RadarRecordUtil.getSRMMovement(record);
            StringBuilder movementStateBuff = new StringBuilder();

            if (movementTime != null) {
                // The time is current if it is within 60 seconds of the
                // current
                // time
                Calendar currTime = Calendar.getInstance();
                currTime.add(Calendar.SECOND, -60);

                movementStateBuff.append("MVMT ");
                if (movementTime.compareTo(currTime.getTime()) >= 0) {
                    movementStateBuff.append("IS CURRENT");
                } else {
                    hhmmFormat.setTimeZone(TimeZone.getTimeZone("UTC"));
                    movementStateBuff.append("FROM ")
                            .append(hhmmFormat.format(movementTime))
                            .append("Z");
                }
            } else {
                movementStateBuff.append("NOT AVAILABLE");
            }
            return movementStateBuff.toString();
        }
        return "";
    }
}