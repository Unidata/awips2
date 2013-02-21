package com.raytheon.uf.common.dataplugin.scan.data;

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

import java.util.ArrayList;
import java.util.Date;
import java.util.HashMap;

import com.raytheon.uf.common.dataplugin.binlightning.BinLightningRecord;
import com.raytheon.uf.common.serialization.ISerializableObject;
import com.raytheon.uf.common.serialization.annotations.DynamicSerialize;
import com.raytheon.uf.common.time.util.TimeUtil;

/**
 * 
 * SCAN Lightning Data
 * 
 * <pre>
 * SOFTWARE HISTORY
 * Date         Ticket#    Engineer    Description
 * ------------ ---------- ----------- --------------------------
 * 02/01/13     1569        D. Hladky   removed XML where not needed
 * </pre>
 * 
 * @author dhladky
 * @version 1.0
 * 
 */

@DynamicSerialize
public class LightningData implements ISerializableObject {

    private final HashMap<Date, BinLightningRecord> lightningMap;

    /**
     * Public constructor
     */
    public LightningData() {
        lightningMap = new HashMap<Date, BinLightningRecord>();
    }

    /**
     * Add a lightning record
     * 
     * @param record
     */
    public void addLightningRecord(BinLightningRecord record) {

        Date timeKey = record.getStopTime().getTime();

        if (!lightningMap.containsKey(timeKey)) {
            lightningMap.put(timeKey, record);
            purge(timeKey);
        }
    }

    /**
     * Gets a list of records
     * 
     * @param date
     * @return
     */
    public ArrayList<BinLightningRecord> getLightningRecords(Date date) {
        ArrayList<BinLightningRecord> records = new ArrayList<BinLightningRecord>();

        for (Date date1 : lightningMap.keySet()) {
            if (date1.after(date)) {
                records.add(lightningMap.get(date1));
            }
        }

        return records;
    }

    /**
     * Gets all of records
     * 
     * @param date
     * @return
     */
    public ArrayList<BinLightningRecord> getLightningRecords() {
        ArrayList<BinLightningRecord> records = new ArrayList<BinLightningRecord>();

        for (Date date1 : lightningMap.keySet()) {
            records.add(lightningMap.get(date1));
        }

        return records;
    }

    /**
     * check to see if you have anything available
     * 
     * @param date
     * @return
     */
    public boolean hasRecords(Date date) {
        for (Date date1 : lightningMap.keySet()) {
            if (date1.after(date)) {
                return true;
            }
        }

        return false;
    }

    /**
     * Keep the hash clean
     * 
     * @param date
     */
    private void purge(Date date) {

        // keep no more than 15 minutes worth
        long timeBarrier = 15 * TimeUtil.MILLIS_PER_MINUTE;
        Date backTime = new Date(date.getTime() - timeBarrier);
        ArrayList<Date> old = new ArrayList<Date>();
        // purge old records
        for (Date date1 : lightningMap.keySet()) {
            if (date1.before(backTime)) {
                old.add(date1);
            }
        }

        for (Date date2 : old) {
            lightningMap.remove(date2);
        }
    }

}
