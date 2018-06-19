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
package com.raytheon.viz.mpe.ui.rsc;

import java.util.ArrayDeque;
import java.util.Calendar;
import java.util.Deque;
import java.util.HashMap;
import java.util.Map;
import java.util.Set;

import com.raytheon.uf.common.mpe.fieldgen.PrecipDataKey;
import com.raytheon.uf.common.mpe.fieldgen.PrecipField;
import com.raytheon.uf.common.time.util.TimeUtil;

/**
 * Keeps track of the available inventory of MPE precip data for a given precip
 * field and caches data that has already been retrieved.
 * 
 * <pre>
 *
 * SOFTWARE HISTORY
 *
 * Date         Ticket#    Engineer    Description
 * ------------ ---------- ----------- --------------------------
 * Sep 20, 2017 6407       bkowal      Initial creation
 * Oct 06, 2017 6407       bkowal      No longer cache data endlessly.
 *
 * </pre>
 *
 * @author bkowal
 */

public final class MPEPrecipInventoryManager {

    /*
     * Cache a maximum of two days worth of data (24 * 2) across all
     * precipitation fields. Note: a user can only access a maximum of 10 days
     * worth of data through MPE.
     */
    private static final int CACHE_SIZE = 48;

    private static final int MAX_ACCESS_DAYS = 10;

    private static final MPEPrecipInventoryManager INSTANCE = new MPEPrecipInventoryManager();

    private final Map<PrecipField, Map<PrecipDataKey, MPEPrecipData>> inventory = new HashMap<>();

    /*
     * Keeps track of the order that data was accessed in. Will be used to purge
     */
    private final Deque<PrecipDataKey> accessQueue = new ArrayDeque<>(
            CACHE_SIZE);

    private MPEPrecipInventoryManager() {
    }

    /**
     * Returns an instance of the {@link MPEPrecipInventoryManager} that has
     * been initialized for the specified {@link PrecipField}.
     * 
     * @param field
     *            the specified {@link PrecipField}.
     * @return an initialized instance of the {@link MPEPrecipInventoryManager}
     * @throws MPEDataLoadException
     */
    public static synchronized MPEPrecipInventoryManager getInstance(
            final PrecipField field) throws MPEDataLoadException {
        if (!INSTANCE.inventoryExists(field)) {
            INSTANCE.loadInitialInventoryForField(field);
        }
        return INSTANCE;
    }

    /**
     * Determines whether or not there is inventory available for the specified
     * {@link PrecipField}.
     * 
     * @param field
     *            the specified {@link PrecipField}
     * @return {@code true} if inventory is available; {@code false} otherwise.
     */
    public synchronized boolean inventoryExists(final PrecipField field) {
        return inventory.containsKey(field);
    }

    /**
     * Determines whether or not there is both inventory and data (based on the
     * date/hour contained within the precip data key) available for the
     * specified {@link PrecipDataKey}.
     * 
     * @param precipDataKey
     *            the specified {@link PrecipDataKey}
     * @return {@code true} if both inventory and data are available;
     *         {@code false} otherwise.
     */
    public synchronized boolean inventoryExistsAndDataAvailable(
            final PrecipDataKey precipDataKey) {
        return inventory.containsKey(precipDataKey.getField()) && inventory
                .get(precipDataKey.getField()).containsKey(precipDataKey);
    }

    /**
     * Retrieves the requested {@link MPEPrecipData} associated with the
     * specified {@link PrecipDataKey}.
     * 
     * @param precipDataKey
     *            the specified {@link PrecipDataKey}
     * @return the retrieved {@link MPEPrecipData} if available or {@code null}
     *         otherwise.
     */
    public synchronized MPEPrecipData retrieveFromInventory(
            final PrecipDataKey precipDataKey) {
        if (!inventoryExists(precipDataKey.getField())) {
            return null;
        }
        return inventory.get(precipDataKey.getField()).get(precipDataKey);
    }

    public synchronized void addToInventory(final PrecipDataKey precipDataKey) {
        addToInventory(precipDataKey, null);
    }

    /**
     * Adds the specified {@link MPEPrecipData} to the inventory and associates
     * it with the specified {@link PrecipDataKey}.
     * 
     * @param precipDataKey
     *            the specified {@link PrecipDataKey}
     * @param data
     *            the specified {@link MPEPrecipData}
     */
    public synchronized void addToInventory(final PrecipDataKey precipDataKey,
            final MPEPrecipData data) {
        if (!inventoryExists(precipDataKey.getField())) {
            /*
             * Do not add to the inventory if the inventory was never initially
             * populated for this precip field.
             */
            return;
        }
        final Calendar purgeCalendar = TimeUtil.newGmtCalendar();
        purgeCalendar.add(Calendar.DATE, -MAX_ACCESS_DAYS);
        if (precipDataKey.getCalendar().before(purgeCalendar)) {
            /*
             * ignore data that is too old to be accessible.
             */
            return;
        }
        inventory.get(precipDataKey.getField()).put(precipDataKey, data);
        if (data == null) {
            /*
             * Remove any instances of the specified precip data key from the
             * access queue because there is not any cached data associated with
             * it any longer.
             */
            accessQueue.remove(precipDataKey);
        } else {
            accessQueue.addLast(precipDataKey);
            /*
             * Has the cache limit been exceeded?
             */
            if (accessQueue.size() > CACHE_SIZE) {
                /*
                 * Clear cached information until this is no longer the case.
                 */
                int removalCount = accessQueue.size() - CACHE_SIZE;
                while (removalCount > 0) {
                    final PrecipDataKey toClear = accessQueue.pollLast();
                    --removalCount;
                    /*
                     * Completely remove any keys older than 10 days because any
                     * associated data will never be accessed again.
                     */
                    if (toClear.getCalendar().before(purgeCalendar)) {
                        inventory.get(toClear.getField()).remove(toClear);
                    } else {
                        inventory.get(toClear.getField()).put(toClear, null);
                    }
                }
            }
        }
    }

    private synchronized void loadInitialInventoryForField(
            final PrecipField field) throws MPEDataLoadException {
        Set<PrecipDataKey> inventorySet = MPEPrecipDataLoader
                .getInventory(field);
        final Map<PrecipDataKey, MPEPrecipData> precipInventory = new HashMap<>(
                inventorySet.size(), 1.0f);
        for (PrecipDataKey precipDataKey : inventorySet) {
            precipInventory.put(precipDataKey, null);
        }
        inventory.put(field, precipInventory);
    }
}