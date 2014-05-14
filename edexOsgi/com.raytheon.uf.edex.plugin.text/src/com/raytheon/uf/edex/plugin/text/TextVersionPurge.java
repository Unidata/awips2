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
package com.raytheon.uf.edex.plugin.text;

import java.util.ArrayList;
import java.util.HashSet;
import java.util.List;
import java.util.Set;

import com.raytheon.edex.textdb.dao.StdTextProductDao;
import com.raytheon.uf.edex.database.purge.PurgeLogger;

/**
 * TODO Add Description
 * 
 * <pre>
 * 
 * SOFTWARE HISTORY
 * 
 * Date         Ticket#    Engineer    Description
 * ------------ ---------- ----------- --------------------------
 * Jun 17, 2011            rjpeter     Initial creation
 * 
 * </pre>
 * 
 * @author rjpeter
 * @version 1.0
 */

public class TextVersionPurge {

    private Set<String> accumulatedAfosIds = new HashSet<String>();

    private Set<String> afosIdsToPurge = new HashSet<String>();

    private int emptyPurgeCount = 0;

    public TextVersionPurge() {
        Thread verifyThread = new Thread() {
            @Override
            public void run() {
                StdTextProductDao dao = new StdTextProductDao();
                dao.checkSiteVersionPurge();
            }
        };
        verifyThread.start();
    }

    public void accumulateAfosIdToPurge(TextRecord records[]) {
        synchronized (accumulatedAfosIds) {
            for (TextRecord rec : records) {
                String afosId = rec.getProductId();
                accumulatedAfosIds.add(afosId);
            }
        }
    }

    public void addAfosIdsToPurge(List<String> afosIds) {
        synchronized (afosIdsToPurge) {
            afosIdsToPurge.addAll(afosIds);
        }
    }

    public List<String> getAfosIdsToPurge() {
        List<String> curIdsToPurge = null;
        synchronized (accumulatedAfosIds) {
            if (accumulatedAfosIds.size() > 0) {
                curIdsToPurge = new ArrayList<String>(accumulatedAfosIds);
                accumulatedAfosIds.clear();
            } else {
                curIdsToPurge = new ArrayList<String>(0);
            }
        }

        return curIdsToPurge;
    }

    public void purgeAfosIds() {
        StdTextProductDao dao = new StdTextProductDao();
        List<String> curIdsToPurge = getAfosIdsToPurge();
        int rowsPurged = 0;
        long t0 = System.currentTimeMillis();
        synchronized (afosIdsToPurge) {
            if (afosIdsToPurge.size() > 0) {
                curIdsToPurge = new ArrayList<String>(afosIdsToPurge);
                afosIdsToPurge.clear();
            } else {
                curIdsToPurge = new ArrayList<String>(0);
            }
        }

        for (String afosId : curIdsToPurge) {
            rowsPurged += dao.versionPurge(afosId);
        }

        if (rowsPurged > 0) {
            emptyPurgeCount = 0;
            long t1 = System.currentTimeMillis();
            PurgeLogger.logInfo("Incremental purge: Purged " + rowsPurged
                    + " rows in " + (t1 - t0) + " ms", "TEXT");
        } else {
            emptyPurgeCount++;
            if (emptyPurgeCount > 30) {
                PurgeLogger.logWarn("Incremental purge: Has purged no items "
                        + emptyPurgeCount
                        + " times in a row.  Verify text data feed", "TEXT");

            }
        }
    }
}
