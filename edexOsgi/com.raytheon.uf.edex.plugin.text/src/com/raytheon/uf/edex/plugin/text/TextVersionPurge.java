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

import java.util.List;

import com.raytheon.uf.common.wmo.AFOSProductId;
import com.raytheon.uf.edex.core.EDEXUtil;
import com.raytheon.uf.edex.database.purge.PurgeLogger;
import com.raytheon.uf.edex.plugin.text.dao.StdTextProductDao;

/**
 * Incremental purger for text database
 *
 * <pre>
 *
 * SOFTWARE HISTORY
 *
 * Date          Ticket#  Engineer  Description
 * ------------- -------- --------- --------------------------------------------
 * Jun 17, 2011           rjpeter   Initial creation
 * Feb 16, 2016  16950    arickert  Prevent extra purging, removed call to
 *                                  getAfosIdsToPurge
 * Apr 25, 2018  6966     randerso  Changed incremental purge to purge all
 *                                  distinct AFOS IDs ingeseted since it last
 *                                  ran.
 *
 * </pre>
 *
 * @author rjpeter
 */

public class TextVersionPurge {

    private Long lastIncrementalPurgeTime = System.currentTimeMillis();

    private int emptyPurgeCount = 0;

    /**
     * Constructor
     */
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

    /**
     *
     */
    public void purgeAfosIds() {
        long t0 = System.currentTimeMillis();
        // TODO: this only purges the Operational table.
        // Should we be incrementally purging the Practice table also?
        StdTextProductDao dao = new StdTextProductDao();
        List<AFOSProductId> curIdsToPurge = dao
                .getAfosIdsToPurge(lastIncrementalPurgeTime);

        int rowsPurged = 0;
        for (AFOSProductId afosId : curIdsToPurge) {
            // if edex is trying to shut down, exit now
            if (EDEXUtil.isShuttingDown()) {
                break;
            }

            rowsPurged += dao.versionPurge(afosId);
        }

        lastIncrementalPurgeTime = t0;

        if (rowsPurged > 0) {
            emptyPurgeCount = 0;
            long t1 = System.currentTimeMillis();
            PurgeLogger.logInfo("Incremental purge: Purged " + rowsPurged
                    + " rows in " + (t1 - t0) + " ms", "TEXT");
        } else {
            emptyPurgeCount++;
            if (emptyPurgeCount > 30) {
                PurgeLogger.logWarn(
                        "Incremental purge: Has purged no items "
                                + emptyPurgeCount
                                + " times in a row.  Verify text data feed",
                        "TEXT");

            }
        }
    }
}
