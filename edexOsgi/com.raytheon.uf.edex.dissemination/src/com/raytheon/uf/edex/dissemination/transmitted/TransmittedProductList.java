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
package com.raytheon.uf.edex.dissemination.transmitted;

import com.raytheon.uf.common.status.IUFStatusHandler;
import com.raytheon.uf.common.status.UFStatus;
import com.raytheon.uf.common.status.UFStatus.Priority;
import com.raytheon.uf.edex.database.cluster.ClusterLockUtils;
import com.raytheon.uf.edex.database.cluster.ClusterLockUtils.LockState;
import com.raytheon.uf.edex.database.cluster.ClusterTask;
import com.raytheon.uf.edex.database.cluster.handler.CurrentTimeClusterLockHandler;

/**
 * TODO Add Description
 * 
 * <pre>
 * 
 * SOFTWARE HISTORY
 * Date         Ticket#    Engineer    Description
 * ------------ ---------- ----------- --------------------------
 * Nov 10, 2009            njensen     Initial creation
 * 08/20/2012   DR 15340   D. Friedman Fix BBB problems
 * 10/12/2012   DR 15418   D. Friedman Make BBB determination cluster-aware
 * 
 * </pre>
 * 
 * @author njensen
 * @version 1.0
 */

public class TransmittedProductList {
    private static final transient IUFStatusHandler statusHandler = UFStatus.getHandler(TransmittedProductList.class);

    private static final String LOCK_NAME = "OUP-TransProdList";

    /** Represents a BBB field that is set to an empty value (as opposed to
     * an "unknown" or "not set" state.
     */
    private static final String EMPTY_BBB_VAL = "-";

    private static final long CLUSTER_LOCK_TIMEOUT = 15 * 1000;

    public static String getBBB(String productId, String wmoId,
            String productTime, String productBBB) {
        /* If the user has assigned a value to the BBB field, just pass the
         * product through without incrementing the BBB value.  If that
         * assigned value is RRx, still need to need to update the
         * cluster-shared header list.
         */
        boolean getNextBBB = true;
        if (productBBB.length() == 3) {
            String left2 = productBBB.substring(0, 2);
            if (left2.equals("RR"))
                getNextBBB = false;
            else if (left2.equals("AA") || left2.equals("CC"))
                return productBBB;
        }

        String lockName = LOCK_NAME;
        CurrentTimeClusterLockHandler lockHandler = null;
        lockHandler = new CurrentTimeClusterLockHandler(CLUSTER_LOCK_TIMEOUT,
                false);
        ClusterTask ct = ClusterLockUtils.lock(lockName,
                wmoId, lockHandler, true);
        if (! ct.getLockState().equals(LockState.SUCCESSFUL))
            statusHandler.handle(Priority.ERROR,
                    String.format("Unable to get cluster lock for %s %s. Proceeding without it.",
                            wmoId, productId));
        try {
            TphInfo info = parse(ct.getExtraInfo());
            String result;
            if (getNextBBB) {
                String tplBBB = info.getBBBForTime(productTime);
                String bbbToUse = getNextBBB(productBBB, tplBBB);
                info.setBBBForTime(productTime, isSet(bbbToUse) ? bbbToUse : EMPTY_BBB_VAL);
                statusHandler.handle(isSet(bbbToUse) ? Priority.INFO : Priority.VERBOSE,
                        String.format("For %s %s DDHHMM=%s,BBB=%s,tplBBB=%s, use BBB=%s",
                                wmoId, productId, productTime, productBBB, tplBBB, bbbToUse));
                // Current protocol is to return null for empty case
                result = isSet(bbbToUse) ? bbbToUse : null;
            } else {
                statusHandler.handle(Priority.INFO,
                        String.format("Product %s %s DDHHMM=%s explicity requested BBB=%s",
                                wmoId, productId, productTime, productBBB));
                info.setBBBForTime(productTime, productBBB);
                result = productBBB;
            }
            lockHandler.setExtraInfo(info.format());
            return result;
        } finally {
            if (ct.getLockState().equals(LockState.SUCCESSFUL))
                ClusterLockUtils.unlock(ct, false);
        }
    }

    private static String getNextBBB(String productBBB, String transmittedBBB) {
        if (! isSet(transmittedBBB))
            return "";
        else if (EMPTY_BBB_VAL.equals(transmittedBBB))
            return "RRA";

        char newX = transmittedBBB.charAt(2);
        if (newX == 'X') {
            newX = 'A';
        } else {
            newX++;
        }
        return transmittedBBB.substring(0, 2) + Character.toString(newX);
    }

    public static boolean isSet(String s) {
        return s != null && s.length() > 0;
    }

    /** Manages the storage of transmitted product header state in the 
     * cluster lock table.  Currently only supports tracking state for
     * one minute at a time (like AWIPS I.)
     */
    private static class TphInfo {
        private String time;
        private String bbb;

        public String format() {
            if (isSet(time))
                return String.format("%s:%s", time, isSet(bbb) ? bbb : "");
            else
                return "";
        }

        public void setBBBForTime(String productTime, String bbbToUse) {
            time = productTime;
            bbb = isSet(bbbToUse) ? bbbToUse : null;
        }

        public String getBBBForTime(String productTime) {
            if (productTime != null && productTime.equals(time))
                return isSet(bbb) ? bbb : null;
            else
                return null;
        }
    }

    public static TphInfo parse(String input) {
        TphInfo inf = new TphInfo();
        if (input != null) {
            String[] parts = input.split(":");
            if (parts.length == 2) {
                inf.time = parts[0]; // Only compared via String.equals; no need to validate further
                if (validateBBB(parts[1]))
                    inf.bbb = parts[1];
            }
        }
        return inf;
    }

    private static boolean validateBBB(String bbb) {
        if (EMPTY_BBB_VAL.equals(bbb))
            return true;
        else if (bbb.length() == 3) {
            int i;
            for (i = 0; i < bbb.length(); ++i)
                if (bbb.charAt(i) < 'A' || bbb.charAt(i) > 'Z')
                    break;
            if (i == bbb.length())
                return true;
        }
        statusHandler.handle(Priority.ERROR,
                String.format("Invalid BBB in cluster lock info: \"%s\"", bbb));
        return false;
    }
}
