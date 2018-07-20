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
package com.raytheon.uf.edex.plugin.mpe.rocchecker;

import com.raytheon.uf.common.dataplugin.shef.tables.Datalimits;
import com.raytheon.uf.common.dataplugin.shef.tables.Locdatalimits;

/**
 * POJO to wrap data limit values so that it will not be necessary to keep
 * distinguishing between {@link Datalimits} and {@link Locdatalimits}.
 * 
 * <pre>
 * 
 * SOFTWARE HISTORY
 * 
 * Date         Ticket#    Engineer    Description
 * ------------ ---------- ----------- --------------------------
 * Jun 23, 2016 5699       bkowal      Initial creation
 * 
 * </pre>
 * 
 * @author bkowal
 */

public class ObsLimits {

    public static final double LIMIT_MULTIPLIER = 100.0;

    private final Double thresholdROCMax;

    private final Double thresholdAlarmROCLimit;

    private final Double thresholdAlertROCLimit;

    public ObsLimits(final Locdatalimits locdatalimits) {
        if (locdatalimits == null) {
            throw new IllegalArgumentException(
                    "Required argument 'locdatalimits' cannot be NULL.");
        }
        thresholdROCMax = locdatalimits.getRocMax();
        thresholdAlarmROCLimit = locdatalimits.getAlarmRocLimit();
        thresholdAlertROCLimit = locdatalimits.getAlertRocLimit();
    }

    public ObsLimits(final Datalimits datalimits) {
        if (datalimits == null) {
            throw new IllegalArgumentException(
                    "Required argument 'datalimits' cannot be NULL.");
        }
        thresholdROCMax = datalimits.getRocMax();
        thresholdAlarmROCLimit = datalimits.getAlarmRocLimit();
        thresholdAlertROCLimit = datalimits.getAlertRocLimit();
    }

    /**
     * Determines if at least one of the three limit thresholds has been
     * specified.
     * 
     * @return {@code true} if at least one of the thresholds has been specified
     *         (not null); {@code false} otherwise
     */
    public boolean oneLimitDefined() {
        return (thresholdROCMax != null || thresholdAlarmROCLimit != null || thresholdAlertROCLimit != null);
    }

    public Double getThresholdROCMax() {
        return thresholdROCMax;
    }

    public Double getThresholdAlarmROCLimit() {
        return thresholdAlarmROCLimit;
    }

    public Double getThresholdAlertROCLimit() {
        return thresholdAlertROCLimit;
    }

    @Override
    public String toString() {
        StringBuilder sb = new StringBuilder("ObsLimits [");
        sb.append("thresholdROCMax=").append(thresholdROCMax);
        sb.append(", thresholdAlarmROCLimit=").append(thresholdAlarmROCLimit);
        sb.append(", thresholdAlertROCLimit=").append(thresholdAlertROCLimit);
        sb.append("]");

        return sb.toString();
    }
}