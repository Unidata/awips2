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
package com.raytheon.uf.edex.datadelivery.service.services.overlap;

import com.raytheon.uf.common.datadelivery.registry.Coverage;
import com.raytheon.uf.common.datadelivery.registry.DataType;
import com.raytheon.uf.common.datadelivery.registry.GriddedCoverage;
import com.raytheon.uf.common.datadelivery.registry.GriddedTime;
import com.raytheon.uf.common.datadelivery.registry.PointTime;
import com.raytheon.uf.common.datadelivery.registry.Subscription;
import com.raytheon.uf.common.datadelivery.registry.Time;
import com.raytheon.uf.common.datadelivery.service.subscription.ISubscriptionOverlapService;
import com.raytheon.uf.common.datadelivery.service.subscription.SubscriptionOverlapConfig;
import com.raytheon.uf.common.datadelivery.service.subscription.SubscriptionOverlapService;

/**
 * Overlap data factory
 * 
 * <pre>
 * 
 * SOFTWARE HISTORY
 * 
 * Date         Ticket#    Engineer    Description
 * ------------ ---------- ----------- --------------------------
 * Oct 22, 2013    2292    mpduff      Initial creation
 * 
 * </pre>
 * 
 * @author mpduff
 * @version 1.0
 * @param <T>
 * @param <C>
 */

public class OverlapDataFactory<T extends Time, C extends Coverage> {
    private static SubscriptionOverlapConfig config;

    /**
     * Get the overlap data object.
     * 
     * @param sub1
     * @param sub2
     * @return
     */
    public static OverlapData<?, ?> getOverlapData(Subscription<?, ?> sub1,
            Subscription<?, ?> sub2) {
        if (sub1.getDataSetType() != sub2.getDataSetType()) {
            throw new IllegalArgumentException(
                    "Subscriptions must be of the same data type.");
        }
        ISubscriptionOverlapService<?, ?> overlapService = new SubscriptionOverlapService();
        DataType dt = sub1.getDataSetType();
        config = overlapService.getConfigFile(dt);
        if (dt == DataType.GRID) {
            return new GridOverlapData<GriddedTime, GriddedCoverage>(sub1,
                    sub2, config);
        } else if (dt == DataType.POINT) {
            return new PointOverlapData<PointTime, Coverage>(sub1, sub2, config);
        }

        throw new IllegalArgumentException("Invalid Data Type: "
                + sub1.getDataSetType());
    }

}
