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
package com.raytheon.uf.common.datadelivery.bandwidth;

import java.util.Collections;
import java.util.Set;

/**
 * The response interface for a propose schedule operation.
 * 
 * <pre>
 * 
 * SOFTWARE HISTORY
 * 
 * Date         Ticket#    Engineer    Description
 * ------------ ---------- ----------- --------------------------
 * Nov 29, 2012 1286       djohnson     Initial creation
 * 
 * </pre>
 * 
 * @author djohnson
 * @version 1.0
 */

public interface IProposeScheduleResponse {

    /**
     * Represents a null response.
     */
    IProposeScheduleResponse NULL_OBJECT = new IProposeScheduleResponse() {
        @Override
        public Set<String> getUnscheduledSubscriptions() {
            return Collections.emptySet();
        }

        @Override
        public int getRequiredLatency() {
            return REQUIRED_LATENCY_NOT_SET;
        }
    };

    int REQUIRED_LATENCY_NOT_SET = -1;

    /**
     * Get the set of subscription names that would be unscheduled if the
     * proposed schedule were to be force applied.
     * 
     * @return the subscription names
     */
    Set<String> getUnscheduledSubscriptions();

    /**
     * Get the required latency for the subscription to not unschedule any
     * subscriptions.
     * 
     * @return the required latency
     */
    int getRequiredLatency();
}
