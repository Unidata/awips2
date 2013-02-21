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

import com.raytheon.uf.common.serialization.annotations.DynamicSerialize;
import com.raytheon.uf.common.serialization.annotations.DynamicSerializeElement;

/**
 * Instances of this class should not be used directly, all access should be
 * restrained through {@link IProposeScheduleResponse}.
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
@DynamicSerialize
public class ProposeScheduleResponse implements IProposeScheduleResponse {

    @DynamicSerializeElement
    private Set<String> unscheduledSubscriptions = Collections.emptySet();

    @DynamicSerializeElement
    private int requiredLatency = IProposeScheduleResponse.REQUIRED_LATENCY_NOT_SET;

    /**
     * {@inheritDoc}
     */
    @Override
    public Set<String> getUnscheduledSubscriptions() {
        return unscheduledSubscriptions;
    }

    /**
     * @param unscheduledSubscriptions
     *            the unscheduledSubscriptions to set
     */
    public void setUnscheduledSubscriptions(Set<String> unscheduledSubscriptions) {
        this.unscheduledSubscriptions = unscheduledSubscriptions;
    }

    /**
     * @param requiredLatency
     */
    public void setRequiredLatency(int requiredLatency) {
        this.requiredLatency = requiredLatency;
    }

    /**
     * {@inheritDoc}
     */
    @Override
    public int getRequiredLatency() {
        return requiredLatency;
    }
}
