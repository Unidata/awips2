package com.raytheon.uf.edex.datadelivery.bandwidth.retrieval;

import java.util.List;

import com.raytheon.uf.edex.datadelivery.bandwidth.dao.BandwidthAllocation;

/**
 * Interface for scheduling {@link BandwidthAllocation} in a
 * {@link RetrievalPlan} by the {@link RetrievalManager}.
 * 
 * <pre>
 * 
 * SOFTWARE HISTORY
 * Date         Ticket#    Engineer    Description
 * ------------ ---------- ----------- --------------------------
 * Oct 07, 2012 726        jspinks      Initial release.
 * Oct 26, 2012 1286       djohnson     Return list of unscheduled allocations.
 * 
 * </pre>
 * 
 * @version 1.0
 */
public interface IRetrievalScheduler {

    /**
     * Attempt to schedule a BandwidthAllocation in the specified RetrievalPlan.
     * 
     * @param plan
     *            The RetrievalPlan to attempt to schedule the
     *            BandwidthAllocation in.
     * 
     * @param allocation
     *            The BandwidthAllocation to schedule.
     * @return the {@link BandwidthAllocation}s that are unable to be scheduled,
     *         this can be formerly scheduled allocations that were booted to
     *         make room for an allocation deemed more important
     */
    public List<BandwidthAllocation> schedule(RetrievalPlan plan,
            BandwidthAllocation allocation);
}
