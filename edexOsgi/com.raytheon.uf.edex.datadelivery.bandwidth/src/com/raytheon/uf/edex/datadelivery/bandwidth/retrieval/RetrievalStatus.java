package com.raytheon.uf.edex.datadelivery.bandwidth.retrieval;

import com.raytheon.uf.edex.datadelivery.bandwidth.IBandwidthManager;
import com.raytheon.uf.edex.datadelivery.bandwidth.dao.BandwidthAllocation;

/**
 * 
 * Enumeration of the various stages {@link IBandwidthManager} uses during the
 * processing of a {@link BandwidthAllocation}.
 * 
 * <pre>
 * 
 * SOFTWARE HISTORY
 * 
 * Date         Ticket#    Engineer    Description
 * ------------ ---------- ----------- --------------------------
 * Oct 26, 2012 1286       djohnson     Added SW history and UNSCHEDULED.
 * 
 * </pre>
 * 
 * @author jspinks
 * @version 1.0
 */
public enum RetrievalStatus {
    // Intermediate state during processing
    PROCESSING,
    // In process of being retrieved
    RETRIEVAL,
    // Retrieval completed
    FULFILLED,
    // Canceled by user request
    CANCELLED,
    // BandwidthAllocation has been successfully scheduled.
    SCHEDULED,
    // Processing of the BandwidthAllocation failed
    FAILED,
    // BandwidthAllocation is ready to be processed.
    READY,
    // BandwidthAllocation has been rescheduled as a result of processing
    RESCHEDULE,
    // Reserve space in the RetrievalPlan for BandwidthAllocations
    // that do not fit into one BandwidthBucket.
    RESERVED,
    // The allocation has been postponed as the requested time is outside
    // the current plan time.
    DEFERRED,
    // The allocation was unable to be scheduled, or was otherwise removed from
    // the retrieval plan
    UNSCHEDULED;
}
