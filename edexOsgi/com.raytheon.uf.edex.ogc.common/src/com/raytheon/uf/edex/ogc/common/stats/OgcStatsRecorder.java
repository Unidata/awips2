/**
 * Copyright 09/24/12 Raytheon Company.
 *
 * Unlimited Rights
 * This software was developed pursuant to Contract Number 
 * DTFAWA-10-D-00028 with the US Government. The US Government’s rights 
 * in and to this copyrighted software are as specified in DFARS
 * 252.227-7014 which was made part of the above contract. 
 */
package com.raytheon.uf.edex.ogc.common.stats;



/**
 * interface for recording ogc service stats
 * 
 * <pre>
 * 
 * SOFTWARE HISTORY
 * 
 * Date         Ticket#    Engineer    Description
 * ------------ ---------- ----------- --------------------------
 * Jun 25, 2013            bclement     Initial creation
 * 
 * </pre>
 * 
 * @author bclement
 * @version 1.0
 */
public interface OgcStatsRecorder {

    /**
     * Records the request that was made with some basic raw information about
     * it.
     * 
     * @param time
     *            time of observation in milliseconds
     * @param duration
     *            duration of request in nanoseconds
     * @param service
     *            Id for service
     * @param success
     *            indicates if the request being timed was successful
     */
    public void recordRequest(long time, long duration, String service,
            boolean success);

    /**
     * Retrieve statistic about service
     * 
     * @param service
     * @return
     */
    public long getMinRequestTime(String service);

    /**
     * Retrieve statistic about service
     * 
     * @param service
     * @return
     */
    public long getMaxRequestTime(String service);

    /**
     * Retrieve statistic about service
     * 
     * @param service
     * @return
     */
    public long getAvgRequestTime(String service);

}
