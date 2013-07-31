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
 * Lightweight placeholder stats object, does not record stats
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
public class NoopStatsRecorder implements OgcStatsRecorder {

    /*
     * (non-Javadoc)
     * 
     * @see com.raytheon.uf.edex.ogc.common.OgcStatsRecorder#recordRequest(long,
     * long, java.lang.String, boolean)
     */
    @Override
    public void recordRequest(long time, long duration, String service,
            boolean success) {
        // do nothing
    }

    /*
     * (non-Javadoc)
     * 
     * @see
     * com.raytheon.uf.edex.ogc.common.OgcStatsRecorder#getMinRequestTime(java
     * .lang.String)
     */
    @Override
    public long getMinRequestTime(String service) {
        return 0;
    }

    /*
     * (non-Javadoc)
     * 
     * @see
     * com.raytheon.uf.edex.ogc.common.OgcStatsRecorder#getMaxRequestTime(java
     * .lang.String)
     */
    @Override
    public long getMaxRequestTime(String service) {
        return 0;
    }

    /*
     * (non-Javadoc)
     * 
     * @see
     * com.raytheon.uf.edex.ogc.common.OgcStatsRecorder#getAvgRequestTime(java
     * .lang.String)
     */
    @Override
    public long getAvgRequestTime(String service) {
        return 0;
    }

}
