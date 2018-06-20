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
package com.raytheon.uf.common.dataplugin.bufrua;

import com.raytheon.uf.common.time.DataTime;

/**
 * 
 * TODO Add Description
 * 
 * <pre>
 *
 * SOFTWARE HISTORY
 *
 * Date         Ticket#    Engineer    Description
 * ------------ ---------- ----------- --------------------------
 * Dec 10, 2008            jkorman     Initial creation
 *
 * </pre>
 *
 * @author jkorman
 * @version 1.0
 */
class ObsMapKey {
    private final DataTime dataTime;
    private final String stationId;
    
    /**
     * 
     * @param obs
     */
    public ObsMapKey(UAObs obs) {
        dataTime = obs.getDataTime();
        stationId = obs.getStationId();
    }

    /* (non-Javadoc)
     * @see java.lang.Object#hashCode()
     */
    @Override
    public int hashCode() {
        final int prime = 31;
        int result = 1;
        result = prime * result
                + ((dataTime == null) ? 0 : dataTime.hashCode());
        result = prime * result
                + ((stationId == null) ? 0 : stationId.hashCode());
        return result;
    }

    /* (non-Javadoc)
     * @see java.lang.Object#equals(java.lang.Object)
     */
    @Override
    public boolean equals(Object obj) {
        if (this == obj)
            return true;
        if (obj == null)
            return false;
        if (getClass() != obj.getClass())
            return false;
        final ObsMapKey other = (ObsMapKey) obj;
        if (dataTime == null) {
            if (other.dataTime != null)
                return false;
        } else if (!dataTime.equals(other.dataTime))
            return false;
        if (stationId == null) {
            if (other.stationId != null)
                return false;
        } else if (!stationId.equals(other.stationId))
            return false;
        return true;
    }
}
