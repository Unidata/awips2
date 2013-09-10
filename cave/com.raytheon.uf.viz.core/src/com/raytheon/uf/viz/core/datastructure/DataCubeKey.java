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
package com.raytheon.uf.viz.core.datastructure;

import com.raytheon.uf.common.style.level.SingleLevel;
import com.raytheon.uf.common.time.DataTime;

/**
 * Key for data in the data cube
 * 
 * <pre>
 * SOFTWARE HISTORY
 * Date         Ticket#    Engineer    Description
 * ------------ ---------- ----------- --------------------------
 * Nov 28, 2007            njensen     Initial creation	
 * 
 * </pre>
 * 
 * @author njensen
 * @version 1.0
 */

public class DataCubeKey {

    private DataTime time;

    private SingleLevel level;

    /**
     * @param time
     * @param level
     */
    public DataCubeKey(DataTime time, SingleLevel level) {
        this.time = time;
        this.level = level;
    }

    /**
     * @return the time
     */
    public DataTime getTime() {
        return time;
    }

    /**
     * @param time
     *            the time to set
     */
    public void setTime(DataTime time) {
        this.time = time;
    }

    /**
     * @return the level
     */
    public SingleLevel getLevel() {
        return level;
    }

    /**
     * @param level
     *            the level to set
     */
    public void setLevel(SingleLevel level) {
        this.level = level;
    }

    /*
     * (non-Javadoc)
     * 
     * @see java.lang.Object#hashCode()
     */
    @Override
    public int hashCode() {
        final int prime = 31;
        int result = 1;
        result = prime * result + ((level == null) ? 0 : level.hashCode());
        result = prime * result + ((time == null) ? 0 : time.hashCode());
        return result;
    }

    /*
     * (non-Javadoc)
     * 
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
        final DataCubeKey other = (DataCubeKey) obj;
        if (level == null) {
            if (other.level != null)
                return false;
        } else if (!level.equals(other.level))
            return false;
        if (time == null) {
            if (other.time != null)
                return false;
        } else if (!time.equals(other.time))
            return false;
        return true;
    }

}
