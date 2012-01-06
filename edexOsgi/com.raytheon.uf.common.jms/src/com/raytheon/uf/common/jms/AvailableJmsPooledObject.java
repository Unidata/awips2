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
package com.raytheon.uf.common.jms;

/**
 * TODO Add Description
 * 
 * <pre>
 * 
 * SOFTWARE HISTORY
 * 
 * Date         Ticket#    Engineer    Description
 * ------------ ---------- ----------- --------------------------
 * Aug 11, 2011            rjpeter     Initial creation
 * 
 * </pre>
 * 
 * @author rjpeter
 * @version 1.0
 */

public class AvailableJmsPooledObject<T> {
    private T pooledObject;

    private long availableTime = System.currentTimeMillis();

    public AvailableJmsPooledObject(T pooledObject) {
        this.pooledObject = pooledObject;
    }

    public T getPooledObject() {
        return pooledObject;
    }

    public long getAvailableTime() {
        return availableTime;
    }

    /**
     * 
     * @param curTime
     * @param timeOut
     *            in millis
     * @return
     */
    public boolean expired(long curTime, int timeOut) {
        return (curTime - availableTime) >= timeOut;
    }

    @Override
    public int hashCode() {
        final int prime = 31;
        int result = 1;
        result = prime * result
                + ((pooledObject == null) ? 0 : pooledObject.hashCode());
        return result;
    }

    public void reset() {
        availableTime = System.currentTimeMillis();
    }

    /**
     * Allows equals with the base pooled object for easy removal from lists.
     */
    @Override
    public boolean equals(Object obj) {
        if (this == obj)
            return true;
        if (obj == null)
            return false;
        if (getClass() == obj.getClass()) {
            AvailableJmsPooledObject other = (AvailableJmsPooledObject) obj;
            if (pooledObject == null) {
                if (other.pooledObject != null)
                    return false;
            } else if (!pooledObject.equals(other.pooledObject))
                return false;
        } else {
            if (pooledObject != null && obj != null) {
                if (pooledObject.getClass() != obj.getClass())
                    return false;
                else if (!pooledObject.equals(obj))
                    return false;
            } else {
                return false;
            }
        }
        return true;
    }
}
