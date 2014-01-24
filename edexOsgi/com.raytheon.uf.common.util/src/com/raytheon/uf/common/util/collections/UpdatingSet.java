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
package com.raytheon.uf.common.util.collections;

import java.util.Collections;
import java.util.Set;

/**
 * Manages a set that is updated upon access if the time since last update is
 * greater than a specified timeout. The user of the set only needs to call the
 * 'get' method to get a read-only set without worrying about updating the set
 * themselves.
 * 
 * <pre>
 * 
 * SOFTWARE HISTORY
 * 
 * Date         Ticket#    Engineer    Description
 * ------------ ---------- ----------- --------------------------
 * Jan 23, 2014 2701       bclement     Initial creation
 * 
 * </pre>
 * 
 * @author bclement
 * @version 1.0
 */
public abstract class UpdatingSet<T> extends UpdatingCollection<Set<T>, T> {

    /**
     * @see UpdatingCollection#UpdatingCollection(long)
     * @param timeoutMillis
     *            timeout before new update in milliseconds
     */
    public UpdatingSet(long timeoutMillis) {
        super(timeoutMillis);
    }

    /**
     * @see UpdatingCollection#UpdatingCollection(String, long)
     * @param timeoutProp
     *            system property name
     * @param defaultTimeoutMillis
     *            default timeout before new update in milliseconds
     */
    public UpdatingSet(String timeoutProp, long defaultTimeoutMillis) {
        super(timeoutProp, defaultTimeoutMillis);
    }

    /*
     * (non-Javadoc)
     * 
     * @see
     * com.raytheon.uf.common.util.collections.UpdatingCollection#wrap(java.
     * util.Collection)
     */
    @Override
    protected Set<T> wrap(Set<T> collection) {
        return Collections.unmodifiableSet(collection);
    }

    /*
     * (non-Javadoc)
     * 
     * @see
     * com.raytheon.uf.common.util.collections.UpdatingCollection#getEmpty()
     */
    @Override
    protected Set<T> getEmpty() {
        return Collections.emptySet();
    }

}
