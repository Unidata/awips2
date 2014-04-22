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

import java.util.Collection;

/**
 * Manages a collection that is updated upon access if the time since last
 * update is greater than a specified timeout. The user of the collection only
 * needs to call the 'get' method to get a read-only collection without worrying
 * about updating the collection themselves.
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
public abstract class UpdatingCollection<C extends Collection<T>, T> {

    private final long timeoutMillis;

    private C collection;

    private long lastUpdate;

    private boolean enabled = true;

    /**
     * @param timeoutMillis
     *            timeout before new update in milliseconds
     */
    public UpdatingCollection(long timeoutMillis) {
        this.timeoutMillis = timeoutMillis;
    }

    /**
     * Attempt to get timeout from system property
     * 
     * @param timeoutProp
     *            system property name
     * @param defaultTimeoutMillis
     *            default timeout before new update in milliseconds
     */
    public UpdatingCollection(String timeoutProp, long defaultTimeoutMillis) {
        this(Long.getLong(timeoutProp, defaultTimeoutMillis));
    }

    /**
     * Update collection. This method will be called if the collection is not
     * initialized or if the time since last update is greater than timeout. If
     * this method returns null, the time used in the timeout check will not be
     * updated and the get method will return an empty collection.
     * 
     * @return null if there was a problem with update
     */
    abstract protected C update();

    /**
     * Access the collection. The returned value is unmodifiable and guaranteed
     * to not be null.
     * 
     * @return
     */
    public synchronized C get() {
        if (!enabled) {
            return collection != null ? collection : getEmpty();
        }
        C rval = collection;
        long sinceLastUpdate = System.currentTimeMillis() - lastUpdate;
        if (collection == null || sinceLastUpdate > timeoutMillis) {
            rval = update();
            if (rval != null) {
                rval = collection = wrap(rval);
                lastUpdate = System.currentTimeMillis();
            } else {
                rval = getEmpty();
            }
        }
        return rval;
    }

    /**
     * Wrap collection in unmodifiable collection.
     * 
     * @return
     */
    protected abstract C wrap(C collection);

    /**
     * @return an empty, unmodifiable collection
     */
    protected abstract C getEmpty();

    /**
     * Prevent the collection from attempting to update.
     */
    public synchronized void disable() {
        this.enabled = false;
    }

    /**
     * Allow the collection to attempt to update.
     */
    public synchronized void enable() {
        this.enabled = true;
    }

    /**
     * @return the timeout in milliseconds
     */
    public long getTimeout() {
        return timeoutMillis;
    }

}

