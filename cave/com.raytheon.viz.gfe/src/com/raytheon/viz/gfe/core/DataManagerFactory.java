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
package com.raytheon.viz.gfe.core;

import java.util.Map;
import java.util.concurrent.ConcurrentHashMap;

import com.raytheon.uf.common.status.IUFStatusHandler;
import com.raytheon.uf.common.status.UFStatus;
import com.raytheon.uf.common.status.UFStatus.Priority;
import com.raytheon.viz.gfe.GFEServerException;

/**
 * Factory class for construct {@link DataManager} objects for an arbitrary Object
 * discriminator. Manages {@link DataManager}s so only one per discriminator exists.
 * Takes instance of this class when constructing {@link DataManager} to for
 * construction of {@link ISpatialDisplayManager} for the {@link DataManager}
 * 
 * <pre>
 * 
 * SOFTWARE HISTORY
 * 
 * Date         Ticket#    Engineer    Description
 * ------------ ---------- ----------- --------------------------
 * Nov 27, 2012            mschenke     Initial creation
 * 
 * </pre>
 * 
 * @author mschenke
 * @version 1.0
 */

public abstract class DataManagerFactory {

    protected static final transient IUFStatusHandler statusHandler = UFStatus
            .getHandler(DataManagerUIFactory.class);

    private static class DataManagerKey {

        private final Object discriminator;

        DataManagerKey(Object discriminator) {
            this.discriminator = discriminator;
        }

        /*
         * (non-Javadoc)
         * 
         * @see java.lang.Object#hashCode()
         */
        @Override
        public int hashCode() {
            return (discriminator == null ? 0 : discriminator.hashCode());
        }

        /*
         * (non-Javadoc)
         * 
         * @see java.lang.Object#equals(java.lang.Object)
         */
        @Override
        public boolean equals(Object obj) {
            if (obj instanceof DataManagerKey) {
                Object otherDiscriminator = ((DataManagerKey) obj).discriminator;
                if (discriminator == null) {
                    return otherDiscriminator == null;
                }
                return discriminator.equals(otherDiscriminator);
            }
            return false;
        }
    }

    private static Map<DataManagerKey, DataManager> instanceMap = new ConcurrentHashMap<DataManagerKey, DataManager>();;

    /**
     * Find the DataManager associated with a discriminator
     * 
     * If it does not exist, return null.
     * 
     * @param discriminator
     *            the discriminator
     * @return the DataManager associated with the specified discriminator
     */
    static DataManager findInstance(Object discriminator) {
        return instanceMap.get(new DataManagerKey(discriminator));
    }

    /**
     * Gets an instance of the DataManager for the given discriminator, factory
     * will be used to create display manager
     * 
     * @param factory
     * @param discriminator
     * @return
     */
    public static DataManager getInstance(DataManagerFactory factory,
            Object discriminator) {
        synchronized (DataManagerKey.class) {
            DataManagerKey key = new DataManagerKey(discriminator);
            DataManager dm = instanceMap.get(key);
            if (dm == null) {
                try {
                    dm = new DataManager(factory, discriminator);
                    instanceMap.put(key, dm);
                    waitForJobs(dm);
                } catch (GFEServerException e) {
                    statusHandler.handle(Priority.PROBLEM,
                            e.getLocalizedMessage(), e);
                }
            }
            return dm;
        }
    }

    /**
     * @param dataManager
     */
    private static void waitForJobs(DataManager dataManager) {
        // wait to ensure init jobs are done
        long waitForJobs = 0;
        while (!dataManager.getInitStatus().isDone()) {
            try {
                waitForJobs += 10;
                Thread.sleep(10);
            } catch (InterruptedException e) {
                statusHandler.handle(Priority.PROBLEM, e.getLocalizedMessage(),
                        e);
            }
        }
        System.out.println("Waiting on Jobs: " + waitForJobs);
    }

    /**
     * Dispose an instance of the DataManager associated with a particular
     * discriminator
     * 
     * @param window
     */
    public static void dispose(Object discriminator) {
        synchronized (DataManagerKey.class) {
            DataManager dm = instanceMap.remove(new DataManagerKey(
                    discriminator));

            if (dm != null) {
                dm.dispose();
            }
        }
    }

    /**
     * Create an {@link ISpatialDisplayManager} for the {@link DataManager} and
     * discriminator
     * 
     * @param dm
     * @param discriminator
     * @return
     */
    protected abstract ISpatialDisplayManager createSpatialDisplayManager(
            DataManager dm, Object discriminator);
}
