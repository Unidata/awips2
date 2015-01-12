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
package com.raytheon.uf.edex.plugin.modelsounding;

import java.util.Iterator;
import java.util.LinkedHashMap;
import java.util.List;

import com.raytheon.uf.common.dataplugin.PluginDataObject;
import com.raytheon.uf.common.status.IUFStatusHandler;
import com.raytheon.uf.common.status.UFStatus;
import com.raytheon.uf.common.status.UFStatus.Priority;
import com.raytheon.uf.edex.core.EDEXUtil;
import com.raytheon.uf.edex.core.EdexException;
import com.raytheon.uf.edex.core.IContextStateProcessor;

/**
 * Thread for storing Model Soundings asynchronously. If decode thread decodes
 * more data for an hdf5 record that hasn't been stored yet it will allow
 * appending to the pending store.
 * 
 * <pre>
 * 
 * SOFTWARE HISTORY
 * 
 * Date         Ticket#    Engineer    Description
 * ------------ ---------- ----------- --------------------------
 * Jul 17, 2013 2161       bkowal      Initial creation.
 * Mar 11, 2014 2726       rjpeter     Graceful shutdown, don't forward empty pdo lists.
 * </pre>
 * 
 * @author bkowal
 * @version 1.0
 */

public class ModelSoundingPersistenceManager implements IContextStateProcessor {
    /** The logger */
    private final IUFStatusHandler logger = UFStatus
            .getHandler(ModelSoundingPersistenceManager.class);

    private final LinkedHashMap<String, ModelSoundingStorageContainer> containerMap;

    private volatile boolean run = false;

    private volatile boolean shutdown = false;

    /**
     *
     */
    public ModelSoundingPersistenceManager() {
        // super("ModelSoundingStore");
        this.containerMap = new LinkedHashMap<String, ModelSoundingStorageContainer>(
                64, 1);
    }

    public void run() {
        run = true;

        // continue as long as there is data in the map
        while (run || !containerMap.isEmpty()) {
            try {
                ModelSoundingStorageContainer container = null;
                synchronized (containerMap) {
                    while (run && containerMap.isEmpty()) {
                        try {
                            containerMap.wait();
                        } catch (InterruptedException e) {
                            // ignore
                        }
                    }

                    Iterator<String> iter = containerMap.keySet().iterator();
                    if (iter.hasNext()) {
                        // remove first entry and process
                        String key = iter.next();
                        container = containerMap.remove(key);
                        if (logger.isPriorityEnabled(Priority.DEBUG)) {
                            logger.debug("Persisting "
                                    + container.getPdos().size()
                                    + " PluginDataObject(s) for : " + key);
                        }
                    }
                }

                if (container != null) {
                    List<PluginDataObject> pdoList = container.getPdos();
                    if ((pdoList != null) && !pdoList.isEmpty()) {
                        PluginDataObject[] pdos = pdoList
                                .toArray(new PluginDataObject[pdoList.size()]);
                        try {
                            EDEXUtil.getMessageProducer().sendSync(
                                    "modelSoundingPersistIndexAlert", pdos);
                        } catch (EdexException e) {
                            logger.error("Failed to persist " + pdos.length
                                    + " PluginDataObject(s)!", e);
                        }
                    }
                }
            } catch (Throwable e) {
                // fail safe so store thread doesn't fail
                logger.error(
                        "Caught unknown exception on modelsounding store thread",
                        e);
            }
        }

        shutdown = true;
        synchronized (containerMap) {
            containerMap.notifyAll();
        }
    }

    protected void storeContainer(ModelSoundingStorageContainer container) {
        List<PluginDataObject> pdoList = container.getPdos();
        if ((pdoList != null) && !pdoList.isEmpty()) {
            PluginDataObject[] pdos = pdoList
                    .toArray(new PluginDataObject[pdoList.size()]);
            try {
                EDEXUtil.getMessageProducer().sendSync(
                        "modelSoundingPersistIndexAlert", pdos);
            } catch (EdexException e) {
                logger.error("Failed to persist " + pdos.length
                        + " PluginDataObject(s)!", e);
            }
        }
    }

    /**
     * Checking in a model sounding container makes it available for the store
     * thread to store. A decode thread can check out the model sounding
     * container for appending more data to if the store thread has not yet
     * stored the container.
     * 
     * @param persistRecordKey
     * @param container
     */
    public void checkIn(String persistRecordKey,
            ModelSoundingStorageContainer container) {
        boolean storeLocal = true;
        synchronized (containerMap) {
            if (run) {
                ModelSoundingStorageContainer prev = containerMap.put(
                        persistRecordKey, container);
                storeLocal = false;
                if (prev != null) {
                    // technically only possible in an environment where there
                    // are multiple decode threads running, just append the
                    // pdo's from the first, their pdc will not be used again
                    if (logger.isPriorityEnabled(Priority.DEBUG)) {
                        logger.debug("PDC for time already exists, appending previous PDC data");
                    }

                    container.addPdos(prev.getPdos());
                }
                containerMap.notify();
            }
        }

        if (storeLocal) {
            storeContainer(container);
        }
    }

    /**
     * Checks out a sounding container if one is pending store. This allows a
     * decode thread to append more data to the store if the writing thread has
     * not yet gotten to the data.
     * 
     * @param persistRecordKey
     * @return
     */
    public ModelSoundingStorageContainer checkOut(String persistRecordKey) {
        synchronized (containerMap) {
            return containerMap.remove(persistRecordKey);
        }
    }

    @Override
    public void preStart() {
    }

    @Override
    public void postStart() {
    }

    @Override
    public void preStop() {
        if (run) {
            synchronized (containerMap) {
                run = false;
                containerMap.notifyAll();

                while (!shutdown) {
                    try {
                        containerMap.wait(1000);
                    } catch (Exception e) {
                        // ignore
                    }
                }
            }
        }
    }

    @Override
    public void postStop() {
    }
}