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
package com.raytheon.edex.plugin.grib;

import java.io.File;
import java.util.HashMap;
import java.util.HashSet;
import java.util.Iterator;
import java.util.LinkedHashMap;
import java.util.LinkedList;
import java.util.List;
import java.util.Map;
import java.util.Set;

import org.apache.camel.Headers;

import com.raytheon.uf.common.dataplugin.PluginDataObject;
import com.raytheon.uf.common.dataplugin.grid.GridRecord;
import com.raytheon.uf.common.dataplugin.persist.IHDFFilePathProvider;
import com.raytheon.uf.common.status.IUFStatusHandler;
import com.raytheon.uf.common.status.UFStatus;
import com.raytheon.uf.common.util.SizeUtil;
import com.raytheon.uf.edex.core.EDEXUtil;
import com.raytheon.uf.edex.core.EdexException;
import com.raytheon.uf.edex.core.IContextStateProcessor;
import com.raytheon.uf.edex.database.plugin.PluginFactory;

/**
 * Class to persist grids asynchronously from decode. Grids queued by hdf5 file.
 * 
 * <pre>
 * 
 * SOFTWARE HISTORY
 * 
 * Date         Ticket#    Engineer    Description
 * ------------ ---------- ----------- --------------------------
 * Sep 10, 2015 4868       rjpeter     Initial creation
 * 
 * </pre>
 * 
 * @author rjpeter
 * @version 1.0
 */

public class GribPersister implements IContextStateProcessor {
    private static final String HEADERS_ATTRIBUTE = "HEADERS";

    private final IUFStatusHandler statusHandler = UFStatus
            .getHandler(GribPersister.class);

    private final Map<String, List<GridRecord>> gridsByFile = new LinkedHashMap<>();

    private final Set<String> inProcessFiles = new HashSet<>();

    private volatile boolean running = true;

    public IHDFFilePathProvider pathProvider;

    private long maxBytesInMemory = 0;

    private long bytesInMemory = 0;

    private int gridsPending = 0;

    private int gridsInProcess = 0;

    private final GribPersistThread[] persistThreads;

    public GribPersister(String pluginName, String numThreads,
            String maxGridsInMb) {
        pathProvider = PluginFactory.getInstance().getPathProvider(pluginName);
        int numPersistThreads = 0;
        try {
            numPersistThreads = Integer.parseInt(numThreads);
        } catch (NumberFormatException e) {
            // ignore
        }

        if (numPersistThreads <= 0) {
            numPersistThreads = 4;
            statusHandler.warn("Invalid numThreads [" + numThreads
                    + "], using default of [" + numPersistThreads + "]");
        }

        int maxInMb = 0;
        try {
            maxInMb = Integer.parseInt(maxGridsInMb);
        } catch (NumberFormatException e) {
            // ignore
        }

        if (maxInMb <= 0) {
            maxInMb = 100;
            statusHandler.warn("Invalid maxGridInMb [" + maxGridsInMb
                    + "], using default of [" + maxInMb + "]");
        }

        maxBytesInMemory = maxInMb * 1024l * 1024l;

        persistThreads = new GribPersistThread[numPersistThreads];
        for (int i = 0; i < persistThreads.length; i++) {
            persistThreads[i] = new GribPersistThread();
            persistThreads[i].setName("GribPersist-" + (i + 1));
            persistThreads[i].start();
        }
    }

    public void persist(@Headers Map<String, Object> headers,
            GridRecord... records) {
        boolean storeNow = true;

        if (records != null) {
            storeNow = !addPendingRecords(headers, records);
        }

        if (storeNow) {
            try {
                sendToEndpoint("persistIndex", records);
            } catch (Exception e) {
                statusHandler.error(
                        "Error occurred sending grids to persistIndex", e);
            }
        }
    }

    /**
     * Adds the records for storage by the GribPersistThreads. Return of true
     * indicates grids will be stored, false otherwise. This is intended for
     * shutdown scenario where the main threads need to store the final grids
     * themselves.
     * 
     * @param headers
     * @param records
     * @return True if records will be stored by the GribPersistThreads.
     */
    private boolean addPendingRecords(@Headers Map<String, Object> headers,
            GridRecord[] records) {
        if (records != null) {
            StringBuilder path = new StringBuilder();
            synchronized (gridsByFile) {
                if (!running) {
                    return false;
                }

                for (GridRecord record : records) {
                    String plugin = record.getPluginName();
                    path.setLength(0);
                    path.append(pathProvider.getHDFPath(plugin, record))
                            .append(File.separatorChar)
                            .append(pathProvider.getHDFFileName(plugin, record));
                    String filePath = path.toString();
                    List<GridRecord> recs = gridsByFile.get(filePath);

                    if (recs == null) {
                        recs = new LinkedList<>();
                        gridsByFile.put(filePath, recs);
                    }

                    recs.add(record);

                    /*
                     * since grids will be bulk stored by file, track the
                     * original headers for purposes of logging and stats
                     */
                    record.addExtraAttribute(HEADERS_ATTRIBUTE, headers);

                    // update bytesInMemory
                    bytesInMemory += ((float[]) record.getMessageData()).length * 4;
                }

                gridsPending += records.length;

                // wake up any sleeping persist threads
                gridsByFile.notifyAll();
                boolean logMessage = true;

                while (bytesInMemory > maxBytesInMemory) {
                    if (logMessage) {
                        statusHandler.info("Max Grids in memory for "
                                + getClass().getName()
                                + " exceeded.  Waiting for grids to process");
                        logMessage = false;
                    }

                    try {
                        gridsByFile.wait();
                    } catch (InterruptedException e) {
                        // ignore
                    }
                }
            }

            return true;
        }

        return false;
    }

    private void sendToEndpoint(String endpoint, PluginDataObject... pdos)
            throws EdexException {
        EDEXUtil.getMessageProducer().sendSync(endpoint, pdos);
    }

    /**
     * Handle case of multiple quadrants stored in one transaction.
     * 
     * @param pdos
     * @return
     */
    public PluginDataObject[] eliminateDuplicates(PluginDataObject... pdos) {
        if ((pdos != null) && (pdos.length > 1)) {
            // dup elim by dataURI
            Map<String, PluginDataObject> pdoMap = new HashMap<>(pdos.length, 1);
            for (PluginDataObject pdo : pdos) {
                pdoMap.put(pdo.getDataURI(), pdo);
            }

            if (pdoMap.size() < pdos.length) {
                pdos = pdoMap.values().toArray(
                        new PluginDataObject[pdoMap.size()]);
            }
        }

        return pdos;
    }

    /**
     * This is called by the splitter to get an individual log statement per
     * data_store file. The hdf5 and index is done on a bulk basis and the one
     * to one relationship with data_store to log entry needs to be restored for
     * proper tracking and statistics. The original header object is kept in the
     * extraAttributes of the GridRecord. This is done in addPendingRecords.
     * 
     * @param record
     * @param header
     */
    public void updateLogHeader(GridRecord record,
            @Headers Map<String, Object> header) {
        @SuppressWarnings("unchecked")
        Map<String, Object> recHeader = (Map<String, Object>) record
                .getExtraAttributes().get(HEADERS_ATTRIBUTE);
        String[] fieldsToCopy = new String[] { "dataType", "pluginName",
                "ingestFileName", "dequeueTime", "enqueueTime" };
        for (String field : fieldsToCopy) {
            Object val = recHeader.get(field);
            if (val != null) {
                header.put(field, val);
            }
        }

    }

    private class GribPersistThread extends Thread {
        @Override
        public void run() {
            while (running) {
                try {
                    String file = null;
                    List<GridRecord> recordsToStore = null;

                    synchronized (gridsByFile) {
                        while (running && gridsByFile.isEmpty()) {
                            try {
                                gridsByFile.wait();
                            } catch (InterruptedException e) {
                                // ignore
                            }
                        }

                        if (!gridsByFile.isEmpty()) {
                            Iterator<String> iter = gridsByFile.keySet()
                                    .iterator();
                            while (iter.hasNext()) {
                                file = iter.next();
                                if (!inProcessFiles.contains(file)) {
                                    inProcessFiles.add(file);
                                    recordsToStore = gridsByFile.get(file);
                                    iter.remove();
                                    gridsPending -= recordsToStore.size();
                                    gridsInProcess += recordsToStore.size();
                                    break;
                                }
                            }

                            if (recordsToStore == null) {
                                // all files currently storing on other threads
                                try {
                                    gridsByFile.wait();
                                } catch (InterruptedException e) {
                                    // ignore
                                }

                                continue;
                            }
                        }
                    }

                    if (recordsToStore != null) {
                        long timeToStore = System.currentTimeMillis();
                        try {
                            sendToEndpoint(
                                    "gribPersistIndexAlert",
                                    recordsToStore
                                            .toArray(new PluginDataObject[recordsToStore
                                                    .size()]));
                        } catch (Exception e) {
                            /*
                             * TODO: Compile list of headers that this store
                             * affected.
                             */
                            statusHandler.error(
                                    "Error occurred persisting grids", e);
                        }

                        timeToStore = System.currentTimeMillis() - timeToStore;
                        long bytesFree = 0;
                        for (GridRecord rec : recordsToStore) {
                            bytesFree += ((float[]) rec.getMessageData()).length * 4;
                        }

                        int gridsLeft = 0;
                        int gridsStoringOnOtherThreads = 0;
                        long bytesUsedByGrids = 0;

                        synchronized (gridsByFile) {
                            inProcessFiles.remove(file);
                            bytesInMemory -= bytesFree;
                            bytesUsedByGrids = bytesInMemory;
                            gridsInProcess -= recordsToStore.size();
                            gridsStoringOnOtherThreads = gridsInProcess;
                            gridsLeft = gridsPending;
                            gridsByFile.notifyAll();
                        }

                        if (gridsLeft > 0) {
                            StringBuilder msg = new StringBuilder(80);
                            msg.append(gridsLeft)
                                    .append((gridsLeft == 1 ? " grid "
                                            : " grids "))
                                    .append("pending, ")
                                    .append(gridsStoringOnOtherThreads)
                                    .append((gridsStoringOnOtherThreads == 1 ? " grid "
                                            : " grids "))
                                    .append("in process on other threads, ")
                                    .append(SizeUtil
                                            .prettyByteSize(bytesUsedByGrids))
                                    .append(" in memory.");
                            statusHandler.info(msg.toString());
                        }
                    }
                } catch (Throwable e) {
                    statusHandler.error(
                            "Unhandled error occurred persist grids", e);
                }
            }
        }
    }

    @Override
    public void preStart() {
        // NOOP
    }

    @Override
    public void postStart() {
        // NOOP
    }

    @Override
    public void preStop() {
        running = false;

        synchronized (gridsByFile) {
            gridsByFile.notifyAll();
            if (gridsByFile.size() > 0) {
                statusHandler.info("Waiting for " + gridsByFile.size()
                        + " hdf5 files to be persisted");
            }
        }

        for (GribPersistThread thread : persistThreads) {
            try {
                thread.join();
            } catch (InterruptedException e) {
                // ignore
            }
        }
    }

    @Override
    public void postStop() {
        // NOOP
    }
}
