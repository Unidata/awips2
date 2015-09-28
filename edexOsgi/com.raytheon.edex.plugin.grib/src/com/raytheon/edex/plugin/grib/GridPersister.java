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
import java.util.Collection;
import java.util.HashMap;
import java.util.Iterator;
import java.util.LinkedHashMap;
import java.util.LinkedList;
import java.util.List;
import java.util.Map;

import org.apache.camel.Headers;

import com.raytheon.uf.common.dataplugin.PluginDataObject;
import com.raytheon.uf.common.dataplugin.grid.GridRecord;
import com.raytheon.uf.common.dataplugin.persist.IHDFFilePathProvider;
import com.raytheon.uf.common.status.IUFStatusHandler;
import com.raytheon.uf.common.status.UFStatus;
import com.raytheon.uf.common.time.util.TimeUtil;
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

public class GridPersister implements IContextStateProcessor {
    private static final String HEADERS_ATTRIBUTE = "HEADERS";

    private final IUFStatusHandler statusHandler = UFStatus
            .getHandler(GridPersister.class);

    private final Map<String, LinkedList<GridPersistSet>> gridsByFile = new LinkedHashMap<>();

    private final Map<Thread, String> inProcessFiles = new HashMap<>(4, 1);

    private volatile boolean running = true;

    public IHDFFilePathProvider pathProvider;

    private final long maxBytesInMemory;

    private final long maxBytesPerStore;

    private final int maxGridsPerStore;

    private long bytesInMemory = 0;

    private int gridsPending = 0;

    private int gridsInProcess = 0;

    private final GridPersistThread[] persistThreads;

    public GridPersister(String pluginName, int numThreads, int maxGridsInMb,
            int maxGridsPerStore, int maxBytesInMbPerStore) {
        pathProvider = PluginFactory.getInstance().getPathProvider(pluginName);

        if (numThreads <= 0) {
            statusHandler.warn("Invalid numThreads [" + numThreads
                    + "], using default of [" + 4 + "]");
            numThreads = 4;
        }

        if (maxGridsInMb > 0) {
            maxBytesInMemory = maxGridsInMb * 1024L * 1024L;
        } else {
            // no limit
            maxBytesInMemory = Long.MAX_VALUE;
        }

        if (maxGridsPerStore > 0) {
            this.maxGridsPerStore = maxGridsPerStore;
        } else {
            // no limit
            this.maxGridsPerStore = Integer.MAX_VALUE;
        }

        if (maxBytesInMbPerStore > 0) {
            this.maxBytesPerStore = maxBytesInMbPerStore * 1024L * 1024L;
        } else {
            // no limit
            this.maxBytesPerStore = Long.MAX_VALUE;
        }

        persistThreads = new GridPersistThread[numThreads];
        for (int i = 0; i < persistThreads.length; i++) {
            persistThreads[i] = new GridPersistThread();
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
            StringBuilder pathBuilder = new StringBuilder();
            String[] paths = new String[records.length];
            long bytesForRecords = 0;

            for (int i = 0; i < records.length; i++) {
                pathBuilder.setLength(0);
                GridRecord record = records[i];
                String plugin = record.getPluginName();
                pathBuilder.append(pathProvider.getHDFPath(plugin, record))
                        .append(File.separatorChar)
                        .append(pathProvider.getHDFFileName(plugin, record));
                paths[i] = pathBuilder.toString();

                // set processing time to this point
                Long dequeueTime = (Long) headers.get("dequeueTime");
                if (dequeueTime != null) {
                    long processingTime = System.currentTimeMillis()
                            - dequeueTime;
                    headers.put("processingTime", processingTime);
                }

                /*
                 * since grids will be bulk stored by file, track the original
                 * headers for purposes of logging and stats
                 */
                record.addExtraAttribute(HEADERS_ATTRIBUTE, headers);

                bytesForRecords += ((float[]) record.getMessageData()).length * 4;
            }

            synchronized (gridsByFile) {
                if (!running) {
                    return false;
                }

                for (int i = 0; i < records.length; i++) {
                    String path = paths[i];
                    LinkedList<GridPersistSet> gridsForFile = gridsByFile
                            .get(path);

                    if (gridsForFile == null) {
                        gridsForFile = new LinkedList<>();
                        gridsForFile.add(new GridPersistSet());
                        gridsByFile.put(path, gridsForFile);
                    }

                    if (!gridsForFile.getLast().addGrid(records[i])) {
                        GridPersistSet persistSet = new GridPersistSet();
                        persistSet.addGrid(records[i]);
                        gridsForFile.add(persistSet);
                    }
                }

                // wake up a sleeping persist thread
                gridsByFile.notifyAll();
                gridsPending += records.length;

                // update bytesInMemory
                bytesInMemory += bytesForRecords;

                if (bytesInMemory > maxBytesInMemory) {
                    statusHandler.info("Max Grids in memory for "
                            + getClass().getSimpleName()
                            + " exceeded.  Waiting for grids to process.");

                    while (bytesInMemory > maxBytesInMemory) {
                        try {
                            gridsByFile.wait();
                        } catch (InterruptedException e) {
                            // ignore
                        }
                    }

                    statusHandler
                            .info("Max Grid lock released.  Resuming processing.");
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

    private class GridPersistSet {
        private final List<GridRecord> records = new LinkedList<>();

        private int sizeInBytes;

        public boolean addGrid(GridRecord record) {

            if ((records.size() < maxGridsPerStore)
                    && (sizeInBytes < maxBytesPerStore)) {
                sizeInBytes += ((float[]) record.getMessageData()).length * 4;
                records.add(record);
                return true;
            }

            return false;
        }

        public int getSizeInBytes() {
            return sizeInBytes;
        }

        public List<GridRecord> getRecords() {
            return records;
        }
    }

    private class GridPersistThread extends Thread {
        @Override
        public void run() {
            String logMsg = "Processed %d grid(s) to %s in %s. %d grid(s) pending, %d grid(s) in process on other threads, %s in memory";
            String file = null;
            GridPersistSet persistSet = null;
            long timeToStore = System.currentTimeMillis();
            boolean gridsLeftToProcess = false;

            while (running || gridsLeftToProcess) {
                file = null;
                persistSet = null;

                try {
                    synchronized (gridsByFile) {
                        while (running && gridsByFile.isEmpty()) {
                            try {
                                gridsByFile.wait();
                            } catch (InterruptedException e) {
                                // ignore
                            }
                        }

                        gridsLeftToProcess = !gridsByFile.isEmpty();

                        if (gridsLeftToProcess) {
                            Iterator<String> iter = gridsByFile.keySet()
                                    .iterator();
                            Collection<String> fileBeingStored = inProcessFiles
                                    .values();
                            while (iter.hasNext()) {
                                file = iter.next();
                                if (fileBeingStored.contains(file)) {
                                    file = null;
                                } else {
                                    break;
                                }
                            }

                            if (file == null) {
                                /*
                                 * all hdf5 files currently have a persist in
                                 * progress, grab first/oldest file to jump
                                 * start serialization
                                 */
                                file = gridsByFile.keySet().iterator().next();
                            }

                            inProcessFiles.put(this, file);
                            LinkedList<GridPersistSet> gridsForFile = gridsByFile
                                    .get(file);
                            persistSet = gridsForFile.pop();

                            if (gridsForFile.isEmpty()) {
                                gridsByFile.remove(file);
                            }

                            int numRecords = persistSet.getRecords().size();
                            gridsPending -= numRecords;
                            gridsInProcess += numRecords;
                        }
                    }

                    if (persistSet != null) {
                        timeToStore = System.currentTimeMillis();
                        List<GridRecord> recordsToStore = persistSet
                                .getRecords();
                        /*
                         * update dequeueTime to ignore time spent in persist
                         * queue, which is accounted for in latency
                         */
                        for (GridRecord rec : recordsToStore) {
                            @SuppressWarnings("unchecked")
                            Map<String, Object> recHeader = (Map<String, Object>) rec
                                    .getExtraAttributes()
                                    .get(HEADERS_ATTRIBUTE);
                            Long processingTime = (Long) recHeader
                                    .get("processingTime");
                            recHeader.put("dequeueTime", timeToStore
                                    - processingTime);
                        }

                        try {
                            sendToEndpoint(
                                    "gridPersistIndexAlert",
                                    recordsToStore
                                            .toArray(new PluginDataObject[recordsToStore
                                                    .size()]));
                        } catch (Exception e) {
                            statusHandler.error(
                                    "Error occurred persisting grids", e);
                        }
                    }
                } catch (Throwable e) {
                    statusHandler.error(
                            "Unhandled error occurred persist grids", e);
                } finally {
                    if (persistSet != null) {
                        timeToStore = System.currentTimeMillis() - timeToStore;
                        int numRecords = persistSet.getRecords().size();
                        long bytesFree = persistSet.getSizeInBytes();
                        int gridsLeft = 0;
                        int gridsStoringOnOtherThreads = 0;
                        long bytesUsedByGrids = 0;

                        synchronized (gridsByFile) {
                            inProcessFiles.remove(this);
                            gridsInProcess -= numRecords;
                            gridsStoringOnOtherThreads = gridsInProcess;
                            gridsLeft = gridsPending;

                            long oldBytes = bytesInMemory;
                            bytesInMemory -= bytesFree;
                            bytesUsedByGrids = bytesInMemory;

                            if ((oldBytes > maxBytesInMemory)
                                    && (bytesInMemory < maxBytesInMemory)) {
                                // wake any pending decode threads
                                gridsByFile.notifyAll();
                            }

                            gridsLeftToProcess = !gridsByFile.isEmpty();
                        }

                        statusHandler.info(String.format(logMsg, numRecords,
                                file, TimeUtil.prettyDuration(timeToStore),
                                gridsLeft, gridsStoringOnOtherThreads,
                                SizeUtil.prettyByteSize(bytesUsedByGrids)));
                    }
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
            if (gridsByFile.size() > 0) {
                statusHandler.info("Waiting for " + gridsByFile.size()
                        + " hdf5 files to be persisted");
            }

            gridsByFile.notifyAll();
        }

        for (GridPersistThread thread : persistThreads) {
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
