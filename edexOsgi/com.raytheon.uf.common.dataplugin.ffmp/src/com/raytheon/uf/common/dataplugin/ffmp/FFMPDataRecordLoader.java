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
package com.raytheon.uf.common.dataplugin.ffmp;

import java.io.File;
import java.util.ArrayList;
import java.util.HashMap;
import java.util.List;
import java.util.Map;
import java.util.Map.Entry;

import com.raytheon.uf.common.datastorage.DataStoreFactory;
import com.raytheon.uf.common.datastorage.IDataStore;
import com.raytheon.uf.common.datastorage.Request;
import com.raytheon.uf.common.datastorage.records.FloatDataRecord;
import com.raytheon.uf.common.datastorage.records.IDataRecord;
import com.raytheon.uf.common.status.IUFStatusHandler;
import com.raytheon.uf.common.status.UFStatus;
import com.raytheon.uf.common.status.UFStatus.Priority;

/**
 * Implements a bulk retrieval mechanism for FFMP.
 * 
 * <pre>
 * 
 * SOFTWARE HISTORY
 * 
 * Date         Ticket#    Engineer    Description
 * ------------ ---------- ----------- --------------------------
 * Apr 5, 2013  1912       bsteffen     Initial creation
 * 
 * </pre>
 * 
 * @author bsteffen
 * @version 1.0
 */

public class FFMPDataRecordLoader {

    private static final transient IUFStatusHandler statusHandler = UFStatus
            .getHandler(FFMPDataRecordLoader.class);

    // Anything that needs bulk loading will need to extend this task.
    public static abstract class LoadTask {

        private final File datastoreFile;

        private final String datasetGroupPath;

        public LoadTask(File datastoreFile, String datasetGroupPath) {
            this.datastoreFile = datastoreFile;
            this.datasetGroupPath = datasetGroupPath;
        }

        public abstract void process(FloatDataRecord record);

        public File getDatastoreFile() {
            return datastoreFile;
        }

        public String getDatasetGroupPath() {
            return datasetGroupPath;
        }

    }

    /**
     * Bulk load all records for a set of tasks. Tasks are guaranteed to be
     * executed in order but the data records will be loaded as efficiently as
     * possible.
     * 
     * @param tasks
     */
    public static void loadRecords(List<LoadTask> tasks) {
        // sort all the tasks by file.
        Map<File, List<LoadTask>> fileMap = new HashMap<File, List<LoadTask>>();
        for (LoadTask task : tasks) {
            List<LoadTask> taskList = fileMap.get(task.getDatastoreFile());
            if (taskList == null) {
                taskList = new ArrayList<LoadTask>();
                fileMap.put(task.getDatastoreFile(), taskList);
            }
            taskList.add(task);
        }
        Map<LoadTask, FloatDataRecord> dataMap = new HashMap<LoadTask, FloatDataRecord>();
        // load each file
        for (Entry<File, List<LoadTask>> fileEntry : fileMap.entrySet()) {
            IDataStore dataStore = DataStoreFactory.getDataStore(fileEntry
                    .getKey());
            List<LoadTask> taskList = fileEntry.getValue();
            // assemble all the paths.
            String[] datasetGroupPath = new String[taskList.size()];
            for (int i = 0; i < datasetGroupPath.length; i += 1) {
                datasetGroupPath[i] = taskList.get(i).getDatasetGroupPath();
            }
            // perform the data request.
            IDataRecord[] dataRecords = null;
            try {
                dataRecords = dataStore.retrieveDatasets(datasetGroupPath,
                        Request.ALL);
            } catch (Exception e) {
                // If something went wrong try to retrieve each record
                // individually so only records with errors are skipped.
                dataRecords = new IDataRecord[datasetGroupPath.length];
                for (int i = 0; i < datasetGroupPath.length; i += 1) {
                    try {
                        IDataRecord[] drs = dataStore.retrieveDatasets(
                                new String[] { datasetGroupPath[i] },
                                Request.ALL);
                        dataRecords[i] = drs[0];
                    } catch (Exception e1) {
                        statusHandler.handle(Priority.DEBUG,
                                "FFMPRecord: no data record for: "
                                        + datasetGroupPath, e1);
                    }
                }
            }
            // correlate them in the dataMap.
            for (int i = 0; i < dataRecords.length; i += 1) {
                if (dataRecords[i] != null
                        && dataRecords[i] instanceof FloatDataRecord) {
                    dataMap.put(taskList.get(i),
                            (FloatDataRecord) dataRecords[i]);
                }
            }
        }
        // execute all tasks.
        for (LoadTask task : tasks) {
            FloatDataRecord rec = dataMap.get(task);
            if (rec != null) {
                task.process(rec);
            }
        }
    }

}
