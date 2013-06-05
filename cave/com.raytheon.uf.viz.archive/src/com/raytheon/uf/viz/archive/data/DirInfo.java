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
package com.raytheon.uf.viz.archive.data;

import java.io.File;
import java.util.ArrayList;
import java.util.Calendar;
import java.util.List;
import java.util.concurrent.ConcurrentLinkedQueue;
import java.util.concurrent.atomic.AtomicBoolean;

import org.eclipse.core.runtime.IProgressMonitor;
import org.eclipse.core.runtime.IStatus;
import org.eclipse.core.runtime.Status;
import org.eclipse.core.runtime.jobs.Job;

import com.raytheon.uf.common.archive.config.ArchiveConfigManager;
import com.raytheon.uf.common.archive.config.ArchiveConfigManager.DisplayData;
import com.raytheon.uf.common.util.FileUtil;

/**
 * This class uses a obtains information on a File in a Job in order to remove
 * from the UI thread.
 * 
 * <pre>
 * 
 * SOFTWARE HISTORY
 * 
 * Date         Ticket#    Engineer    Description
 * ------------ ---------- ----------- --------------------------
 * May 15, 2013 1966       rferrel     Initial creation
 * 
 * </pre>
 * 
 * @author rferrel
 * @version 1.0
 */

public class DirInfo {

    private static final SizeJob sizeJob = new DirInfo.SizeJob();

    final private DisplayData displayInfo;

    private final List<File> files = new ArrayList<File>();

    private final Calendar startCal;

    private final Calendar endCal;

    public static void clearQueue() {
        sizeJob.clearQueue();
    }

    public static void addUpdateListener(IUpdateListener listener) {
        sizeJob.listeners.add(listener);
    }

    public static void removeUpdateListener(IUpdateListener listener) {
        sizeJob.listeners.remove(listener);
    }

    public DirInfo(DisplayData displayInfo, Calendar startCal, Calendar endCal) {
        this.displayInfo = displayInfo;
        this.startCal = startCal;
        this.endCal = endCal;
        DirInfo.sizeJob.queue(this);
    }

    public DisplayData getDisplayInfo() {
        return displayInfo;
    }

    /**
     * Job to determine the size for a directory and its contents on a non-UI
     * thread.
     */
    static private class SizeJob extends Job {
        private ConcurrentLinkedQueue<DirInfo> queue = new ConcurrentLinkedQueue<DirInfo>();

        private final ConcurrentLinkedQueue<DisplayData> selectedQueue = new ConcurrentLinkedQueue<ArchiveConfigManager.DisplayData>();

        private final AtomicBoolean stopComputeSize = new AtomicBoolean(false);

        List<IUpdateListener> listeners = new ArrayList<IUpdateListener>();

        /**
         * Add entry to queue and if pending selected entries add them to the
         * queue with same start/end times.
         * 
         * @param fileInfo
         */
        protected void queue(DirInfo fileInfo) {
            queue.add(fileInfo);

            if (getState() == Job.NONE) {
                schedule();
            }
        }

        /**
         * Clear list off pending data but save selected entries still needing
         * sizes.
         */
        protected void clearQueue() {
            List<DirInfo> pending = new ArrayList<DirInfo>();

            // Drain queue queue.removeAll() doesn't work.
            while (!queue.isEmpty()) {
                pending.add(queue.remove());
            }

            if (getState() != NONE) {
                cancel();
            }

            // Save selected items that do not have sizes.
            for (DirInfo dirInfo : pending) {
                DisplayData displayData = dirInfo.getDisplayInfo();

                if (displayData.isSelected() && displayData.getSize() < 0L) {
                    if (!selectedQueue.contains(displayData)) {
                        selectedQueue.add(displayData);
                    }
                }
            }
        }

        public SizeJob() {
            super("Size Job");
            setSystem(true);
        }

        @Override
        protected IStatus run(IProgressMonitor monitor) {
            if (monitor.isCanceled()) {
                return Status.OK_STATUS;
            }

            ArchiveConfigManager manager = ArchiveConfigManager.getInstance();

            mainLoop: while (!queue.isEmpty()) {
                DirInfo dirInfo = queue.remove();

                stopComputeSize.set(false);
                DisplayData displayData = dirInfo.displayInfo;
                Calendar startCal = dirInfo.startCal;
                Calendar endCal = dirInfo.endCal;

                List<File> files = manager.getDisplayFiles(displayData,
                        startCal, endCal);

                // Is size still needed.
                if (!displayData.isSelected() && stopComputeSize.get()) {
                    continue;
                }

                dirInfo.files.clear();
                dirInfo.files.addAll(files);
                long size = 0L;
                for (File file : dirInfo.files) {

                    // Skip when size no longer needed.
                    if (!displayData.isSelected() && stopComputeSize.get()) {
                        continue mainLoop;
                    }

                    if (file.isDirectory()) {
                        size += FileUtil.sizeOfDirectory(file);
                    } else {
                        size += file.length();
                    }
                }

                displayData.setSize(size);

                List<DirInfo> list = new ArrayList<DirInfo>();
                list.add(dirInfo);
                for (IUpdateListener listener : listeners) {
                    listener.update(list);
                }

                if (!stopComputeSize.get()) {
                    // Place any pending selections at end of the queue.
                    while (!selectedQueue.isEmpty()) {
                        DisplayData data = selectedQueue.remove();
                        new DirInfo(data, startCal, endCal);
                    }
                }
            }

            return Status.OK_STATUS;
        }

        @Override
        protected void canceling() {
            stopComputeSize.set(true);
        }
    }
}
