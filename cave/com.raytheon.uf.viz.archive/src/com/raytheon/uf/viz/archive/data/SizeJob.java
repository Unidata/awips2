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
import com.raytheon.uf.common.archive.config.DisplayData;
import com.raytheon.uf.common.util.FileUtil;

/**
 * Job to determine the size for a directory and its contents on a non-UI
 * thread.
 * 
 * <pre>
 * 
 * SOFTWARE HISTORY
 * 
 * Date         Ticket#    Engineer    Description
 * ------------ ---------- ----------- --------------------------
 * Jun 13, 2013            rferrel     Initial creation
 * 
 * </pre>
 * 
 * @author rferrel
 * @version 1.0
 */
public class SizeJob extends Job {

    /** The queue for requested sizes. */
    private final ConcurrentLinkedQueue<SizeJobRequest> queue = new ConcurrentLinkedQueue<SizeJobRequest>();

    /**
     * Pending selected entries that still need to have the sizes determined.
     */
    private final ConcurrentLinkedQueue<DisplayData> selectedQueue = new ConcurrentLinkedQueue<DisplayData>();

    /**
     * Indicates the job should stop computing the size of the current
     * non-selected entry.
     */
    private final AtomicBoolean stopComputeSize = new AtomicBoolean(false);

    /**
     * The listeners to inform when job is done with an entry.
     */
    private final List<IUpdateListener> listeners = new ArrayList<IUpdateListener>();

    /**
     * Constructor.
     */
    public SizeJob() {
        super("Size Job");
        setSystem(true);
    }

    /**
     * Add a Listener to inform when job has completed information on an entry.
     * 
     * @param listener
     */
    public void addUpdateListener(IUpdateListener listener) {
        listeners.add(listener);
    }

    /**
     * Remove a listener.
     * 
     * @param listener
     */
    public void removeUpdateListener(IUpdateListener listener) {
        listeners.remove(listener);
    }

    /**
     * Add entry to queue and if pending selected entries add them to the queue
     * with same start/end times.
     * 
     * @param fileInfo
     */
    public void queue(SizeJobRequest fileInfo) {
        queue.add(fileInfo);

        if (getState() == Job.NONE) {
            schedule();
        }
    }

    /**
     * Clear queue but save selected entries still needing sizes.
     */
    public void clearQueue() {
        List<SizeJobRequest> pending = new ArrayList<SizeJobRequest>();

        // Drain queue queue.removeAll() doesn't work.
        while (!queue.isEmpty()) {
            pending.add(queue.remove());
        }

        if (getState() != NONE) {
            cancel();
        }

        // Save selected items that do not have sizes.
        for (SizeJobRequest dirInfo : pending) {
            DisplayData displayData = dirInfo.getDisplayData();

            if (displayData.isSelected() && displayData.getSize() < 0L) {
                if (!selectedQueue.contains(displayData)) {
                    selectedQueue.add(displayData);
                }
            }
        }
    }

    /**
     * Requeue pending entries.
     * 
     * @param startCal
     * @param endCal
     */
    public void requeueSelected(Calendar startCal, Calendar endCal) {
        if (!selectedQueue.isEmpty()) {
            DisplayData displayData = selectedQueue.remove();
            queue(new SizeJobRequest(displayData, startCal, endCal));
        }
    }

    /*
     * (non-Javadoc)
     * 
     * @see org.eclipse.core.runtime.jobs.Job#run(org.eclipse.core.runtime.
     * IProgressMonitor)
     */
    @Override
    protected IStatus run(IProgressMonitor monitor) {
        if (monitor.isCanceled()) {
            return Status.OK_STATUS;
        }

        ArchiveConfigManager manager = ArchiveConfigManager.getInstance();

        mainLoop: while (!queue.isEmpty()) {
            SizeJobRequest dirInfo = queue.remove();

            stopComputeSize.set(false);
            DisplayData displayData = dirInfo.displayData;
            Calendar startCal = dirInfo.startCal;
            Calendar endCal = dirInfo.endCal;

            List<File> files = manager.getDisplayFiles(displayData, startCal,
                    endCal);

            // Size no longer needed.
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

            List<SizeJobRequest> list = new ArrayList<SizeJobRequest>();
            list.add(dirInfo);
            for (IUpdateListener listener : listeners) {
                listener.update(list);
            }
        }

        return Status.OK_STATUS;
    }

    /*
     * (non-Javadoc)
     * 
     * @see org.eclipse.core.runtime.jobs.Job#canceling()
     */
    @Override
    protected void canceling() {
        queue.clear();
        stopComputeSize.set(true);
    }
}