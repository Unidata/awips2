package com.raytheon.uf.viz.archive.data;

import java.io.File;
import java.util.ArrayList;
import java.util.Calendar;
import java.util.List;
import java.util.Map;
import java.util.Set;
import java.util.concurrent.ConcurrentHashMap;
import java.util.concurrent.ConcurrentLinkedQueue;
import java.util.concurrent.atomic.AtomicBoolean;

import org.eclipse.core.runtime.IProgressMonitor;
import org.eclipse.core.runtime.IStatus;
import org.eclipse.core.runtime.Status;
import org.eclipse.core.runtime.jobs.Job;

import com.raytheon.uf.common.archive.config.ArchiveConfigManager;
import com.raytheon.uf.common.archive.config.DisplayData;

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
 * Jul 24, 2012 #2220      rferrel     Change to get all data sizes only one time.
 * 
 * </pre>
 * 
 * @author rferrel
 * @version 1.0
 */
public class SizeJob extends Job {

    /**
     * Mapping of display data by archive and category names.
     */
    private final Map<String, ArchiveInfo> archiveInfoMap = new ConcurrentHashMap<String, ArchiveInfo>();

    /**
     * Current archive name needing sizes.
     */
    private String displayArchive;

    /**
     * Current category name needing sizes.
     */
    private String displayCategory;

    /**
     * Set to true when all sizes are computed for the current display
     * archive/category list.
     */
    private final AtomicBoolean displaySizesComputed = new AtomicBoolean(false);

    /**
     * Request queue for selected data not in current display
     */
    private final ConcurrentLinkedQueue<DisplayData> selectedQueue = new ConcurrentLinkedQueue<DisplayData>();

    /**
     * Request queue for data not in current displayed or selected.
     */
    private final ConcurrentLinkedQueue<DisplayData> backgroundQueue = new ConcurrentLinkedQueue<DisplayData>();

    /**
     * Indicates the job should stop computing the size of the current
     * non-selected entry.
     */
    private final AtomicBoolean stopComputeSize = new AtomicBoolean(false);

    /**
     * What should happen to the processing request that is being stopped.
     */
    private final AtomicBoolean requeueRequest = new AtomicBoolean(false);

    /**
     * Set to true when running job should stop and never rescheduled.
     */
    private final AtomicBoolean shutdown = new AtomicBoolean(false);

    /**
     * The listeners to inform when job is done with a request.
     */
    private final List<IUpdateListener> listeners = new ArrayList<IUpdateListener>();

    /**
     * Current start time.
     */
    Calendar startCal;

    /**
     * Current end time.
     */
    Calendar endCal;

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
     * Start and/or end time has changed so all sizes need to be recomputed.
     * 
     * @param startCal
     * @param endCal
     */
    public void resetTime(Calendar startCal, Calendar endCal) {
        this.startCal = startCal;
        this.endCal = endCal;
        recomputeSize();
    }

    /**
     * Force getting the sizes for all data in the archive Information map.
     */
    public void recomputeSize() {
        clearQueue();
        for (ArchiveInfo archiveInfo : archiveInfoMap.values()) {
            for (String categoryName : archiveInfo.getCategoryNames()) {
                CategoryInfo categoryInfo = archiveInfo.get(categoryName);
                for (DisplayData displayData : categoryInfo
                        .getDisplayDataList()) {
                    displayData.setSize(DisplayData.UNKNOWN_SIZE);
                    if (displayData.isSelected()) {
                        selectedQueue.add(displayData);
                    } else {
                        backgroundQueue.add(displayData);
                    }
                    if (shutdown.get()) {
                        return;
                    }
                }
            }
        }

        // Forces update of current display.
        displaySizesComputed.set(false);

        if (getState() == Job.NONE) {
            schedule();
        }
    }

    /**
     * If request is for unknown size then add request to the appropriate queue.
     * 
     * @param fileInfo
     */
    private void requeue(DisplayData displayData) {
        if (!shutdown.get()) {
            requeueRequest.set(false);
            if (displayData.isSelected()) {
                selectedQueue.add(displayData);
                backgroundQueue.remove(displayData);
            } else {
                selectedQueue.remove(displayData);
                backgroundQueue.add(displayData);
            }

            if (getState() == Job.NONE) {
                schedule();
            }
        }
    }

    /**
     * Add entry to the archive information map.
     * 
     * @param archiveName
     * @param archiveInfo
     */
    public void put(String archiveName, ArchiveInfo archiveInfo) {
        if (archiveInfoMap.isEmpty()) {
            displayArchive = archiveName;
            displayCategory = archiveInfo.getCategoryNames().iterator().next();
        }
        archiveInfoMap.put(archiveName, archiveInfo);
    }

    /**
     * Get an entry from the archive information map.
     * 
     * @param archiveName
     * @return
     */
    public ArchiveInfo get(String archiveName) {
        return archiveInfoMap.get(archiveName);
    }

    /**
     * @return archiveNames
     */
    public Set<String> getArchiveNames() {
        return archiveInfoMap.keySet();
    }

    /**
     * Change the selection state and requeue size request.
     * 
     * @param archiveName
     * @param categoryName
     * @param displayName
     * @param selected
     */
    public void setSelect(String archiveName, String categoryName,
            String displayName, boolean selected) {
        for (DisplayData displayData : archiveInfoMap.get(archiveName)
                .get(categoryName).getDisplayDataList()) {
            if (displayName.equals(displayData.getDisplayLabel())) {
                if (displayData.isSelected() != selected) {
                    displayData.setSelected(selected);
                    if (displayData.getSize() == DisplayData.UNKNOWN_SIZE) {
                        requeue(displayData);
                    }
                }
                break;
            }
        }
    }

    /**
     * Change the archive/category display.
     * 
     * @param archiveName
     * @param categoryName
     */
    public void changeDisplayQueue(String archiveName, String categoryName) {
        if (!archiveName.equals(displayArchive)
                || !categoryName.equals(displayCategory)) {
            synchronized (this) {
                if (getState() != Job.NONE) {
                    requeueRequest.set(true);
                    stopComputeSize.set(true);
                }
                displaySizesComputed.set(false);
                displayArchive = archiveName;
                displayCategory = categoryName;
            }
        }
    }

    /**
     * Clear request queues and stop current request.
     */
    public void clearQueue() {
        if (getState() != Job.NONE) {
            displaySizesComputed.set(false);
            selectedQueue.clear();
            backgroundQueue.clear();
            requeueRequest.set(false);
            stopComputeSize.set(true);
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

        mainLoop: while (!shutdown.get()) {
            DisplayData displayData = null;
            if (!displaySizesComputed.get()) {
                synchronized (this) {
                    // Get sizes for the current display.
                    List<DisplayData> displayDatas = archiveInfoMap
                            .get(displayArchive).get(displayCategory)
                            .getDisplayDataList();
                    for (DisplayData dd : displayDatas) {
                        if (dd.getSize() == DisplayData.UNKNOWN_SIZE) {
                            displayData = dd;
                            break;
                        }
                    }
                    displaySizesComputed.set(displayData == null);
                }
            }

            if (displayData == null) {
                if (!selectedQueue.isEmpty()) {
                    displayData = selectedQueue.remove();
                } else if (!backgroundQueue.isEmpty()) {
                    displayData = backgroundQueue.remove();
                }

                if (displayData == null) {
                    break mainLoop;
                } else if (displayData.getSize() >= 0) {
                    continue mainLoop;
                }
            }

            stopComputeSize.set(false);

            List<File> files = manager.getDisplayFiles(displayData, startCal,
                    endCal);

            // Size no longer needed.
            if (stopComputeSize.get()) {
                if (requeueRequest.get()) {
                    requeue(displayData);
                }
                continue mainLoop;
            }

            long size = 0L;
            for (File file : files) {
                if (file.isDirectory()) {
                    size += sizeOfDirectory(file);
                } else {
                    size += file.length();
                }

                // Skip when size no longer needed.
                if (stopComputeSize.get()) {
                    if (requeueRequest.get()) {
                        requeue(displayData);
                    }
                    continue mainLoop;
                }
            }

            displayData.setSize(size);

            List<DisplayData> list = new ArrayList<DisplayData>(1);
            list.add(displayData);
            for (IUpdateListener listener : listeners) {
                listener.update(list);
            }
        }

        return Status.OK_STATUS;
    }

    /**
     * Determine the total size of a directory; unless stop flag is set then
     * result is unknown.
     * 
     * @param directory
     * @return size
     */
    private long sizeOfDirectory(File directory) {
        long size = 0;
        for (File file : directory.listFiles()) {
            if (stopComputeSize.get()) {
                break;
            }
            if (file.isDirectory()) {
                size += sizeOfDirectory(file);
            } else {
                size += file.length();
            }
        }
        return size;
    }

    /*
     * (non-Javadoc)
     * 
     * @see org.eclipse.core.runtime.jobs.Job#canceling()
     */
    @Override
    protected void canceling() {
        selectedQueue.clear();
        backgroundQueue.clear();
        shutdown.set(true);
        stopComputeSize.set(true);
    }
}