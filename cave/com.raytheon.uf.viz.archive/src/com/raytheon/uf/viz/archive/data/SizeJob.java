package com.raytheon.uf.viz.archive.data;

import java.io.File;
import java.util.ArrayList;
import java.util.Calendar;
import java.util.Comparator;
import java.util.HashSet;
import java.util.Iterator;
import java.util.LinkedList;
import java.util.List;
import java.util.Map;
import java.util.PriorityQueue;
import java.util.Set;
import java.util.Timer;
import java.util.TimerTask;
import java.util.concurrent.ConcurrentHashMap;
import java.util.concurrent.LinkedBlockingQueue;
import java.util.concurrent.PriorityBlockingQueue;
import java.util.concurrent.atomic.AtomicBoolean;

import org.eclipse.core.runtime.IProgressMonitor;
import org.eclipse.core.runtime.IStatus;
import org.eclipse.core.runtime.Status;
import org.eclipse.core.runtime.jobs.Job;

import com.raytheon.uf.common.archive.config.ArchiveConfigManager;
import com.raytheon.uf.common.archive.config.ArchiveConstants;
import com.raytheon.uf.common.archive.config.ArchiveConstants.Type;
import com.raytheon.uf.common.archive.config.DisplayData;
import com.raytheon.uf.common.archive.config.SelectConfig;
import com.raytheon.uf.common.archive.config.select.ArchiveSelect;
import com.raytheon.uf.common.archive.config.select.CategorySelect;
import com.raytheon.uf.common.status.IUFStatusHandler;
import com.raytheon.uf.common.status.UFStatus;
import com.raytheon.uf.common.status.UFStatus.Priority;
import com.raytheon.uf.common.time.util.TimeUtil;

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
 * Jul 24, 2013 #2220      rferrel     Change to get all data sizes only one time.
 * Aug 02, 2013 #2224      rferrel     Changes for new configuration files.
 * Aug 06, 2013 #2222      rferrel     Changes to display all selected data.
 * Dec 11, 2013 #2603      rferrel     Selected list changed to a Set.
 * Dec 11, 2013 #2624      rferrel     Clear display variables when recomputing sizes.
 * 
 * </pre>
 * 
 * @author rferrel
 * @version 1.0
 */
public class SizeJob extends Job {
    private final IUFStatusHandler statusHandler = UFStatus
            .getHandler(SizeJob.class);

    /**
     * The private variable from PriorityQueue.
     */
    private final int DEFAULT_INITIAL_CAPACITY = 11;

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
     * Queue of display data needing sizes.
     */
    private final PriorityBlockingQueue<DisplayData> sizeQueue = new PriorityBlockingQueue<DisplayData>(
            DEFAULT_INITIAL_CAPACITY, DisplayData.PRIORITY_ORDER);

    /**
     * Data to send to listeners.
     */
    private final LinkedBlockingQueue<DisplayData> displayQueue = new LinkedBlockingQueue<DisplayData>();

    /**
     * Current list of visible data.
     */
    private List<DisplayData> visibleList = new ArrayList<DisplayData>(0);

    /**
     * Set to true when running job should stop and never be rescheduled.
     */
    private final AtomicBoolean shutdown = new AtomicBoolean(false);

    /**
     * The listeners to inform when job is done with a request.
     */
    private final List<IUpdateListener> listeners = new ArrayList<IUpdateListener>();

    /**
     * The display data whose size is being computed.
     */
    private DisplayData currentDisplayData;

    /**
     * Method to call when loading a new selection for retention/case creation
     * to update times.
     */
    private IRetentionHour iRetentionHour;

    /**
     * Current start time.
     */
    private Calendar startCal;

    /**
     * Current end time.
     */
    private Calendar endCal;

    /**
     * Timer to periodically update the GUI display computed sizes.
     */
    private Timer displayTimer;

    /**
     * Flag to shutdown the display timer.
     */
    private final AtomicBoolean shutdownDisplayTimer = new AtomicBoolean(false);

    /**
     * Frequency for performing peek while computing sizes.
     */
    private final int peekFrequency = 50;

    /**
     * Counter to reduce the number of times a peek is performed while computing
     * sizes.
     */
    private int peekCnt;

    /**
     * Flag to stop computing sizes; only accessed by a single thread.
     */
    private boolean stopComputeSize;

    /**
     * Priority queue for getting display data for an archive/category.
     */
    // Do not use a PriorityBlockingQueue since the load select and change
    // display methods need to be notified when the display data is available.
    private final PriorityQueue<MissingData> missingDataQueue = new PriorityQueue<SizeJob.MissingData>(
            DEFAULT_INITIAL_CAPACITY, new Comparator<MissingData>() {

                @Override
                public int compare(MissingData o1, MissingData o2) {
                    if (o1.visiable != o2.visiable) {
                        return o1.visiable ? -1 : +1;
                    }
                    if (o1.isSelected() != o2.isSelected()) {
                        return o1.isSelected() ? -1 : +1;
                    }

                    int result = o1.archive.compareToIgnoreCase(o2.archive);

                    if (result == 0) {
                        result = o1.category.compareToIgnoreCase(o2.category);
                    }
                    return result;
                }
            });

    /**
     * Job for processing the missing data queue.
     */
    private final MissingDataJob missingDataJob = new MissingDataJob();

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
        displayArchive = null;
        displayCategory = null;
        for (ArchiveInfo archiveInfo : archiveInfoMap.values()) {
            for (String categoryName : archiveInfo.getCategoryNames()) {
                CategoryInfo categoryInfo = archiveInfo.get(categoryName);
                for (DisplayData displayData : categoryInfo
                        .getDisplayDataList()) {
                    displayData.setSize(DisplayData.UNKNOWN_SIZE);
                    sizeQueue.add(displayData);

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
     * Add entry to the archive information map.
     * 
     * @param archiveName
     * @param archiveInfo
     */
    public void put(String archiveName, ArchiveInfo archiveInfo) {
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
     * Check all displayData selection state so only the data in selections are
     * set.
     * 
     * @param selections
     */
    public void loadSelect(String selectName, ArchiveConstants.Type type) {
        ArchiveConfigManager manager = ArchiveConfigManager.getInstance();
        String fileName = ArchiveConstants.selectFileName(type, selectName);
        SelectConfig selections = manager.loadSelection(fileName);
        if (selections == null) {
            selections = new SelectConfig();
            selections.setName(ArchiveConstants.defaultSelectName);
        }
        iRetentionHour.setRetentionTimes(selections.getStarRetentionHours());

        for (String archiveName : getArchiveNames()) {
            ArchiveInfo archiveInfo = get(archiveName);
            for (String categoryName : archiveInfo.getCategoryNames()) {
                Set<String> selectionsSet = selections.getSelectedSet(
                        archiveName, categoryName);
                MissingData missingData = removeMissingData(archiveName,
                        categoryName);
                if (missingData != null) {
                    missingData.setSelectedSet(selectionsSet);
                    addMissingData(missingData);
                } else {
                    CategoryInfo categoryInfo = archiveInfo.get(categoryName);
                    for (DisplayData displayData : categoryInfo
                            .getDisplayDataList()) {
                        String displayLabel = displayData.getDisplayLabel();
                        boolean selected = selectionsSet
                                .contains(displayLabel);
                        if (selected != displayData.isSelected()) {
                            setSelect(displayData, selected);
                        }
                    }
                }
            }
        }
    }

    /**
     * Get list of all selected data.
     * 
     * @return selected
     */
    public List<DisplayData> getSelectAll() {
        synchronized (missingDataQueue) {
            while (!missingDataQueue.isEmpty()) {
                if (missingDataJob.currentMissingData == null
                        || missingDataJob.currentMissingData.isSelected()) {
                    missingDataQueueWait();
                } else {
                    break;
                }
            }
        }
        List<DisplayData> selected = new LinkedList<DisplayData>();
        for (ArchiveInfo archiveInfo : archiveInfoMap.values()) {
            for (String categoryName : archiveInfo.getCategoryNames()) {
                CategoryInfo categoryInfo = archiveInfo.get(categoryName);
                for (DisplayData displayData : categoryInfo
                        .getDisplayDataList()) {
                    if (displayData.isSelected()) {
                        selected.add(displayData);
                    }
                }
            }
        }
        return selected;
    }

    /**
     * Save selections to the desired file.
     * 
     * @param selectName
     * @param type
     * @param startRetentionMS
     * @return
     */
    public String saveSelect(String selectName, ArchiveConstants.Type type,
            long startRetentionMS) {
        String errorMsg = null;
        SelectConfig selections = new SelectConfig();
        selections.setName(selectName);
        if (type == Type.Case) {
            long startRetentionHours = startRetentionMS
                    / TimeUtil.MILLIS_PER_HOUR;
            selections.setStarRetentionHours(startRetentionHours);
        }
        for (String archiveName : getArchiveNames()) {
            ArchiveInfo archiveInfo = get(archiveName);
            ArchiveSelect archiveSelect = new ArchiveSelect();
            archiveSelect.setName(archiveName);
            for (String categoryName : archiveInfo.getCategoryNames()) {
                CategoryInfo categoryInfo = archiveInfo.get(categoryName);
                CategorySelect categorySelect = new CategorySelect();
                categorySelect.setName(categoryName);
                for (DisplayData displayData : categoryInfo
                        .getDisplayDataList()) {
                    if (displayData.isSelected()) {
                        categorySelect.add(displayData.getDisplayLabel());
                    }
                }

                if (!categorySelect.isEmpty()) {
                    archiveSelect.add(categorySelect);
                }
            }
            if (!archiveSelect.isEmpty()) {
                selections.add(archiveSelect);
            }
        }
        String fileName = ArchiveConstants.selectFileName(type, selectName);

        try {
            ArchiveConfigManager.getInstance().saveSelections(selections,
                    fileName);
        } catch (Exception e) {
            errorMsg = "Unable to save file: " + fileName;
            statusHandler.handle(Priority.ERROR, errorMsg, e);
        }
        return errorMsg;
    }

    /**
     * Change the selection state and reset priority.
     * 
     * @param displayData
     * @param state
     */
    public void setSelect(DisplayData displayData, boolean state) {
        if (displayData.isSelected() != state) {
            if (sizeQueue.remove(displayData)) {
                displayData.setSelected(state);
                sizeQueue.add(displayData);
            } else {
                displayData.setSelected(state);
            }
        }
    }

    /**
     * Change visibility state and reset priority.
     * 
     * @param displayData
     * @param state
     */
    private void setVisible(DisplayData displayData, boolean state) {
        if (displayData.isVisible() != state) {
            if (sizeQueue.remove(displayData)) {
                displayData.setVisible(state);
                sizeQueue.add(displayData);
            } else {
                displayData.setVisible(state);
            }
        }
    }

    /**
     * Change the archive/category display.
     * 
     * @param archiveName
     * @param categoryName
     * @return displayData when display needs to change otherwise null
     */
    public List<DisplayData> changeDisplay(String archiveName,
            String categoryName) {
        List<DisplayData> displayDatas = null;
        if (!archiveName.equals(displayArchive)
                || !categoryName.equals(displayCategory)) {
            MissingData missingData = removeMissingData(archiveName,
                    categoryName);
            if (missingData != null) {
                missingData.setVisiable(true);
                synchronized (missingDataQueue) {
                    addMissingData(missingData);
                    while (missingDataQueue.contains(missingData)) {
                        missingDataQueueWait();
                    }
                }
            }
            displayDatas = archiveInfoMap.get(archiveName).get(categoryName)
                    .getDisplayDataList();
            displayArchive = archiveName;
            displayCategory = categoryName;
            changeDisplay(displayDatas);
        }
        return displayDatas;
    }

    /**
     * Change to display all selected data..
     * 
     * @return displayhData when display needs to change otherwise null.
     */
    public List<DisplayData> changeDisplayAll() {
        List<DisplayData> selectedData = null;
        if (displayArchive != null) {
            displayArchive = null;
            displayCategory = null;
            selectedData = getSelectAll();
            changeDisplay(selectedData);
        }
        return selectedData;
    }

    public String initData(ArchiveConstants.Type type, String selectName,
            String displayArchive, String displayCategory,
            IRetentionHour iRetentionHour) {
        this.iRetentionHour = iRetentionHour;
        ArchiveConfigManager manager = ArchiveConfigManager.getInstance();
        String fileName = ArchiveConstants.selectFileName(type, selectName);
        SelectConfig selections = manager.loadSelection(fileName);
        if (selections == null) {
            selections = new SelectConfig();
            selections.setName(ArchiveConstants.defaultSelectName);
        }
        iRetentionHour.setRetentionTimes(selections.getStarRetentionHours());

        missingDataQueue.clear();

        visibleList = manager.getDisplayData(displayArchive, displayCategory,
                false);
        Set<String> selectedSet = selections.getSelectedSet(displayArchive,
                displayCategory);
        for (DisplayData displayData : visibleList) {
            displayData.setSelected(selectedSet.contains(displayData
                    .getDisplayLabel()));
        }

        for (String archiveName : manager.getArchiveDataNamesList()) {
            ArchiveInfo archiveInfo = new ArchiveInfo();
            String[] categoryNames = manager.getCategoryNames(manager
                    .getArchive(archiveName));
            for (String categoryName : categoryNames) {
                CategoryInfo categoryInfo = new CategoryInfo(archiveName,
                        categoryName, null);
                archiveInfo.add(categoryInfo);
                if (archiveName.equals(displayArchive)
                        && categoryName.equals(displayCategory)) {
                    categoryInfo.setDisplayDataList(visibleList);
                    if (!visibleList.isEmpty()) {
                        schedule();
                    }
                } else {
                    selectedSet = selections.getSelectedSet(archiveName,
                            categoryName);
                    MissingData missingData = new MissingData(archiveName,
                            categoryName, selectedSet);
                    missingDataQueue.add(missingData);
                }
            }
            put(archiveName, archiveInfo);
        }

        missingDataJob.schedule();

        return selections.getName();
    }

    /**
     * Find and remove the missing data from the missing data queue.
     * 
     * @param archiveName
     * @param categoryName
     * @return missingData or null if not on the missing data queue
     */
    private MissingData removeMissingData(String archiveName,
            String categoryName) {
        MissingData missingData = null;
        synchronized (missingDataQueue) {
            if (missingDataJob.currentMissingData != null
                    && archiveName
                            .equals(missingDataJob.currentMissingData.archive)
                    && categoryName
                            .equals(missingDataJob.currentMissingData.category)) {
                // Finish the process of getting the data.
                missingDataQueueWait();
            } else if (!missingDataQueue.isEmpty()) {
                Iterator<MissingData> iterator = missingDataQueue.iterator();
                while (iterator.hasNext()) {
                    MissingData md = iterator.next();
                    if (md.archive.equals(archiveName)
                            && md.category.equals(categoryName)) {
                        iterator.remove();
                        missingData = md;
                        break;
                    }
                }
            }
        }
        return missingData;
    }

    /**
     * Wait for notification that current missing data is finished processing.
     * 
     * @return false when interrupted exception
     */
    private boolean missingDataQueueWait() {
        boolean state = true;
        try {
            missingDataQueue.wait();
        } catch (InterruptedException e) {
            state = false;
            statusHandler.handle(Priority.INFO, e.getLocalizedMessage(), e);
        }
        return state;
    }

    /**
     * 
     * @param missingData
     */
    private void addMissingData(MissingData missingData) {
        synchronized (missingDataQueue) {
            missingDataQueue.add(missingData);
            if (missingDataJob.getState() == Job.NONE) {
                missingDataJob.schedule();
            }
        }
    }

    /**
     * Change update visible to the new list.
     * 
     * @param newDisplays
     */
    private void changeDisplay(List<DisplayData> newDisplays) {
        List<DisplayData> oldDisplays = visibleList;
        visibleList = newDisplays;
        List<DisplayData> visibleList = new ArrayList<DisplayData>(newDisplays);

        for (DisplayData displayData : oldDisplays) {
            if (!visibleList.remove(displayData)) {
                setVisible(displayData, false);
            }
        }

        for (DisplayData displayData : visibleList) {
            setVisible(displayData, true);
        }
    }

    /**
     * Clear request queues and stop current request.
     */
    public void clearQueue() {
        sizeQueue.clear();
        displayQueue.clear();
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

        updateDisplayTimer();

        mainLoop: while (!shutdown.get()) {

            currentDisplayData = sizeQueue.peek();
            if (currentDisplayData == null) {
                break mainLoop;
            }

            List<File> files = manager.getDisplayFiles(currentDisplayData,
                    startCal, endCal);

            // Size no longer needed.
            if (currentDisplayData != sizeQueue.peek()) {
                continue mainLoop;
            }

            long size = 0L;
            peekCnt = peekFrequency;
            stopComputeSize = false;

            for (File file : files) {
                if (file.isDirectory()) {
                    size += sizeOfDirectory(file);
                } else {
                    size += file.length();
                }

                // Skip when size no longer needed.
                if (stopComputeSize) {
                    continue mainLoop;
                }
            }

            sizeQueue.remove(currentDisplayData);
            currentDisplayData.setSize(size);
            displayQueue.add(currentDisplayData);
        }

        shutdownDisplayTimer.set(true);
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
            if (stopComputeSize) {
                break;
            }
            if (--peekCnt == 0) {
                if (currentDisplayData != sizeQueue.peek()) {
                    // Forces break out of recursion.
                    stopComputeSize = true;
                    break;
                }
                peekCnt = peekFrequency;
            }

            if (file.isDirectory()) {
                size += sizeOfDirectory(file);
            } else {
                size += file.length();
            }
        }
        return size;
    }

    /**
     * Start timer to update GUI's display form data on the display Queue.
     */
    private void updateDisplayTimer() {
        if (displayTimer != null) {
            displayTimer.cancel();
        }

        shutdownDisplayTimer.set(false);

        displayTimer = new Timer();

        TimerTask updateDisplayTask = new TimerTask() {
            @Override
            public void run() {
                if (!displayQueue.isEmpty()) {
                    List<DisplayData> list = new ArrayList<DisplayData>(
                            displayQueue.size());
                    displayQueue.drainTo(list);

                    for (IUpdateListener listener : listeners) {
                        listener.update(list);
                    }
                } else if (shutdownDisplayTimer.get()) {
                    displayTimer.cancel();
                    displayTimer = null;
                }
            }
        };

        displayTimer.schedule(updateDisplayTask, 1000, 2000);
    }

    /*
     * (non-Javadoc)
     * 
     * @see org.eclipse.core.runtime.jobs.Job#canceling()
     */
    @Override
    protected void canceling() {
        clearQueue();
        missingDataQueue.clear();
        missingDataJob.cancel();
        shutdown.set(true);
    }

    /**
     * Class used by the missing data job to obtain display data for given
     * archive/category off the UI thread.
     */
    private static class MissingData {
        protected final String archive;

        protected final String category;

        protected final Set<String> selectedSet;

        protected boolean visiable = false;

        public MissingData(String archive, String category,
                Set<String> selectedSet) {
            this.archive = archive;
            this.category = category;
            this.selectedSet = new HashSet<String>(selectedSet);
        }

        public boolean isSelected() {
            return !selectedSet.isEmpty();
        }

        public void setVisiable(boolean state) {
            this.visiable = state;
        }

        public void setSelectedSet(Set<String> selectedSet) {
            this.selectedSet.clear();
            this.selectedSet.addAll(selectedSet);
        }

        @Override
        public String toString() {
            StringBuilder sb = new StringBuilder("MissingData[");
            sb.append("archive: ").append(archive);
            sb.append(", category: ").append(category);
            sb.append(", visible: ").append(visiable);
            sb.append(", isSelected: ").append(isSelected());
            sb.append("]");
            return sb.toString();
        }
    }

    /**
     * This handles getting the display data in the missing data queue and
     * queuing the results for the size job.
     */
    private class MissingDataJob extends Job {

        private final AtomicBoolean shutdown = new AtomicBoolean(false);

        protected MissingData currentMissingData = null;

        public MissingDataJob() {
            super("MissingData");
        }

        @Override
        protected IStatus run(IProgressMonitor monitor) {
            if (monitor.isCanceled()) {
                return Status.OK_STATUS;
            }

            ArchiveConfigManager manager = ArchiveConfigManager.getInstance();

            while (!shutdown.get()) {
                synchronized (missingDataQueue) {
                    if (currentMissingData != null) {
                        missingDataQueue.notifyAll();
                    }
                    currentMissingData = missingDataQueue.poll();
                }

                if (currentMissingData == null) {
                    break;
                }

                String archiveName = currentMissingData.archive;
                String categoryName = currentMissingData.category;
                Set<String> selectedSet = currentMissingData.selectedSet;
                List<DisplayData> displayDatas = manager.getDisplayData(
                        archiveName, categoryName, false);
                if (shutdown.get()) {
                    break;
                }

                for (DisplayData displayData : displayDatas) {
                    displayData.setSelected(selectedSet.contains(displayData
                            .getDisplayLabel()));
                    sizeQueue.add(displayData);
                }

                archiveInfoMap.get(archiveName).get(categoryName)
                        .setDisplayDataList(displayDatas);

                if (SizeJob.this.getState() == Job.NONE) {
                    SizeJob.this.schedule();
                }
            }

            return Status.OK_STATUS;
        }

        @Override
        protected void canceling() {
            shutdown.set(true);
        }
    }

}