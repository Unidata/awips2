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
 * Mar 27, 2014 #2879      rferrel     Loading Case no longer changes Start/End times.
 * Apr 23, 2014 #3045      rferrel     Changes to prevent race condition while getting labels.
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
     * Priority queue for getting display data all archive/category tables.
     */
    // Do not use a PriorityBlockingQueue since the load select and change
    // display methods need to be notified when the display data is available.
    private final PriorityQueue<LoadDisplayData> loadDisplayDataQueue = new PriorityQueue<SizeJob.LoadDisplayData>(
            DEFAULT_INITIAL_CAPACITY, new Comparator<LoadDisplayData>() {

                @Override
                public int compare(LoadDisplayData o1, LoadDisplayData o2) {
                    if (o1.visible != o2.visible) {
                        return o1.visible ? -1 : +1;
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
     * Job for obtaining display data for all the archive/category tables.
     */
    private LoadDisplayDataJob loadDisplayDataJob;

    /**
     * Listener to invoke when all display data loaded.
     */
    private final ILoadDisplayDataListener loadDisplayDataListener;

    /**
     * Constructor.
     */
    public SizeJob(ILoadDisplayDataListener loadDisplayDataListener) {
        super("Size Job");
        setSystem(true);
        this.loadDisplayDataListener = loadDisplayDataListener;
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
     * @param selectName
     * @param type
     * @return errorMessage when unable to load else null
     */
    public String loadSelect(String selectName, ArchiveConstants.Type type) {
        ArchiveConfigManager manager = ArchiveConfigManager.getInstance();
        String fileName = ArchiveConstants.selectFileName(type, selectName);
        SelectConfig selections = manager.loadSelection(fileName);
        if (selections == null) {
            selections = new SelectConfig();
            selections.setName(ArchiveConstants.defaultSelectName);
        }

        for (String archiveName : getArchiveNames()) {
            ArchiveInfo archiveInfo = get(archiveName);
            for (String categoryName : archiveInfo.getCategoryNames()) {
                Set<String> selectionsSet = selections.getSelectedSet(
                        archiveName, categoryName);
                CategoryInfo categoryInfo = archiveInfo.get(categoryName);
                for (DisplayData displayData : categoryInfo
                        .getDisplayDataList()) {
                    String displayLabel = displayData.getDisplayLabel();
                    boolean selected = selectionsSet.contains(displayLabel);
                    if (selected != displayData.isSelected()) {
                        setSelect(displayData, selected);
                    }
                }
            }
        }
        return null;
    }

    /**
     * Get list of all selected data.
     * 
     * @return selected
     */
    public List<DisplayData> getSelectAll() {
        synchronized (loadDisplayDataQueue) {
            while (!loadDisplayDataQueue.isEmpty()) {
                if ((loadDisplayDataJob.currentLoadDisplayData == null)
                        || loadDisplayDataJob.currentLoadDisplayData
                                .isVisible()
                        || loadDisplayDataJob.currentLoadDisplayData
                                .isSelected()) {
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
     * 
     * @param archiveName
     *            - non null value
     * @param categoryName
     *            - non null value
     * @return true if archiveName and categoryName match the current display
     */
    public boolean isCurrentDisplay(String archiveName, String categoryName) {
        return archiveName.equals(displayArchive)
                && categoryName.equals(displayCategory);
    }

    /**
     * This method returns a display data list to replace the contents of the
     * GUI's achive/category display table or a null list if no update is
     * needed. It is assumed this method is called from an non-UI thread.
     * 
     * @param archiveName
     *            - archive for the new display table
     * @param categoryName
     *            - category for the new display table
     * @param shutdown
     *            - Becomes true when user requests a different table while this
     *            is waiting for data.
     * @return displayData
     */
    public List<DisplayData> changeDisplay(String archiveName,
            String categoryName, AtomicBoolean shutdown) {
        List<DisplayData> displayDatas = null;

        // Only get data when the display really needs to be changed.
        if (!archiveName.equals(displayArchive)
                || !categoryName.equals(displayCategory)) {

            // Update visible status of current display.
            if ((displayArchive != null) && (displayCategory != null)) {
                LoadDisplayData currentMissingData = removeLoadDisplayData(
                        displayArchive, displayCategory);
                if (currentMissingData != null) {
                    currentMissingData.setVisible(false);
                    addLoadDisplayData(currentMissingData);
                }
            }

            LoadDisplayData missingData = removeLoadDisplayData(archiveName,
                    categoryName);
            displayArchive = archiveName;
            displayCategory = categoryName;

            // Update visible status of the new current display.
            if (missingData != null) {
                synchronized (loadDisplayDataQueue) {
                    missingData.setVisible(true);
                    addLoadDisplayData(missingData);

                    /*
                     * Wait for the display data to be loaded or no longer
                     * needed.
                     */
                    while (loadDisplayDataJob.processing(missingData)
                            && !shutdown.get()) {
                        missingDataQueueWait(500L);
                    }
                }
            }

            /*
             * If user still needs the data update status of old visible data
             * and the new visible data.
             */
            if (!shutdown.get()) {
                displayDatas = archiveInfoMap.get(archiveName)
                        .get(categoryName).getDisplayDataList();
                changeDisplay(displayDatas);
            }
        }
        return displayDatas;
    }

    /**
     * Change to display all selected data..
     * 
     * @return displayData when display needs to change otherwise null.
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
        ArchiveConfigManager manager = ArchiveConfigManager.getInstance();
        String fileName = ArchiveConstants.selectFileName(type, selectName);
        SelectConfig selections = manager.loadSelection(fileName);
        if (selections == null) {
            selections = new SelectConfig();
            selections.setName(ArchiveConstants.defaultSelectName);
        }
        iRetentionHour.setRetentionTimes(selections.getStarRetentionHours());

        loadDisplayDataQueue.clear();

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
                    LoadDisplayData missingData = new LoadDisplayData(
                            archiveName, categoryName, selectedSet);
                    loadDisplayDataQueue.add(missingData);
                }
            }
            put(archiveName, archiveInfo);
        }

        loadDisplayDataJob = new LoadDisplayDataJob(loadDisplayDataListener,
                loadDisplayDataQueue.size());

        loadDisplayDataJob.schedule();

        return selections.getName();
    }

    /**
     * Find and remove the associated load display data from the queue.
     * 
     * @param archiveName
     * @param categoryName
     * @return loadDisplayData or null if no longer on the queue
     */
    private LoadDisplayData removeLoadDisplayData(String archiveName,
            String categoryName) {
        LoadDisplayData loadDisplayData = null;
        synchronized (loadDisplayDataQueue) {
            if (!loadDisplayDataQueue.isEmpty()) {
                Iterator<LoadDisplayData> iterator = loadDisplayDataQueue
                        .iterator();
                while (iterator.hasNext()) {
                    LoadDisplayData md = iterator.next();
                    if (md.archive.equals(archiveName)
                            && md.category.equals(categoryName)) {
                        iterator.remove();
                        loadDisplayData = md;
                        break;
                    }
                }
            }
        }
        return loadDisplayData;
    }

    /**
     * Wait for notification that current missing data is finished processing.
     * 
     * @return false when interrupted exception
     */
    private boolean missingDataQueueWait() {
        return missingDataQueueWait(0L);
    }

    /**
     * Wait for notification that the current missing data is finished
     * processing or the desired time has lapsed.
     * 
     * @param time
     * @return false when interrupted exception
     */
    private boolean missingDataQueueWait(long time) {
        boolean state = true;
        try {
            loadDisplayDataQueue.wait(time);
        } catch (InterruptedException e) {
            state = false;
            statusHandler.handle(Priority.INFO, e.getLocalizedMessage(), e);
        }
        return state;
    }

    /**
     * This inserts a load display data onto the queue. Should
     * 
     * @param loadDisplayData
     */
    private void addLoadDisplayData(LoadDisplayData loadDisplayData) {
        synchronized (loadDisplayDataQueue) {
            loadDisplayDataQueue.add(loadDisplayData);
            if (loadDisplayDataJob.getState() == Job.NONE) {
                loadDisplayDataJob.schedule();
            }
        }
    }

    /**
     * Change visible status to reflect the new list.
     * 
     * @param newDisplays
     */
    private void changeDisplay(List<DisplayData> newDisplays) {
        List<DisplayData> oldDisplays = visibleList;
        visibleList = newDisplays;

        for (DisplayData displayData : oldDisplays) {
            setVisible(displayData, false);
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
        loadDisplayDataQueue.clear();
        loadDisplayDataJob.cancel();
        shutdown.set(true);
    }

    /**
     * Class used to track missing display data for the archive/category tables.
     * Allowing the information to be retrieved in a non-UI thread.
     */
    private static class LoadDisplayData {
        protected final String archive;

        protected final String category;

        protected final Set<String> selectedSet;

        protected boolean visible = false;

        public LoadDisplayData(String archive, String category,
                Set<String> selectedSet) {
            this.archive = archive;
            this.category = category;
            this.selectedSet = new HashSet<String>(selectedSet);
        }

        public boolean isSelected() {
            return !selectedSet.isEmpty();
        }

        public boolean isVisible() {
            return visible;
        }

        public void setVisible(boolean state) {
            this.visible = state;
        }

        @Override
        public String toString() {
            StringBuilder sb = new StringBuilder("LoadDisplayData[");
            sb.append("archive: ").append(archive);
            sb.append(", category: ").append(category);
            sb.append(", visible: ").append(isVisible());
            sb.append(", isSelected: ").append(isSelected());
            sb.append("]");
            return sb.toString();
        }
    }

    /**
     * This handles getting the display data in the missing data queue and
     * queuing the results for the size job.
     */
    private class LoadDisplayDataJob extends Job {

        private final AtomicBoolean shutdown = new AtomicBoolean(false);

        protected LoadDisplayData currentLoadDisplayData = null;

        private final ILoadDisplayDataListener loadDisplayDataListener;

        /**
         * Must be set to the maximum number of LoadDisplayData to process.
         * Cannot use the queue size since adjusting the queue to change the
         * priority may make its size 0. This could cause the job to be
         * rescheduled to finish when data added back onto the queue.
         */
        private int loadDisplayDataCnt;

        /**
         * The constructor.
         * 
         * @param loadDisplayDataListener
         * @param loadDisplayDataCnt
         */
        public LoadDisplayDataJob(
                ILoadDisplayDataListener loadDisplayDataListener,
                int loadDisplayDataCnt) {
            super("Loading Data Sets");
            this.loadDisplayDataListener = loadDisplayDataListener;
            this.loadDisplayDataCnt = loadDisplayDataCnt;
        }

        @Override
        protected IStatus run(IProgressMonitor monitor) {
            if (monitor.isCanceled()) {
                return Status.OK_STATUS;
            }

            ArchiveConfigManager manager = ArchiveConfigManager.getInstance();

            while (!shutdown.get()) {
                synchronized (loadDisplayDataQueue) {
                    if (currentLoadDisplayData != null) {
                        currentLoadDisplayData = null;
                        loadDisplayDataQueue.notifyAll();
                        --loadDisplayDataCnt;
                    }
                    currentLoadDisplayData = loadDisplayDataQueue.poll();
                    if (currentLoadDisplayData == null) {
                        break;
                    }
                }

                String archiveName = currentLoadDisplayData.archive;
                String categoryName = currentLoadDisplayData.category;
                Set<String> selectedSet = currentLoadDisplayData.selectedSet;
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

            if (loadDisplayDataCnt == 0) {
                if (loadDisplayDataListener != null) {
                    loadDisplayDataListener.loadedAllDisplayData();
                }
            }

            return Status.OK_STATUS;
        }

        /**
         * 
         * @param loadDisplayData
         * @return true when missingData still needs to be processed.
         */
        public boolean processing(LoadDisplayData loadDisplayData) {
            synchronized (loadDisplayDataQueue) {
                return loadDisplayDataQueue.contains(loadDisplayData)
                        || loadDisplayData.equals(currentLoadDisplayData);

            }
        }

        @Override
        protected void canceling() {
            shutdown.set(true);
        }
    }
}