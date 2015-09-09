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

package com.raytheon.edex.plugin.grib.decoderpostprocessors;

import java.io.File;
import java.io.FilenameFilter;
import java.util.ArrayList;
import java.util.Arrays;
import java.util.HashMap;
import java.util.Iterator;
import java.util.LinkedHashMap;
import java.util.List;
import java.util.Map;

import javax.xml.bind.JAXBException;

import com.raytheon.edex.plugin.grib.exception.GribException;
import com.raytheon.edex.plugin.grib.spatial.GribSpatialCache;
import com.raytheon.edex.util.Util;
import com.raytheon.edex.util.grib.CompositeModel;
import com.raytheon.uf.common.dataplugin.PluginDataObject;
import com.raytheon.uf.common.dataplugin.grid.GridConstants;
import com.raytheon.uf.common.dataplugin.grid.GridRecord;
import com.raytheon.uf.common.datastorage.records.FloatDataRecord;
import com.raytheon.uf.common.gridcoverage.GridCoverage;
import com.raytheon.uf.common.localization.IPathManager;
import com.raytheon.uf.common.localization.LocalizationContext.LocalizationLevel;
import com.raytheon.uf.common.localization.LocalizationContext.LocalizationType;
import com.raytheon.uf.common.localization.PathManagerFactory;
import com.raytheon.uf.common.serialization.SerializationException;
import com.raytheon.uf.common.serialization.SingleTypeJAXBManager;
import com.raytheon.uf.common.status.IUFStatusHandler;
import com.raytheon.uf.common.status.UFStatus;
import com.raytheon.uf.common.status.UFStatus.Priority;
import com.raytheon.uf.common.time.util.TimeUtil;
import com.raytheon.uf.common.util.CollectionUtil;
import com.raytheon.uf.common.util.FileUtil;
import com.raytheon.uf.common.util.GridUtil;
import com.raytheon.uf.common.util.file.FilenameFilters;
import com.raytheon.uf.edex.core.EDEXUtil;
import com.raytheon.uf.edex.core.IContextStateProcessor;
import com.raytheon.uf.edex.database.cluster.ClusterLockUtils;
import com.raytheon.uf.edex.database.cluster.ClusterLockUtils.LockState;
import com.raytheon.uf.edex.database.cluster.ClusterTask;
import com.raytheon.uf.edex.database.plugin.DataURIDatabaseUtil;
import com.raytheon.uf.edex.database.plugin.PluginFactory;
import com.raytheon.uf.edex.plugin.grid.dao.GridDao;

/**
 * The EnsembleGridAssembler class is part of the ingest process for grib data.
 * Some grib model come in as octants. This class will combine those octants
 * into a single grid
 * 
 * <pre>
 * 
 * SOFTWARE HISTORY
 * 
 * Date          Ticket#  Engineer    Description
 * ------------- -------- ----------- --------------------------
 * Apr 09, 2010  4638     bphillip    Initial Creation
 * Mar 14, 2013  1794     djohnson    FileUtil.listFiles now returns List.
 * Mar 27, 2013  1821     bsteffen    Reduce db and pypies requests in grid
 *                                    assembler.
 * Aug 30, 2013  2298     rjpeter     Make getPluginName abstract
 * Oct 15, 2013  2473     bsteffen    Remove deprecated method calls.
 * Nov 19, 2013  2478     rjpeter     Make update process update database also.
 * Dec 06, 2013  2170     rjpeter     Update to pass PluginDataObject[] to notification.
 * Apr 21, 2014  2060     njensen     Remove dependency on grid dataURI column
 * Jul 21, 2014  3373     bclement    JAXB manager api changes
 * Aug 18, 2014  4360     rferrel     Set secondaryId in {@link #createAssembledRecord(GridRecord, CompositeModel)}
 * Sep 09, 2015  4868     rjpeter     Move grid assembly to a dedicated thread.
 * </pre>
 * 
 * @author bphillip
 * @version 1
 */
public class EnsembleGridAssembler implements IDecoderPostProcessor,
        IContextStateProcessor {
    private static final transient IUFStatusHandler statusHandler = UFStatus
            .getHandler(EnsembleGridAssembler.class);

    /** The map of the models that come in sections */
    private static final Map<String, CompositeModel> thinnedModels = new HashMap<>();;

    private static final String CLUSTER_TASK_NAME = "EnsembleGrid";

    private static final Map<GridRecord, List<GridRecord>> pendingRecords = new LinkedHashMap<>();

    private static long maxPointsInMemory = 0;

    private static long pointsInMemory = 0;

    private static GridAssembler[] assemblerThreads = null;

    private static int numAssemblerThreads = 1;

    static {
        loadThinnedModels();
    }

    /**
     * Loads the models from the localization store and stores them in memory
     */
    private static void loadThinnedModels() {
        IPathManager pm = PathManagerFactory.getPathManager();
        File commonPath = pm.getFile(pm.getContext(
                LocalizationType.EDEX_STATIC, LocalizationLevel.BASE),
                "/grib/thinnedModels");

        FilenameFilter filter = FilenameFilters.byFilters(
                FilenameFilters.ACCEPT_FILES,
                FilenameFilters.byFileExtension(".xml"));

        List<File> thinnedModelFiles = FileUtil.listFiles(commonPath, filter,
                false);

        SingleTypeJAXBManager<CompositeModel> jaxbManager;
        try {
            jaxbManager = new SingleTypeJAXBManager<CompositeModel>(true,
                    CompositeModel.class);
        } catch (JAXBException e) {
            statusHandler.handle(Priority.PROBLEM,
                    "Unable to load thinned model files.", e);
            return;
        }

        for (File file : thinnedModelFiles) {
            try {
                CompositeModel model = jaxbManager.unmarshalFromXmlFile(file);
                thinnedModels.put(model.getModelName(), model);
            } catch (SerializationException e) {
                statusHandler.handle(Priority.PROBLEM,
                        "Error deserializing thinned model file", e);
            }
        }
    }

    public void setMaxPointsInMemory(int maxPoints) {
        maxPointsInMemory = maxPoints;
    }

    public void setNumAssemblerThreads(int numThreads) {
        if (numThreads > 0) {
            numAssemblerThreads = numThreads;
        } else {
            statusHandler
                    .error("Number of assembler threads must be > 0, keeping previous value of "
                            + numAssemblerThreads);
        }
    }

    @Override
    public GridRecord[] process(GridRecord rec) throws GribException {
        CompositeModel compositeModel = getCompositeModel(rec.getDatasetId());
        if (compositeModel != null) {
            GridRecord assembledRecord = createAssembledRecord(rec,
                    compositeModel);
            GridCoverage coverage = rec.getLocation();
            long pointsInGrid = coverage.getNx() * coverage.getNy();

            synchronized (pendingRecords) {
                if (assemblerThreads == null) {
                    /*
                     * Start assembler threads if they haven't been started yet
                     */
                    assemblerThreads = new GridAssembler[numAssemblerThreads];
                    for (int i = 0; i < assemblerThreads.length; i++) {
                        assemblerThreads[i] = new GridAssembler(
                                "GridAssembler-" + (i + 1));
                        assemblerThreads[i].start();
                    }
                }

                boolean logMessage = true;

                pointsInMemory += pointsInGrid;
                List<GridRecord> pendingList = pendingRecords
                        .get(assembledRecord);
                if (pendingList == null) {
                    pendingList = new ArrayList<>(4);
                    pendingRecords.put(assembledRecord, pendingList);
                }

                pendingList.add(rec);
                pendingRecords.notifyAll();

                while (pointsInMemory > maxPointsInMemory) {
                    if (logMessage) {
                        statusHandler.info("MaxPoints in "
                                + getClass().getName()
                                + " exceeded.  Waiting for grids to process");
                        logMessage = false;
                    }

                    try {
                        pendingRecords.wait();
                    } catch (InterruptedException e) {
                        // ignore
                    }
                }

            }

            // grid was assembled in to a larger grid, discard original
            return new GridRecord[0];
        }

        // wasn't a grid to be assembled
        return new GridRecord[] { rec };
    }

    /**
     * Gets the composite model for the provided model name0
     * 
     * @param modelName
     *            The model name to determine the composite model name for
     * @return The composite model. Null if not found
     */
    private CompositeModel getCompositeModel(String modelName) {
        for (CompositeModel mod : thinnedModels.values()) {
            if (mod.getModelList().contains(modelName)) {
                return mod;
            }
        }
        return null;
    }

    private GridRecord createAssembledRecord(GridRecord record,
            CompositeModel thinned) {
        GridRecord newRecord = new GridRecord(record);

        GridCoverage coverage = GribSpatialCache.getInstance().getGridByName(
                thinned.getGrid());

        newRecord.setLocation(coverage);
        newRecord.setDatasetId(thinned.getModelName());

        return newRecord;
    }

    @Override
    public void preStart() {
        // null op
    }

    @Override
    public void postStart() {
        // null op
    }

    @Override
    public void preStop() {
        // null op
    }

    /*
     * (non-Javadoc)
     * 
     * @see com.raytheon.uf.edex.core.IContextStateProcessor#postStop()
     */
    @Override
    public void postStop() {
        if (assemblerThreads != null) {
            for (GridAssembler assembler : assemblerThreads) {
                assembler.running = false;
            }

            synchronized (pendingRecords) {
                pendingRecords.notifyAll();
                if (pendingRecords.size() > 0) {
                    statusHandler.info("Waiting for " + pendingRecords.size()
                            + " grids to be assembled");
                }
            }

            for (GridAssembler assembler : assemblerThreads) {
                try {
                    assembler.join();
                } catch (InterruptedException e) {
                    // ignore
                }
            }
        }
    }

    /**
     * Thread to assemble sectorized grids into the overall parent grid.
     */
    private static class GridAssembler extends Thread {
        protected volatile boolean running = true;

        public GridAssembler(String name) {
            super(name);
        }

        @Override
        public void run() {
            boolean keepProcessing = running;
            long timeToAssembleGrid = 0;
            do {
                GridRecord compositeRecord = null;
                List<GridRecord> recordsToAssemble = null;
                ClusterTask ct = null;

                try {
                    int index = 0;
                    do {
                        synchronized (pendingRecords) {
                            // avoid holding lock for extended period
                            while (pendingRecords.isEmpty() && running) {
                                try {
                                    pendingRecords.wait();
                                } catch (InterruptedException e) {
                                    // ignore
                                }
                            }

                            if (pendingRecords.size() > 0) {
                                Iterator<GridRecord> iter = pendingRecords
                                        .keySet().iterator();
                                if (index >= pendingRecords.size()) {
                                    index = 0;
                                }

                                // skip previously checked entries
                                for (int i = 0; i < index; i++) {
                                    iter.next();
                                }

                                compositeRecord = iter.next();
                            }
                        }

                        if (compositeRecord != null) {
                            /*
                             * check compositeRecord in case a shutdown was
                             * triggered
                             */
                            String lockName = compositeRecord.getDataURI();
                            ct = ClusterLockUtils.lock(CLUSTER_TASK_NAME,
                                    lockName, 120000, false);

                            if (!LockState.SUCCESSFUL.equals(ct.getLockState())) {
                                compositeRecord = null;
                                ct = null;
                                index++;
                            }

                            if (compositeRecord != null) {
                                synchronized (pendingRecords) {
                                    recordsToAssemble = pendingRecords
                                            .remove(compositeRecord);
                                }
                            }
                        }
                    } while ((recordsToAssemble == null) && running);

                    if (recordsToAssemble != null) {
                        long t0 = System.currentTimeMillis();
                        processGrids(compositeRecord, recordsToAssemble);
                        timeToAssembleGrid = System.currentTimeMillis() - t0;
                    }
                } catch (Throwable e) {
                    statusHandler.error(
                            "Uncaught exception while assembling grids", e);
                } finally {
                    if (ct != null) {
                        /*
                         * lock is time based, need to delete lock instead of
                         * just unlocking
                         */
                        ClusterLockUtils.deleteLock(ct.getId().getName(), ct
                                .getId().getDetails());
                    }

                    if (!CollectionUtil.isNullOrEmpty(recordsToAssemble)) {
                        long points = 0;
                        for (GridRecord rec : recordsToAssemble) {
                            GridCoverage location = rec.getLocation();
                            points += location.getNx() * location.getNy();
                        }

                        int remaining = 0;

                        synchronized (pendingRecords) {
                            pointsInMemory -= points;
                            pendingRecords.notifyAll();
                            remaining = pendingRecords.size();
                        }

                        int count = recordsToAssemble.size();
                        StringBuilder msg = new StringBuilder(80);
                        msg.append("Took ").append(timeToAssembleGrid)
                                .append("ms to merge ").append(count)
                                .append((count == 1 ? " grid. " : " grids. "))
                                .append(remaining)
                                .append((remaining == 1 ? " grid" : " grids"))
                                .append(" remaining to merge.");
                        statusHandler.info(msg.toString());
                    }
                }

                keepProcessing = running;
                if (!keepProcessing) {
                    synchronized (pendingRecords) {
                        keepProcessing = !pendingRecords.isEmpty();
                    }
                }
            } while (keepProcessing);
        }

        /**
         * Processes a single GridRecord
         * 
         * @param record
         *            The GridRecord to process
         * @param thinned
         *            The composite model for which the GridRecord is a part of
         * @return The new grib record
         * @throws Exception
         */
        private void processGrids(GridRecord assembledRecord,
                List<GridRecord> recordsToAssemble) throws Exception {
            GridDao dao = (GridDao) PluginFactory.getInstance().getPluginDao(
                    GridConstants.GRID);
            boolean exists = DataURIDatabaseUtil
                    .existingDataURI(assembledRecord);

            if (!exists) {
                GridCoverage coverage = assembledRecord.getLocation();
                float[] data = new float[coverage.getNx() * coverage.getNy()];
                Arrays.fill(data, GridUtil.GRID_FILL_VALUE);
                assembledRecord.setMessageData(data);
            } else {
                FloatDataRecord rec = (FloatDataRecord) dao.getHDF5Data(
                        assembledRecord, -1)[0];
                assembledRecord.setMessageData(rec.getFloatData());
            }

            mergeData(assembledRecord, recordsToAssemble);
            assembledRecord.setOverwriteAllowed(true);
            assembledRecord.setInsertTime(TimeUtil.newGmtCalendar());
            dao.persistRecords(assembledRecord);
            EDEXUtil.getMessageProducer().sendAsync("notificationAggregation",
                    new PluginDataObject[] { assembledRecord });
        }

        /**
         * Merges the data from a GridRecord into the composite GridRecord
         * 
         * @param record
         *            The GridRecord containing the data to add
         * @param assembledRecord
         *            The composite GridRecord
         * @param thinned
         *            The composite model definition
         * @throws GribException
         */
        private void mergeData(GridRecord assembledRecord,
                List<GridRecord> recordsToAssemble) throws GribException {
            CompositeModel thinned = thinnedModels.get(assembledRecord
                    .getDatasetId());
            GridCoverage assembledCoverage = assembledRecord.getLocation();
            float[][] assembledData = Util.resizeDataTo2D(
                    (float[]) assembledRecord.getMessageData(),
                    assembledCoverage.getNx(), assembledCoverage.getNy());

            for (GridRecord record : recordsToAssemble) {
                String modelName = record.getDatasetId();
                GridCoverage coverage = record.getLocation();

                int nx = coverage.getNx();
                int ny = coverage.getNy();

                List<String> compModels = thinned.getModelList();

                int modIndex = compModels.indexOf(modelName);
                if (modIndex == -1) {
                    /*
                     * Shouldn't be possible since was how it was found in the
                     * first place
                     */
                    throw new GribException(
                            "Error assembling grids.  Thinned grid definition does not contain "
                                    + modelName);
                }

                /*
                 * TODO: This should map the UL corner of record to
                 * assembledRecord instead of relying on index in list
                 */
                Util.insertSubgrid(assembledData, Util.resizeDataTo2D(
                        (float[]) record.getMessageData(), coverage.getNx(),
                        coverage.getNy()), (nx * modIndex) - modIndex, 0, nx,
                        ny);
            }

            assembledRecord.setMessageData(Util.resizeDataTo1D(assembledData,
                    assembledCoverage.getNy(), assembledCoverage.getNx()));
        }
    }

}
