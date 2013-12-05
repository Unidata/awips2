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
import java.util.Arrays;
import java.util.Calendar;
import java.util.HashMap;
import java.util.List;
import java.util.Map;

import javax.xml.bind.JAXBException;

import com.raytheon.edex.plugin.grib.exception.GribException;
import com.raytheon.edex.plugin.grib.spatial.GribSpatialCache;
import com.raytheon.edex.util.Util;
import com.raytheon.edex.util.grib.CompositeModel;
import com.raytheon.uf.common.dataplugin.PluginException;
import com.raytheon.uf.common.dataplugin.grid.GridConstants;
import com.raytheon.uf.common.dataplugin.grid.GridRecord;
import com.raytheon.uf.common.datastorage.StorageException;
import com.raytheon.uf.common.datastorage.StorageStatus;
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
import com.raytheon.uf.common.util.FileUtil;
import com.raytheon.uf.common.util.GridUtil;
import com.raytheon.uf.common.util.file.FilenameFilters;
import com.raytheon.uf.edex.core.EDEXUtil;
import com.raytheon.uf.edex.database.cluster.ClusterLockUtils;
import com.raytheon.uf.edex.database.cluster.ClusterLockUtils.LockState;
import com.raytheon.uf.edex.database.cluster.ClusterTask;
import com.raytheon.uf.edex.database.plugin.PluginFactory;
import com.raytheon.uf.edex.database.query.DatabaseQuery;
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
 * 
 * </pre>
 * 
 * @author bphillip
 * @version 1
 */
public class EnsembleGridAssembler implements IDecoderPostProcessor {
    private static final transient IUFStatusHandler statusHandler = UFStatus
            .getHandler(EnsembleGridAssembler.class);

    /** The map of the models that come in sections */
    private static Map<String, CompositeModel> thinnedModels;

    private static final String CLUSTER_TASK_NAME = "EnsembleGrid";

    /**
     * Creates a new GridAssemble instance
     */
    public EnsembleGridAssembler() {
        if (thinnedModels == null) {
            loadThinnedModels();
        }
    }

    /**
     * Loads the models from the localization store and stores them in memory
     */
    private void loadThinnedModels() {
        thinnedModels = new HashMap<String, CompositeModel>();
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
            jaxbManager = new SingleTypeJAXBManager<CompositeModel>(
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

    @Override
    public GridRecord[] process(GridRecord rec) throws GribException {
        String compositeModel = getCompositeModel(rec.getDatasetId());
        if (compositeModel != null) {
            String lockName = compositeModel + "_"
                    + rec.getParameter().getAbbreviation() + "_"
                    + rec.getLevel().toString();
            ClusterTask ct = ClusterLockUtils.lock(CLUSTER_TASK_NAME, lockName,
                    120000, true);
            boolean clearTime = false;

            try {
                while (!LockState.SUCCESSFUL.equals(ct.getLockState())) {
                    if (LockState.FAILED.equals(ct.getLockState())) {
                        throw new GribException(
                                "Failed to get cluster lock to process ensemble grids");
                    }
                    ct = ClusterLockUtils.lock(CLUSTER_TASK_NAME, lockName,
                            120000, true);
                }
                processGrid(rec, getCompositeModelObject(compositeModel));
            } catch (Exception e) {
                clearTime = true;
                throw new GribException("Error processing ensemble grid", e);
            } finally {
                ClusterLockUtils.unlock(ct, clearTime);
            }

            return new GridRecord[] { rec };
        }
        return new GridRecord[] { rec };
    }

    /**
     * Gets the composite model name for which the provided model name is a part
     * of
     * 
     * @param modelName
     *            The model name to determine the composite model name for
     * @return The composite model name. Null if not found
     */
    private String getCompositeModel(String modelName) {
        for (CompositeModel mod : thinnedModels.values()) {
            if (mod.getModelList().contains(modelName)) {
                return mod.getModelName();
            }
        }
        return null;
    }

    /**
     * Gets the composite model object
     * 
     * @param modelName
     *            The model name to get the composite model object for
     * @return The composite model object
     */
    private CompositeModel getCompositeModelObject(String modelName) {
        return thinnedModels.get(modelName);
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
    private void processGrid(GridRecord record, CompositeModel thinned)
            throws Exception {

        GridDao dao = (GridDao) PluginFactory.getInstance().getPluginDao(
                GridConstants.GRID);
        GridRecord assembledRecord = createAssembledRecord(record, thinned);
        DatabaseQuery query = new DatabaseQuery(GridRecord.class);
        query.addReturnedField("dataURI");
        query.addQueryParam("dataURI", assembledRecord.getDataURI());
        List<?> result = dao.queryByCriteria(query);
        if (result.isEmpty()) {
            persistNewRecord(record, assembledRecord, thinned, dao);
        } else {
            updateExistingRecord(record, assembledRecord, thinned, dao);
        }
        EDEXUtil.getMessageProducer().sendAsync("notificationAggregation",
                record);
    }

    private GridRecord createAssembledRecord(GridRecord record,
            CompositeModel thinned) {
        GridRecord newRecord = new GridRecord();

        GridCoverage coverage = GribSpatialCache.getInstance().getGridByName(
                thinned.getGrid());

        newRecord.setLocation(coverage);
        newRecord.setDatasetId(thinned.getModelName());
        newRecord.setLevel(record.getLevel());
        newRecord.setParameter(record.getParameter());
        newRecord.setEnsembleId(record.getEnsembleId());
        newRecord.setDataTime(record.getDataTime());
        newRecord.setDataURI(null);
        newRecord.setInsertTime(Calendar.getInstance());

        return newRecord;
    }

    private void persistNewRecord(GridRecord record,
            GridRecord assembledRecord, CompositeModel thinned, GridDao dao)
            throws GribException {
        GridCoverage coverage = assembledRecord.getLocation();
        float[] data = new float[coverage.getNx() * coverage.getNy()];
        Arrays.fill(data, GridUtil.GRID_FILL_VALUE);
        assembledRecord.setMessageData(data);
        mergeData(record, assembledRecord, thinned);
        try {
            StorageStatus ss = dao.persistToHDF5(assembledRecord);
            StorageException[] exceptions = ss.getExceptions();
            // Only one record is stored, so logically there should only be one
            // possible exception in the exception array
            if (exceptions.length > 0) {
                throw new GribException("Error storing new record to HDF5",
                        exceptions[0]);
            }
            dao.persistToDatabase(assembledRecord);
        } catch (PluginException e) {
            throw new GribException("Error storing new record to HDF5", e);
        }
    }

    private void updateExistingRecord(GridRecord record,
            GridRecord assembledRecord, CompositeModel thinned, GridDao dao)
            throws GribException {
        try {
            FloatDataRecord rec = (FloatDataRecord) dao.getHDF5Data(
                    assembledRecord, -1)[0];
            assembledRecord.setMessageData(rec.getFloatData());
            mergeData(record, assembledRecord, thinned);
            assembledRecord.setOverwriteAllowed(true);
            dao.persistRecords(assembledRecord);
        } catch (PluginException e) {
            throw new GribException("Error storing assembled grid to HDF5", e);
        }
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
    private void mergeData(GridRecord record, GridRecord assembledRecord,
            CompositeModel thinned) throws GribException {
        String modelName = record.getDatasetId();
        GridCoverage coverage = record.getLocation();
        GridCoverage assembledCoverage = assembledRecord.getLocation();

        float[][] assembledData = Util.resizeDataTo2D(
                (float[]) assembledRecord.getMessageData(),
                assembledCoverage.getNx(), assembledCoverage.getNy());

        int nx = coverage.getNx();
        int ny = coverage.getNy();

        List<String> compModels = thinned.getModelList();

        int modIndex = compModels.indexOf(modelName);
        if (modIndex == -1) {
            throw new GribException(
                    "Error assembling grids.  Thinned grid definition does not contain "
                            + modelName);
        }
        if (modIndex == 0) {
            Util.insertSubgrid(assembledData, Util.resizeDataTo2D(
                    (float[]) record.getMessageData(), coverage.getNx(),
                    coverage.getNy()), nx * modIndex, 0, nx, ny);
        } else {
            Util.insertSubgrid(assembledData, Util.resizeDataTo2D(
                    (float[]) record.getMessageData(), coverage.getNx(),
                    coverage.getNy()), (nx * modIndex) - modIndex, 0, nx, ny);
        }

        assembledRecord.setMessageData(Util.resizeDataTo1D(assembledData,
                assembledCoverage.getNy(), assembledCoverage.getNx()));
    }
}
