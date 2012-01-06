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
import java.util.Calendar;
import java.util.HashMap;
import java.util.List;
import java.util.Map;

import com.raytheon.edex.plugin.grib.dao.GribDao;
import com.raytheon.edex.plugin.grib.spatial.GribSpatialCache;
import com.raytheon.edex.plugin.grib.util.GribModelCache;
import com.raytheon.edex.util.Util;
import com.raytheon.edex.util.grib.CompositeModel;
import com.raytheon.uf.common.dataplugin.PluginException;
import com.raytheon.uf.common.dataplugin.grib.GribModel;
import com.raytheon.uf.common.dataplugin.grib.GribRecord;
import com.raytheon.uf.common.dataplugin.grib.StatusConstants;
import com.raytheon.uf.common.dataplugin.grib.exception.GribException;
import com.raytheon.uf.common.dataplugin.grib.spatial.projections.GridCoverage;
import com.raytheon.uf.common.dataplugin.grib.spatial.projections.LatLonGridCoverage;
import com.raytheon.uf.common.datastorage.StorageException;
import com.raytheon.uf.common.datastorage.StorageStatus;
import com.raytheon.uf.common.datastorage.records.FloatDataRecord;
import com.raytheon.uf.common.localization.IPathManager;
import com.raytheon.uf.common.localization.LocalizationContext.LocalizationLevel;
import com.raytheon.uf.common.localization.LocalizationContext.LocalizationType;
import com.raytheon.uf.common.localization.PathManagerFactory;
import com.raytheon.uf.common.serialization.SerializationException;
import com.raytheon.uf.common.serialization.SerializationUtil;
import com.raytheon.uf.common.status.IUFStatusHandler;
import com.raytheon.uf.common.status.UFStatus;
import com.raytheon.uf.common.status.UFStatus.Priority;
import com.raytheon.uf.common.util.FileUtil;
import com.raytheon.uf.edex.database.DataAccessLayerException;
import com.raytheon.uf.edex.database.cluster.ClusterLockUtils;
import com.raytheon.uf.edex.database.cluster.ClusterTask;
import com.raytheon.uf.edex.database.cluster.ClusterLockUtils.LockState;
import com.raytheon.uf.edex.database.plugin.PluginFactory;

/**
 * The EnsembleGridAssembler class is part of the ingest process for grib data.
 * Some grib model come in as octants. This class will combine those octants
 * into a single grid
 * 
 * <pre>
 * 
 * SOFTWARE HISTORY
 * 
 * Date         Ticket#     Engineer    Description
 * ------------ ----------  ----------- --------------------------
 * 4/09/10      4638        bphillip    Initial Creation
 * 
 * </pre>
 * 
 * @author bphillip
 * @version 1
 */
public class EnsembleGridAssembler implements IDecoderPostProcessor {
    private static final transient IUFStatusHandler statusHandler = UFStatus.getHandler(EnsembleGridAssembler.class);

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
        FilenameFilter filter = new FilenameFilter() {
            @Override
            public boolean accept(File dir, String name) {
                return (!new File(dir.getPath() + File.separator + name)
                        .isDirectory() && name.endsWith(".xml"));
            }
        };
        ArrayList<File> thinnedModelFiles = FileUtil.listFiles(commonPath,
                filter, false);

        for (File file : thinnedModelFiles) {
            try {
                CompositeModel model = (CompositeModel) SerializationUtil
                        .jaxbUnmarshalFromXmlFile(file.getPath());
                thinnedModels.put(model.getModelName(), model);
            } catch (SerializationException e) {
                statusHandler.handle(Priority.PROBLEM,
                        "Error deserializing thinned model file", e);
            }
        }
    }

    public GribRecord[] process(GribRecord rec) throws GribException {
        Map<Integer, GribRecord> newRecords = new HashMap<Integer, GribRecord>();
        String compositeModel = getCompositeModel(rec.getModelInfo()
                .getModelName());
        if (compositeModel != null) {
            GribRecord newRec = null;
            String lockName = compositeModel + "_"
                    + rec.getModelInfo().getParameterAbbreviation() + "_"
                    + rec.getModelInfo().getLevel().toString();
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
                newRec = processGrid(rec,
                        getCompositeModelObject(compositeModel));
                newRecords.put(newRec.getId(), newRec);
            } catch (Exception e) {
                clearTime = true;
                throw new GribException("Error processing ensemble grid", e);
            } finally {
                ClusterLockUtils.unlock(ct, clearTime);
            }

            return new GribRecord[] { rec, newRec };
        }
        return new GribRecord[] { rec };
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
     * Processes a single GribRecord
     * 
     * @param record
     *            The GribRecord to process
     * @param thinned
     *            The composite model for which the GribRecord is a part of
     * @return The new grib record
     * @throws Exception
     */
    private GribRecord processGrid(GribRecord record, CompositeModel thinned)
            throws Exception {

        GribDao dao = (GribDao) PluginFactory.getInstance()
                .getPluginDao("grib");
        String modelName = record.getModelInfo().getModelName();
        String dataURI = record.getDataURI();
        String assembledDataURI = dataURI.replace(modelName,
                thinned.getModelName());

        List<?> result = dao.queryBySingleCriteria("dataURI", assembledDataURI);
        GribRecord assembledRecord = null;
        if (result.isEmpty()) {
            assembledRecord = createRecord(record, dao, thinned);
        } else {
            assembledRecord = (GribRecord) result.get(0);
            FloatDataRecord rec = (FloatDataRecord) dao.getHDF5Data(
                    assembledRecord, 0)[0];
            assembledRecord.setMessageData(rec);
            assembledRecord.setPluginName("grib");
        }

        mergeData(record, assembledRecord, dao, thinned);
        return assembledRecord;

    }

    /**
     * Merges the data from a GribRecord into the composite GribRecord
     * 
     * @param record
     *            The GribRecord containing the data to add
     * @param assembledRecord
     *            The composite GribRecord
     * @param dao
     *            An instance of the grib data access object
     * @param thinned
     *            The composite model definition
     * @return The composite GribRecord
     * @throws Exception
     */
    private GribRecord mergeData(GribRecord record, GribRecord assembledRecord,
            GribDao dao, CompositeModel thinned) throws Exception {

        String modelName = record.getModelInfo().getModelName();
        GridCoverage coverage = record.getModelInfo().getLocation();

        long[] sizes = ((FloatDataRecord) assembledRecord.getMessageData())
                .getSizes();

        float[][] assembledData = Util.resizeDataTo2D(
                ((FloatDataRecord) assembledRecord.getMessageData())
                        .getFloatData(), (int) sizes[0], (int) sizes[1]);

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
                    coverage.getNy()), nx * modIndex - modIndex, 0, nx, ny);
        }

        assembledRecord.setMessageData(Util.resizeDataTo1D(assembledData,
                (int) sizes[1], (int) sizes[0]));
        assembledRecord.setOverwriteAllowed(true);
        try {
            dao.persistToHDF5(assembledRecord);
        } catch (PluginException e) {
            throw new GribException("Error storing assembled grid to HDF5", e);
        }
        assembledRecord.setMessageData(null);
        return assembledRecord;

    }

    /**
     * Creates the composite grib record and stores it to the HDF5 repository
     * 
     * @param record
     *            The recieved GribRecord used to initialize the composite grid
     *            with
     * @param dao
     *            An instance of the grib data access object
     * @param thinned
     *            The composite grid definition
     * @return The composite record
     * @throws GribException
     */
    private GribRecord createRecord(GribRecord record, GribDao dao,
            CompositeModel thinned) throws GribException {
        LatLonGridCoverage coverage = (LatLonGridCoverage) GribSpatialCache
                .getInstance().getGridByName(thinned.getGrid());

        float[] data = new float[coverage.getNx() * coverage.getNy()];
        for (int i = 0; i < data.length; i++) {
            data[i] = Util.GRID_FILL_VALUE;
        }
        GribRecord newRecord = new GribRecord();
        GribModel newModel = new GribModel(record.getModelInfo());
        newModel.setGridid(coverage.getName());
        newModel.setGridNumber(Integer.parseInt(coverage.getName()));
        newModel.setModelName(thinned.getModelName());
        newModel.setLocation(coverage);
        newModel.generateId();

        try {
            newModel = GribModelCache.getInstance().getModel(newModel);
        } catch (DataAccessLayerException e) {
            throw new GribException("Unable to get model info from the cache!",
                    e);
        }
        newRecord.setModelInfo(newModel);
        newRecord.setMessageData(data);
        newRecord.setDataTime(record.getDataTime());
        newRecord.setDataURI(null);
        newRecord.setPluginName("grib");
        newRecord.setInsertTime(Calendar.getInstance());

        try {
            newRecord.constructDataURI();
        } catch (PluginException e) {
            throw new GribException(
                    "Error constructing DataURI for grib record", e);
        }
        try {
            StorageStatus ss = dao.persistToHDF5(newRecord);
            StorageException[] exceptions = ss.getExceptions();
            // Only one record is stored, so logically there should only be one
            // possible exception in the exception array
            if (exceptions.length > 0) {
                throw new GribException("Error storing new record to HDF5",
                        exceptions[0]);
            }
            dao.persistToDatabase(newRecord);
            newRecord = (GribRecord) dao.getMetadata(newRecord.getDataURI());
            FloatDataRecord rec = (FloatDataRecord) dao.getHDF5Data(newRecord,
                    0)[0];
            newRecord.setMessageData(rec);
            newRecord.setPluginName("grib");
        } catch (PluginException e) {
            throw new GribException("Error storing new record to HDF5", e);
        }
        return newRecord;
    }
}
