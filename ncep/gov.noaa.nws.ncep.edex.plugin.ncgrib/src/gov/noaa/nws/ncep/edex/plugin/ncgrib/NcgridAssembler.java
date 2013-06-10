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

package gov.noaa.nws.ncep.edex.plugin.ncgrib;

import gov.noaa.nws.ncep.common.dataplugin.ncgrib.NcgribModel;
import gov.noaa.nws.ncep.common.dataplugin.ncgrib.NcgribRecord;
import gov.noaa.nws.ncep.common.dataplugin.ncgrib.exception.GribException;
import gov.noaa.nws.ncep.common.dataplugin.ncgrib.spatial.projections.LatLonNcgridCoverage;
import gov.noaa.nws.ncep.common.dataplugin.ncgrib.spatial.projections.NcgridCoverage;
import gov.noaa.nws.ncep.edex.plugin.ncgrib.dao.NcgribDao;
import gov.noaa.nws.ncep.edex.plugin.ncgrib.spatial.NcgribSpatialCache;
import gov.noaa.nws.ncep.edex.plugin.ncgrib.util.NcgribModelCache;
import gov.noaa.nws.ncep.edex.util.ncgrib.NccompositeModel;

import java.io.File;
import java.io.FilenameFilter;
import java.util.Calendar;
import java.util.HashMap;
import java.util.List;
import java.util.Map;

import com.raytheon.edex.util.Util;
import com.raytheon.uf.common.dataplugin.PluginException;
import com.raytheon.uf.common.datastorage.records.FloatDataRecord;
import com.raytheon.uf.common.localization.IPathManager;
import com.raytheon.uf.common.localization.LocalizationContext.LocalizationLevel;
import com.raytheon.uf.common.localization.LocalizationContext.LocalizationType;
import com.raytheon.uf.common.localization.PathManagerFactory;
import com.raytheon.uf.common.serialization.SerializationException;
import com.raytheon.uf.common.serialization.SerializationUtil;
import com.raytheon.uf.common.util.FileUtil;
import com.raytheon.uf.common.util.file.FilenameFilters;
import com.raytheon.uf.edex.database.DataAccessLayerException;
import com.raytheon.uf.edex.database.plugin.PluginFactory;

/**
 * The GridAssmebler class is part of the ingest process for grib data. Some
 * grib model come in as octants. This class will combine those octants into a
 * single grid
 * 
 * <pre>
 * 
 * SOFTWARE HISTORY
 * 
 * Date         Ticket#     Engineer    Description
 * ------------ ----------  ----------- --------------------------
 * 4/09/10      4638        bphillip    Initial Creation
 * Mar 14, 2013 1794        djohnson    FileUtil.listFiles now returns List.
 * 
 * </pre>
 * 
 * @author bphillip
 * @version 1
 */
public class NcgridAssembler {

    /** The map of the models that come in sections */
    private Map<String, NccompositeModel> ncthinnedModels;

    /**
     * Creates a new NcgridAssemble instance
     */
    public NcgridAssembler() {
        if (ncthinnedModels == null) {
            //System.out.println(" load nc thin models commonPath:");

            loadNcThinnedModels();
        }
    }

    /**
     * Loads the models from the localization store and stores them in memory
     */
    private void loadNcThinnedModels() {
        ncthinnedModels = new HashMap<String, NccompositeModel>();
        IPathManager pm = PathManagerFactory.getPathManager();
        File commonPath = pm.getFile(pm.getContext(
                LocalizationType.EDEX_STATIC, LocalizationLevel.BASE),
                "/ncgrib/ncthinnedModels");
        
        //System.out.println(" load thin models commonPath=" + commonPath);

        FilenameFilter filter = FilenameFilters.byFilters(
                FilenameFilters.ACCEPT_FILES,
                FilenameFilters.byFileExtension(".xml"));

        List<File> thinnedModelFiles = FileUtil.listFiles(commonPath,
                filter, false);

        for (File file : thinnedModelFiles) {
            //System.out.println(" load thin models file=" + file.getName());

            try {
                NccompositeModel model = (NccompositeModel) SerializationUtil
                        .jaxbUnmarshalFromXmlFile(file.getPath());
                //System.out.println(" load thin models model=" + model.getModelName());

                ncthinnedModels.put(model.getModelName(), model);
            } catch (SerializationException e) {
                e.printStackTrace();
            }
        }
    }

    /**
     * Processes a list of ingested GribRecords
     * 
     * @param records
     *            The records to process
     * @return An array of NcgribRecords
     */
    public synchronized NcgribRecord[] process(NcgribRecord[] records) {

        Map<Integer, NcgribRecord> newRecords = new HashMap<Integer, NcgribRecord>();
        String compositeModel = null;
        for (NcgribRecord rec : records) {
            compositeModel = getNccompositeModel(rec.getModelInfo()
                    .getModelName());
            if (compositeModel != null) {
                try {
                    NcgribRecord newRec = processGrid(rec,
                            getNccompositeModelObject(compositeModel));
                    newRecords.put(newRec.getId(), newRec);
                } catch (Exception e) {
                    // TODO Auto-generated catch block
                    e.printStackTrace();
                }
            }
        }

        NcgribRecord[] recs = new NcgribRecord[records.length
                + newRecords.keySet().size()];
        for (int i = 0; i < records.length; i++) {
            recs[i] = records[i];
            if (i == records.length - 1) {
                for (NcgribRecord newRecord : newRecords.values()) {
                    i++;
                    recs[i] = newRecord;
                }
            }
        }

        return recs;
    }

    /**
     * Gets the composite model name for which the provided model name is a part
     * of
     * 
     * @param modelName
     *            The model name to determine the composite model name for
     * @return The composite model name. Null if not found
     */
    private String getNccompositeModel(String modelName) {
        for (NccompositeModel mod : ncthinnedModels.values()) {
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
    private NccompositeModel getNccompositeModelObject(String modelName) {
        return ncthinnedModels.get(modelName);
    }

    /**
     * Processes a single NcgribRecord
     * 
     * @param record
     *            The NcgribRecord to process
     * @param thinned
     *            The composite model for which the NcgribRecord is a part of
     * @return The new grib record
     * @throws Exception
     */
    private NcgribRecord processGrid(NcgribRecord record, NccompositeModel thinned)
            throws Exception {

        NcgribDao dao = (NcgribDao) PluginFactory.getInstance()
                .getPluginDao("ncgrib");
        String modelName = record.getModelInfo().getModelName();
        String dataURI = record.getDataURI();
        String assembledDataURI = dataURI.replace(modelName, thinned
                .getModelName());

        List<?> result = dao.queryBySingleCriteria("dataURI", assembledDataURI);
        NcgribRecord assembledRecord = null;
        if (result.isEmpty()) {
            assembledRecord = createRecord(record, dao, thinned);
        } else {
            assembledRecord = (NcgribRecord) result.get(0);
            FloatDataRecord rec = (FloatDataRecord) dao.getHDF5Data(
                    assembledRecord, 0)[0];
            assembledRecord.setMessageData(rec);
            assembledRecord.setPluginName("ncgrib");
        }

        mergeData(record, assembledRecord, dao, thinned);
        return assembledRecord;

    }

    /**
     * Merges the data from a NcgribRecord into the composite NcgribRecord
     * 
     * @param record
     *            The NcgribRecord containing the data to add
     * @param assembledRecord
     *            The composite NcgribRecord
     * @param dao
     *            An instance of the grib data access object
     * @param thinned
     *            The composite model definition
     * @return The composite NcgribRecord
     * @throws Exception
     */
    private NcgribRecord mergeData(NcgribRecord record, NcgribRecord assembledRecord,
            NcgribDao dao, NccompositeModel thinned) throws Exception {

        String modelName = record.getModelInfo().getModelName();
        NcgridCoverage coverage = record.getModelInfo().getLocation();

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
                    "Error assembling ncgrids.  Thinned ncgrid definition does not contain "
                            + modelName);
        }
        Util.insertSubgrid(assembledData, Util.resizeDataTo2D((float[]) record
                .getMessageData(), coverage.getNx(), coverage.getNy()), nx
                * modIndex, 0, nx, ny);

        assembledRecord.setMessageData(Util.resizeDataTo1D(assembledData,
                (int) sizes[1], (int) sizes[0]));
        dao.replaceRecord(assembledRecord);
        return assembledRecord;

    }

    /**
     * Creates the composite grib record and stores it to the HDF5 repository
     * 
     * @param record
     *            The recieved NcgribRecord used to initialize the composite grid
     *            with
     * @param dao
     *            An instance of the grib data access object
     * @param thinned
     *            The composite grid definition
     * @return The composite record
     * @throws GribException
     */
    private NcgribRecord createRecord(NcgribRecord record, NcgribDao dao,
            NccompositeModel thinned) throws GribException {
        LatLonNcgridCoverage coverage = (LatLonNcgridCoverage) NcgribSpatialCache
                .getInstance().getGridByName(thinned.getGrid());

        NcgridCoverage gridLoc = record.getModelInfo().getLocation();

        float[] data = new float[(gridLoc.getNx() * 4) * gridLoc.getNy()];
        for(int i = 0; i < data.length;i++){
            data[i] = Util.GRID_FILL_VALUE;
        }
        NcgribRecord newRecord = new NcgribRecord();
        NcgribModel newModel = new NcgribModel(record.getModelInfo());
        newModel.setGridid(coverage.getName());
        newModel.setGridNumber(Integer.parseInt(coverage.getName()));
        newModel.setModelName(thinned.getModelName());
        newModel.setLocation(coverage);
        newModel.generateId();

        try {
            newModel = NcgribModelCache.getInstance().getModel(newModel);
        } catch (DataAccessLayerException e) {
            throw new GribException("Unable to get ncep model info from the cache!",
                    e);
        }
        newRecord.setModelInfo(newModel);
        newRecord.setMessageData(data);
        newRecord.setDataTime(record.getDataTime());
        newRecord.setDataURI(null);
        newRecord.setPluginName("ncgrib");
        newRecord.setInsertTime(Calendar.getInstance());

        try {
            newRecord.constructDataURI();
        } catch (PluginException e) {
            throw new GribException(
                    "Error constructing DataURI for ncgrib record", e);
        }
        try {
            dao.persistToHDF5(newRecord);
            dao.persistToDatabase(newRecord);
            newRecord = (NcgribRecord) dao.getMetadata(newRecord.getDataURI());
            FloatDataRecord rec = (FloatDataRecord) dao.getHDF5Data(newRecord,
                    0)[0];
            newRecord.setMessageData(rec);
            newRecord.setPluginName("ncgrib");
        } catch (PluginException e) {
            throw new GribException("Error storing new ncrecord to HDF5", e);
        }
        return newRecord;
    }
}
