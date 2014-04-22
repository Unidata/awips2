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
package com.raytheon.uf.edex.pointdata;

import java.awt.Point;
import java.io.File;
import java.io.FileNotFoundException;
import java.io.InputStream;
import java.util.ArrayList;
import java.util.Arrays;
import java.util.Date;
import java.util.HashMap;
import java.util.List;
import java.util.Map;
import java.util.Set;
import java.util.concurrent.LinkedBlockingQueue;

import javax.xml.bind.JAXBException;

import net.sf.cglib.beans.BeanMap;

import com.raytheon.uf.common.dataplugin.PluginDataObject;
import com.raytheon.uf.common.dataplugin.PluginException;
import com.raytheon.uf.common.dataplugin.persist.DefaultPathProvider;
import com.raytheon.uf.common.dataplugin.persist.IPersistable;
import com.raytheon.uf.common.datastorage.DataStoreFactory;
import com.raytheon.uf.common.datastorage.IDataStore;
import com.raytheon.uf.common.datastorage.IDataStore.StoreOp;
import com.raytheon.uf.common.datastorage.Request;
import com.raytheon.uf.common.datastorage.StorageException;
import com.raytheon.uf.common.datastorage.StorageProperties;
import com.raytheon.uf.common.datastorage.StorageStatus;
import com.raytheon.uf.common.datastorage.records.IDataRecord;
import com.raytheon.uf.common.datastorage.records.IntegerDataRecord;
import com.raytheon.uf.common.pointdata.IPointData;
import com.raytheon.uf.common.pointdata.PointDataContainer;
import com.raytheon.uf.common.pointdata.PointDataDescription;
import com.raytheon.uf.common.pointdata.PointDataView;
import com.raytheon.uf.common.serialization.SerializationException;
import com.raytheon.uf.common.status.IUFStatusHandler;
import com.raytheon.uf.common.status.UFStatus;
import com.raytheon.uf.common.time.DataTime;
import com.raytheon.uf.edex.core.dataplugin.PluginRegistry;
import com.raytheon.uf.edex.database.plugin.PluginDao;

/**
 * Provides an extension to PluginDao that provides access for PointData types
 * 
 * <pre>
 * 
 * SOFTWARE HISTORY
 * Date          Ticket#  Engineer    Description
 * ------------- -------- ----------- -----------------------------------------
 * Apr 13, 2009           chammack    Initial creation
 * Jan 14, 2013  1469     bkowal      Removed the hdf5 data directory.
 * Feb 27, 2013  1638     mschenke    Switched logger to use statusHandler
 * Apr 15, 2013  1868     bsteffen    Rewrite mergeAll in PluginDao.
 * Apr 29, 2013  1861     bkowal      Refactor hdf5 filename generation during
 *                                    reads into its own method so modelsounding
 *                                    dao can override it.
 * Jan 03, 2014  2309     bsteffen    Allow fcstTime in hdf5 path.
 * Jan 09, 2014 1998       clement    fixed NPE in persistToHDF5 when store failed
 * 
 * </pre>
 * 
 * @author chammack
 * @version 1.0
 */

public abstract class PointDataPluginDao<T extends PluginDataObject> extends
        PluginDao {
    private static final transient IUFStatusHandler statusHandler = UFStatus
            .getHandler(PointDataPluginDao.class);

    public static enum LevelRequest {
        ALL, NONE, SPECIFIC;

        private String parameter;

        private double[] values;

        public void setLevels(String parameter, double[] values) {
            if (this != SPECIFIC) {
                throw new IllegalArgumentException(
                        "Can't specify specific levels for level + "
                                + this.name());
            }

            this.parameter = parameter;
            this.values = values;
        }

        /**
         * @return the parameter
         */
        public String getParameter() {
            return parameter;
        }

        /**
         * @return the values
         */
        public double[] getValues() {
            return values;
        }

    };

    private final LinkedBlockingQueue<BeanMap> beanMapCache;

    protected PointDataDbDescription dbDataDescription;

    protected PointDataDescription hdf5DataDescription;

    public PointDataPluginDao(String pluginName) throws PluginException {
        super(pluginName);
        this.pathProvider = new PointDataHDFFileProvider();
        this.beanMapCache = new LinkedBlockingQueue<BeanMap>();
    }

    /*
     * (non-Javadoc)
     * 
     * @see
     * com.raytheon.edex.db.dao.PluginDao#persistToHDF5(com.raytheon.uf.common
     * .dataplugin.PluginDataObject[])
     */
    @Override
    public StorageStatus persistToHDF5(PluginDataObject... records)
            throws PluginException {
        long t0 = System.currentTimeMillis();

        // NOTE: currently making the assumption that models aren't
        // mixed in the records aggregate. If this isn't true,
        // some pre-processing will be needed.
        Map<PointDataContainer, List<PointDataView>> containerMap = new HashMap<PointDataContainer, List<PointDataView>>(
                records.length);
        Map<PointDataContainer, File> fileMap = new HashMap<PointDataContainer, File>();

        for (PluginDataObject p : records) {
            if (p instanceof IPointData) {
                PointDataView pdv = ((IPointData) p).getPointDataView();
                List<PointDataView> views = containerMap
                        .get(pdv.getContainer());
                if (views == null) {
                    views = new ArrayList<PointDataView>();
                    containerMap.put(pdv.getContainer(), views);
                }
                views.add(pdv);
                File file = fileMap.get(pdv.getContainer());
                if (file == null) {
                    file = getFullFilePath(p);
                    fileMap.put(pdv.getContainer(), file);
                }

            }
        }

        List<StorageStatus> ssList = new ArrayList<StorageStatus>();
        try {
            for (PointDataContainer container : containerMap.keySet()) {
                IDataStore ds = DataStoreFactory.getDataStore(fileMap
                        .get(container));
                StorageProperties sp = new StorageProperties();
                String compression = PluginRegistry.getInstance()
                        .getRegisteredObject(pluginName).getCompression();
                if (compression != null) {
                    sp.setCompression(StorageProperties.Compression
                            .valueOf(compression));
                }

                Set<String> params = container.getParameters();
                for (String param : params) {
                    try {
                        IDataRecord idr = container.getParameterRecord(param);
                        ds.addDataRecord(idr, sp);
                    } catch (StorageException e) {
                        throw new PluginException("Error adding record", e);
                    }
                }

                try {
                    StorageStatus ss = ds.store(StoreOp.APPEND);
                    if (!ss.hasExceptions()
                            && ss.getOperationPerformed() == StoreOp.APPEND) {
                        // increment the indices
                        List<PointDataView> views = containerMap.get(container);
                        int idx = (int) ss.getIndexOfAppend()[0];
                        container.incrementIds(idx, views);
                    }
                    ssList.add(ss);
                } catch (StorageException e) {
                    throw new PluginException("Error updating point file", e);
                }
            }
            // Aggregate the storage status errors
            StorageStatus aggregatedStatus = new StorageStatus();
            List<StorageException> se = new ArrayList<StorageException>();
            for (StorageStatus ss : ssList) {
                if (ss.hasExceptions()) {
                    se.addAll(Arrays.asList(ss.getExceptions()));
                }
            }

            aggregatedStatus.setExceptions(se.toArray(new StorageException[se
                    .size()]));
            return aggregatedStatus;
        }

        finally {
            System.out.println("Time spent in persist: "
                    + (System.currentTimeMillis() - t0));
        }

    }

    public File getFullFilePath(PluginDataObject p) {
        File file;
        String directory = p.getPluginName() + File.separator
                + pathProvider.getHDFPath(p.getPluginName(), (IPersistable) p);
        file = new File(directory
                + File.separator
                + pathProvider.getHDFFileName(p.getPluginName(),
                        (IPersistable) p));
        return file;
    }

    @Override
    protected IDataStore populateDataStore(IDataStore dataStore,
            IPersistable obj) throws Exception {
        // TODO Auto-generated method stub
        return null;
    }

    public PointDataDbDescription getPointDataDbDescription() {
        if (dbDataDescription == null) {
            InputStream stream = this.getClass().getResourceAsStream(
                    "/res/pointdata/" + pluginName + "db.xml");
            if (stream != null) {
                try {
                    dbDataDescription = PointDataDbDescription
                            .fromStream(stream);
                } catch (JAXBException e) {
                    statusHandler.error("Unable to load " + pluginName
                            + " Point Data Database Description", e);
                }
            }
        }
        return dbDataDescription;
    }

    public PointDataDescription getPointDataDescription(Map<String, Object> obj) {
        if (hdf5DataDescription == null) {
            try {
                hdf5DataDescription = PointDataDescription.fromStream(this
                        .getClass().getResourceAsStream(
                                "/res/pointdata/" + pluginName + ".xml"));
            } catch (SerializationException e) {
                statusHandler.error("Unable to load " + pluginName
                        + " Point Data Description", e);
            }
        }
        return hdf5DataDescription;
    }

    private static class IndexIdPair implements Comparable<IndexIdPair> {
        public int index;

        public int id;

        @Override
        public int compareTo(IndexIdPair o) {
            if (index == o.index) {
                return 0;
            }

            return index < o.index ? -1 : 1;
        }

    }

    public PointDataContainer getPointData(File file, int[] indexes, int[] ids,
            String[] attributes, LevelRequest request) throws StorageException,
            FileNotFoundException {

        IndexIdPair[] iip = new IndexIdPair[ids.length];
        for (int i = 0; i < iip.length; i++) {
            iip[i] = new IndexIdPair();
            iip[i].index = indexes[i];
            iip[i].id = ids[i];
        }

        Arrays.sort(iip);

        for (int i = 0; i < iip.length; i++) {
            indexes[i] = iip[i].index;
            ids[i] = iip[i].id;
        }

        // For now, because the levels could be at different indices throughout,
        // for now we will retrieve all levels and then post-process the result

        IDataStore ds = DataStoreFactory.getDataStore(file);
        Point[] pts = new Point[indexes.length];

        Request dsRequest = null;
        if (request == LevelRequest.NONE) {
            for (int i = 0; i < indexes.length; i++) {
                pts[i] = new Point(indexes[i], 0);
            }
            dsRequest = Request.buildPointRequest(pts);
        } else if ((request == LevelRequest.ALL)
                || (request == LevelRequest.SPECIFIC)) {
            int[] copy = new int[indexes.length];
            System.arraycopy(indexes, 0, copy, 0, indexes.length);
            dsRequest = Request.buildYLineRequest(copy);
        } else {
            throw new IllegalArgumentException("Unknown LevelRequest: "
                    + request);
        }

        long t0 = System.currentTimeMillis();
        IDataRecord[] recs = ds.retrieveDatasets(attributes, dsRequest);
        long t1 = System.currentTimeMillis();
        System.out.println("Time spent on pointdata hdf5 retrieval from file "
                + file.getPath() + ": " + (t1 - t0));

        List<IDataRecord> recList = new ArrayList<IDataRecord>();
        if (request != LevelRequest.SPECIFIC) {
            recList.addAll(Arrays.asList(recs));
        } else {
            // Post process specific level request
            String parameter = request.getParameter();
            if (parameter == null) {
                throw new IllegalArgumentException(
                        "Specific level requested without parameter specified");
            }

            double[] vals = request.getValues();
            if ((vals == null) || (vals.length == 0)) {
                throw new IllegalArgumentException(
                        "Specific level requested without values specified");
            }

            IDataRecord rec = null;
            for (IDataRecord dr : recs) {
                if (dr.getName().equals(parameter)) {
                    rec = dr;
                    break;
                }
            }

            if (rec == null) {
                throw new IllegalArgumentException(
                        "Specific level parameter not present in return data");
            }

            // Build up a list of 1D indices we want to save
            Object dataObj = rec.getDataObject();
            int[] indices = new int[(int) rec.getSizes()[1]];
            Arrays.fill(indices, -1);

            if (dataObj instanceof int[]) {
                int[] intData = (int[]) dataObj;
                int dimX = (int) rec.getSizes()[0];
                int dimY = (int) rec.getSizes()[1];

                int idx = 0;
                double v = 0;
                for (int i = 0; i < dimY; i++) {
                    nextData: for (int j = 0; j < dimX; j++) {
                        idx = dimX * dimY + j;
                        v = intData[idx];
                        for (int k = 0; k < vals.length; k++) {
                            if (v == vals[k]) {
                                indices[i] = idx;
                                break nextData;
                            }
                        }
                    }

                }
            } else if (dataObj instanceof float[]) {
                float[] floatData = (float[]) dataObj;
                int dimX = (int) rec.getSizes()[0];
                int dimY = (int) rec.getSizes()[1];

                int idx = 0;
                double v = 0;
                for (int i = 0; i < dimY; i++) {
                    nextData: for (int j = 0; j < dimX; j++) {
                        idx = dimX * dimY + j;
                        v = floatData[idx];
                        for (int k = 0; k < vals.length; k++) {
                            if (v == vals[k]) {
                                indices[i] = idx;
                                break nextData;
                            }
                        }
                    }

                }
            } else {
                throw new IllegalArgumentException(
                        "Unhandled level data type: " + dataObj);
            }

            for (IDataRecord dr : recs) {
                dr.reduce(indices);
                recList.add(dr);
            }

        }

        // Correlate the ids from the indexes that were actually retrieved
        // these can actually be different than what was requested
        int[] retrievedIndexes = null;
        if (dsRequest.getType() == Request.Type.YLINE) {
            retrievedIndexes = dsRequest.getIndices();
        } else if (dsRequest.getType() == Request.Type.POINT) {
            Point[] retrievedPoints = dsRequest.getPoints();
            retrievedIndexes = new int[retrievedPoints.length];
            for (int i = 0; i < retrievedIndexes.length; i++) {
                retrievedIndexes[i] = retrievedPoints[i].x;
            }
        }

        int[] correlatedIds = new int[retrievedIndexes.length];
        int originalPointer = 0;
        for (int i = 0; i < correlatedIds.length; i++) {
            int k;
            search: for (k = originalPointer; k < iip.length; k++) {
                if (iip[k].index == retrievedIndexes[i]) {
                    correlatedIds[i] = iip[k].id;
                    originalPointer = k + 1;
                    break search;
                }
            }

            if (k >= iip.length) {
                // went off the end of search. double check the other half of
                // the array
                boolean found = false;
                search2: for (k = 0; (k < originalPointer) && (k < iip.length); k++) {
                    if (iip[k].index == retrievedIndexes[i]) {
                        correlatedIds[i] = iip[k].id;
                        break search2;
                    }
                }

                if (!found) {
                    correlatedIds[i] = -1;
                }
            }

        }

        IntegerDataRecord idr = new IntegerDataRecord("id", "", correlatedIds);
        recList.add(idr);
        return PointDataContainer.build(recList.toArray(new IDataRecord[recList
                .size()]));
    }

    public abstract String[] getKeysRequiredForFileName();

    @SuppressWarnings("unchecked")
    public String getPointDataFileName(Map<String, Object> obj) {
        BeanMap bm = this.beanMapCache.poll();
        if (bm == null) {
            bm = BeanMap.create(newObject());
        }
        try {
            if (obj.containsKey("dataTime.refTime")) {
                Date d = (Date) obj.remove("dataTime.refTime");
                DataTime dt = null;
                if (obj.containsKey("dataTime.fcstTime")) {
                    int fcstTime = (Integer) obj.remove("dataTime.fcstTime");
                    dt = new DataTime(d, fcstTime);
                }else{
                    dt = new DataTime(d);
                }
                obj.put("dataTime", dt);
            }
            bm.putAll(obj);
            T bean = (T) bm.getBean();
            return this.generatePointDataFileName(bean);
        } finally {
            this.beanMapCache.offer(bm);
        }
    }

    protected String generatePointDataFileName(T bean) {
        return this.pluginName
                + File.separator
                + this.pathProvider.getHDFPath(this.pluginName,
                        (IPersistable) bean)
                + File.separator
                + getPointDataFileName(bean).replace(".h5", "")
                + DefaultPathProvider.fileNameFormat.get().format(
                        ((PluginDataObject) bean).getDataTime().getRefTime())
                + ".h5";
    }

    public abstract T newObject();

    public abstract String getPointDataFileName(T p);

    public class PointDataHDFFileProvider extends DefaultPathProvider {

        @SuppressWarnings("unchecked")
        @Override
        public String getHDFFileName(String pluginName, IPersistable persistable) {
            StringBuilder tmp = new StringBuilder(getPointDataFileName(
                    (T) persistable).replace(".h5", ""));
            Date refTime = ((PluginDataObject) persistable).getDataTime()
                    .getRefTime();
            tmp.append(fileNameFormat.get().format(refTime));
            tmp.append(".h5");
            return tmp.toString();
        }

    }

}
