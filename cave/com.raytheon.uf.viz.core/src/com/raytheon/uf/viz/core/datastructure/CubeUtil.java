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
package com.raytheon.uf.viz.core.datastructure;

import java.awt.Point;
import java.io.File;
import java.util.ArrayList;
import java.util.Arrays;
import java.util.HashMap;
import java.util.List;
import java.util.Map;

import com.raytheon.uf.common.dataplugin.PluginDataObject;
import com.raytheon.uf.common.datastorage.DataStoreFactory;
import com.raytheon.uf.common.datastorage.IDataStore;
import com.raytheon.uf.common.datastorage.Request;
import com.raytheon.uf.common.datastorage.records.IDataRecord;
import com.raytheon.uf.common.geospatial.ISpatialEnabled;
import com.raytheon.uf.common.status.IUFStatusHandler;
import com.raytheon.uf.common.status.UFStatus;
import com.raytheon.uf.common.status.UFStatus.Priority;
import com.raytheon.uf.viz.core.Activator;
import com.raytheon.uf.viz.core.HDF5Util;
import com.raytheon.uf.viz.core.exception.VizException;
import com.raytheon.uf.viz.core.status.StatusConstants;

/**
 * Utilities for the data cube
 * 
 * <pre>
 * SOFTWARE HISTORY
 * Date         Ticket#    Engineer    Description
 * ------------ ---------- ----------- --------------------------
 * Jan 16, 2008            njensen     Initial creation
 * Jan 14, 2013 1469       bkowal      The hdf5 root will no longer be appended to the
 *                                     beginning of the file name.
 * 
 * </pre>
 * 
 * @author njensen
 * @version 1.0
 */

public class CubeUtil {
    private static final transient IUFStatusHandler statusHandler = UFStatus.getHandler(CubeUtil.class);

    public static final double MISSING = -9998.0;

    public static final double INVALID = -9999.0;

    /**
     * Gets the file path of an hdf5 file
     * 
     * @param record
     *            the record
     * @param type
     *            the type of record
     * @return the path to the file
     */
    private static String getFilename(PluginDataObject record, String type) {
        String filename = null;
        if (record != null) {
            File file = HDF5Util.findHDF5Location(record);
            if (file != null)
                filename = file.getPath();
        }
        return filename;
    }

    /**
     * Returns the data for the record
     * 
     * @param record
     *            the record
     * @param type
     *            the type of data (e.g. grib)
     * @return the data record
     * @throws VizException
     */
    public static IDataRecord retrieveData(PluginDataObject record, String type)
            throws VizException {
        return retrieveData(record, type, Request.ALL, null);
    }

    public static IDataRecord retrieveData(PluginDataObject record,
            String type, Request req, String dataset) throws VizException {
        IDataRecord dr = null;
        try {
            String fileName = getFilename(record, type);
            String group = record.getDataURI();
            if (dataset == null) {
                dataset = "Data";
            }

            IDataStore ds = DataStoreFactory.getDataStore(new File(fileName));
            dr = ds.retrieve("", group + "/" + dataset, req);
        } catch (Exception e) {
            throw new VizException("Error retrieving data for record.", e);
        }

        return dr;
    }

    /**
     * Populates the records with the first IDataRecord returned from
     * DataCubeContainer.getDataRecord
     * 
     * @param records
     *            records to retrieve data for
     */
    public static void retrieveData(PluginDataObject[] records) {
        for (int i = 0; i < records.length; ++i) {
            try {
                if (records[i] != null && records[i].getMessageData() == null) {
                    IDataRecord[] recs = DataCubeContainer
                            .getDataRecord(records[i]);
                    if (recs != null && recs.length > 0) {
                        records[i].setMessageData(recs[0]);
                    }
                }
            } catch (VizDataCubeException e) {
                statusHandler.handle(Priority.PROBLEM,
                        "Error retrieving data", new VizException(
                                "Error retrieving data for record: "
                                        + records[i].getDataURI(), e));
            }
        }
    }

    /**
     * Returns the data for a set of records at a specific index, uses
     * DataCubeContainer to retrieve all data then indexes into data
     * 
     * @param record
     *            the records to retrieve data from
     * @param x
     *            the x index in the record
     * @param y
     *            the y index in the record
     * @return the data in order of the records
     * @throws VizException
     */
    public static float[] retrieveData(PluginDataObject[] record, int x, int y)
            throws VizException {
        retrieveData(record);
        float[] endResult = new float[record.length];
        for (int i = 0; i < record.length; i++) {
            float val = (float) MISSING;
            if (record[i] != null) {
                int index = y
                        * ((ISpatialEnabled) record[i]).getSpatialObject()
                                .getNx() + x;
                IDataRecord rec = (IDataRecord) record[i].getMessageData();
                if (rec != null) {
                    val = ((float[]) rec.getDataObject())[index];
                }
            }
            endResult[i] = val;
        }
        return endResult;
    }

    /**
     * Returns the data in a set of records for a series of points, uses
     * DataCubeContainer to retrieve data
     * 
     * @param record
     *            the records to retrieve
     * 
     * @param coords
     *            the positions in the data to retrieve
     * @return the data at the positions, in order of each value at the
     *         specified coordinates for one record, then the values at the
     *         coordinates for the next record, and so on
     * @throws VizException
     */
    public static float[] retrieveData(PluginDataObject[] record,
            List<Point> coords) throws VizException {
        retrieveData(record);
        float[] endResult = new float[record.length * coords.size()];

        for (int i = 0; i < record.length; i++) {
            float[] data = null;
            if (record[i] != null) {
                IDataRecord rec = (IDataRecord) record[i].getMessageData();
                if (rec != null) {
                    data = (float[]) rec.getDataObject();
                }
            }
            for (int j = 0; j < coords.size(); ++j) {
                float val = (float) MISSING;
                if (data != null) {
                    Point p = coords.get(j);
                    int index = p.y
                            * ((ISpatialEnabled) record[i]).getSpatialObject()
                                    .getNx() + p.x;
                    val = data[index];
                }
                endResult[i * coords.size() + j] = val;
            }
        }
        return endResult;
    }

    public static float[] efficientRetirevePoint(PluginDataObject[] objects,
            Point point, List<PluginDataObject> newOrderedObjects)
            throws VizException {
        float[] rval = new float[objects.length];
        Map<String, List<PluginDataObject>> fileMap = new HashMap<String, List<PluginDataObject>>();

        for (PluginDataObject pdo : objects) {
            String file = getFilename(pdo, pdo.getPluginName());
            if (file != null) {
                List<PluginDataObject> objs = fileMap.get(file);
                if (objs == null) {
                    objs = new ArrayList<PluginDataObject>();
                    fileMap.put(file, objs);
                }
                objs.add(pdo);
            }
        }

        int index = 0;
        for (String file : fileMap.keySet()) {
            List<PluginDataObject> objs = fileMap.get(file);
            String[] groups = new String[objs.size()];
            int i = 0;
            for (PluginDataObject obj : objs) {
                groups[i++] = obj.getDataURI();
                newOrderedObjects.add(obj);
            }
            IDataRecord[] dr = null;

            try {
                IDataStore ds = DataStoreFactory.getDataStore(new File(file));
                Request request = Request
                        .buildPointRequest(new Point[] { point });
                dr = ds.retrieveGroups(groups, request);

                for (int k = 0; k < dr.length; k++, index++) {
                    float[] data = (float[]) dr[k].getDataObject();
                    rval[index] = data[0];
                }
            } catch (Exception e) {
                throw new VizException("Error retrieving data for record.", e);
            }

        }

        return rval;
    }

    public static float[][] efficientRetireve(PluginDataObject[] objects,
            List<PluginDataObject> newOrderedObjects) throws VizException {
        float[][] rval = new float[objects.length][];
        Map<String, List<PluginDataObject>> fileMap = new HashMap<String, List<PluginDataObject>>();

        for (PluginDataObject pdo : objects) {
            String file = getFilename(pdo, pdo.getPluginName());
            if (file != null) {
                List<PluginDataObject> objs = fileMap.get(file);
                if (objs == null) {
                    objs = new ArrayList<PluginDataObject>();
                    fileMap.put(file, objs);
                }
                objs.add(pdo);
            }
        }

        int index = 0;
        for (String file : fileMap.keySet()) {
            List<PluginDataObject> objs = fileMap.get(file);
            String[] groups = new String[objs.size()];
            int i = 0;
            for (PluginDataObject obj : objs) {
                groups[i++] = obj.getDataURI();
                newOrderedObjects.add(obj);
            }
            IDataRecord[] dr = null;

            try {
                IDataStore ds = DataStoreFactory.getDataStore(new File(file));
                Request request = Request.ALL;
                dr = ds.retrieveGroups(groups, request);

                for (int k = 0; k < dr.length; k++, index++) {
                    float[] data = (float[]) dr[k].getDataObject();
                    rval[index] = data;
                }
            } catch (Exception e) {
                throw new VizException("Error retrieving data for record.", e);
            }

        }

        return rval;
    }

    public static List<IDataRecord> retrieveData(List<PluginDataObject> objects)
            throws VizException {
        Map<String, List<PluginDataObject>> fileMap = new HashMap<String, List<PluginDataObject>>();
        for (PluginDataObject pdo : objects) {
            String file = getFilename(pdo, pdo.getPluginName());
            if (file != null) {
                List<PluginDataObject> objs = fileMap.get(file);
                if (objs == null) {
                    objs = new ArrayList<PluginDataObject>();
                    fileMap.put(file, objs);
                }
                objs.add(pdo);
            }
        }

        IDataRecord[] records = new IDataRecord[objects.size()];

        for (String file : fileMap.keySet()) {
            List<PluginDataObject> objs = fileMap.get(file);
            String[] groups = new String[objs.size()];

            for (int i = 0; i < objs.size(); i++) {
                groups[i] = objs.get(i).getDataURI();
            }

            try {
                IDataStore ds = DataStoreFactory.getDataStore(new File(file));
                Request request = Request.ALL;
                IDataRecord[] dr = ds.retrieveGroups(groups, request);

                for (int i = 0; i < dr.length; i++) {
                    records[objects.indexOf(objs.get(i))] = dr[i];
                }
            } catch (Exception e) {
                throw new VizException("Error retrieving data for record.", e);
            }

        }
        return Arrays.asList(records);
    }
}
