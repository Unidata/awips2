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

import java.io.File;
import java.util.ArrayList;
import java.util.Arrays;
import java.util.HashMap;
import java.util.HashSet;
import java.util.List;
import java.util.Map;
import java.util.Set;

import com.raytheon.edex.uengine.tasks.query.TableQuery;
import com.raytheon.uf.common.dataplugin.PluginException;
import com.raytheon.uf.common.datastorage.records.FloatDataRecord;
import com.raytheon.uf.common.datastorage.records.IDataRecord;
import com.raytheon.uf.common.datastorage.records.IntegerDataRecord;
import com.raytheon.uf.common.datastorage.records.LongDataRecord;
import com.raytheon.uf.common.datastorage.records.StringDataRecord;
import com.raytheon.uf.common.message.response.ResponseMessageCatalog;
import com.raytheon.uf.common.pointdata.ParameterDescription;
import com.raytheon.uf.common.pointdata.PointDataContainer;
import com.raytheon.uf.common.pointdata.PointDataDescription;
import com.raytheon.uf.common.pointdata.PointDataView;
import com.raytheon.uf.edex.database.DataAccessLayerException;
import com.raytheon.uf.edex.database.plugin.PluginDao;
import com.raytheon.uf.edex.database.plugin.PluginFactory;
import com.raytheon.uf.edex.pointdata.PointDataPluginDao.LevelRequest;

/**
 * A query task for accessing point data
 * 
 * <pre>
 * 
 * SOFTWARE HISTORY
 * Date         Ticket#    Engineer    Description
 * ------------ ---------- ----------- --------------------------
 * Apr 15, 2009            chammack     Initial creation
 * 
 * </pre>
 * 
 * @author chammack
 * @version 1.0
 */

public class PointDataQuery {

    protected PointDataPluginDao<?> dao;

    protected TableQuery tq;

    protected String[] attribs;

    protected PointDataPluginDao.LevelRequest requestStyle = LevelRequest.NONE;

    public PointDataQuery(final String plugin) throws DataAccessLayerException,
            PluginException {
        this.tq = new TableQuery(PluginFactory.getInstance()
                .getDatabase(plugin), PluginFactory.getInstance()
                .getPluginRecordClass(plugin).getName());
        try {
            PluginDao pd = PluginFactory.getInstance().getPluginDao(plugin);
            if (!(pd instanceof PointDataPluginDao)) {
                throw new PluginException(plugin
                        + " DAO is not a point data DAO");
            }
            this.dao = (PointDataPluginDao<?>) pd;
        } catch (Exception e) {
            e.printStackTrace();
            throw new DataAccessLayerException(
                    "Unable to instantiate data access object on database: "
                            + plugin, e);
        }
    }

    public void setParameters(final String params) {
        String[] strs = params.split(",");
        for (int i = 0; i < strs.length; i++) {
            strs[i] = strs[i].trim();
        }
        this.attribs = strs;
    }

    public void addParameter(final String name, final String value,
            String operand) {
        if (operand == null) {
            operand = "=";
        }

        tq.addParameter(name, value, operand);

    }

    public void requestAllLevels() {
        this.requestStyle = LevelRequest.ALL;
    }

    public void requestSpecificLevel(final String parameter, final String vals) {
        String[] valList = vals.split(",");
        double[] d = new double[valList.length];
        for (int i = 0; i < d.length; i++) {
            d[i] = Double.parseDouble(valList[i]);
        }

        this.requestStyle = LevelRequest.SPECIFIC;
        this.requestStyle.setLevels(parameter, d);
    }

    public ResponseMessageCatalog getAvailableParameters() throws Exception {

        Set<String> parameters = new HashSet<String>();

        PointDataDbDescription dbDesc = dao.getPointDataDbDescription();
        if (dbDesc != null) {
            for (DbParameterDescription parameter : dbDesc.parameters) {
                parameters.add(parameter.getParameterName());
            }
        }
        boolean needsDbQuery = false;
        for (String key : dao.getKeysRequiredForFileName()) {
            if (!key.equals("dataTime.refTime")) {
                needsDbQuery = true;
                break;
            }
        }
        if (needsDbQuery) {
            for (Map<String, Object> workingMap : performDbQuery(
                    Arrays.asList(dao.getKeysRequiredForFileName()), 1)) {
                PointDataDescription desc = dao
                        .getPointDataDescription(workingMap);
                if (desc != null) {
                    for (ParameterDescription param : desc.parameters) {
                        parameters.add(param.getParameterName());
                    }
                }
            }
        } else {
            PointDataDescription desc = dao.getPointDataDescription(null);
            if (desc != null) {
                for (ParameterDescription param : desc.parameters) {
                    parameters.add(param.getParameterName());
                }
            }
        }

        ResponseMessageCatalog cat = new ResponseMessageCatalog();
        cat.setValues(parameters.toArray(new String[0]));

        return cat;
    }

    private List<Map<String, Object>> performDbQuery(final List<String> fields,
            final int limit) throws Exception {

        for (String field : fields) {
            tq.addReturnedField(field, null);
        }
        tq.setCount(limit);
        List<?> queryResults = tq.execute();

        List<Map<String, Object>> results = new ArrayList<Map<String, Object>>();

        for (Object o : queryResults) {
            Map<String, Object> workingMap = new HashMap<String, Object>();
            if (o instanceof Object[]) {
                Object[] oArr = (Object[]) o;
                for (int i = 0; i < fields.size(); i++) {
                    workingMap.put(fields.get(i), oArr[i]);
                }
            } else if (fields.size() == 1) {
                workingMap.put(fields.get(0), o);
            }
            results.add(workingMap);
        }

        return results;

    }

    /*
     * (non-Javadoc)
     * 
     * @see com.raytheon.edex.uengine.tasks.query.TableQuery#execute()
     */
    public PointDataContainer execute() throws Exception {

        List<String> hdf5attribList = new ArrayList<String>();
        HashSet<String> dbAttribSet = new HashSet<String>();
        List<DbParameterDescription> dbParamDesc = new ArrayList<DbParameterDescription>();

        PointDataDbDescription dbDesc = dao.getPointDataDbDescription();
        if (dbDesc == null) {
            hdf5attribList.addAll(Arrays.asList(attribs));
        } else {
            for (String attrib : attribs) {
                DbParameterDescription desc = dbDesc.getDescription(attrib);
                if (desc != null) {
                    dbAttribSet.add(desc.getQueryName());
                    dbParamDesc.add(desc);
                } else {
                    hdf5attribList.add(attrib);
                }
            }
        }

        dbAttribSet.add("id");
        if (!hdf5attribList.isEmpty()) {
            dbAttribSet.add("pointDataView.curIdx");
            dbAttribSet.addAll(Arrays.asList(dao.getKeysRequiredForFileName()));
        }

        List<Map<String, Object>> dbResults = performDbQuery(
                new ArrayList<String>(dbAttribSet), 999999);

        if ((dbResults == null) || dbResults.isEmpty()) {
            return null;
        }

        Map<Integer, Map<String, Object>> dbResultMap = new HashMap<Integer, Map<String, Object>>();
        PointDataContainer masterPDC = null;

        if (hdf5attribList.isEmpty()) {
            int[] idArr = new int[dbResults.size()];
            for (int j = 0; j < dbResults.size(); j++) {
                Map<String, Object> workingMap = dbResults.get(j);
                idArr[j] = (Integer) workingMap.get("id");
                dbResultMap.put(idArr[j], workingMap);
            }
            masterPDC = PointDataContainer
                    .build(new IDataRecord[] { new IntegerDataRecord("id", "",
                            idArr) });
            masterPDC.setCurrentSz(masterPDC.getAllocatedSz());
        } else {
            List<String> files = new ArrayList<String>();
            List<List<Integer>> ids = new ArrayList<List<Integer>>();
            List<List<Integer>> indexes = new ArrayList<List<Integer>>();

            for (Map<String, Object> workingMap : dbResults) {
                int id = (Integer) workingMap.get("id");
                int idx = (Integer) workingMap.get("pointDataView.curIdx");
                dbResultMap.put(id, workingMap);
                String fileName = dao.getPointDataFileName(workingMap);
                int listIndex = files.indexOf(fileName);
                if (listIndex == -1) {
                    listIndex = files.size();
                    files.add(fileName);
                    ids.add(new ArrayList<Integer>());
                    indexes.add(new ArrayList<Integer>());
                    hdf5attribList.retainAll(Arrays.asList(dao
                            .getPointDataDescription(workingMap)
                            .getParameterNames()));
                }
                ids.get(listIndex).add(id);
                indexes.get(listIndex).add(idx);
            }
            long t0 = System.currentTimeMillis();
            for (int i = 0; i < files.size(); i++) {
                File file = new File(files.get(i));
                List<String> attribSet = new ArrayList<String>(hdf5attribList);
                int[] idxArr = new int[indexes.get(i).size()];
                int[] idArr = new int[ids.get(i).size()];
                for (int j = 0; j < idArr.length; j++) {
                    idxArr[j] = indexes.get(i).get(j);
                    idArr[j] = ids.get(i).get(j);
                }
                PointDataContainer pdc = dao.getPointData(file, idxArr, idArr,
                        attribSet.toArray(new String[0]), this.requestStyle);
                if (masterPDC == null) {
                    masterPDC = pdc;
                    masterPDC.setCurrentSz(masterPDC.getAllocatedSz());
                } else {
                    masterPDC.combine(pdc);
                    masterPDC.setCurrentSz(masterPDC.getAllocatedSz());
                }
            }
            long t1 = System.currentTimeMillis();
            System.out
                    .println("Total time spent on pointdata hdf5 retrieval (all files): "
                            + (t1 - t0));
        }

        if (!dbParamDesc.isEmpty()) {
            for (DbParameterDescription desc : dbParamDesc) {
                switch (desc.getType()) {
                case FLOAT:
                    float[] fdata = new float[masterPDC.getCurrentSz()];
                    FloatDataRecord frec = new FloatDataRecord(
                            desc.getParameterName(), "", fdata);
                    if (desc.getFillValue() != null) {
                        frec.setFillValue(Float.parseFloat(desc.getFillValue()));
                    }
                    masterPDC.add(frec, desc.getUnit());
                    break;
                case INT:
                    int[] idata = new int[masterPDC.getCurrentSz()];
                    masterPDC
                            .add(new IntegerDataRecord(desc.getParameterName(),
                                    "", idata), desc.getUnit());
                    break;
                case LONG:
                    long[] ldata = new long[masterPDC.getCurrentSz()];
                    masterPDC.add(new LongDataRecord(desc.getParameterName(),
                            "", ldata), desc.getUnit());
                    break;
                case STRING:
                    String[] sdata = new String[masterPDC.getCurrentSz()];
                    masterPDC.add(new StringDataRecord(desc.getParameterName(),
                            "", sdata), desc.getUnit());
                    break;
                }
            }
            for (int i = 0; i < masterPDC.getAllocatedSz(); i++) {
                PointDataView pdv = masterPDC.readRandom(i);
                Map<String, Object> dbMap = dbResultMap.get(pdv.getInt("id"));
                for (DbParameterDescription desc : dbParamDesc) {
                    Object obj = dbMap.get(desc.getQueryName());
                    if (obj == null) {
                        obj = pdv.getContainer()
                                .getParameterRecord(desc.getParameterName())
                                .getFillValue();
                        if (obj == null) {
                            continue;
                        }
                    }
                    switch (desc.getType()) {
                    case FLOAT:
                        pdv.setFloat(desc.getParameterName(),
                                ((Number) obj).floatValue());
                        break;
                    case INT:

                        pdv.setInt(desc.getParameterName(),
                                ((Number) obj).intValue());
                        break;
                    case LONG:
                        pdv.setLong(desc.getParameterName(),
                                ((Number) obj).longValue());
                        break;
                    case STRING:
                        pdv.setString(desc.getParameterName(), obj.toString());
                        break;
                    }
                }
            }
        }

        return masterPDC;
    }
}

