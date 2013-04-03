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
package com.raytheon.edex.plugin.grib.handler;

import java.sql.Timestamp;
import java.util.Arrays;
import java.util.List;

import com.raytheon.uf.common.dataplugin.PluginDataObject;
import com.raytheon.uf.common.dataplugin.grib.GribThriftContainer;
import com.raytheon.uf.common.dataplugin.grib.GribThriftRecord;
import com.raytheon.uf.common.dataplugin.grib.request.GridDataRequestMessage;
import com.raytheon.uf.common.dataplugin.grid.GridConstants;
import com.raytheon.uf.common.dataplugin.grid.GridRecord;
import com.raytheon.uf.common.dataplugin.level.Level;
import com.raytheon.uf.common.dataquery.db.QueryParam.QueryOperand;
import com.raytheon.uf.common.datastorage.records.IDataRecord;
import com.raytheon.uf.common.serialization.comm.IRequestHandler;
import com.raytheon.uf.common.time.DataTime;
import com.raytheon.uf.edex.database.query.DatabaseQuery;
import com.raytheon.uf.edex.plugin.grid.dao.GridDao;

/**
 * TODO Add Description
 * 
 * <pre>
 * 
 * SOFTWARE HISTORY
 * Date         Ticket#    Engineer    Description
 * ------------ ---------- ----------- --------------------------
 * Jul 12, 2010            brockwoo     Initial creation
 * 
 * </pre>
 * 
 * @author brockwoo
 * @version 1.0
 */
public class GridDataHandler implements IRequestHandler<GridDataRequestMessage> {

    @Override
    public Object handleRequest(GridDataRequestMessage request)
            throws Exception {

        GridDao dao = new GridDao();
        DatabaseQuery query = new DatabaseQuery(GridRecord.class);

        if (request.getModelName() != null
                && !"".equals(request.getModelName())) {
            query.addQueryParam(GridConstants.DATASET_ID,
                    request.getModelName(), QueryOperand.EQUALS);
        }

        if (request.getLevelOne() != Level.INVALID_VALUE) {
            query.addQueryParam(GridConstants.LEVEL_ONE, request.getLevelOne(),
                    QueryOperand.EQUALS);
            if (request.getLevelType() != null) {
                query.addQueryParam(GridConstants.MASTER_LEVEL_NAME,
                        request.getLevelType(), QueryOperand.EQUALS);
            }
            query.addQueryParam(GridConstants.LEVEL_TWO, request.getLevelTwo(),
                    QueryOperand.EQUALS);
        }

        if (request.getParameterAbbreviation() != null
                && !"".equals(request.getParameterAbbreviation())) {
            query.addQueryParam(GridConstants.PARAMETER_ABBREVIATION,
                    request.getParameterAbbreviation(), QueryOperand.EQUALS);
        }

        if (request.getStartTime() != GridDataRequestMessage.MISSING) {
            Timestamp stamp = new Timestamp(request.getStartTime());
            DataTime newTime = new DataTime(stamp, request.getForecastTime());
            query.addQueryParam("dataTime", newTime, QueryOperand.EQUALS);
        }

        GribThriftContainer container = new GribThriftContainer();
        try {
            PluginDataObject[] records = null;
            records = dao.getFullRecord(query, -1);

            if (records != null && records.length > 0) {
                List<IDataRecord[]> hdfRecords = dao.getHDF5Data(
                        Arrays.asList(records), -1);
                if (hdfRecords.size() != records.length) {
                    container.setNumOfRecords(-1);
                    return container;
                }
                GribThriftRecord thriftRecords[] = new GribThriftRecord[records.length];
                for (int i = 0; i < records.length; i++) {
                    thriftRecords[i] = new GribThriftRecord();
                    IDataRecord[] data = hdfRecords.get(i);
                    GridRecord thisGribRecord = (GridRecord) records[i];
                    GridDataRequestMessage thisGribInfo = new GridDataRequestMessage();
                    thisGribInfo.setInfoFromRecord(thisGribRecord);
                    thriftRecords[i].setMessage(thisGribInfo);
                    thriftRecords[i].setDataFields(data);
                }
                container.setRecords(thriftRecords);
            }
        } catch (Exception e) {
            container.setNumOfRecords(-1);
        }
        return container;
    }
}
