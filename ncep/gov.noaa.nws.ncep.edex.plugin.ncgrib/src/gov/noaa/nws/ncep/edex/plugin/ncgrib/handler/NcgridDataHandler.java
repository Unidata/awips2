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
package gov.noaa.nws.ncep.edex.plugin.ncgrib.handler;

import java.sql.Timestamp;
import java.util.Arrays;
import java.util.List;

import gov.noaa.nws.ncep.edex.plugin.ncgrib.dao.NcgribDao;
import com.raytheon.uf.common.dataplugin.PluginDataObject;
import gov.noaa.nws.ncep.common.dataplugin.ncgrib.NcgribRecord;
import gov.noaa.nws.ncep.common.dataplugin.ncgrib.NcgribThriftContainer;
import gov.noaa.nws.ncep.common.dataplugin.ncgrib.NcgribThriftRecord;
import gov.noaa.nws.ncep.common.dataplugin.ncgrib.request.NcgridDataRequestMessage;
import com.raytheon.uf.common.dataplugin.level.Level;
import com.raytheon.uf.common.dataquery.db.QueryParam.QueryOperand;
import com.raytheon.uf.common.datastorage.records.IDataRecord;
import com.raytheon.uf.common.serialization.comm.IRequestHandler;
import com.raytheon.uf.common.time.DataTime;
import com.raytheon.uf.edex.database.query.DatabaseQuery;

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
public class NcgridDataHandler implements IRequestHandler<NcgridDataRequestMessage> {

    @Override
    public Object handleRequest(NcgridDataRequestMessage request)
            throws Exception {

        NcgribDao dao = new NcgribDao();
        DatabaseQuery query = new DatabaseQuery(NcgribRecord.class);

        if (request.getModelName() != null) {
            query.addQueryParam("modelInfo.modelName", request.getModelName(),
                    QueryOperand.EQUALS);
        }

        if (request.getLevelOne() != Level.INVALID_VALUE) {
            query.addQueryParam("modelInfo.level.levelonevalue", request
                    .getLevelOne(), QueryOperand.EQUALS);
            if (request.getLevelType() != null) {
                query.addQueryParam("modelInfo.level.masterLevel.name", request
                        .getLevelType(), QueryOperand.EQUALS);
            }
            query.addQueryParam("modelInfo.level.leveltwovalue", request
                    .getLevelTwo(), QueryOperand.EQUALS);
        }

        if (request.getParameterAbbreviation() != null) {
            query.addQueryParam("modelInfo.parameterAbbreviation", request
                    .getParameterAbbreviation(), QueryOperand.EQUALS);
        }

        if (request.getStartTime() != NcgridDataRequestMessage.MISSING) {
            Timestamp stamp = new Timestamp(request.getStartTime());
            DataTime newTime = new DataTime(stamp, request.getForecastTime());
            query.addQueryParam("dataTime", newTime, QueryOperand.EQUALS);
        }

        NcgribThriftContainer container = new NcgribThriftContainer();
        try {
            PluginDataObject[] records = null;
            records = dao.getFullRecord(query, -1);

            if (records != null && records.length > 0) {
                List<IDataRecord[]> hdfRecords = dao.getHDF5Data(Arrays
                        .asList(records), -1);
                if (hdfRecords.size() != records.length) {
                    container.setNumOfRecords(-1);
                    return container;
                }
                NcgribThriftRecord thriftRecords[] = new NcgribThriftRecord[records.length];
                for (int i = 0; i < records.length; i++) {
                    thriftRecords[i] = new NcgribThriftRecord();
                    IDataRecord[] data = hdfRecords.get(i);
                    NcgribRecord thisGribRecord = (NcgribRecord) records[i];
                    NcgridDataRequestMessage thisGribInfo = new NcgridDataRequestMessage();
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
