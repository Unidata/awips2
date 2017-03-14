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
package com.raytheon.edex.plugin.radar.handler;

import java.util.List;

import com.raytheon.edex.plugin.radar.dao.RadarDao;
import com.raytheon.uf.common.dataplugin.PluginDataObject;
import com.raytheon.uf.common.dataplugin.radar.RadarRecord;
import com.raytheon.uf.common.dataplugin.radar.request.GetRadarDataRecordRequest;
import com.raytheon.uf.common.dataplugin.radar.response.GetRadarDataRecordResponse;
import com.raytheon.uf.common.dataplugin.radar.response.RadarDataRecord;
import com.raytheon.uf.common.dataquery.db.QueryParam.QueryOperand;
import com.raytheon.uf.common.datastorage.records.IDataRecord;
import com.raytheon.uf.common.serialization.comm.IRequestHandler;
import com.raytheon.uf.edex.database.query.DatabaseQuery;

/**
 * Request handler for {@link GetRadarDataRecordRequest}s.
 *
 * <pre>
 *
 * SOFTWARE HISTORY
 *
 * Date         Ticket#    Engineer    Description
 * ------------ ---------- ----------- --------------------------
 * Aug 7, 2014  3393       nabowle     Initial creation
 *
 * </pre>
 *
 * @author nabowle
 * @version 1.0
 */
public class GetRadarDataRecordHandler implements
        IRequestHandler<GetRadarDataRecordRequest> {
    public static final String ENTITY = RadarRecord.class.getName().toString();

    /**
     * Constructor.
     */
    public GetRadarDataRecordHandler() {
        super();
    }

    /*
     * (non-Javadoc)
     *
     * @see
     * IRequestHandler#handleRequest(com.raytheon.uf.common.serialization.comm
     * .IServerRequest)
     */
    @Override
    public GetRadarDataRecordResponse handleRequest(GetRadarDataRecordRequest request)
            throws Exception {
        GetRadarDataRecordResponse response = new GetRadarDataRecordResponse();
        DatabaseQuery query = buildQuery(request);

        RadarDao dao = new RadarDao();
        List<PluginDataObject> objects = (List<PluginDataObject>) dao
                .queryByCriteria(query);

        if (objects != null && objects.size() > 0) {
            List<IDataRecord[]> hdf5Records = dao.getHDF5Data(objects, -1);

            RadarRecord record;
            RadarDataRecord data;
            RadarDataRecord[] responseData = new RadarDataRecord[objects.size()];
            for (int i = 0; i < objects.size(); i++) {
                data = new RadarDataRecord();
                data.setHdf5Data(hdf5Records.get(i));

                record = (RadarRecord) objects.get(i);
                // Strings are used to match MSAS/LAPS uEngine precision for
                // side by side comparison
                data.setDataTime(record.getDataTime());
                data.setLatitude(record.getLatitude().toString());
                data.setLongitude(record.getLongitude().toString());
                data.setElevation(record.getElevation().toString());
                data.setElevationNumber(record.getElevationNumber().toString());
                data.setTrueElevationAngle(record.getTrueElevationAngle()
                        .toString());
                data.setVolumeCoveragePattern(record.getVolumeCoveragePattern()
                        .toString());

                responseData[i] = data;
            }
            response.setData(responseData);
        }

        return response;
    }

    /**
     * Build the database query.
     *
     * @param request
     *            The request.
     * @return A database query using the fields of the request.
     */
    private DatabaseQuery buildQuery(GetRadarDataRecordRequest request) {
        DatabaseQuery query = new DatabaseQuery(ENTITY);

        query.addQueryParam("icao", request.getRadarId());
        query.addQueryParam("productCode", request.getProductCode());
        query.addQueryParam("primaryElevationAngle",
                request.getPrimaryElevationAngle());
        query.addQueryParam("dataTime.refTime", request.getTimeRange()
                .getStart(), QueryOperand.GREATERTHANEQUALS);
        query.addQueryParam("dataTime.refTime",
                request.getTimeRange().getEnd(), QueryOperand.LESSTHANEQUALS);

        return query;
    }

}
