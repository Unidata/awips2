package gov.noaa.nws.ncep.edex.plugin.geomag.handler;

import gov.noaa.nws.ncep.common.dataplugin.geomag.dao.GeoMagK1minDao;
import gov.noaa.nws.ncep.common.dataplugin.geomag.request.RetrieveK1minRequest;
import gov.noaa.nws.ncep.common.dataplugin.geomag.request.RetrieveK1minRequest.RetrieveK1minRequestType;

import java.util.List;
import java.util.Map;
import java.util.logging.Logger;

import com.raytheon.uf.common.dataquery.responses.DbQueryResponse;
import com.raytheon.uf.common.serialization.comm.IRequestHandler;

/**
 * 
 * Handler for RetrieveKiminRequest. Retrieves the GeoMagK1min for the given
 * datauri
 * 
 * <pre>
 * 
 * SOFTWARE HISTORY
 * 
 * Date         Ticket#    Engineer    Description
 * ------------ ---------- ----------- --------------------------
 * 2014/02/12   #1110      qzhou       Init
 * 03/05/2014   R4078      sgurung     Modified method handleRequest() to handle additional 
 *                                     requests based on RetrieveK1minRequestTypes.
 * 
 * </pre>
 * 
 * @author qzhou
 * @version 1.0
 */
public class RetrieveK1minRequestHandler implements
        IRequestHandler<RetrieveK1minRequest> {

    private static Logger logger = Logger
            .getLogger(RetrieveK1minRequestHandler.class.toString());

    private GeoMagK1minDao dao;

    @Override
    public Object handleRequest(RetrieveK1minRequest request) throws Exception {

        logger.info("RetrieveK1minRequest for " + request.getStationCode());

        try {
            dao = new GeoMagK1minDao();

            if (RetrieveK1minRequestType.KP.equals(request.getRequestType())) {
                List<Map<String, Object>> resultMaps = dao.getEstKpIndex1min(
                        request.getStationCodeList(), request.getStartTime(),
                        request.getEndTime());
                DbQueryResponse response = new DbQueryResponse();
                response.setResults(resultMaps);
                return response;
            } else if (RetrieveK1minRequestType.K.equals(request
                    .getRequestType())) {
                return dao.getEstKIndex1min(request.getStationCodeList(),
                        request.getStartTime(), request.getEndTime());

            } else if (RetrieveK1minRequestType.LATEST_K.equals(request
                    .getRequestType())) {
                List<Map<String, Object>> resultMaps = dao.getLatestEstKIndex(
                        request.getStationCodeList(), request.getStartTime(),
                        request.getEndTime());
                DbQueryResponse response = new DbQueryResponse();
                response.setResults(resultMaps);
                return response;
            } else if (RetrieveK1minRequestType.LAST_DATA_DATE.equals(request
                    .getRequestType())) {
                return dao.getLastDataDate(request.getStationCodeList(),
                        request.getStartTime(), request.getEndTime());

            } else {
                return dao.getRangeK1min(request.getStationCode(),
                        request.getStartTime(), request.getEndTime());
            }

        } catch (Exception e) {
            logger.warning("Error retrieving K1min record for "
                    + request.getStationCode());
        }

        return null;
    }
}
