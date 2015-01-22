/**
 * This code has unlimited rights, and is provided "as is" by the National Centers 
 * for Environmental Prediction, without warranty of any kind, either expressed or implied, 
 * including but not limited to the implied warranties of merchantability and/or fitness 
 * for a particular purpose.
 * 
 * This code has been developed by the NCEP-SIB for use in the AWIPS2 system.
 * 
 **/
package gov.noaa.nws.ncep.edex.plugin.geomag.handler;

import gov.noaa.nws.ncep.common.dataplugin.geomag.dao.GeoMagK3hrStateDao;
import gov.noaa.nws.ncep.common.dataplugin.geomag.request.RetrieveStationStateRequest;

import java.util.List;
import java.util.Map;
import java.util.logging.Logger;

import com.raytheon.uf.common.dataquery.responses.DbQueryResponse;
import com.raytheon.uf.common.serialization.comm.IRequestHandler;

/**
 * 
 * Handler for RetrieveStationStateRequest.
 * 
 * <pre>
 * 
 * SOFTWARE HISTORY
 * 
 * Date         Ticket#    Engineer    Description
 * ------------ ---------- ----------- --------------------------
 * 07/01/2014   R4078      sgurung     Initial Creation
 * 
 * </pre>
 * 
 * @author sgurung
 * @version 1.0
 */
public class RetrieveStationStateRequestHandler implements
        IRequestHandler<RetrieveStationStateRequest> {

    private static Logger logger = Logger
            .getLogger(RetrieveStationStateRequestHandler.class.toString());

    private GeoMagK3hrStateDao dao;

    @Override
    public Object handleRequest(RetrieveStationStateRequest request)
            throws Exception {

        logger.info("RetrieveStationStateRequest...");

        try {
            dao = new GeoMagK3hrStateDao();

            if (request.getStationCodeList() != null) {
                List<Map<String, Object>> resultMaps = dao.getKpStationsStates(
                        request.getStationCodeList(), request.getRefTime());
                DbQueryResponse response = new DbQueryResponse();
                response.setResults(resultMaps);
                return response;
            } else if (request.getStationCode() != null) {
                List<String> results = dao.getKpStationStates(
                        request.getStationCode(), request.getRefTime());
                return results;
            }
        } catch (Exception e) {
            logger.warning("Error retrieving station state request...");
        }

        return null;
    }
}
