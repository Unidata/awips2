package gov.noaa.nws.ncep.edex.plugin.geomag.handler;

import gov.noaa.nws.ncep.common.dataplugin.geomag.dao.GeoMagDao;
import gov.noaa.nws.ncep.common.dataplugin.geomag.request.RetrieveGeoMagDataRequest;
import gov.noaa.nws.ncep.common.dataplugin.geomag.request.RetrieveGeoMagDataRequest.RetrieveGeoMagDataRequestType;

import java.util.logging.Logger;

import com.raytheon.uf.common.serialization.comm.IRequestHandler;

/**
 * 
 * Handler for RetrieveGeoMagDataRequest.
 * 
 * <pre>
 * 
 * SOFTWARE HISTORY
 * 
 * Date         Ticket#    Engineer    Description
 * ------------ ---------- ----------- --------------------------
 * 07/31/2014   R4078      sgurung     Initial creation.
 * 
 * </pre>
 * 
 * @author sgurung
 * @version 1.0
 */
public class RetrieveGeoMagDataRequestHandler implements
        IRequestHandler<RetrieveGeoMagDataRequest> {

    private static Logger logger = Logger
            .getLogger(RetrieveGeoMagDataRequestHandler.class.toString());

    private GeoMagDao dao;

    @Override
    public Object handleRequest(RetrieveGeoMagDataRequest request)
            throws Exception {

        logger.info("RetrieveGeoMagDataRequest for " + request.getStationCode());

        try {
            dao = new GeoMagDao("geomag"); // PluginFactory.getInstance().getPluginDao(GeoMag);

            if (RetrieveGeoMagDataRequestType.DATA_LIST.equals(request
                    .getRequestType())) {
                return dao.getGeoMagRecords(request.getStationCode(),
                        request.getStartTime(), request.getEndTime(),
                        request.getSourceId());
            } else if (RetrieveGeoMagDataRequestType.COUNT.equals(request
                    .getRequestType())) {
                return dao.getGeoMagRecordsCount(request.getStationCode(),
                        request.getStartTime(), request.getEndTime(),
                        request.getSourceId());
            }

        } catch (Exception e) {
            logger.warning("Error during RetrieveGeoMagDataRequestHandler request "
                    + request.getStationCode()
                    + ". Error: "
                    + e.getMessage()
                    + "\n" + e.getStackTrace());
        }

        return null;
    }
}
