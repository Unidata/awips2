package gov.noaa.nws.ncep.edex.plugin.geomag.handler;

import gov.noaa.nws.ncep.common.dataplugin.geomag.GeoMagAvg;
import gov.noaa.nws.ncep.common.dataplugin.geomag.dao.GeoMagAvgDao;
import gov.noaa.nws.ncep.common.dataplugin.geomag.request.RetrieveHrAvgRequest;

import java.util.List;
import java.util.logging.Logger;

import com.raytheon.uf.common.serialization.comm.IRequestHandler;

/**
 * 
 * Handler for RetrieveHrAvgRequest. Retrieves the GeoMagAvg for the given
 * datauri
 * 
 * <pre>
 * 
 * SOFTWARE HISTORY
 * 
 * Date         Ticket#    Engineer    Description
 * ------------ ---------- ----------- --------------------------
 * 2014/02/12   #1110      qzhou       Init
 * 
 * </pre>
 * 
 * @author qzhou
 * @version 1.0
 */
public class RetrieveHrAvgRequestHandler implements
        IRequestHandler<RetrieveHrAvgRequest> {

    private static Logger logger = Logger
            .getLogger(RetrieveHrAvgRequestHandler.class.toString());

    private GeoMagAvgDao dao;

    @Override
    public Object handleRequest(RetrieveHrAvgRequest request) throws Exception {
        List<GeoMagAvg> resultsList = null;
        logger.info("RetrieveHrAvgRequest for " + request.getStationCode());

        try {
            dao = new GeoMagAvgDao(); // PluginFactory.getInstance().getPluginDao(GeoMag);

            resultsList = dao.getAvgForStation(request.getStationCode(),
                    request.getStartTime(), request.getEndTime());

            // logger.info("resultsList.size() " + request.getStationCode());

        } catch (Exception e) {
            logger.warning("Error retrieving hourly average record for "
                    + request.getStationCode());
        }

        return resultsList;
    }
}
