package gov.noaa.nws.ncep.edex.plugin.geomag.handler;

import gov.noaa.nws.ncep.common.dataplugin.geomag.GeoMagK1min;
import gov.noaa.nws.ncep.common.dataplugin.geomag.dao.GeoMagK1minDao;
import gov.noaa.nws.ncep.common.dataplugin.geomag.request.RetrieveSingleK1minRequest;

import java.util.List;
import java.util.logging.Logger;

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
 * 
 * </pre>
 * 
 * @author qzhou
 * @version 1.0
 */
public class RetrieveSingleK1minRequestHandler implements
        IRequestHandler<RetrieveSingleK1minRequest> {

    private static Logger logger = Logger
            .getLogger(RetrieveSingleK1minRequestHandler.class.toString());

    private GeoMagK1minDao dao;

    @Override
    public Object handleRequest(RetrieveSingleK1minRequest request)
            throws Exception {
        List<GeoMagK1min> resultsList = null;
        logger.info("RetrieveSingleK1minRequest for "
                + request.getStationCode());

        try {
            dao = new GeoMagK1minDao();

            resultsList = dao.getSingleK1min(request.getStationCode(),
                    request.getRefTime());

        } catch (Exception e) {
            logger.warning("Error retrieving K1min record for "
                    + request.getStationCode());
        }

        return resultsList;
    }
}
