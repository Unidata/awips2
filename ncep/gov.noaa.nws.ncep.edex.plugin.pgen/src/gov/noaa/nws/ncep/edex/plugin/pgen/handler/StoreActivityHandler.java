package gov.noaa.nws.ncep.edex.plugin.pgen.handler;

import gov.noaa.nws.ncep.common.dataplugin.pgen.ActivityInfo;
import gov.noaa.nws.ncep.common.dataplugin.pgen.PgenRecord;
import gov.noaa.nws.ncep.common.dataplugin.pgen.ResponseMessageValidate;
import gov.noaa.nws.ncep.common.dataplugin.pgen.request.StoreActivityRequest;

import java.util.logging.Logger;

import com.raytheon.uf.common.datastorage.StorageStatus;
import com.raytheon.uf.common.serialization.comm.IRequestHandler;
import com.raytheon.uf.common.time.DataTime;
import com.raytheon.uf.edex.database.plugin.PluginDao;
import com.raytheon.uf.edex.database.plugin.PluginFactory;

/**
 * 
 * Handler for StoreActivityRequest. Stores a new, or overwrites an existing,
 * PGEN Activity
 * 
 * <pre>
 * 
 * SOFTWARE HISTORY
 * 
 * Date         Ticket#    Engineer    Description
 * ------------ ---------- ----------- --------------------------
 * Apr 22, 2013            sgilbert     Initial creation
 * 
 * </pre>
 * 
 * @author sgilbert
 * @version 1.0
 */
public class StoreActivityHandler implements
        IRequestHandler<StoreActivityRequest> {

    // private static final transient IUFStatusHandler statusHandler = UFStatus
    // .getHandler(StoreActivityHandler.class);

    private static Logger logger = Logger.getLogger(StoreActivityHandler.class
            .toString());

    private PluginDao dao;

    private final static String PGEN = "pgen";

    @Override
    public ResponseMessageValidate handleRequest(StoreActivityRequest request)
            throws Exception {

        ResponseMessageValidate response = new ResponseMessageValidate();

        dao = PluginFactory.getInstance().getPluginDao(PGEN);
        ActivityInfo info = request.getActivityInfo();

        PgenRecord record = new PgenRecord();
        record.setActivityLabel(info.getActivityLabel());
        record.setActivityName(info.getActivityName());
        record.setActivityType(info.getActivityType());
        record.setActivitySubtype(info.getActivitySubtype());
        record.setSite(info.getSite());
        record.setDesk(info.getDesk());
        record.setForecaster(info.getForecaster());
        record.setOperatingMode(info.getMode());
        record.setStatus(info.getStatus());

        record.setDataTime(new DataTime(info.getRefTime()));

        record.setActivityXML(request.getActivityXML());

        record.setOverwriteAllowed(true);
        record.setPluginName(PGEN);
        record.constructDataURI();
        response.setDataURI(record.getDataURI());

        logger.info("StoreActivityRequest for " + record.getDataURI());

        try {
            // dao.persistRecords(record);
            StorageStatus status = dao.persistToHDF5(record);

            if (status.getExceptions().length > 0) {
                response.setMessage(status.getExceptions()[0].getMessage());
                response.setResult(Boolean.FALSE);
                return response;
            } else {

                dao.persistToDatabase(record);
            }
        } catch (Exception e) {
            response.setMessage(e.getMessage());
            response.setResult(Boolean.FALSE);
            return response;
        }

        response.setResult(Boolean.TRUE);

        return response;
    }

}
