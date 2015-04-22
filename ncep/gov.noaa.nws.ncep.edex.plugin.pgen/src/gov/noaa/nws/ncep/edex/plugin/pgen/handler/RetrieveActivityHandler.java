package gov.noaa.nws.ncep.edex.plugin.pgen.handler;

import gov.noaa.nws.ncep.common.dataplugin.pgen.PgenRecord;
import gov.noaa.nws.ncep.common.dataplugin.pgen.request.RetrieveActivityRequest;

import java.util.logging.Logger;

import com.raytheon.uf.common.dataplugin.persist.IPersistable;
import com.raytheon.uf.common.datastorage.IDataStore;
import com.raytheon.uf.common.datastorage.Request;
import com.raytheon.uf.common.datastorage.records.IDataRecord;
import com.raytheon.uf.common.datastorage.records.StringDataRecord;
import com.raytheon.uf.common.message.response.AbstractResponseMessage;
import com.raytheon.uf.common.message.response.ResponseMessageError;
import com.raytheon.uf.common.message.response.ResponseMessageGeneric;
import com.raytheon.uf.common.serialization.comm.IRequestHandler;
import com.raytheon.uf.edex.database.plugin.PluginDao;
import com.raytheon.uf.edex.database.plugin.PluginFactory;

/**
 * 
 * Handler for RetrieveActivityRequest. Retrieves the PGEN Activity XML for the
 * given datauri
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
public class RetrieveActivityHandler implements
        IRequestHandler<RetrieveActivityRequest> {

    private static Logger logger = Logger
            .getLogger(RetrieveActivityHandler.class.toString());

    private PluginDao dao;

    private final static String PGEN = "pgen";

    @Override
    public AbstractResponseMessage handleRequest(RetrieveActivityRequest request)
            throws Exception {

        AbstractResponseMessage response = null;
        IDataRecord records = null;

        logger.info("RetrieveActivityRequest for " + request.getDataURI());

        try {
            dao = PluginFactory.getInstance().getPluginDao(PGEN);

            PgenRecord record = new PgenRecord(request.getDataURI());
            IDataStore dataStore = dao.getDataStore((IPersistable) record);
            records = dataStore.retrieve(request.getDataURI(),
                    PgenRecord.ACTIVITY_XML, Request.ALL);

        } catch (Exception e) {
            response = ResponseMessageError.generateErrorResponse(
                    "Could not retrieve Activity request", e);
            return response;
        }

        if (records instanceof StringDataRecord) {
            String[] stringdata = ((StringDataRecord) records).getStringData();
            response = new ResponseMessageGeneric(stringdata[0]);
            response.setDataURI(request.getDataURI());
        } else
            response = ResponseMessageError.generateErrorResponse(
                    "Unexpected item returned", null);

        return response;
    }
}
