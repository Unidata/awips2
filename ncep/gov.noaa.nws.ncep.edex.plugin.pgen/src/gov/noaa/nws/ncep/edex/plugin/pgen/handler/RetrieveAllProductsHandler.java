package gov.noaa.nws.ncep.edex.plugin.pgen.handler;

import gov.noaa.nws.ncep.common.dataplugin.pgen.PgenRecord;
import gov.noaa.nws.ncep.common.dataplugin.pgen.request.RetrieveAllProductsRequest;

import java.util.logging.Logger;

import com.raytheon.uf.common.dataplugin.persist.IPersistable;
import com.raytheon.uf.common.datastorage.IDataStore;
import com.raytheon.uf.common.datastorage.records.IDataRecord;
import com.raytheon.uf.common.serialization.comm.IRequestHandler;
import com.raytheon.uf.edex.database.plugin.PluginDao;
import com.raytheon.uf.edex.database.plugin.PluginFactory;

/**
 * 
 * Handler for RetrieveAllProductsRequest. Retrieves the PGEN activity XML and
 * all derived products for the given datauri.
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
public class RetrieveAllProductsHandler implements
        IRequestHandler<RetrieveAllProductsRequest> {

    private static Logger logger = Logger
            .getLogger(RetrieveAllProductsHandler.class.toString());

    private PluginDao dao;

    private final static String PGEN = "pgen";

    /**
     *
     */
    @Override
    public IDataRecord[] handleRequest(RetrieveAllProductsRequest request)
            throws Exception {

        IDataRecord[] records = new IDataRecord[] {};

        logger.info("RetrieveAllProductsRequest for " + request.getDataURI());

        try {
            dao = PluginFactory.getInstance().getPluginDao(PGEN);

            PgenRecord record = new PgenRecord(request.getDataURI());
            IDataStore dataStore = dao.getDataStore((IPersistable) record);
            records = dataStore.retrieve(request.getDataURI());

        } catch (Exception e) {
            logger.warning("Error retrieving PGEN products for "
                    + request.getDataURI());
            // response = ResponseMessageError.generateErrorResponse(
            // "Could not retrieve all products request", e);
            //
        }

        return records;
    }
}
