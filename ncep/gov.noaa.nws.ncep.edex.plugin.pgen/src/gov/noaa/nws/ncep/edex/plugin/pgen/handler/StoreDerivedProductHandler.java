package gov.noaa.nws.ncep.edex.plugin.pgen.handler;

import gov.noaa.nws.ncep.common.dataplugin.pgen.DerivedProduct;
import gov.noaa.nws.ncep.common.dataplugin.pgen.PgenRecord;
import gov.noaa.nws.ncep.common.dataplugin.pgen.ResponseMessageValidate;
import gov.noaa.nws.ncep.common.dataplugin.pgen.request.StoreDerivedProductRequest;

import java.util.HashMap;
import java.util.Map;
import java.util.logging.Logger;

import com.raytheon.uf.common.dataplugin.persist.IPersistable;
import com.raytheon.uf.common.datastorage.IDataStore;
import com.raytheon.uf.common.datastorage.IDataStore.StoreOp;
import com.raytheon.uf.common.datastorage.records.AbstractStorageRecord;
import com.raytheon.uf.common.datastorage.records.ByteDataRecord;
import com.raytheon.uf.common.datastorage.records.StringDataRecord;
import com.raytheon.uf.common.serialization.comm.IRequestHandler;
import com.raytheon.uf.edex.database.plugin.PluginDao;
import com.raytheon.uf.edex.database.plugin.PluginFactory;

/**
 * 
 * Handler for StoreDerivedProductRequest. Stores a new, or overwrites an
 * existing, derived product for a given PGEN Activity
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
public class StoreDerivedProductHandler implements
        IRequestHandler<StoreDerivedProductRequest> {

    private static Logger logger = Logger
            .getLogger(StoreDerivedProductHandler.class.toString());

    private PluginDao dao;

    private final static String PGEN = "pgen";

    private final static String DATASET_TYPE = "TYPE";

    @Override
    public ResponseMessageValidate handleRequest(
            StoreDerivedProductRequest request) throws Exception {

        AbstractStorageRecord dataset = null;
        Map<String, Object> dataAttributes = null;
        ResponseMessageValidate response = new ResponseMessageValidate();

        dao = PluginFactory.getInstance().getPluginDao(PGEN);
        PgenRecord record = new PgenRecord(request.getDataURI());
        IDataStore dataStore = dao.getDataStore((IPersistable) record);

        for (DerivedProduct prod : request.getProductList()) {
            if (prod.getProduct() instanceof String) {
                dataset = new StringDataRecord(prod.getName(),
                        request.getDataURI(),
                        new String[] { (String) prod.getProduct() });
                dataAttributes = new HashMap<String, Object>();
                dataAttributes.put(DATASET_TYPE, prod.getProductType());
                dataset.setDataAttributes(dataAttributes);
            } else if (prod.getProduct() instanceof byte[]) {
                dataset = new ByteDataRecord(prod.getName(),
                        request.getDataURI(), (byte[]) prod.getProduct());
                dataAttributes = new HashMap<String, Object>();
                dataAttributes.put(DATASET_TYPE, prod.getProductType());
                dataset.setDataAttributes(dataAttributes);
            } else {
                StringBuilder sb = new StringBuilder(
                        "StoreDerivedProductRequest: Product type ");
                sb.append(prod.getProductType());
                sb.append(" not recognized for ");
                sb.append(prod.getName());
                logger.warning(sb.toString());
            }
            dataStore.addDataRecord(dataset);
        }

        response.setDataURI(record.getDataURI());

        logger.info("StoreDerivedProductRequest for " + record.getDataURI()
                + " with " + request.getProductList().size() + " Datasets.");

        try {
            dataStore.store(StoreOp.REPLACE);
        } catch (Exception e) {
            response.setMessage(e.getMessage());
            response.setResult(Boolean.FALSE);
            return response;
        }

        response.setResult(Boolean.TRUE);

        return response;
    }
}
