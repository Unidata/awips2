package gov.noaa.nws.ncep.ui.pgen.store;

import gov.noaa.nws.ncep.common.dataplugin.pgen.ActivityInfo;
import gov.noaa.nws.ncep.common.dataplugin.pgen.DerivedProduct;
import gov.noaa.nws.ncep.common.dataplugin.pgen.PgenRecord;
import gov.noaa.nws.ncep.common.dataplugin.pgen.ResponseMessageValidate;
import gov.noaa.nws.ncep.common.dataplugin.pgen.request.RetrieveActivityRequest;
import gov.noaa.nws.ncep.common.dataplugin.pgen.request.StoreActivityRequest;
import gov.noaa.nws.ncep.common.dataplugin.pgen.request.StoreDerivedProductRequest;
import gov.noaa.nws.ncep.ui.pgen.elements.Product;
import gov.noaa.nws.ncep.ui.pgen.file.ProductConverter;
import gov.noaa.nws.ncep.ui.pgen.file.Products;

import java.io.File;
import java.util.ArrayList;
import java.util.List;

import javax.xml.bind.JAXBException;

import org.eclipse.jface.dialogs.MessageDialog;
import org.eclipse.ui.PlatformUI;

import com.raytheon.uf.common.dataplugin.PluginException;
import com.raytheon.uf.common.dataquery.requests.DbQueryRequest;
import com.raytheon.uf.common.dataquery.requests.RequestConstraint;
import com.raytheon.uf.common.dataquery.requests.RequestConstraint.ConstraintType;
import com.raytheon.uf.common.dataquery.responses.DbQueryResponse;
import com.raytheon.uf.common.datastorage.DataStoreFactory;
import com.raytheon.uf.common.datastorage.IDataStore;
import com.raytheon.uf.common.message.response.AbstractResponseMessage;
import com.raytheon.uf.common.message.response.ResponseMessageError;
import com.raytheon.uf.common.message.response.ResponseMessageGeneric;
import com.raytheon.uf.common.serialization.SerializationUtil;
import com.raytheon.uf.common.time.DataTime;
import com.raytheon.uf.viz.core.HDF5Util;
import com.raytheon.uf.viz.core.exception.VizException;
import com.raytheon.uf.viz.core.requests.ThriftClient;
import com.raytheon.viz.core.mode.CAVEMode;

/**
 * 
 * Utility class containing static methods to retrieve and store PGEN Activities
 * in EDEX
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
public class StorageUtils {

    private static final String STORAGE_ERROR = "Pgen Storage Exception";

    /**
     * Store/Overwrite ActivityXML for given PGEN Product
     * 
     * @param prod
     *            PGEN Product
     * @return dataURI associated with this product
     * @throws PgenStorageException
     */
    public static String storeProduct(Product prod) throws PgenStorageException {
        return storeProduct(prod, false);
    }

    /**
     * Store/Overwrite ActivityXML for given PGEN Product
     * 
     * @param prod
     *            PGEN Product
     * @param promptOnOverwrite
     *            if True, user will be prompted to confirm overwrite of
     *            product, if it already exists
     * @return dataURI associated with this product
     * @throws PgenStorageException
     */
    public static String storeProduct(Product prod, boolean promptOnOverwrite)
            throws PgenStorageException {

        ActivityInfo info = new ActivityInfo();
        info.setActivityName(prod.getName());
        info.setActivityType(prod.getType());
        info.setActivityLabel(prod.getOutputFile());
        info.setRefTime(prod.getTime().getStartTime());
        info.setSite(prod.getCenter());
        info.setForecaster(prod.getForecaster());
        info.setMode(CAVEMode.getMode().name());

        return StorageUtils.storeProduct(info, prod, promptOnOverwrite);

    }

    /**
     * Store/Overwrite ActivityXML for given PGEN Product using the given
     * ActivityInfo to identify the product
     * 
     * @param info
     *            Activity information identifying the product
     * @param prod
     *            PGEN Product
     * @param promptOnOverwrite
     *            if True, user will be prompted to confirm overwrite of
     *            product, if it already exists
     * @return
     * @throws PgenStorageException
     */
    public static String storeProduct(ActivityInfo info, Product prod)
            throws PgenStorageException {
        return storeProduct(info, prod, false);
    }

    /**
     * Store/Overwrite ActivityXML for given PGEN Product using the given
     * ActivityInfo to identify the product
     * 
     * @param info
     *            Activity information identifying the product
     * @param prod
     *            PGEN Product
     * @return
     * @throws PgenStorageException
     */
    public static String storeProduct(ActivityInfo info, Product prod,
            boolean promptOnOverwrite) throws PgenStorageException {
        ResponseMessageValidate result = null;

        if (promptOnOverwrite) {
            boolean answer = promptIfActivityExists(info);
            if (!answer)
                return null;
        }

        try {
            String activityXML = serializeProduct(prod);

            StoreActivityRequest request = new StoreActivityRequest(info,
                    activityXML);
            result = (ResponseMessageValidate) ThriftClient
                    .sendRequest(request);
        } catch (Exception e) {
            e.printStackTrace();
            throw new PgenStorageException("Unable to store PGEN Activity.", e);
        }

        if (result.getResult() == Boolean.FALSE) {
            throw new PgenStorageException(
                    "Request to store PGEN Activity failed:"
                            + result.getMessage());
        } else {
            return result.getDataURI();
        }
        // throw new PgenStorageException(
        // "testline1\nline2\netcetera\nline4\n55555555\n6666666"
        // + "\n7777777");
    }

    /*
     * Display Overwrite Confirmation Dialog, if Activity already exists
     */
    private static boolean promptIfActivityExists(ActivityInfo info)
            throws PgenStorageException {
        boolean canWrite = false;

        if (activityExists(info)) {
            // display confirmation dialog
            String msg = "Activity already exists. Overwrite?";
            MessageDialog confirmDlg = new MessageDialog(PlatformUI
                    .getWorkbench().getActiveWorkbenchWindow().getShell(),
                    "Confirm", null, msg, MessageDialog.QUESTION, new String[] {
                            "OK", "Cancel" }, 0);
            confirmDlg.open();

            if (confirmDlg.getReturnCode() == MessageDialog.OK) {
                canWrite = true;
            }
        } else {
            canWrite = true;
        }

        return canWrite;
    }

    /*
     * Checks to see if an Activity already exists in EDEX datastore. returns
     * true if it exists
     */
    private static boolean activityExists(ActivityInfo info)
            throws PgenStorageException {
        boolean exists = false;

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
        record.setPluginName("pgen");
        try {
            record.constructDataURI();
        } catch (PluginException e1) {
            throw new PgenStorageException("Error constructing dataURI", e1);
        }

        String dataURI = record.getDataURI();

        DbQueryRequest request = new DbQueryRequest();
        request.setEntityClass(PgenRecord.class.getName());
        request.addRequestField(PgenRecord.DATAURI);
        request.addConstraint(PgenRecord.DATAURI, new RequestConstraint(
                dataURI, ConstraintType.EQUALS));

        DbQueryResponse response;
        try {
            response = (DbQueryResponse) ThriftClient.sendRequest(request);
            if (response.getResults().size() == 1)
                exists = true;
            System.out.println("GOT RESPONSE BACK = "
                    + response.getResults().size());
        } catch (Exception e) {
            throw new PgenStorageException(
                    "Error determinimg if activity exists", e);
        }

        return exists;
    }

    /**
     * Retrieves the PGEN Product with the given dataURI
     * 
     * @param dataURI
     * @return PGEN Product
     * @throws PgenStorageException
     */
    public static List<Product> retrieveProduct(String dataURI)
            throws PgenStorageException {

        RetrieveActivityRequest req = new RetrieveActivityRequest(dataURI);
        AbstractResponseMessage resp;
        try {
            resp = (AbstractResponseMessage) ThriftClient.sendRequest(req);
        } catch (VizException e) {
            throw new PgenStorageException(
                    "Error sending activity retrieval request.", e);
        }

        if (resp instanceof ResponseMessageError) {
            ResponseMessageError err = (ResponseMessageError) resp;
            StringBuilder sb = new StringBuilder(
                    "Error Retrieving Activity from EDEX");
            if (((ResponseMessageError) resp).getErrorChain() != null) {
                for (String msg : err.getErrorChain()) {
                    sb.append("\n");
                    sb.append(msg);
                }
            }
            throw new PgenStorageException(sb.toString());
        }

        String xml = (String) ((ResponseMessageGeneric) resp).getContents();
        List<Product> prods = deserializeProduct(xml);
        return prods;
    }

    /**
     * Serializes the product into XML
     * 
     * @param prod
     * @return XML representation of the Product
     * @throws PgenStorageException
     */
    public static String serializeProduct(Product prod)
            throws PgenStorageException {

        ArrayList<Product> prodlist = new ArrayList<Product>();
        prodlist.add(prod);

        Products filePrds = ProductConverter.convert(prodlist);

        try {
            return SerializationUtil.marshalToXml(filePrds);
        } catch (JAXBException e) {
            throw new PgenStorageException("Unable to serialize PGEN Activity",
                    e);
        }
    }

    /**
     * Deserializes an XML string into a PGEN Product
     * 
     * @param activityXML
     * @return Pgen Product
     * @throws PgenStorageException
     */
    private static List<Product> deserializeProduct(String activityXML)
            throws PgenStorageException {

        Products prods = null;

        try {
            prods = SerializationUtil.unmarshalFromXml(Products.class,
                    activityXML);
            return ProductConverter.convert(prods);
        } catch (Exception e) {
            throw new PgenStorageException(
                    "Unable to deserialize PGEN Activity", e);
        }

    }

    /**
     * Displays an Exception in a Message Dialog
     * 
     * @param e
     */
    public static void showError(Exception e) {

        StringBuilder sb = new StringBuilder(e.getMessage());

        Throwable temp = e;
        while ((temp = temp.getCause()) != null) {
            sb.append("\n");
            sb.append(temp.getMessage());
        }

        MessageDialog errorDlg = new MessageDialog(PlatformUI.getWorkbench()
                .getActiveWorkbenchWindow().getShell(), STORAGE_ERROR, null,
                sb.toString(), MessageDialog.ERROR, new String[] { "OK" }, 0);

        errorDlg.open();
    }

    /**
     * Store/Overwrite a derived product under the Activity with the given
     * datauri
     * 
     * @param dataURI
     * @param name
     *            Name of derived product
     * @param productType
     *            Type of derived product
     * @param product
     *            derived product
     * @throws PgenStorageException
     */
    public static void storeDerivedProduct(String dataURI, String name,
            String productType, Object product) throws PgenStorageException {
        storeDerivedProduct(dataURI, name, productType, product, false);
    }

    /**
     * Store/Overwrite a derived product under the Activity with the given
     * datauri
     * 
     * @param dataURI
     * @param name
     *            Name of derived product
     * @param productType
     *            Type of derived product
     * @param product
     *            derived product
     * @param promptOnOverwrite
     *            if True, user will be prompted to confirm overwrite of
     *            product, if it already exists
     * @throws PgenStorageException
     */
    public static void storeDerivedProduct(String dataURI, String name,
            String productType, Object product, boolean promptOnOverwrite)
            throws PgenStorageException {
        ResponseMessageValidate result = null;

        if (promptOnOverwrite) {
            boolean answer = promptIfProductExists(dataURI, name);
            if (!answer)
                return;
        }

        StoreDerivedProductRequest request = new StoreDerivedProductRequest(
                dataURI, name, productType, product);
        try {
            result = (ResponseMessageValidate) ThriftClient
                    .sendRequest(request);
        } catch (VizException e) {
            throw new PgenStorageException(
                    "Unable to store PGEN Derived Product.", e);
        }

        if (result.getResult() == Boolean.FALSE) {
            throw new PgenStorageException(
                    "Request to store PGEN DerivedProduct failed:"
                            + result.getMessage());
        }

    }

    /*
     * Display Overwrite Confirmation Dialog, if derived product already exists
     */
    private static boolean promptIfProductExists(String dataURI, String name)
            throws PgenStorageException {
        boolean canWrite = false;

        if (productExists(dataURI, name)) {
            // display confirmation dialog
            String msg = "Derived Product " + name
                    + " already exists. Overwrite?";
            MessageDialog confirmDlg = new MessageDialog(PlatformUI
                    .getWorkbench().getActiveWorkbenchWindow().getShell(),
                    "Confirm", null, msg, MessageDialog.QUESTION, new String[] {
                            "OK", "Cancel" }, 0);
            confirmDlg.open();

            if (confirmDlg.getReturnCode() == MessageDialog.OK) {
                canWrite = true;
            }
        } else {
            canWrite = true;
        }

        return canWrite;
    }

    /*
     * Checks to see if a derived product already exists in EDEX datastore.
     * Returns true if it exists
     */
    private static boolean productExists(String dataURI, String name)
            throws PgenStorageException {
        boolean exists = false;

        PgenRecord record = new PgenRecord(dataURI);
        File loc = HDF5Util.findHDF5Location(record);
        IDataStore dataStore = DataStoreFactory.getDataStore(loc);
        String[] products = new String[] {};
        try {
            products = dataStore.getDatasets(dataURI);
        } catch (Exception e) {
            throw new PgenStorageException(
                    "Cannot retrieve list of derived product names.", e);
        }
        for (String prod : products) {
            if (prod.equals(name))
                return true;
        }

        return exists;
    }

    /**
     * Store/Overwrite a list of derived products under the Activity with the
     * given datauri
     * 
     * @param dataURI
     * @param prodList
     *            List of Derived Products
     * @throws PgenStorageException
     */
    public static void storeDerivedProducts(String dataURI,
            List<DerivedProduct> prodList) throws PgenStorageException {

        ResponseMessageValidate result = null;

        StoreDerivedProductRequest request = new StoreDerivedProductRequest();
        request.setDataURI(dataURI);
        request.setProductList(prodList);

        try {
            result = (ResponseMessageValidate) ThriftClient
                    .sendRequest(request);
        } catch (VizException e) {
            throw new PgenStorageException(
                    "Unable to store PGEN Derived Product.", e);
        }

        if (result.getResult() == Boolean.FALSE) {
            throw new PgenStorageException(
                    "Request to store PGEN DerivedProduct failed:"
                            + result.getMessage());
        }

    }

}
