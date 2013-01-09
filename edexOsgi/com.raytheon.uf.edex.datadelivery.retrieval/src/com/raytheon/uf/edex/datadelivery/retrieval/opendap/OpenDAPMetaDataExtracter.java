package com.raytheon.uf.edex.datadelivery.retrieval.opendap;

import java.io.FileNotFoundException;
import java.io.IOException;
import java.net.MalformedURLException;
import java.text.SimpleDateFormat;
import java.util.Date;
import java.util.HashMap;
import java.util.Map;

import javax.persistence.Transient;

import com.raytheon.uf.common.datadelivery.registry.Connection;
import com.raytheon.uf.common.datadelivery.registry.Provider.ServiceType;
import com.raytheon.uf.common.datadelivery.retrieval.util.HarvesterServiceManager;
import com.raytheon.uf.common.status.IUFStatusHandler;
import com.raytheon.uf.common.status.UFStatus;
import com.raytheon.uf.common.status.UFStatus.Priority;
import com.raytheon.uf.edex.datadelivery.retrieval.MetaDataExtracter;
import com.raytheon.uf.edex.datadelivery.retrieval.util.ConnectionUtil;

import dods.dap.DAS;
import dods.dap.DASException;
import dods.dap.DConnect;
import dods.dap.DODSException;
import dods.dap.parser.ParseException;

/**
 * Extract OpenDAP MetaData over the web.
 * 
 * This class should remain package-private, all access should be limited
 * through the {@link OpenDapServiceFactory}.
 * 
 * <pre>
 * 
 * SOFTWARE HISTORY
 * Date         Ticket#    Engineer    Description
 * ------------ ---------- ----------- --------------------------
 * Feb 20, 2011    218      dhladky     Initial creation
 * Jun 28, 2012    819      djohnson    Use utility class for DConnect.
 * Jul 25, 2012    955      djohnson    Make package-private.
 * Aug 06, 2012   1022      djohnson    Cache a retrieved DAS instance.
 * Nov 19, 2012 1166       djohnson     Clean up JAXB representation of registry objects.
 * 
 * </pre>
 * 
 * @author dhladky
 * @version 1.0
 */
class OpenDAPMetaDataExtracter extends MetaDataExtracter {

    /**
     * DAP Type
     * 
     * <pre>
     * 
     * SOFTWARE HISTORY
     * 
     * Date         Ticket#    Engineer    Description
     * ------------ ---------- ----------- --------------------------
     * Feb 16, 2012            dhladky     Initial creation
     * 
     * </pre>
     * 
     * @author dhladky
     * @version 1.0
     */
    enum DAP_TYPE {

        DAS("das"), DDS("dds"), INFO("info"), DODS("dods");

        private final String dapType;

        private DAP_TYPE(String name) {
            dapType = name;
        }

        public String getDapType() {
            return dapType;
        }
    }

    private static final IUFStatusHandler statusHandler = UFStatus
            .getHandler(OpenDAPMetaDataExtracter.class);

    private String rootUrl;

    private SimpleDateFormat sdf = null;

    @Transient
    private transient DAS das;

    OpenDAPMetaDataExtracter(Connection conn) {
        super(conn);
        serviceConfig = HarvesterServiceManager.getInstance().getServiceConfig(
                ServiceType.OPENDAP);
        sdf = new SimpleDateFormat();
        sdf.applyLocalizedPattern(HarvesterServiceManager.getInstance()
                .getServiceConfig(ServiceType.OPENDAP)
                .getConstantValue("DATE_COMPARE_FORMAT"));
    }

    /**
     * Checks whether or not the data is new
     */
    @Override
    public boolean checkLastUpdate(Date date) {

        if (date.before(getDataDate())) {
            return true;
        }

        return false;
    }

    @Override
    public Map<String, Object> extractMetaData() throws Exception {
        try {
            Map<String, Object> metaData = new HashMap<String, Object>();

            // we only need DAS
            metaData.put(DAP_TYPE.DAS.getDapType(), getDASData());
            return metaData;
        } catch (Exception e) {
            statusHandler.handle(Priority.PROBLEM,
                    "Can't extract MetaData from URL " + url, e);
            throw e;
        }
    };

    /**
     * Get an OpenDAP protocol connection
     * 
     * @param extension
     * @return
     */
    private DConnect getConnection(String curl) {
        try {
            DConnect conn = ConnectionUtil.getDConnect(curl);
            return conn;
        } catch (FileNotFoundException e) {
            statusHandler.handle(Priority.PROBLEM, e.getLocalizedMessage(), e);
        }

        return null;
    }

    private DAS getDASData() throws MalformedURLException, DASException,
            IOException, ParseException, DODSException {
        if (das == null) {
            DConnect conn = getConnection(rootUrl);
            das = conn.getDAS();
        }
        return das;
    }

    /**
     * Sets the data date for comparison
     * 
     * @throws Exception
     */
    @Override
    public void setDataDate() throws Exception {
        try {
            DAS das = getDASData();

            String history = das
                    .getAttributeTable(
                            serviceConfig.getConstantValue("NC_GLOBAL"))
                    .getAttribute(serviceConfig.getConstantValue("HISTORY"))
                    .getValueAt(0);
            String[] histories = history.split(":");
            String time = OpenDAPParseUtility.getInstance().trim(histories[0].trim()
                    + histories[1].trim() + histories[2].trim());

            Date dataDate = null;
            try {
                dataDate = sdf.parse(time);
            } catch (java.text.ParseException e) {
                statusHandler.handle(Priority.PROBLEM, e.getLocalizedMessage(),
                        e);
            }
            setDataDate(dataDate);
        } catch (Exception e) {
            statusHandler.handle(Priority.PROBLEM, e.getLocalizedMessage(), e);
            throw e;
        }
    }

    private void setRootUrl() {
        String webUrl = getUrl();
        int index = webUrl.lastIndexOf(".");
        rootUrl = webUrl.substring(0, index);
    }

    @Override
    public void setUrl(String url) {
        super.setUrl(url);
        setRootUrl();

        // If the URL changes, then the das object is also invalid
        das = null;
    }
}
