package com.raytheon.uf.edex.datadelivery.retrieval.util;

import java.io.File;

import com.raytheon.uf.common.datadelivery.registry.Connection;
import com.raytheon.uf.common.datadelivery.registry.Provider;
import com.raytheon.uf.common.localization.IPathManager;
import com.raytheon.uf.common.localization.LocalizationContext;
import com.raytheon.uf.common.localization.LocalizationContext.LocalizationLevel;
import com.raytheon.uf.common.localization.LocalizationContext.LocalizationType;
import com.raytheon.uf.common.localization.LocalizationFile;
import com.raytheon.uf.common.localization.PathManagerFactory;
import com.raytheon.uf.common.serialization.SerializationUtil;
import com.raytheon.uf.common.status.IUFStatusHandler;
import com.raytheon.uf.common.status.UFStatus;
import com.raytheon.uf.common.status.UFStatus.Priority;
import com.raytheon.uf.edex.datadelivery.retrieval.db.ProviderKeyDao;
import com.raytheon.uf.edex.datadelivery.retrieval.db.ProviderKeyRecord;
/**
 * This software was developed and / or modified by Raytheon Company,
 * pursuant to Contract DG133W-05-CQ-1067 with the US Government.
 * 
 * U.S. EXPORT CONTROLLED TECHNICAL DATA
 * This software product contains export-restricted data whose
 * export/transfer/disclosure is restricted by U.S. law. Dissemination
 * to non-U.S. persons whether in the United States or abroad requires
 * an export license or other authorization.
 * 
 * Contractor Name:        Raytheon Company
 * Contractor Address:     6825 Pine Street, Suite 340
 *                         Mail Stop B8
 *                         Omaha, NE 68106
 *                         402.291.0100
 * 
 * See the AWIPS II Master Rights File ("Master Rights File.pdf") for
 * further licensing information.
 **/

/**
 * 
 * Provider Credentials Util
 * 
 * <pre>
 * 
 * SOFTWARE HISTORY
 * 
 * Date         Ticket#    Engineer    Description
 * ------------ ---------- ----------- --------------------------
 * Jul 10, 2013 2180       dhladky     Initial
 * 
 * </pre>
 * 
 * @author dhladky
 * @version 1.0
 */

public class ProviderCredentialsUtil {
    
    private static final IUFStatusHandler statusHandler = UFStatus
            .getHandler(ProviderCredentialsUtil.class);
    
    private static final String CONNECTION_FILE_PREFIX = "datadelivery"
            + IPathManager.SEPARATOR + "connection" + IPathManager.SEPARATOR;
    
    private static final String CONNECTION_FILE_SUFFIX = "-connection.xml";
        
    /**
     * Saves the connection for the provider encrypted to Localization by providerKey
     * This will be used by the SSMI to update userName and password stores
     * @param providerKey
     * @param conn
     */
    public static boolean saveCredentials(String providerKey, Provider provider) {

        Connection conn = null;

        try {
            // encrypt userName and password
            conn = provider.getConnection();
            conn.setProviderKey(providerKey);
            conn.encryptUserName();
            conn.encryptPassword();
        } catch (Exception e) {
            statusHandler.handle(Priority.ERROR,
                    "Failed! Couldn't encrypt credentials!", e);
            return false;
        }

        try {
            ProviderKeyRecord pkr = new ProviderKeyRecord(provider.getName(), providerKey);
            ProviderKeyDao pkd = new ProviderKeyDao();
            pkd.addOrUpdateRecord(pkr);
        } catch (Exception e) {
            statusHandler.handle(Priority.ERROR,
                    "Failed! Couldn't store provider key record!", e);
            return false;
        }

        if (conn != null && providerKey != null) {
            try {
                storeConnection(conn, providerKey);
            } catch (Exception e) {
                statusHandler
                        .handle(Priority.ERROR,
                                "Failed! Couldn't store encrypted Connection to Localization!",
                                e);
                return false;
            }
        }
        
        return true;
    }
    
    /**
     * Gets the encrypted credentials connection object stored locally
     * 
     * @param providerKey
     * @return
     */
    public static Connection retrieveCredentials(String providerName)
            throws Exception {

        // retrieve the providerKey from the name
        Connection conn = getConnection(providerName);

        if (conn != null) {

            ProviderKeyDao pkd = new ProviderKeyDao();
            ProviderKeyRecord pkr = pkd.queryByProvider(providerName);
            if (pkr != null) {
                conn.setProviderKey(pkr.getProviderKey());
            }
        }

        return conn;
    }

    /**
     * Gets the connection file containing the encryption method, username, and
     * encrypted password.
     * 
     * @param providerName
     * @return
     */
    private static Connection getConnection(String providerName) throws Exception {

        IPathManager pm = PathManagerFactory.getPathManager();
        LocalizationContext lc = pm.getContext(LocalizationType.COMMON_STATIC,
                LocalizationLevel.SITE);

        String connectionFileName = CONNECTION_FILE_PREFIX + providerName
                + CONNECTION_FILE_SUFFIX;

        LocalizationFile lf = pm.getLocalizationFile(lc, connectionFileName);
        File file = lf.getFile();
        // System.out.println("Reading -- " + file.getAbsolutePath());
        if (!file.exists()) {
            statusHandler.handle(Priority.DEBUG, providerName
                    + " connection file: " + file.getAbsolutePath()
                    + " does not exist.");
            return null;
        }

        Connection conn = SerializationUtil.jaxbUnmarshalFromXmlFile(
                Connection.class, file.getAbsolutePath());

        return conn;

    }

    /**
     * Stores the local connection file containing the encryption method,
     * username, and encrypted password.
     * 
     * @param Connection
     * @param providerName
     * @return
     */
    private static void storeConnection(Connection conn, String providerName)
            throws Exception {

        IPathManager pm = PathManagerFactory.getPathManager();
        LocalizationContext lc = pm.getContext(LocalizationType.COMMON_STATIC,
                LocalizationLevel.SITE);

        String connectionFileName = CONNECTION_FILE_PREFIX + providerName
                + CONNECTION_FILE_SUFFIX;

        LocalizationFile lf = pm.getLocalizationFile(lc, connectionFileName);
        File file = lf.getFile();

        SerializationUtil.jaxbMarshalToXmlFile(conn, file.getAbsolutePath());
    }

}
