package com.raytheon.uf.edex.datadelivery.retrieval.util;

import java.io.File;

import com.raytheon.uf.common.datadelivery.registry.Connection;
import com.raytheon.uf.common.datadelivery.registry.Provider;
import com.raytheon.uf.common.datadelivery.registry.ProviderCredentials;
import com.raytheon.uf.common.datadelivery.registry.ProviderKeyRequest.Status;
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
 * Aug 08, 2013 2180       mpduff      Corrected the filename and blanked the key before saving
 * Aug 23, 2013 2180       mpduff      Changed return types and add status messages.
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
     * Saves the connection for the provider encrypted to Localization by
     * providerKey This will be used by the SSMI to update userName and password
     * stores
     * 
     * @param creds
     *            ProviderCredentials object
     * 
     * @return ProviderCredentials object with status and message set
     */
    public static ProviderCredentials saveCredentials(ProviderCredentials creds) {
        Provider provider = creds.getProvider();
        Connection conn = null;
        String providerKey = creds.getProviderKey();

        try {
            // encrypt userName and password
            conn = provider.getConnection();
            conn.setProviderKey(providerKey);
            conn.encryptUserName();
            conn.encryptPassword();
        } catch (Exception e) {
            statusHandler.handle(Priority.ERROR,
                    "Failed! Couldn't encrypt credentials!", e);
            creds.setMessage("Error encrypting credentials.  See server log for error details.");
            creds.setStatus(Status.FAILURE);
            return creds;
        }

        try {
            ProviderKeyRecord pkr = new ProviderKeyRecord(provider.getName(),
                    providerKey);
            ProviderKeyDao pkd = new ProviderKeyDao();
            pkd.addOrUpdateRecord(pkr);
        } catch (Exception e) {
            statusHandler.handle(Priority.ERROR,
                    "Failed! Couldn't store provider key record!", e);
            creds.setMessage("Error storing provider key record.  See server log for error details.");
            creds.setStatus(Status.FAILURE);
            return creds;
        }

        if (conn != null && providerKey != null) {
            try {
                storeConnection(conn, provider.getName());
            } catch (Exception e) {
                statusHandler
                        .handle(Priority.ERROR,
                                "Failed! Couldn't store encrypted Connection to Localization!",
                                e);
                creds.setStatus(Status.FAILURE);
                creds.setMessage("Error saving encrypted connection.  See server log for error details.");
                return creds;
            }
        }

        creds.setStatus(Status.SUCCESS);
        return creds;
    }

    /**
     * Deletes the connection for the provider.
     * 
     * @param creds
     *            ProviderCredentials object
     * 
     * @return ProviderCredentials object with status and message set
     */
    public static ProviderCredentials deleteCredentials(
            ProviderCredentials creds) {
        String providerName = creds.getProvider().getName();
        IPathManager pm = PathManagerFactory.getPathManager();
        LocalizationContext lc = pm.getContext(LocalizationType.COMMON_STATIC,
                LocalizationLevel.SITE);

        String connectionFileName = CONNECTION_FILE_PREFIX + providerName
                + CONNECTION_FILE_SUFFIX;
        try {
            LocalizationFile lf = pm
                    .getLocalizationFile(lc, connectionFileName);
            lf.delete();

            ProviderKeyDao pkd = new ProviderKeyDao();
            ProviderKeyRecord pkr = pkd.queryByProvider(providerName);
            if (pkr != null) {
                pkd.delete(pkr);
            }
            creds.setStatus(Status.SUCCESS);
        } catch (Exception e) {
            statusHandler.handle(Priority.ERROR,
                    "Couldn't delete encrypted Connection!", e);
            creds.setStatus(Status.FAILURE);
            creds.setMessage("Error deleting encrypted connection.  See server log for error details.");
        }
        return creds;
    }

    /**
     * Gets the ProviderCredentials object containing the encrytped credentials.
     * 
     * @param providerName
     *            The data provider name
     * 
     * @return The ProviderCredentials object
     * @throws Exception
     *             Exception on error
     */
    public static ProviderCredentials retrieveCredentials(String providerName)
            throws Exception {
        ProviderCredentials creds = new ProviderCredentials();

        Connection conn = getConnection(providerName);

        if (conn != null) {
            ProviderKeyDao pkd = new ProviderKeyDao();
            ProviderKeyRecord pkr = pkd.queryByProvider(providerName);
            if (pkr != null) {
                conn.setProviderKey(pkr.getProviderKey());
            }
        }
        creds.setConnection(conn);
        return creds;
    }

    /**
     * Gets the connection file containing the encryption method, username, and
     * encrypted password.
     * 
     * @param providerName
     *            The data provider name
     * @return The Connection object, or null if localization file doesn't
     *         exist.
     * @throws Exception
     *             On error
     */
    public static Connection getConnection(String providerName)
            throws Exception {

        IPathManager pm = PathManagerFactory.getPathManager();
        LocalizationContext lc = pm.getContext(LocalizationType.COMMON_STATIC,
                LocalizationLevel.SITE);

        String connectionFileName = CONNECTION_FILE_PREFIX + providerName
                + CONNECTION_FILE_SUFFIX;

        LocalizationFile lf = pm.getLocalizationFile(lc, connectionFileName);
        File file = lf.getFile();

        if (!file.exists()) {
            if (statusHandler.isPriorityEnabled(Priority.DEBUG)) {
                statusHandler.handle(Priority.DEBUG, providerName
                        + " connection file: " + file.getAbsolutePath()
                        + " does not exist.");
            }
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
     *            The Connection object
     * @param providerName
     *            The data provider's name
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

        /*
         * Nulling the providerKey for security reasons. You never want the
         * providerKey and encrypted username and password stored together
         */
        conn.setProviderKey(null);
        SerializationUtil.jaxbMarshalToXmlFile(conn, file.getAbsolutePath());
    }
}
