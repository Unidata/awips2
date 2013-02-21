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
package com.raytheon.uf.common.util;

import java.io.ByteArrayInputStream;
import java.io.IOException;
import java.util.Properties;

/**
 * Configuration for deploy tests.
 * 
 * <pre>
 * 
 * SOFTWARE HISTORY
 * 
 * Date         Ticket#    Engineer    Description
 * ------------ ---------- ----------- --------------------------
 * Sep 14, 2012 1169       djohnson     Initial creation
 * Dec 06, 2012 1397       djohnson     Also set the request server.
 * 
 * </pre>
 * 
 * @author djohnson
 * @version 1.0
 */
public final class DeployTestProperties {

    private static volatile DeployTestProperties INSTANCE;

    private final String dataDeliveryServer;

    private final String userId;

    private final String requestServer;

    /**
     * Private constructor.
     * 
     * @param properties
     */
    private DeployTestProperties(Properties properties) {
        this.dataDeliveryServer = properties
                .getProperty("datadelivery.server");
        this.requestServer = properties.getProperty("request.server");
        this.userId = properties.getProperty("user.id");
    }

    /**
     * @return the dataDeliveryServer
     */
    public String getDataDeliveryServer() {
        return dataDeliveryServer;
    }

    /**
     * @return
     */
    public String getRequestServer() {
        return requestServer;
    }

    /**
     * @return the user id
     */
    public String getUserId() {
        return userId;
    }

    /**
     * Return the singleton instance.
     * 
     * @param dataDeliveryServer
     *            the url
     * @see http://en.wikipedia.org/wiki/Double-checked_locking#Usage_in_Java
     */
    public static DeployTestProperties getInstance() {
        DeployTestProperties result = INSTANCE;
        if (result == null) {
            synchronized (DeployTestProperties.class) {
                result = INSTANCE;
                if (result == null) {
                    // If the configuration grows beyond several properties,
                    // should probably change to use Spring and a custom
                    // properties object
                    byte[] file = TestUtil.readResource(
                            DeployTestProperties.class,
                            "deploy-test.properties");
                    Properties properties;
                    try {
                        properties = PropertiesUtil
                                .read(new ByteArrayInputStream(file));
                    } catch (IOException e) {
                        throw new IllegalStateException(
                                "Unable to read the deploy test configuration file!",
                                e);
                    }
                    INSTANCE = result = new DeployTestProperties(properties);
                }
            }
        }
        return result;
    }
}
