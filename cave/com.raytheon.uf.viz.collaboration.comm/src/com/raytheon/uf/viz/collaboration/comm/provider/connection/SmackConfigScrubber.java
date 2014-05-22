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
package com.raytheon.uf.viz.collaboration.comm.provider.connection;

import java.lang.reflect.Field;

import org.jivesoftware.smack.ConnectionConfiguration;

import com.raytheon.uf.common.status.IUFStatusHandler;

/**
 * Sanitizes smack configuration object to keep sensitive information out of the
 * heap
 * 
 * <pre>
 * 
 * SOFTWARE HISTORY
 * 
 * Date         Ticket#    Engineer    Description
 * ------------ ---------- ----------- --------------------------
 * Apr 11, 2014 2903       bclement     Initial creation
 * 
 * </pre>
 * 
 * @author bclement
 * @version 1.0
 */
public class SmackConfigScrubber {

    private static final IUFStatusHandler log = CollaborationConnection
            .getStatusHandler();

    private static volatile Field passwordField;

    private static final String PASSWORD_FIELD_NAME = "password";

    private SmackConfigScrubber() {
    }

    /**
     * remove sensitive information from smack configuration
     * 
     * @param config
     */
    public static void scrubConfig(ConnectionConfiguration config) {
        if (passwordField == null) {
            synchronized (SmackConfigScrubber.class) {
                if (passwordField == null) {
                    passwordField = createPasswordField();
                }
            }
        }
        if (passwordField != null) {
            try {
                passwordField.set(config, null);
            } catch (Exception e) {
                log.debug("Unable to scrub xmpp configuration: "
                        + e.getLocalizedMessage());
            }
        }
    }

    /**
     * Create a field object for the connection configuration class object
     * 
     * @return null if unable to create
     */
    private static Field createPasswordField() {
        try {
            Class<?> c = ConnectionConfiguration.class;
            Field rval = c.getDeclaredField(PASSWORD_FIELD_NAME);
            rval.setAccessible(true);
            return rval;
        } catch (Exception e) {
            log.debug("Unable to access xmpp configuration: "
                    + e.getLocalizedMessage());
            return null;
        }
    }
}
