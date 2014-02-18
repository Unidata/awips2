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
package com.raytheon.openfire.plugin.configuration.collaboration.configuration;

import java.io.File;
import java.io.FileInputStream;
import java.io.IOException;
import java.util.Properties;

import org.jivesoftware.util.JiveGlobals;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

/**
 * Preferences wrapper for packet format
 * 
 * <pre>
 * 
 * SOFTWARE HISTORY
 * 
 * Date         Ticket#    Engineer    Description
 * ------------ ---------- ----------- --------------------------
 * Feb 10, 2014 2756       bclement     Initial creation
 * 
 * </pre>
 * 
 * @author bclement
 * @version 1.0
 */
public class PacketPreferences {

    private static final Logger log = LoggerFactory
            .getLogger(PacketPreferences.class);

    // format property file

    public static final String CONFIG_FILE_KEY = "plugin.collaboration.packet.format.file";

    private static final String DEFAULT_PLUGIN_CONFIG_FILE = "conf"
            + File.separator + "configurationPacketFormat.properties";

    private static final Properties properties = new Properties();

    static {
        String confFile = JiveGlobals.getProperty(CONFIG_FILE_KEY,
                DEFAULT_PLUGIN_CONFIG_FILE);
        File f = new File(JiveGlobals.getHomeDirectory(), confFile);
        if (f.exists()) {
            try {
                properties.load(new FileInputStream(f));
            } catch (IOException e) {
                // defaults will be used
                log.error("Problem loading packet format configuration file: "
                        + f.getAbsolutePath(), e);
            }
        } else {
            log.info("Using default config packet format since there was no format file at "
                    + f.getAbsolutePath());
        }
    }

    /**
     * Get property from configuration file. Returns defaultValue if key doesn't
     * exist
     * 
     * @param key
     * @param defaultValue
     * @return
     */
    public static String get(String key, String defaultValue) {
        return properties.getProperty(key, defaultValue);
    }

}
