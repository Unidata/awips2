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
package com.raytheon.viz.gfe.dialogs.formatterlauncher;

/**
 * Config data class for the formatter launcher.
 * 
 * <pre>
 * SOFTWARE HISTORY
 * Date         Ticket#    Engineer    Description
 * ------------ ---------- ----------- --------------------------
 * 18 APR 2008  ###        lvenable    Initial creation 
 * 
 * </pre>
 * 
 * @author lvenable
 * @version 1.0
 * 
 */
public final class ConfigData {
    /**
     * Instance of the GHG configuration data.
     */
    private static ConfigData classInstance;

    /**
     * Enumeration of product states.
     * 
     * @author lvenable
     * 
     */
    public enum productStateEnum {
        New, Queued, Running, Finished, Transmitted, Failed;
    }

    /**
     * Private constructor.
     */
    private ConfigData() {
    }

    /**
     * Get an instance of this class.
     * 
     * @return An instance of this class.
     */
    public static synchronized ConfigData getInstance() {
        // If the Formatter Launcher configuration data has not been created
        // then create a new instance.
        if (classInstance == null) {
            classInstance = new ConfigData();
        }
        return classInstance;
    }
}
