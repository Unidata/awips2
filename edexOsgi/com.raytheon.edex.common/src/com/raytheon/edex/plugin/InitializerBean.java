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

package com.raytheon.edex.plugin;

import org.apache.commons.logging.Log;
import org.apache.commons.logging.LogFactory;

/**
 * This class is instantiated on startup by the ESB spring container context
 * before any other beans. Therefore, any system level initialization activities
 * may be placed here.
 * <p>
 * The basic functionality of this class will initialize all the plugins
 * 
 * <pre>
 * SOFTWARE HISTORY
 * Date         Ticket#    Engineer    Description
 * ------------ ---------- ----------- --------------------------
 * 02/06/09     1990       bphillip    Initial creation
 * </pre>
 * 
 * @author bphillip
 * @version 1.0
 */
public class InitializerBean {

    /** The logger */
    protected transient Log logger = LogFactory.getLog(getClass());

    /**
     * Creates a new Initializer bean
     * <p>
     * This bean should never be explicitly instantiated
     */
    public InitializerBean() {
        // Check to see if the plugin version table exists. If not, the schema
        // needs to be exported to the database
        /*
         * pvd = new PluginVersionDao(); if (!pvd.isDbInitialized()) {
         * logger.warn("Databases have not been initialized...");
         * logger.info("Initializing Databases...");
         * logger.info("Exporting common table ddls...");
         * SchemaManager.getInstance().createCommonTables();
         * logger.info("Running setup scripts..."); try {
         * SchemaManager.getInstance().runCommonScripts(); } catch
         * (PluginException e) { logger.fatal("ERROR RUNNING COMMON SCRIPTS!",
         * e); }
         * 
         * }
         */
        logger.info("Database initialization complete!");
    }

}
