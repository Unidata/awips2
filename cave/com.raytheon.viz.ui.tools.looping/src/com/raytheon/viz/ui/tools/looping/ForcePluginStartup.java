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
package com.raytheon.viz.ui.tools.looping;

import org.eclipse.ui.IStartup;

/**
 * A faux Eclipse Activator class that forces the class loader to aggressively
 * load this class and plugin. Allows tools and handlers in this plugin to
 * receive commands that were previously dropped until the plugin was loaded.
 * <p>
 * Refer to DR 14674 or RODO DR 7470 for more information.
 *
 * <pre>
 *
 * SOFTWARE HISTORY
 *
 * Date         Ticket#    Engineer    Description
 * ------------ ---------- ----------- --------------------------
 * Sep 21, 2018  7470      dgilling     Initial creation
 *
 * </pre>
 *
 * @author dgilling
 */

public class ForcePluginStartup implements IStartup {

    @Override
    public void earlyStartup() {
        // no-op
    }

}
