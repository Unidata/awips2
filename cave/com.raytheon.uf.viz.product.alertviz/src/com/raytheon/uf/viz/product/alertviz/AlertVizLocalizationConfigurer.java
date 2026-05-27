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
package com.raytheon.uf.viz.product.alertviz;

import com.raytheon.uf.common.localization.LocalizationContext.LocalizationLevel;
import com.raytheon.uf.viz.core.ProgramArguments;
import com.raytheon.uf.viz.core.localization.LocalizationManager;

/**
 * Configures extra localization information for AlertViz. This whole class
 * exists to get around the fact that plugins can contribute custom localization
 * levels but AlertViz is not full of extra plugins and should not be full of
 * extra plugins.  AlertViz still needs to respect localization levels to some
 * degree, hence this workaround class.
 * 
 * <pre>
 *
 * SOFTWARE HISTORY
 *
 * Date         Ticket#    Engineer    Description
 * ------------ ---------- ----------- --------------------------
 * Sep 21, 2015  4759      njensen     Initial creation
 *
 * </pre>
 * 
 * @author njensen
 * @version 1.0
 */

public class AlertVizLocalizationConfigurer {

    private static final String DESK = "DESK";

    private AlertVizLocalizationConfigurer() {
        // don't allow instantiation
    }

    /**
     * Registers other localization levels that AlertViz may need but not have
     * plugins contributing.
     */
    public static void registerExtraLevels() {
        /*
         * This code is borrowed from NmapCommon and NcPathManager. Should that
         * code change, this code should change.
         */
        String deskName = ProgramArguments.getInstance().getString("-desk");
        if (deskName != null) {
            LocalizationLevel deskLevel = LocalizationLevel.createLevel(DESK,
                    650);
            LocalizationManager.registerContextName(deskLevel, deskName);
        }

    }

}
