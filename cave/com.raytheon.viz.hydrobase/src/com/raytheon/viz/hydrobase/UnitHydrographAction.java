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
package com.raytheon.viz.hydrobase;

import org.eclipse.core.commands.ExecutionException;

import com.raytheon.uf.viz.app.launcher.handlers.AppLauncherHandler;

/**
 * This class displays the main Hydrobase dialog.
 * 
 * <pre>
 * SOFTWARE HISTORY
 * Date         Ticket#     Engineer    Description
 * ------------ ----------  ----------- --------------------------
 * 05/05/2009   2181        mpduff      Initial creation.
 * 
 * </pre>
 * 
 * @author mpduff
 * @version 1.0
 */
public class UnitHydrographAction {
    private static final String UHG_BUNDLE_LOC = "bundles/run-UnitHydrograph.xml";

    /**
     * Empty constructor.
     */
    public UnitHydrographAction() {

    }

    /**
     * Launch the Unit Hydrograph.
     */
    public void launch(String lid) {
        try {
            AppLauncherHandler alh = new AppLauncherHandler();
            alh.execute(UHG_BUNDLE_LOC, lid);
        } catch (ExecutionException e) {
            // TODO Auto-generated catch block
            e.printStackTrace();
        }
    }

}
