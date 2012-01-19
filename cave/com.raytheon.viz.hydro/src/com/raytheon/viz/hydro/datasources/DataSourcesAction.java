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

package com.raytheon.viz.hydro.datasources;

import org.eclipse.core.commands.AbstractHandler;
import org.eclipse.core.commands.ExecutionEvent;
import org.eclipse.core.commands.ExecutionException;
import org.eclipse.swt.widgets.Shell;
import org.eclipse.ui.PlatformUI;

import com.raytheon.viz.hydrocommon.HydroDisplayManager;
import com.raytheon.viz.hydrocommon.datasources.DataSourcesDlg;

/**
 * Action for Data Sources Dialog.
 * 
 * <pre>
 * 
 * SOFTWARE HISTORY
 * 
 * Date       	Ticket#		Engineer	Description
 * ------------	----------	-----------	--------------------------
 * 6/27/06                  lvenable    Initial creation.
 * 10/6/2008    1555        grichard    Support data sources.
 * 12/16/2008   1782        grichard    Refreshed Data Sources.
 * 1/11/2008    1802        askripsk    HydroBase implementation.
 * 
 * </pre>
 * 
 * @author lvenable
 * 
 */
public class DataSourcesAction extends AbstractHandler {

    @Override
    public Object execute(ExecutionEvent arg0) throws ExecutionException {
        Shell shell = PlatformUI.getWorkbench().getActiveWorkbenchWindow()
                .getShell();

        HydroDisplayManager manager = HydroDisplayManager.getInstance();
        if (manager.isCurrentLidSelected(shell)) {
            String lid = manager.getCurrentLid();
            String name = manager.getCurrentData().getName();

            String displayString = " - "
                    + lid
                    + ((name != null && name.compareTo("") != 0) ? " - " + name
                            : "");

            DataSourcesDlg dataSourcesDlg = new DataSourcesDlg(shell,
                    displayString, lid, false);
            dataSourcesDlg.open();
        }

        return null;
    }

}
