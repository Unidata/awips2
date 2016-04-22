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

import java.util.HashMap;
import java.util.Map;

import org.eclipse.core.commands.AbstractHandler;
import org.eclipse.core.commands.ExecutionEvent;
import org.eclipse.core.commands.ExecutionException;
import org.eclipse.swt.widgets.Shell;
import org.eclipse.ui.handlers.HandlerUtil;

import com.raytheon.viz.hydrocommon.HydroDisplayManager;
import com.raytheon.viz.hydrocommon.datasources.DataSourcesDlg;
import com.raytheon.viz.ui.dialogs.ICloseCallback;

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
 * 07/15/2013   2088        rferrel     Changes for non-blocking DataSourceDlg.
 * 04/22/2016   5483        dgilling    Code cleanup.
 * 
 * </pre>
 * 
 * @author lvenable
 * 
 */
public class DataSourcesAction extends AbstractHandler {
    private final Map<String, DataSourcesDlg> dataSourcesDlgMap = new HashMap<>();

    @Override
    public Object execute(ExecutionEvent arg0) throws ExecutionException {
        Shell shell = HandlerUtil.getActiveShellChecked(arg0);

        HydroDisplayManager manager = HydroDisplayManager.getInstance();
        if (manager.isCurrentLidSelected(shell)) {
            final String lid = manager.getCurrentLid();
            DataSourcesDlg dataSourcesDlg = dataSourcesDlgMap.get(lid);

            if ((dataSourcesDlg == null) || (dataSourcesDlg.isDisposed())) {
                String name = manager.getCurrentData().getName();

                StringBuilder displayString = new StringBuilder(" - ");
                displayString.append(lid);
                if (name != null && name.length() > 0) {
                    displayString.append(" - ").append(name);
                }

                dataSourcesDlg = new DataSourcesDlg(shell,
                        displayString.toString(), lid, false);
                dataSourcesDlg.addCloseCallback(new ICloseCallback() {

                    @Override
                    public void dialogClosed(Object returnValue) {
                        dataSourcesDlgMap.remove(lid);
                    }
                });
                dataSourcesDlg.open();
                dataSourcesDlgMap.put(lid, dataSourcesDlg);
            } else {
                dataSourcesDlg.bringToTop();
            }
        }

        return null;
    }

}
