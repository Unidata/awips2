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
package com.raytheon.fcstservice.action;

import ohd.hseb.fcstservice.LhvmApplicationWindow;

import org.eclipse.core.commands.AbstractHandler;
import org.eclipse.core.commands.ExecutionEvent;
import org.eclipse.core.commands.ExecutionException;
import org.eclipse.jface.dialogs.MessageDialog;
import org.eclipse.jface.preference.IPersistentPreferenceStore;
import org.eclipse.swt.widgets.Shell;
import org.eclipse.ui.PlatformUI;

import com.raytheon.monitor.activator.Activator;
import com.raytheon.monitor.preferences.PreferenceConstants;

/**
 * Performs actions needed to open the Forecast Services application.
 * 
 * <pre>
 *
 * SOFTWARE HISTORY
 *
 * Date         Ticket#    Engineer    Description
 * ------------ ---------- ----------- --------------------------
 * 	
 * 
 * </pre>
 *
 * @author mfegan
 * @version 1.0	
 */

public class OpenFcstService extends AbstractHandler {

    /**
     * 
     */
    public OpenFcstService() {
        super();
    }

    /* (non-Javadoc)
     * @see org.eclipse.core.commands.IHandler#execute(org.eclipse.core.commands.ExecutionEvent)
     */
    @Override
    public Object execute(ExecutionEvent event) throws ExecutionException {
        Shell shell = PlatformUI.getWorkbench().getActiveWorkbenchWindow().getShell();
        IPersistentPreferenceStore store = Activator.getDefault().getPreferenceStore();
        String databaseConnectionString = store.getString(PreferenceConstants.JDBC_URL);

        if (databaseConnectionString == null || databaseConnectionString.equals("")) {
            MessageDialog.openError(shell, "Error Starting Forecast Services", "Database connection string not set, or incorrect, please verify River Apps settings on preference page.");
        } else {
            String[] args = {databaseConnectionString};
            LhvmApplicationWindow.main(args);
//          MessageDialog.openInformation(shell, null, "Open Forecast Service is not yet implemented.");
        }
        return null;
    }

}
