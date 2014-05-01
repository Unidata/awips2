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
package com.raytheon.sshp.action;

import ohd.hseb.sshp.SSHP;

import org.eclipse.core.commands.AbstractHandler;
import org.eclipse.core.commands.ExecutionEvent;
import org.eclipse.core.commands.ExecutionException;
import org.eclipse.jface.dialogs.MessageDialog;
import org.eclipse.swt.widgets.Shell;
import org.eclipse.ui.PlatformUI;

import com.raytheon.monitor.activator.Activator;
import com.raytheon.monitor.preferences.PreferenceConstants;
import com.raytheon.uf.common.ohd.AppsDefaults;
import com.raytheon.uf.viz.core.localization.LocalizationManager;

/**
 * Opens the SSHP dialog.
 * 
 * <pre>
 * 
 * SOFTWARE HISTORY
 * Date         Ticket#    Engineer    Description
 * ------------ ---------- ----------- --------------------------
 * 
 * 
 * </pre>
 * 
 * @author mfegan
 * @version 1.0
 */

public class OpenSshpAction extends AbstractHandler {

    public static final String JDBC_URL = "jdbcUrl";

    public static final String MISSING_PRESENTATION = "missingPresentation";

    public static final String HSEB_PREFERENCES = "ohd.hseb.monitor";

//    private static final String DB_CONNECTION_FORMAT = "jdbc:postgresql://%s:%s/%s?user=%s&password=%s";

    private String databaseConnectionString = "";

    private String logFileDir = "";

    /**
     * Constructor.
     */
    public OpenSshpAction() {
        // TODO Auto-generated constructor stub
        super();
    }

    /*
     * (non-Javadoc)
     * 
     * @see
     * org.eclipse.core.commands.IHandler#execute(org.eclipse.core.commands.
     * ExecutionEvent)
     */
    @Override
    public Object execute(ExecutionEvent event) throws ExecutionException {
//        getAppsDefaults();
        Shell shell = PlatformUI.getWorkbench().getActiveWorkbenchWindow()
                .getShell();
        databaseConnectionString = Activator.getDefault()
                .getPreferenceStore().getString(PreferenceConstants.JDBC_URL);
        logFileDir = Activator.getDefault().getPreferenceStore()
                .getString(PreferenceConstants.RIVERMON_LOG_DIR);
        LocalizationManager mgr = LocalizationManager.getInstance();
        String site = mgr.getCurrentSite();
        if (databaseConnectionString == null
                || databaseConnectionString.equals("")) {
            MessageDialog
                    .openError(
                            shell,
                            "Error Starting SSHP",
                            "Database connection string not set, or incorrect, please verify River Apps settings on preference page.");
        } else {
            System.out.println(databaseConnectionString);
            System.out.println(logFileDir);
            String[] args = { databaseConnectionString, logFileDir, site };
            SSHP.main(args);
        }
        return null;
    }

    /**
     * Not currently used, may be added back later.
     */
    private void getAppsDefaults() {
        AppsDefaults ad = AppsDefaults.getInstance();
        logFileDir = ad.getToken("sshp_log_dir");
    }
}
