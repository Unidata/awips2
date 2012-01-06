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
package com.raytheon.monitor.action;

import ohd.hseb.monitor.Monitor;

import org.eclipse.core.commands.AbstractHandler;
import org.eclipse.core.commands.ExecutionEvent;
import org.eclipse.core.commands.ExecutionException;
import org.eclipse.jface.dialogs.MessageDialog;
import org.eclipse.jface.preference.IPersistentPreferenceStore;

import com.raytheon.monitor.activator.Activator;

/**
 * TODO Add Description OpenRiverMonitor.java 11 Sept, 2008
 * 
 * <pre>
 * SOFTWARE HISTORY
 * Date         Ticket#    Engineer    Description
 * ------------ ---------- ----------- --------------------------
 *    10sept2008  #1509     dhladky    Finished Monitor integration.
 * 
 * </pre>
 * 
 * @author dhladky
 * @version 1.0
 */

public class OpenPrecipMonitorAction extends AbstractHandler {

    /*
     * (non-Javadoc)
     * 
     * @see org.eclipse.core.commands.AbstractHandler#execute(org.eclipse.core.commands.ExecutionEvent)
     */
    @Override
    public Object execute(ExecutionEvent arg0) throws ExecutionException {
        IPersistentPreferenceStore store = Activator.getDefault()
                .getPreferenceStore();
        String databaseConnectionString = store
                .getString(com.raytheon.monitor.preferences.PreferenceConstants.JDBC_URL);
        String missingRepresent = store
                .getString(com.raytheon.monitor.preferences.PreferenceConstants.MISSING_PRESENTATION);
        // connectionString
        // missingRepresentation.
        // PRECIP, or else, launches RiverMontior...
        if (databaseConnectionString == null
                || databaseConnectionString.equalsIgnoreCase("")) {
            MessageDialog
                    .openError(
                            null,
                            "Error Starting Precip Monitor",
                            "Database connection string not set, or incorrect, please verify River Apps settings on preference page.");
        } else {
            String[] args = { databaseConnectionString, missingRepresent,
                    "Precip" };
            if (!Monitor.precipExists()) {
                Monitor.getPrecipInstance().main(args);
            } else {
                System.out.println("PrecipMonitor...Already running!");
            }
        }
        return null;
    }
}

