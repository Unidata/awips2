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
package com.raytheon.viz.aviation;

import java.io.FileNotFoundException;

import org.apache.commons.configuration.ConfigurationException;
import org.eclipse.core.commands.AbstractHandler;
import org.eclipse.core.commands.ExecutionEvent;
import org.eclipse.core.commands.ExecutionException;
import org.eclipse.swt.widgets.Display;

import com.raytheon.uf.common.aviation.avnconfig.TafSiteConfigFactory;
import com.raytheon.uf.common.status.IUFStatusHandler;
import com.raytheon.uf.common.status.UFStatus;
import com.raytheon.uf.common.status.UFStatus.Priority;

/**
 * Action for Aviation Configuration Plug-in
 * 
 * <pre>
 * SOFTWARE HISTORY
 * Date         Ticket#    Engineer    Description
 * ------------ ---------- ----------- --------------------------
 * May 15, 2008 1119       grichard     Initial creation.
 * Jul  9, 2010 5078        rferrel     Check for existence of
 *                                      config files in execute.
 * Oct 19, 2010 7347        rferrel     Replace reference to TAF_SITE_CONFIG
 * Oct 08, 2012 1229        rferrel     Changes to work with non-blocking AvnConfigDlg.
 * Feb 13, 2013 1549        rferrel     Minor code clean up.
 * Jan 26, 2016 5054        randerso    Change top level dialog to be parented to the display
 * May 15, 2019 20693   mgamazaychikov  TafSiteConfigFactory refactor
 * 
 * </pre>
 * 
 * @author grichard
 * @version 1.0
 */

public class AvnconfigAction extends AbstractHandler {
    private static final transient IUFStatusHandler statusHandler = UFStatus
            .getHandler(AvnconfigAction.class);

    private AvnconfigDlg avnfpsSetupDlg;

    /*
     * (non-Javadoc)
     * 
     * @see
     * org.eclipse.core.commands.AbstractHandler#execute(org.eclipse.core.commands
     * .ExecutionEvent)
     */
    @Override
    public Object execute(ExecutionEvent arg0) throws ExecutionException {
        // Do nothing when needed configuration files are missing or unreadable.
        try {
            TafSiteConfigFactory.getInstance();
        } catch (ConfigurationException e) {
            statusHandler.handle(Priority.PROBLEM, "Configuration error", e);
            return null;
        } catch (FileNotFoundException e) {
            statusHandler.handle(Priority.PROBLEM,
                    "Missing configuration file", e);
            return null;
        }

        if (avnfpsSetupDlg == null) {
            avnfpsSetupDlg = new AvnconfigDlg(Display.getCurrent());
        }
        avnfpsSetupDlg.open();

        return null;
    }

}
