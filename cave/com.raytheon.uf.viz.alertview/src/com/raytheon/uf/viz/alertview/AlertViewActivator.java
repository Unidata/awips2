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
package com.raytheon.uf.viz.alertview;

import org.eclipse.jface.preference.PreferenceManager;
import org.eclipse.jface.preference.PreferenceNode;
import org.eclipse.ui.IWorkbench;
import org.eclipse.ui.PlatformUI;
import org.osgi.framework.BundleActivator;
import org.osgi.framework.BundleContext;
import org.osgi.framework.Constants;
import org.osgi.framework.InvalidSyntaxException;
import org.osgi.framework.ServiceEvent;
import org.osgi.framework.ServiceListener;

import com.raytheon.uf.viz.alertview.ui.prefs.AlertViewPreferencePage;
import com.raytheon.uf.viz.alertview.ui.prefs.PopupPreferencePage;
import com.raytheon.uf.viz.alertview.ui.prefs.StylePreferencePage;

/**
 * 
 * {@link BundleActivator} that is used to only show the preference pages if
 * AlertView is being used.
 * 
 * <pre>
 * 
 * SOFTWARE HISTORY
 * 
 * Date          Ticket#  Engineer  Description
 * ------------- -------- --------- --------------------------
 * Jun 30, 2015  4474     bsteffen  Initial creation
 * 
 * </pre>
 * 
 * @author bsteffen
 * @version 1.0
 */
public class AlertViewActivator implements BundleActivator {


    @Override
    public void start(BundleContext context) throws InvalidSyntaxException {
        /*
         * Only show the preferences if this bundle is activated which will
         * happen through declarative services if anyone is using the
         * destination.
         */
        if (PlatformUI.isWorkbenchRunning()) {
            addPreferencePages();
        } else {
            String workbenchFilter = "(" + Constants.OBJECTCLASS + "="
                    + IWorkbench.class.getName() + ")";
            context.addServiceListener(new ServiceListener() {

                @Override
                public void serviceChanged(ServiceEvent event) {
                    if (event.getType() == ServiceEvent.REGISTERED) {
                        addPreferencePages();
                    }
                }
            }, workbenchFilter);
        }
    }

    protected void addPreferencePages() {
        PreferenceManager pm = PlatformUI.getWorkbench().getPreferenceManager();
        PreferenceNode alertViewNode = new PreferenceNode(
                AlertViewPreferencePage.class.getName(), "Alert View", null,
                null) {

            @Override
            public void createPage() {
                AlertViewPreferencePage page = new AlertViewPreferencePage();
                page.setTitle(getLabelText());
                page.setDescription("Configure AlertView appearance and behavior.");
                setPage(page);
            }

        };
        PreferenceNode popUpNode = new PreferenceNode(
                PopupPreferencePage.class.getName(), "Popup", null, null) {

            @Override
            public void createPage() {
                PopupPreferencePage page = new PopupPreferencePage();
                page.setTitle(getLabelText());
                page.setDescription("Configure Alert Popup appearance and behavior.");
                setPage(page);
            }

        };
        PreferenceNode styleNode = new PreferenceNode(
                StylePreferencePage.class.getName(), "Style", null, null) {

            @Override
            public void createPage() {
                StylePreferencePage page = new StylePreferencePage();
                page.setTitle(getLabelText());
                page.setDescription("Configure the way alerts appear in the alert table.");
                setPage(page);
            }

        };

        alertViewNode.add(popUpNode);
        alertViewNode.add(styleNode);
        pm.addToRoot(alertViewNode);
    }

    @Override
    public void stop(BundleContext context) throws Exception {
        if (PlatformUI.isWorkbenchRunning()) {
            PreferenceManager pm = PlatformUI.getWorkbench()
                    .getPreferenceManager();
            pm.remove(AlertViewPreferencePage.class.getName());
        }
    }

}
