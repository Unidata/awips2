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
package com.raytheon.uf.viz.alertview.ui.view;

import org.eclipse.jface.action.Action;
import org.eclipse.jface.action.IMenuManager;
import org.eclipse.swt.SWT;
import org.eclipse.swt.custom.SashForm;
import org.eclipse.swt.widgets.Composite;
import org.eclipse.swt.widgets.Display;
import org.eclipse.ui.IViewPart;
import org.eclipse.ui.IWorkbenchPage;
import org.eclipse.ui.IWorkbenchWindow;
import org.eclipse.ui.PartInitException;
import org.eclipse.ui.part.ViewPart;
import org.osgi.framework.BundleContext;
import org.osgi.framework.FrameworkUtil;
import org.osgi.framework.ServiceReference;
import org.osgi.framework.ServiceRegistration;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

import com.raytheon.uf.viz.alertview.Alert;
import com.raytheon.uf.viz.alertview.AlertDestination;
import com.raytheon.uf.viz.alertview.AlertStore;
import com.raytheon.uf.viz.alertview.filter.AlertFilter;
import com.raytheon.uf.viz.alertview.filter.FilterManager;
import com.raytheon.uf.viz.alertview.prefs.AlertViewPreferences;
import com.raytheon.uf.viz.alertview.prefs.AlertViewPreferences.FilterMenu;
import com.raytheon.uf.viz.alertview.prefs.PreferenceFile;

/**
 * 
 * {@link IViewPart} for displaying {@link Alert}s. View is essentially just a
 * combination of an {@link AlertTable} and an {@link AlertConsoleViewer}.
 * 
 * <pre>
 * 
 * SOFTWARE HISTORY
 * 
 * Date          Ticket#  Engineer  Description
 * ------------- -------- --------- --------------------------
 * Jun 18, 2015  4474     bsteffen  Initial creation
 * 
 * </pre>
 * 
 * @author bsteffen
 * @version 1.0
 */
public class AlertView extends ViewPart implements AlertDestination,
        PreferenceFile.Listener<AlertViewPreferences> {

    private static Logger logger = LoggerFactory.getLogger(AlertView.class);

    private ServiceRegistration<AlertDestination> destinationRegistration;

    private SashForm sashForm;

    private AlertTable alertTable;

    private AlertConsoleViewer detailsConsoleViewer;

    @Override
    public void createPartControl(Composite parent) {
        sashForm = new SashForm(parent, SWT.NONE);
        sashForm.setOrientation(SWT.VERTICAL);
        alertTable = new AlertTable(sashForm) {

            @Override
            protected void alertSelected() {
                detailsConsoleViewer.setAlert(alertTable.getSingleSelection());
            }

            @Override
            protected void alertDoubleClick() {
                if (sashForm.getMaximizedControl() == alertTable) {
                    sashForm.setMaximizedControl(null);
                } else {
                    sashForm.setMaximizedControl(alertTable);
                }
            }

        };
        detailsConsoleViewer = new AlertConsoleViewer(sashForm);

        PreferenceFile<AlertViewPreferences> file = new PreferenceFile<>(
                "alert_view.xml", AlertViewPreferences.class, this);

        /* TODO the menu button looks stupid in CAVE because of the small tabs. */
        IMenuManager menuManager = getViewSite().getActionBars()
                .getMenuManager();

        FilterManager filterManager = new FilterManager();
        AlertViewPreferences prefs = file.get();
        for (FilterMenu filter : prefs.getFilterMenu()) {
            Action action = new ShowFilteredAction(filter.getText(),
                    filterManager.getFilter(filter.getFilter()));
            if (prefs.getActiveFilter().equals(filter.getFilter())) {
                action.setChecked(true);
                action.run();
            }
            menuManager.add(action);
        }

        sashForm.setMaximizedControl(alertTable);
        loadAlerts();
    }

    private void loadAlerts() {
        BundleContext context = FrameworkUtil.getBundle(getClass())
                .getBundleContext();
        ServiceReference<AlertStore> ref = context
                .getServiceReference(AlertStore.class);
        if (ref != null) {
            AlertStore store = context.getService(ref);
            for (Alert alert : store.getAlerts()) {
                alertTable.addAlert(alert);
            }
            context.ungetService(ref);
        }
        destinationRegistration = context.registerService(
                AlertDestination.class, this, null);
        alertTable.packColumns();
    }

    @Override
    public void setFocus() {
        alertTable.setFocus();
    }

    public void rebuildColums() {
        Display.getDefault().asyncExec(new Runnable() {
            @Override
            public void run() {
                alertTable.rebuildColums();
                loadAlerts();
            }

        });
    }

    @Override
    public void dispose() {
        destinationRegistration.unregister();
    }

    @Override
    public void handleAlert(Alert alert) {
        alertTable.addAlert(alert);
    }

    private class ShowFilteredAction extends Action {

        private final AlertFilter filter;

        public ShowFilteredAction(String name, AlertFilter filter) {
            super("Show " + name, Action.AS_RADIO_BUTTON);
            this.filter = filter;
        }


        @Override
        public void run() {
            alertTable.setFilter(filter);
            detailsConsoleViewer.setAlert(alertTable.getSingleSelection());
            loadAlerts();
            // TODO save prefs.
        }

    }

    public static void show(IWorkbenchWindow window, Alert alert) {
        IWorkbenchPage activePage = window.getActivePage();

        try {
            AlertView view = (AlertView) activePage.showView(AlertView.class
                    .getName());
            view.alertTable.select(alert);
            view.sashForm.setMaximizedControl(null);
        } catch (PartInitException e) {
            logger.error("Cannot open AlertView", e);
        }
    }

    @Override
    public void update(AlertViewPreferences t) {
        // TODO update fitler menu.
    }
}
