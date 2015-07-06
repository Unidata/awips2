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
import org.eclipse.jface.action.IAction;
import org.eclipse.jface.action.IMenuManager;
import org.eclipse.jface.action.MenuManager;
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
import com.raytheon.uf.viz.alertview.ui.prefs.OpenPreferencesAction;

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
 * Jun 22, 2015  4474     njensen   Fix bugs
 * 
 * </pre>
 * 
 * @author bsteffen
 * @version 1.0
 */
public class AlertView extends ViewPart implements AlertDestination,
        PreferenceFile.Listener<AlertViewPreferences> {

    private static Logger logger = LoggerFactory.getLogger(AlertView.class);

    private FilterManager filterManager = new FilterManager();

    private ServiceRegistration<AlertDestination> destinationRegistration;

    private PreferenceFile<AlertViewPreferences> preferencesFile;

    private SashForm sashForm;

    private AlertTable alertTable;

    private AlertConsoleViewer detailsConsoleViewer;

    @Override
    public void createPartControl(Composite parent) {
        preferencesFile = AlertViewPreferences.load(this);
        AlertViewPreferences preferences = preferencesFile.get();

        sashForm = new SashForm(parent, SWT.NONE);
        sashForm.setOrientation(SWT.VERTICAL);

        alertTable = new AlertTable(sashForm, preferences.getColumns()) {

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

        sashForm.setMaximizedControl(alertTable);

        alertTable.setMergeRepeatInterval(preferences.getMergeRepeatInterval());

        populateFilterMenu();
        destinationRegistration = getBundleContext().registerService(
                AlertDestination.class, this, null);
    }

    protected void populateFilterMenu() {
        /*
         * TODO the menu button creates excessive unusable space in CAVE because
         * there are no toolbar items and the tabs are so small.
         */
        IMenuManager menuManager = getViewSite().getActionBars()
                .getMenuManager();
        menuManager.removeAll();
        MenuManager filterMenu = new MenuManager("Show", null);
        menuManager.add(filterMenu);
        AlertViewPreferences prefs = preferencesFile.get();
        for (FilterMenu menuItem : prefs.getFilterMenu()) {
            Action action = new ShowFilteredAction(menuItem);
            if (prefs.getActiveFilter().equals(menuItem.getFilter())) {
                action.setChecked(true);
                action.run();
            }
            filterMenu.add(action);
        }
        menuManager.add(new OpenPreferencesAction());
    }

    private void loadAlerts() {
        BundleContext context = getBundleContext();
        ServiceReference<AlertStore> ref = context
                .getServiceReference(AlertStore.class);
        if (ref != null) {
            AlertStore store = context.getService(ref);
            for (Alert alert : store.getAlerts()) {
                alertTable.addAlert(alert);
            }
            context.ungetService(ref);
        }
        alertTable.packColumns();
    }

    @Override
    public void setFocus() {
        alertTable.setFocus();
    }

    @Override
    public void dispose() {
        destinationRegistration.unregister();
        preferencesFile.close();
        super.dispose();
    }

    @Override
    public void handleAlert(Alert alert) {
        alertTable.addAlert(alert);
    }

    @Override
    public void update(final AlertViewPreferences preferences) {
        Display display = Display.getCurrent();
        if (display.isDisposed()) {
            return;
        }
        display.asyncExec(new Runnable() {
            @Override
            public void run() {
                if (alertTable.isDisposed()) {
                    return;
                }
                alertTable.setMergeRepeatInterval(preferences
                        .getMergeRepeatInterval());
                alertTable.rebuildColums(preferences.getColumns());
                populateFilterMenu();
            }
        });
    }

    public static void show(IWorkbenchWindow window, Alert alert) {
        IWorkbenchPage activePage = window.getActivePage();

        try {
            AlertView view = (AlertView) activePage.showView(AlertView.class
                    .getName());
            if (alert != null) {
                view.handleAlert(alert);
                view.alertTable.select(alert);
                view.sashForm.setMaximizedControl(null);
            }
        } catch (PartInitException e) {
            logger.error("Cannot open AlertView", e);
        }
    }

    protected static BundleContext getBundleContext() {
        return FrameworkUtil.getBundle(AlertView.class).getBundleContext();
    }

    private class ShowFilteredAction extends Action {

        private final String filterName;

        private final AlertFilter filter;

        public ShowFilteredAction(FilterMenu menuItem) {
            super(menuItem.getText(), IAction.AS_RADIO_BUTTON);
            this.filterName = menuItem.getFilter();
            this.filter = filterManager.getFilter(filterName);
        }

        @Override
        public void run() {
            if (!this.filter.equals(alertTable.getFilter())) {
                alertTable.setFilter(filter);
                detailsConsoleViewer.setAlert(alertTable.getSingleSelection());
                loadAlerts();
                AlertViewPreferences prefs = preferencesFile.get();
                if (!filterName.equals(prefs.getActiveFilter())) {
                    prefs = new AlertViewPreferences(prefs);
                    prefs.setActiveFilter(filterName);
                    preferencesFile.write(prefs);
                }
            }
        }
    }
}
