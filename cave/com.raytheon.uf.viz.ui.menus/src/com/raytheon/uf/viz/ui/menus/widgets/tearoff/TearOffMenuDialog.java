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
package com.raytheon.uf.viz.ui.menus.widgets.tearoff;

import org.eclipse.swt.SWT;
import org.eclipse.swt.custom.ScrolledComposite;
import org.eclipse.swt.graphics.Point;
import org.eclipse.swt.layout.GridData;
import org.eclipse.swt.layout.GridLayout;
import org.eclipse.swt.widgets.Composite;
import org.eclipse.swt.widgets.Control;
import org.eclipse.swt.widgets.Display;
import org.eclipse.swt.widgets.Event;
import org.eclipse.swt.widgets.Listener;
import org.eclipse.swt.widgets.Menu;
import org.eclipse.swt.widgets.MenuItem;
import org.eclipse.swt.widgets.Shell;
import org.eclipse.ui.PlatformUI;
import org.eclipse.ui.services.IServiceLocator;

import com.raytheon.uf.common.time.ISimulatedTimeChangeListener;
import com.raytheon.uf.common.time.SimulatedTime;
import com.raytheon.uf.viz.core.ContextManager;
import com.raytheon.uf.viz.ui.menus.widgets.BundleContributionItem;
import com.raytheon.viz.ui.VizWorkbenchManager;
import com.raytheon.viz.ui.dialogs.CaveSWTDialog;

/**
 * "Tear-off" menus to emulate A1 behavior
 * 
 * <pre>
 * 
 * SOFTWARE HISTORY
 * 
 * Date         Ticket#    Engineer    Description
 * ------------ ---------- ----------- --------------------------
 * Sep 14, 2011            mnash     Initial creation
 * Jan 09, 2013 1442       rferrel     Add Simulated Time Change Listener.
 * 
 * </pre>
 * 
 * @author mnash
 * @version 1.0
 */

public class TearOffMenuDialog extends CaveSWTDialog {

    private MenuItem[] items;

    private ScrolledComposite scrolledComp;

    private Composite fullComp;

    /**
     * Listener to force the dialog's items' display to be updated when user
     * changes Simulated time.
     */
    ISimulatedTimeChangeListener stcl;

    /**
     * @param parentShell
     */
    public TearOffMenuDialog(Menu menu) {
        super(VizWorkbenchManager.getInstance().getCurrentWindow().getShell(),
                SWT.DIALOG_TRIM | SWT.RESIZE, CAVE.DO_NOT_BLOCK);
        String text = menu.getParentItem().getText();
        this.items = menu.getItems();

        // handle for the & that makes key bindings
        setText(text.replace("&", ""));
    }

    @Override
    protected void initializeComponents(final Shell shell) {
        // allow for scrolling if necessary
        scrolledComp = new ScrolledComposite(shell, SWT.V_SCROLL);
        GridData gd = new GridData(SWT.FILL, SWT.FILL, true, true);
        scrolledComp.setLayoutData(gd);
        fullComp = new Composite(scrolledComp, SWT.NONE);
        GridLayout fullLayout = new GridLayout();
        fullLayout.marginHeight = 0;
        fullLayout.marginWidth = 0;
        // don't want any space between the two controls
        fullLayout.horizontalSpacing = 0;
        fullComp.setLayout(fullLayout);
        gd = new GridData(SWT.FILL, SWT.FILL, true, true);
        fullComp.setLayoutData(gd);

        // go through menu items and build MenuItemComposite for each item,
        // which handles all the selection and color of the "MenuItem" in the
        // dialog
        int radioGroup = 0;
        for (int i = 1; i < items.length; i++) {
            MenuItem item = items[i];
            MenuItemComposite comp = new MenuItemComposite(fullComp, SWT.NONE);

            if (item.getStyle() == SWT.RADIO) {
                comp.setData("radioGroup", radioGroup);
            } else {
                radioGroup++;
            }

            GridLayout layout = new GridLayout(2, false);
            layout.marginHeight = 0;
            layout.marginWidth = 0;
            comp.setLayout(layout);
            gd = new GridData(SWT.FILL, SWT.DEFAULT, true, false);
            comp.setLayoutData(gd);

            // add the labels to the dialog with each of the MenuItems
            comp.addLabels(item, SWT.NONE);
        }
        scrolledComp.setContent(fullComp);
        scrolledComp.setExpandHorizontal(true);
        scrolledComp.setExpandVertical(true);
        scrolledComp.setMinSize(fullComp.computeSize(SWT.DEFAULT, SWT.DEFAULT));

        shell.setMinimumSize(150, fullComp.getSize().y);
        shell.pack();
        Point point = Display.getCurrent().getCursorLocation();
        int offset = shell.getBounds().width / 2;
        int x = point.x - offset;
        int y = point.y;
        shell.setLocation(x, y);

        // close the dialog on perspective change
        shell.addListener(SWT.Hide, new Listener() {
            @Override
            public void handleEvent(Event event) {
                close();
            }
        });
    }

    /*
     * (non-Javadoc)
     * 
     * @see com.raytheon.viz.ui.dialogs.CaveSWTDialogBase#opened()
     */
    @Override
    protected void opened() {
        final IServiceLocator locator = PlatformUI.getWorkbench();

        Listener activate = new Listener() {
            @Override
            public void handleEvent(Event event) {
                ContextManager.getInstance(locator).activateContexts(
                        perspectiveManager);
            }
        };
        Listener deactivate = new Listener() {
            @Override
            public void handleEvent(Event event) {
                if (Display.getCurrent().getActiveShell() == getShell()) {
                    ContextManager.getInstance(locator).deactivateContexts(
                            perspectiveManager);
                }
            }
        };

        addListener(SWT.Activate, activate);
        addListener(SWT.Deactivate, deactivate);
        addListener(SWT.Close, deactivate);

        activate.handleEvent(new Event());

        stcl = new ISimulatedTimeChangeListener() {

            @Override
            public void timechanged() {
                updateItems();
            }
        };
        SimulatedTime.getSystemTime().addSimulatedTimeChangeListener(stcl);
    }

    @Override
    protected void disposed() {
        SimulatedTime.getSystemTime().removeSimulatedTimeChangeListener(stcl);
        for (Control control : fullComp.getChildren()) {
            control.dispose();
        }
        super.disposed();
    }

    /**
     * Force update of item's display.
     */
    private void updateItems() {
        // items[0] is the tear off object and is not in the dialog's display.
        for (int index = 1; index < items.length; ++index) {
            MenuItem item = items[index];
            if (item.getData() instanceof BundleContributionItem) {
                ((BundleContributionItem) item.getData()).refreshText();
            }
        }
    }
}
