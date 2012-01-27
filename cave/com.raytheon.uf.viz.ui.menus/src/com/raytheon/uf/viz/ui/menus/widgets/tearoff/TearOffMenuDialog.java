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
import org.eclipse.swt.graphics.Rectangle;
import org.eclipse.swt.layout.GridData;
import org.eclipse.swt.layout.GridLayout;
import org.eclipse.swt.widgets.Composite;
import org.eclipse.swt.widgets.Control;
import org.eclipse.swt.widgets.Display;
import org.eclipse.swt.widgets.Event;
import org.eclipse.swt.widgets.Listener;
import org.eclipse.swt.widgets.Menu;
import org.eclipse.swt.widgets.MenuItem;
import org.eclipse.swt.widgets.Monitor;
import org.eclipse.swt.widgets.Shell;

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
 * 
 * </pre>
 * 
 * @author mnash
 * @version 1.0
 */

public class TearOffMenuDialog extends CaveSWTDialog {

    private MenuItem[] items = null;

    private ScrolledComposite scrolledComp = null;

    private Composite fullComp = null;

    /**
     * @param parentShell
     */
    public TearOffMenuDialog(Menu menu) {
        super(VizWorkbenchManager.getInstance().getCurrentWindow().getShell(),
                SWT.DIALOG_TRIM | SWT.RESIZE, CAVE.DO_NOT_BLOCK);
        String text = menu.getParentItem().getText();

        // handle for the & that makes key bindings
        if (text.contains("&")) {
            text = text.replace("&", "");
        }
        setText(text);
        this.items = menu.getItems();
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

        // remove the first menu item which is the tear off item, so that it
        // doesn't accidentally appear anywhere else
        MenuItem[] preparedItems = new MenuItem[items.length - 1];
        for (int i = 1; i < items.length; i++) {
            preparedItems[i - 1] = items[i];
        }
        items = preparedItems;

        // TODO, handle radio items, probably in here to keep track of what
        // radio items are selected so that they can be deselected

        // go through menu items and build MenuItemComposite for each item,
        // which handles all the selection and color of the "MenuItem" in the
        // dialog
        int radioGroup = 0;
        for (int i = 0; i < items.length; i++) {
            int labelStyle = SWT.NONE;
            if (items[i] == null) {
                labelStyle = SWT.SEPARATOR | SWT.HORIZONTAL;
            }

            final MenuItemComposite comp = new MenuItemComposite(fullComp,
                    SWT.NONE);

            if (items[i].getStyle() == SWT.RADIO) {
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
            comp.addLabels(items[i], labelStyle);
        }
        scrolledComp.setContent(fullComp);
        scrolledComp.setExpandHorizontal(true);
        scrolledComp.setExpandVertical(true);
        scrolledComp.setMinSize(fullComp.computeSize(SWT.DEFAULT, SWT.DEFAULT));

        shell.setMinimumSize(150, fullComp.getSize().y);
        shell.pack();
        // sets the location based on the current shell size (after it is
        // packed)
        Monitor primary = Display.getCurrent().getPrimaryMonitor();
        Rectangle monitorBounds = primary.getBounds();
        Rectangle shellBounds = shell.getBounds();
        int x = (monitorBounds.width / 2) - (shellBounds.width / 2);
        int y = (monitorBounds.height / 2) - (shellBounds.height / 2);
        shell.setLocation(x, y);

        // close the dialog on perspective change
        shell.addListener(SWT.Hide, new Listener() {
            @Override
            public void handleEvent(Event event) {
                close();
            }
        });
    }

    @Override
    protected void disposed() {
        for (Control control : fullComp.getChildren()) {
            control.dispose();
        }
        super.disposed();
    }
}
