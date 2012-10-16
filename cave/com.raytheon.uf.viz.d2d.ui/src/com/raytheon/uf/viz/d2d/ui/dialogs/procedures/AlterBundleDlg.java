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
package com.raytheon.uf.viz.d2d.ui.dialogs.procedures;

import java.util.ArrayList;
import java.util.HashMap;
import java.util.List;
import java.util.Map;
import java.util.Map.Entry;

import org.eclipse.swt.SWT;
import org.eclipse.swt.events.SelectionAdapter;
import org.eclipse.swt.events.SelectionEvent;
import org.eclipse.swt.events.SelectionListener;
import org.eclipse.swt.layout.GridData;
import org.eclipse.swt.layout.GridLayout;
import org.eclipse.swt.widgets.Button;
import org.eclipse.swt.widgets.Composite;
import org.eclipse.swt.widgets.Label;
import org.eclipse.swt.widgets.Layout;
import org.eclipse.swt.widgets.Menu;
import org.eclipse.swt.widgets.MenuItem;
import org.eclipse.swt.widgets.Shell;

import com.raytheon.uf.common.status.IUFStatusHandler;
import com.raytheon.uf.common.status.UFStatus;
import com.raytheon.uf.common.status.UFStatus.Priority;
import com.raytheon.uf.viz.core.procedures.AlterBundleFactory;
import com.raytheon.uf.viz.core.procedures.Bundle;
import com.raytheon.uf.viz.core.procedures.IAlterBundleContributor;
import com.raytheon.viz.ui.dialogs.CaveSWTDialog;
import com.raytheon.viz.ui.widgets.MenuButton;

/**
 * Dialog for selecting an alternate bundle.
 * 
 * <pre>
 * 
 * SOFTWARE HISTORY
 * Date         Ticket#    Engineer    Description
 * ------------ ---------- ----------- --------------------------
 * Jan 4, 2010            mschenke     Initial creation
 * Jul 11, 2012 #875       rferrel     Return Value now only set
 *                                      to bundle on Load. Prevents
 *                                      the window's 'x' close from
 *                                      trying to perform a load.
 * Jul 31, 2012 #875       rferrel     Use MenuButton to organize entries
 *                                      in  menus.
 * 
 * </pre>
 * 
 * @author mschenke
 * @version 1.0
 */
public class AlterBundleDlg extends CaveSWTDialog {
    private static final transient IUFStatusHandler statusHandler = UFStatus
            .getHandler(AlterBundleDlg.class);

    private static final String Top_MENU_KEY = "<top>";

    private static final String MENU_SEP = IAlterBundleContributor.MENU_SEPARATOR;

    private static final String MI_SEP = IAlterBundleContributor.MI_SEPARATOR;

    private static final int MENU_SEP_LEN = MENU_SEP.length();

    private static class AlterBundleEntry {

        IAlterBundleContributor contributor;

        boolean enabled = false;

        String alterKey;

        String alterValue;

    }

    private List<AlterBundleEntry> entries = new ArrayList<AlterBundleEntry>();

    private Button cancelBtn;

    private Button loadBtn;

    private Bundle bundle;

    protected AlterBundleDlg(Bundle bundle, Shell parentShell) {
        super(parentShell);
        setText("Alter Bundle on Loading");

        this.bundle = bundle;
    }

    @Override
    protected Layout constructShellLayout() {
        // Create the main layout for the shell.
        GridLayout mainLayout = new GridLayout(1, true);
        mainLayout.marginHeight = 0;
        mainLayout.marginWidth = 0;
        return mainLayout;
    }

    @Override
    protected void initializeComponents(Shell shell) {
        Composite mainComposite = new Composite(shell, SWT.NONE);
        GridLayout mainLayout = new GridLayout(1, true);
        mainComposite.setLayout(mainLayout);

        initializeCombos(mainComposite);
        initializeBottomButtons(mainComposite);
    }

    private void initializeCombos(Composite parent) {
        Composite comp = new Composite(parent, SWT.NONE);
        GridLayout layout = new GridLayout(3, false);
        comp.setLayout(layout);
        for (IAlterBundleContributor contrib : AlterBundleFactory
                .getContributors()) {
            Map<String, String[]> alterables = contrib.getAlterables();
            final List<AlterBundleEntry> contribEntries = new ArrayList<AlterBundleEntry>(
                    alterables.size());
            // Create Enable button
            final Button enabledBtn = new Button(comp, SWT.CHECK);
            GridData gd = new GridData(SWT.CENTER, SWT.CENTER, false, false);
            gd.widthHint = 17;
            enabledBtn.setLayoutData(gd);
            enabledBtn.addSelectionListener(new SelectionAdapter() {
                /*
                 * (non-Javadoc)
                 * 
                 * @see
                 * org.eclipse.swt.events.SelectionAdapter#widgetSelected(org
                 * .eclipse.swt.events.SelectionEvent)
                 */
                @Override
                public void widgetSelected(SelectionEvent e) {
                    for (AlterBundleEntry entry : contribEntries) {
                        entry.enabled = enabledBtn.getSelection();
                    }
                }
            });

            int i = 0;
            for (Entry<String, String[]> entry : alterables.entrySet()) {
                if (i != 0) {
                    new Label(comp, SWT.NONE);
                }
                Label label = new Label(comp, SWT.CENTER);
                label.setText(entry.getKey() + " = ");

                final AlterBundleEntry abe = new AlterBundleEntry();
                abe.contributor = contrib;
                abe.alterKey = entry.getKey();
                entries.add(abe);
                contribEntries.add(abe);

                SelectionListener listener = new SelectionAdapter() {
                    /*
                     * (non-Javadoc)
                     * 
                     * @see
                     * org.eclipse.swt.events.SelectionAdapter#widgetSelected
                     * (org.eclipse.swt.events.SelectionEvent)
                     */
                    @Override
                    public void widgetSelected(SelectionEvent e) {
                        MenuButton menuButton = (MenuButton) e.getSource();
                        String value = menuButton.getSelectedItem().getText();
                        abe.alterValue = value;
                    }
                };
                MenuButton menuButton = new MenuButton(comp);
                gd = new GridData(SWT.FILL, SWT.FILL, true, true);
                menuButton.setLayoutData(gd);
                menuButton.addSelectionListener(listener);
                Menu menu = new Menu(menuButton);
                MenuItem topMi = createMenu(menu, entry);
                menuButton.setMenu(menu);
                menuButton.setSelectedItem(topMi);

                ++i;
            }

        }
    }

    private MenuItem createMenu(Menu topMenu, Entry<String, String[]> entry) {
        Map<String, Menu> menuMap = new HashMap<String, Menu>();

        menuMap.put(Top_MENU_KEY, topMenu);

        Menu menu = null;
        MenuItem mi = null;
        MenuItem topMi = null;

        for (String value : entry.getValue()) {
            if (value.startsWith(MENU_SEP)) {
                value = value.substring(MENU_SEP_LEN);
            }

            if (!value.contains(MENU_SEP)) {
                menu = menuMap.get(Top_MENU_KEY);
                if (value.equals(MI_SEP)) {
                    new MenuItem(menu, SWT.SEPARATOR);
                } else {
                    mi = new MenuItem(menu, SWT.PUSH);
                    mi.setText(value);
                    if (topMi == null) {
                        topMi = mi;
                    }
                }
            } else if (value.endsWith(MENU_SEP)) {
                // create new sub menu here.
                menu = menuMap.get(value);
                if (menu == null) {
                    String[] vals = parseNameParentKey(value);
                    String name = vals[0];
                    String parentKey = vals[1];
                    Menu parentMenu = menuMap.get(parentKey);
                    if (parentMenu != null) {
                        menu = new Menu(parentMenu);
                        mi = new MenuItem(parentMenu, SWT.CASCADE);
                        mi.setMenu(menu);
                        mi.setText(name);
                        menuMap.put(value, menu);
                    } else {
                        statusHandler.handle(Priority.DEBUG,
                                "Attempting to recreate a menu: " + value);
                    }
                }
            } else {
                // Add item to the desired menu
                String[] vals = parseNameParentKey(value);
                String name = vals[0];
                String parentKey = vals[1];
                menu = menuMap.get(parentKey);
                if (menu != null) {
                    if (name.equals(MI_SEP)) {
                        mi = new MenuItem(menu, SWT.SEPARATOR);
                    } else {
                        mi = new MenuItem(menu, SWT.NONE);
                        mi.setText(name);
                    }
                } else {
                    statusHandler.handle(Priority.DEBUG,
                            "Attempting to add an item to non-existent menu: "
                                    + value);
                }
            }
        }
        return topMi;
    }

    private String[] parseNameParentKey(String value) {
        String[] vals = value.split(MENU_SEP);
        String name = vals[vals.length - 1];
        String parentKey = null;
        if (vals.length == 1) {
            parentKey = Top_MENU_KEY;
        } else {
            StringBuilder sb = new StringBuilder();
            for (int j = 0; j < vals.length - 1; ++j) {
                sb.append(vals[j]).append(MENU_SEP);
            }
            parentKey = sb.toString();
        }
        return new String[] { name, parentKey };
    }

    private void initializeBottomButtons(Composite parent) {
        Composite buttonComp = new Composite(parent, SWT.NONE);
        GridLayout layout = new GridLayout(2, true);
        buttonComp.setLayout(layout);
        GridData gd = new GridData(SWT.CENTER, SWT.DEFAULT, true, true);
        buttonComp.setLayoutData(gd);

        loadBtn = new Button(buttonComp, SWT.PUSH);
        loadBtn.setText("Load");
        gd = new GridData(SWT.CENTER, SWT.CENTER, true, true);
        gd.widthHint = 75;
        loadBtn.setLayoutData(gd);
        loadBtn.addSelectionListener(new SelectionAdapter() {

            @Override
            public void widgetSelected(SelectionEvent e) {
                load();
            }

        });

        cancelBtn = new Button(buttonComp, SWT.PUSH);
        cancelBtn.setText("Cancel");
        gd = new GridData(SWT.CENTER, SWT.CENTER, true, true);
        gd.widthHint = 75;
        cancelBtn.setLayoutData(gd);
        cancelBtn.addSelectionListener(new SelectionAdapter() {

            @Override
            public void widgetSelected(SelectionEvent e) {
                cancel();
            }

        });
    }

    private void load() {
        for (AlterBundleEntry entry : entries) {
            if (entry.enabled) {
                entry.contributor.alterBundle(bundle, entry.alterKey,
                        entry.alterValue);
            }
        }
        setReturnValue(bundle);
        shell.close();
    }

    private void cancel() {
        bundle = null;
        shell.close();
    }
}
