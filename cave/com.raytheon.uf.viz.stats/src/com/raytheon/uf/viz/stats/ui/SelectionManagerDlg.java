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
package com.raytheon.uf.viz.stats.ui;

import java.util.ArrayList;
import java.util.HashMap;
import java.util.List;
import java.util.Map;

import org.eclipse.swt.SWT;
import org.eclipse.swt.events.SelectionAdapter;
import org.eclipse.swt.events.SelectionEvent;
import org.eclipse.swt.layout.GridData;
import org.eclipse.swt.layout.GridLayout;
import org.eclipse.swt.widgets.Button;
import org.eclipse.swt.widgets.Composite;
import org.eclipse.swt.widgets.Event;
import org.eclipse.swt.widgets.Layout;
import org.eclipse.swt.widgets.Listener;
import org.eclipse.swt.widgets.MessageBox;
import org.eclipse.swt.widgets.Shell;
import org.eclipse.swt.widgets.Tree;
import org.eclipse.swt.widgets.TreeItem;

import com.raytheon.uf.common.stats.data.GraphData;
import com.raytheon.viz.ui.dialogs.CaveSWTDialogBase;

/**
 * Stats Selection Manager Dialog.
 *
 * <pre>
 *
 * SOFTWARE HISTORY
 *
 * Date         Ticket#    Engineer    Description
 * ------------ ---------- ----------- --------------------------
 * Oct 18, 2012            lvenable     Initial creation
 *
 * </pre>
 *
 * @author lvenable
 * @version 1.0
 */

public class SelectionManagerDlg extends CaveSWTDialogBase {
    /** Main Composite */
    private Composite mainComp;

    /** Selection tree */
    private Tree selectionTree;

    /** GraphData object */
    private final GraphData graphData;

    /** Group selection callback */
    private final IGroupSelection callback;

    /**
     * Constructor.
     *
     * @param parentShell
     * @param graphData
     * @param callback
     */
    public SelectionManagerDlg(Shell parentShell, GraphData graphData,
            IGroupSelection callback) {
        super(parentShell, SWT.DIALOG_TRIM | SWT.MIN | SWT.RESIZE,
                CAVE.DO_NOT_BLOCK | CAVE.MODE_INDEPENDENT
                        | CAVE.INDEPENDENT_SHELL);
        setText("Selection Manager");

        this.graphData = graphData;
        this.callback = callback;
    }

    /**
     * {@inheritDoc}
     */
    @Override
    protected Layout constructShellLayout() {
        // Create the main layout for the shell.
        GridLayout mainLayout = new GridLayout(1, false);
        mainLayout.marginHeight = 2;
        mainLayout.marginWidth = 2;

        return mainLayout;
    }

    /**
     * {@inheritDoc}
     */
    @Override
    protected void initializeComponents(Shell shell) {
        shell.setMinimumSize(280, 400);
        mainComp = new Composite(shell, SWT.NONE);
        GridLayout gl = new GridLayout(1, false);
        gl.marginHeight = 0;
        gl.marginWidth = 0;
        gl.horizontalSpacing = 0;
        gl.verticalSpacing = 5;
        GridData gd = new GridData(SWT.FILL, SWT.FILL, true, true);
        mainComp.setLayout(gl);
        mainComp.setLayoutData(gd);

        initControls();

        populateTree();
    }

    /**
     * Initialize controls
     */
    private void initControls() {
        createSelectionTree();
        createActionControls();
    }

    /**
     * Create selection tree
     */
    private void createSelectionTree() {
        GridData gd = new GridData(SWT.FILL, SWT.FILL, true, true);
        gd.widthHint = 250;
        gd.heightHint = 350;
        selectionTree = new Tree(mainComp, SWT.BORDER | SWT.CHECK);
        selectionTree.setLayoutData(gd);
        selectionTree.addListener(SWT.Selection, new Listener() {
            @Override
            public void handleEvent(Event event) {
                if (event.detail == SWT.CHECK) {
                    TreeItem item = (TreeItem) event.item;
                    boolean checked = item.getChecked();
                    checkItems(item, checked);
                    checkPath(item.getParentItem(), checked, false);
                }
            }
        });
    }

    /**
     * Create the buttons
     */
    private void createActionControls() {
        Composite buttonComp = new Composite(shell, SWT.NONE);
        buttonComp.setLayout(new GridLayout(3, false));
        buttonComp.setLayoutData(new GridData(SWT.CENTER, SWT.DEFAULT, true,
                false));

        int buttonWidth = 80;

        GridData gd = new GridData(buttonWidth, SWT.DEFAULT);
        Button okBtn = new Button(buttonComp, SWT.PUSH);
        okBtn.setText("OK");
        okBtn.setLayoutData(gd);
        okBtn.addSelectionListener(new SelectionAdapter() {
            @Override
            public void widgetSelected(SelectionEvent e) {
                handleApply();
                close();
            }
        });

        gd = new GridData(buttonWidth, SWT.DEFAULT);
        Button applyBtn = new Button(buttonComp, SWT.PUSH);
        applyBtn.setText("Apply");
        applyBtn.setLayoutData(gd);
        applyBtn.addSelectionListener(new SelectionAdapter() {
            @Override
            public void widgetSelected(SelectionEvent e) {
                handleApply();
            }
        });

        gd = new GridData(buttonWidth, SWT.DEFAULT);
        Button cancelBtn = new Button(buttonComp, SWT.PUSH);
        cancelBtn.setText("Cancel");
        cancelBtn.setLayoutData(gd);
        cancelBtn.addSelectionListener(new SelectionAdapter() {
            @Override
            public void widgetSelected(SelectionEvent e) {
                close();
            }
        });
    }

    /**
     * Check the path of the item.
     *
     * @param item
     * @param checked
     * @param grayed
     */
    private void checkPath(TreeItem item, boolean checked, boolean grayed) {
        if (item == null) {
            return;
        }
        if (grayed) {
            checked = true;
        } else {
            int index = 0;
            TreeItem[] items = item.getItems();
            while (index < items.length) {
                TreeItem child = items[index];
                if (child.getGrayed() || checked != child.getChecked()) {
                    checked = grayed = true;
                    break;
                }
                index++;
            }
        }
        item.setChecked(checked);
        item.setGrayed(grayed);
        checkPath(item.getParentItem(), checked, grayed);
    }

    /**
     * Check or uncheck the items in the TreeItem
     *
     * @param item
     * @param checked
     */
    private void checkItems(TreeItem item, boolean checked) {
        item.setGrayed(false);
        item.setChecked(checked);
        TreeItem[] items = item.getItems();
        for (int i = 0; i < items.length; i++) {
            checkItems(items[i], checked);
        }
    }

    /**
     * Populate the tree
     */
    private void populateTree() {
        Map<String, List<String>> grpMemberMap = graphData
                .getGroupAndNamesMap();

        for (String key : grpMemberMap.keySet()) {
            TreeItem treeItem = new TreeItem(selectionTree, SWT.NONE);
            treeItem.setText(key);
            treeItem.setChecked(true);
            List<String> array = grpMemberMap.get(key);

            for (String subKey : array) {
                TreeItem subTreeItem = new TreeItem(treeItem, SWT.NONE);
                subTreeItem.setText(subKey);

                subTreeItem.setChecked(true);
            }
        }
    }

    /**
     * Apply button action handler.
     */
    private void handleApply() {
        Map<String, Map<String, Boolean>> selectionMap = new HashMap<String, Map<String, Boolean>>();
        List<String> badList = new ArrayList<String>();

        for (TreeItem item : selectionTree.getItems()) {
            if (!item.getChecked()) {
                badList.add(item.getText());
            }

            Map<String, Boolean> map = new HashMap<String, Boolean>();
            for (TreeItem subItem : item.getItems()) {
                map.put(subItem.getText(), subItem.getChecked());
            }
            selectionMap.put(item.getText(), map);
        }

        // Notify user of situation resulting in no data being graphed
        if (badList.size() > 0) {
            StringBuilder sb = new StringBuilder();
            sb.append("The following ");
            if (badList.size() > 1) {
                sb.append("groups have");
            } else {
                sb.append("group has");
            }
            sb.append(" no selections made\n");
            sb.append("and will result in an empty graph.\n\n");

            for (int i = 0; i < badList.size(); i++) {
                sb.append(badList.get(i)).append("\n");
            }
            sb.append("\nDo you wish to continue?");
            MessageBox mb = new MessageBox(shell, SWT.ICON_QUESTION | SWT.YES
                    | SWT.NO);
            mb.setText("Empty Graph");
            mb.setMessage(sb.toString());
            int choice = mb.open();

            if (choice == SWT.YES) {
                callback.setSelections(selectionMap);
            }
        } else {
            callback.setSelections(selectionMap);
        }

    }
}
