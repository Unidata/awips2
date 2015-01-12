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
package com.raytheon.uf.viz.monitor.scan.commondialogs;

import java.util.ArrayList;
import java.util.Arrays;

import org.eclipse.swt.SWT;
import org.eclipse.swt.events.SelectionAdapter;
import org.eclipse.swt.events.SelectionEvent;
import org.eclipse.swt.layout.GridData;
import org.eclipse.swt.layout.GridLayout;
import org.eclipse.swt.widgets.Button;
import org.eclipse.swt.widgets.Composite;
import org.eclipse.swt.widgets.Layout;
import org.eclipse.swt.widgets.Shell;

import com.raytheon.uf.common.monitor.scan.config.SCANConfigEnums.ScanTables;
import com.raytheon.uf.viz.monitor.scan.config.SCANConfig;
import com.raytheon.viz.ui.dialogs.CaveSWTDialog;

/**
 * SCAN Attributes Dialog.
 * 
 * <pre>
 * 
 * SOFTWARE HISTORY
 * 
 * Date         Ticket#    Engineer    Description
 * ------------ ---------- ----------- --------------------------
 * 24 Jul 2013  #2143      skorolev    Changes for non-blocking dialogs.
 * 
 * </pre>
 * 
 * @author
 * @version 1.0
 */
public class SCANAttributesDlg extends CaveSWTDialog implements
        ICommonDialogAction {

    /**
     * Attribute checkboxes.
     */
    private ArrayList<Button> attributeChkBtns;

    /**
     * Attribute Update Checkboxes.
     */
    private final IAttributeUpdate attributeUpdateCb;

    /**
     * SCAN Tables.
     */
    private final ScanTables scanTable;

    /**
     * Constructor
     * 
     * @param parent
     * @param scanTable
     * @param attributeUpdateCb
     */
    public SCANAttributesDlg(Shell parent, ScanTables scanTable,
            IAttributeUpdate attributeUpdateCb) {
        super(parent, SWT.DIALOG_TRIM, CAVE.DO_NOT_BLOCK);
        setText("Attributes");

        this.scanTable = scanTable;
        this.attributeUpdateCb = attributeUpdateCb;
    }

    /*
     * (non-Javadoc)
     * 
     * @see com.raytheon.viz.ui.dialogs.CaveSWTDialogBase#constructShellLayout()
     */
    @Override
    protected Layout constructShellLayout() {
        // Create the main layout for the shell.
        GridLayout mainLayout = new GridLayout(1, false);
        mainLayout.marginWidth = 2;
        mainLayout.verticalSpacing = 2;
        return mainLayout;
    }

    /*
     * (non-Javadoc)
     * 
     * @see
     * com.raytheon.viz.ui.dialogs.CaveSWTDialogBase#initializeComponents(org
     * .eclipse.swt.widgets.Shell)
     */
    @Override
    protected void initializeComponents(Shell shell) {
        attributeChkBtns = new ArrayList<Button>();
        createAttributeControls();
        createBottomButtons();
    }

    /**
     * Create Attribute Controls.
     */
    private void createAttributeControls() {
        SCANConfig scanCfg = SCANConfig.getInstance();

        String[] colNames = scanCfg.getTableColumnNames(scanTable);
        boolean[] visibleCols = scanCfg.getVisibleColumns(scanTable);

        for (int i = 0; i < colNames.length; i++) {
            GridData gd = new GridData();
            gd.horizontalIndent = 20;
            Button btn = new Button(shell, SWT.CHECK);
            btn.setText(colNames[i]);
            btn.setSelection(visibleCols[i]);
            btn.setLayoutData(gd);
            attributeChkBtns.add(btn);
        }
    }

    /**
     * Create Bottom Buttons.
     */
    private void createBottomButtons() {
        Composite buttonComp = new Composite(shell, SWT.NONE);
        buttonComp.setLayout(new GridLayout(2, false));
        buttonComp.setLayoutData(new GridData(SWT.FILL, SWT.DEFAULT, true,
                false));

        GridData gd = new GridData(SWT.CENTER, SWT.DEFAULT, true, false);
        gd.widthHint = 70;
        Button okBtn = new Button(buttonComp, SWT.PUSH);
        okBtn.setText("OK");
        okBtn.setLayoutData(gd);
        okBtn.addSelectionListener(new SelectionAdapter() {
            @Override
            public void widgetSelected(SelectionEvent e) {
                updateAction();
                closeDialog();
            }
        });

        gd = new GridData(SWT.CENTER, SWT.DEFAULT, true, false);
        gd.widthHint = 70;
        Button closeBtn = new Button(buttonComp, SWT.PUSH);
        closeBtn.setText("Exit");
        closeBtn.setLayoutData(gd);
        closeBtn.addSelectionListener(new SelectionAdapter() {
            @Override
            public void widgetSelected(SelectionEvent e) {
                closeDialog();
            }
        });
    }

    /**
     * Update Action.
     */
    private void updateAction() {
        boolean[] selectedCols = new boolean[attributeChkBtns.size()];
        Arrays.fill(selectedCols, true);

        for (int i = 0; i < attributeChkBtns.size(); i++) {
            selectedCols[i] = attributeChkBtns.get(i).getSelection();
        }

        // Call the call back with the updated columns
        attributeUpdateCb.attributeUpdates(selectedCols);
    }

    /*
     * (non-Javadoc)
     * 
     * @see com.raytheon.uf.viz.monitor.scan.commondialogs.ICommonDialogAction#
     * closeDialog()
     */
    @Override
    public void closeDialog() {
        close();
    }

}
