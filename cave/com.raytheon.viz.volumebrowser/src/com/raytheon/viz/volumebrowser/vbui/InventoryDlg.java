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
package com.raytheon.viz.volumebrowser.vbui;

import org.eclipse.swt.SWT;
import org.eclipse.swt.events.MouseAdapter;
import org.eclipse.swt.events.MouseEvent;
import org.eclipse.swt.events.SelectionAdapter;
import org.eclipse.swt.events.SelectionEvent;
import org.eclipse.swt.layout.GridData;
import org.eclipse.swt.layout.GridLayout;
import org.eclipse.swt.widgets.Button;
import org.eclipse.swt.widgets.Composite;
import org.eclipse.swt.widgets.Label;
import org.eclipse.swt.widgets.Layout;
import org.eclipse.swt.widgets.List;
import org.eclipse.swt.widgets.Shell;

import com.raytheon.viz.ui.dialogs.CaveSWTDialog;

/**
 * 
 * Dialog to display an inventory list for the select product.
 * 
 * <pre>
 * 
 * SOFTWARE HISTORY
 * Date         Ticket#    Engineer    Description
 * ------------ ---------- ----------- --------------------------
 * May 27, 2009 #2161      lvenable     Initial creation
 * Jan 16, 2013 #1492      rferrel      Made dialog non-blocking.
 * 
 * </pre>
 * 
 * @author lvenable
 * @version 1.0
 */
public class InventoryDlg extends CaveSWTDialog {

    /**
     * Product name.
     */
    private String productName;

    /**
     * Inventory list.
     */
    private List inventoryList;

    /**
     * Product label.
     */
    private Label productLabel;

    /**
     * Time label.
     */
    private Label timeLabel;

    private String time;

    private ProductInventory inventoryMap;

    public InventoryDlg(Shell parent, ProductTableData tableData) {
        super(parent, SWT.DIALOG_TRIM | SWT.RESIZE, CAVE.DO_NOT_BLOCK);
        setText("Inventory");

        this.productName = tableData.getName();
        this.time = tableData.getTime();
        this.inventoryMap = tableData.getProductInventory();
        setReturnValue(this.productName);
    }

    @Override
    protected Layout constructShellLayout() {
        return new GridLayout(1, false);
    }

    @Override
    protected Object constructShellLayoutData() {
        return new GridData(SWT.FILL, SWT.FILL, true, true);
    }

    @Override
    protected void initializeComponents(Shell shell) {
        // Initialize all of the controls and layouts
        createListControls();
        createCloseButton();
        createInventoryList();
    }

    @Override
    protected void preOpened() {
        shell.setMinimumSize(shell.getSize());
    }

    private void createInventoryList() {
        for (String forcastTime : inventoryMap.getForecastTimes()) {
            inventoryList.add(forcastTime);
        }
    }

    /**
     * Create the list controls.
     */
    private void createListControls() {
        productLabel = new Label(shell, SWT.NONE);
        productLabel.setText(productName);
        productLabel.setLayoutData(new GridData(SWT.FILL, SWT.DEFAULT, true,
                false));

        addSeparator(shell);

        timeLabel = new Label(shell, SWT.NONE);
        timeLabel.setText("Latest Time: " + time);
        timeLabel
                .setLayoutData(new GridData(SWT.FILL, SWT.DEFAULT, true, false));

        addSeparator(shell);

        Label inventoryLbl = new Label(shell, SWT.NONE);
        inventoryLbl.setText("Inventory:");

        GridData gd = new GridData(SWT.FILL, SWT.FILL, true, true);
        gd.widthHint = 250;
        gd.heightHint = 300;
        inventoryList = new List(shell, SWT.BORDER | SWT.V_SCROLL);
        inventoryList.setLayoutData(gd);

        /*
         * Add selection listenerList to deselect any items when the mouse is
         * clicked.
         */
        inventoryList.addSelectionListener(new SelectionAdapter() {
            @Override
            public void widgetSelected(SelectionEvent e) {
                inventoryList.deselectAll();
            }
        });

        inventoryList.addMouseListener(new MouseAdapter() {
            @Override
            public void mouseDown(MouseEvent e) {
                inventoryList.deselectAll();
            }

            @Override
            public void mouseUp(MouseEvent e) {
                inventoryList.deselectAll();
            }
        });
    }

    /**
     * Create the close button.
     */
    private void createCloseButton() {
        Composite buttonComp = new Composite(shell, SWT.NONE);
        GridData gd = new GridData(SWT.CENTER, SWT.DEFAULT, true, false);
        buttonComp.setLayout(new GridLayout(1, false));
        buttonComp.setLayoutData(gd);

        gd = new GridData(80, SWT.DEFAULT);
        Button closeBtn = new Button(buttonComp, SWT.PUSH);
        closeBtn.setText("Close");
        closeBtn.setLayoutData(gd);
        closeBtn.addSelectionListener(new SelectionAdapter() {
            public void widgetSelected(SelectionEvent event) {
                shell.dispose();
            }
        });
    }

    /**
     * Add a horizontal separator to the display.
     * 
     * @param parentComp
     */
    private void addSeparator(Composite parentComp) {
        GridLayout gl = (GridLayout) parentComp.getLayout();

        GridData gd = new GridData(SWT.FILL, SWT.DEFAULT, true, false);
        gd.horizontalSpan = gl.numColumns;
        Label sepLbl = new Label(parentComp, SWT.SEPARATOR | SWT.HORIZONTAL);
        sepLbl.setLayoutData(gd);
    }
}
