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

import java.util.List;

import org.eclipse.swt.SWT;
import org.eclipse.swt.events.SelectionAdapter;
import org.eclipse.swt.events.SelectionEvent;
import org.eclipse.swt.layout.GridData;
import org.eclipse.swt.widgets.Button;
import org.eclipse.swt.widgets.Composite;
import org.eclipse.swt.widgets.Shell;

/**
 * Product table component for the clone dialog.
 * 
 * <pre>
 * 
 * SOFTWARE HISTORY
 * Date         Ticket#    Engineer    Description
 * ------------ ---------- ----------- --------------------------
 * Nov 9, 2009            jelkins     Initial creation
 * 
 * </pre>
 * 
 * @author jelkins
 * @version 1.0
 */

public class CloneProductTableComposite extends ProductTableComp {

    private final String CLOSE_BUTTON_TEXT = "Close";

    private final String NONE_BUTTON_TEXT = "None";

    private final String ALL_BUTTON_TEXT = "All";

    private final String CLONE_BUTTON_TEXT = "Clone";

    private final int BUTTON_WIDTH = 80;

    /**
     * @param parentComp
     * @param selectedProducts
     */
    public CloneProductTableComposite(Composite parentComp,
            List<ProductTableData> selectedProducts) {
        super(parentComp);

        for (ProductTableData productData : selectedProducts) {
            ProductTableData productDataClone = new ProductTableData(
                    productData);
            addProduct(productDataClone);
        }

    }

    /*
     * (non-Javadoc)
     * 
     * @see
     * com.raytheon.viz.volumebrowser.vbui.ProductTableComp#createBottomButtons
     * (org.eclipse.swt.widgets.Button[])
     */
    @Override
    protected void createBottomButtons(Button[] buttons) {

        Button diffButton = buttons[0];
        Button loadButton = buttons[1];

        super
                .createBottomButtons(new Button[] { diffButton,
                        getCloneButton(), getAllButton(), getNoneButton(),
                        loadButton, getCloseButton() });
    }

    /**
     * 
     */
    private Button getCloseButton() {
        Button closeButton = getButton(CLOSE_BUTTON_TEXT);
        closeButton.addSelectionListener(new SelectionAdapter() {
            @Override
            public void widgetSelected(SelectionEvent event) {
                getParent().dispose();
            }

        });
        return closeButton;
    }

    /**
     * 
     * @param buttonText
     * @return a simple button with the given button Text
     */
    private Button getButton(String buttonText) {
        GridData gd = new GridData(BUTTON_WIDTH, SWT.DEFAULT);
        Button button = new Button(getParent(), SWT.PUSH);
        button.setText(buttonText);
        button.setLayoutData(gd);
        return button;
    }

    /**
     * 
     */
    private Button getNoneButton() {
        Button noneButton = getButton(NONE_BUTTON_TEXT);
        noneButton.addSelectionListener(new SelectionAdapter() {
            @Override
            public void widgetSelected(SelectionEvent event) {
                selectNone();
            }

        });
        return noneButton;
    }

    /**
     * 
     */
    private Button getAllButton() {
        Button allButton = getButton(ALL_BUTTON_TEXT);
        allButton.addSelectionListener(new SelectionAdapter() {
            @Override
            public void widgetSelected(SelectionEvent event) {
                selectAll();
            }

        });
        return allButton;
    }

    /**
     * 
     */
    private Button getCloneButton() {
        Button cloneButton = getButton(CLONE_BUTTON_TEXT);
        cloneButton.addSelectionListener(new SelectionAdapter() {
            @Override
            public void widgetSelected(SelectionEvent event) {

                new CloneDialog(new Shell(SWT.MODELESS), getSelectedData())
                        .open();
            }

        });
        return cloneButton;
    }

}
