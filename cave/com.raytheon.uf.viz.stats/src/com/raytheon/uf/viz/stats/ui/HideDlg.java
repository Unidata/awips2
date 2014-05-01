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

import java.util.Arrays;

import org.eclipse.swt.SWT;
import org.eclipse.swt.events.SelectionAdapter;
import org.eclipse.swt.events.SelectionEvent;
import org.eclipse.swt.layout.GridData;
import org.eclipse.swt.layout.GridLayout;
import org.eclipse.swt.widgets.Button;
import org.eclipse.swt.widgets.Composite;
import org.eclipse.swt.widgets.Layout;
import org.eclipse.swt.widgets.List;
import org.eclipse.swt.widgets.Shell;

import com.raytheon.viz.ui.dialogs.CaveSWTDialog;

/**
 * Hide graph lines dialog.
 * 
 * <pre>
 * 
 * SOFTWARE HISTORY
 * 
 * Date         Ticket#    Engineer    Description
 * ------------ ---------- ----------- --------------------------
 * Dec 4, 2012    1357     mpduff      Initial creation.
 * 
 * </pre>
 * 
 * @author mpduff
 * @version 1.0
 */

public class HideDlg extends CaveSWTDialog {
    /** Data List Widget */
    private List dataList;

    /** List of keys */
    private final java.util.List<String> keyList;

    /** Callback to hide the lines */
    private final IGroupSelection callback;

    /**
     * Constructor.
     * 
     * @param parent
     *            The parent shell
     * @param keyList
     *            The list of keys
     * @param callback
     *            the callback
     */
    public HideDlg(Shell parent, java.util.List<String> keyList,
            IGroupSelection callback) {
        super(parent);
        setText("Hide Graph Lines");

        this.keyList = keyList;
        this.callback = callback;
    }

    /**
     * {@inheritDoc}
     */
    @Override
    protected Layout constructShellLayout() {
        // Create the main layout for the shell.
        GridLayout mainLayout = new GridLayout(1, true);
        mainLayout.marginHeight = 1;
        mainLayout.marginWidth = 1;
        return mainLayout;
    }

    /**
     * {@inheritDoc}
     */
    @Override
    protected void initializeComponents(Shell shell) {
        setReturnValue(false);

        Composite mainComp = new Composite(shell, SWT.NONE);
        GridLayout gl = new GridLayout(1, false);
        gl.marginHeight = 0;
        gl.marginWidth = 0;
        gl.horizontalSpacing = 0;
        gl.verticalSpacing = 5;
        GridData gd = new GridData(SWT.FILL, SWT.FILL, true, true);
        mainComp.setLayout(gl);
        mainComp.setLayoutData(gd);

        GridData listData = new GridData(SWT.FILL, SWT.FILL, true, true);
        listData.widthHint = 200;
        listData.heightHint = 300;
        dataList = new List(mainComp, SWT.BORDER | SWT.MULTI | SWT.V_SCROLL
                | SWT.H_SCROLL);
        dataList.setLayoutData(listData);
        dataList.setItems(keyList.toArray(new String[keyList.size()]));

        Composite buttonComp = new Composite(shell, SWT.NONE);
        buttonComp.setLayout(new GridLayout(2, false));
        buttonComp.setLayoutData(new GridData(SWT.CENTER, SWT.DEFAULT, true,
                false));

        int buttonWidth = 120;
        gd = new GridData(buttonWidth, SWT.DEFAULT);
        Button hideBtn = new Button(buttonComp, SWT.PUSH);
        hideBtn.setText("Hide Graph Lines");
        hideBtn.setLayoutData(gd);
        hideBtn.addSelectionListener(new SelectionAdapter() {
            @Override
            public void widgetSelected(SelectionEvent e) {
                handleHide();
            }
        });

        gd = new GridData(buttonWidth, SWT.DEFAULT);
        Button closeBtn = new Button(buttonComp, SWT.PUSH);
        closeBtn.setText("Close");
        closeBtn.setLayoutData(gd);
        closeBtn.addSelectionListener(new SelectionAdapter() {
            @Override
            public void widgetSelected(SelectionEvent e) {
                close();
            }
        });
    }

    /**
     * Hide event handler
     */
    private void handleHide() {
        if (dataList.getSelectionCount() > 0) {
            String[] itemsToHide = dataList.getSelection();
            callback.setItemsOff(Arrays.asList(itemsToHide));
            for (String item : itemsToHide) {
                dataList.remove(item);
            }
        }
    }
}
