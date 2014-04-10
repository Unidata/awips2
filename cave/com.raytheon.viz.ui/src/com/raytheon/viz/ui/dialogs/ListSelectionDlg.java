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
package com.raytheon.viz.ui.dialogs;

import org.eclipse.swt.SWT;
import org.eclipse.swt.events.MouseEvent;
import org.eclipse.swt.events.MouseListener;
import org.eclipse.swt.events.SelectionAdapter;
import org.eclipse.swt.events.SelectionEvent;
import org.eclipse.swt.layout.GridData;
import org.eclipse.swt.layout.GridLayout;
import org.eclipse.swt.widgets.Button;
import org.eclipse.swt.widgets.Composite;
import org.eclipse.swt.widgets.Layout;
import org.eclipse.swt.widgets.List;
import org.eclipse.swt.widgets.Shell;

/**
 * Dialog allowing the user to select one or more items from the provided list.
 * 
 * <pre>
 * 
 * SOFTWARE HISTORY
 * 
 * Date         Ticket#    Engineer    Description
 * ------------ ---------- ----------- --------------------------
 * Apr 09, 2014   2864     mpduff      Initial creation
 * 
 * </pre>
 * 
 * @author mpduff
 * @version 1.0
 */

public class ListSelectionDlg extends CaveSWTDialog {
    /** Stings to populate the list */
    private final String[] textChoices;

    /** Single/Multiple select flag */
    private final boolean singleSelect;

    /** The list widget */
    private List selectList;

    /**
     * Constructor.
     * 
     * @param parent
     *            Parent shell
     * @param textChoices
     *            Items for the list
     * @param singleSelect
     *            true if only a single selection allowed
     */
    public ListSelectionDlg(Shell parent, String[] textChoices,
            boolean singleSelect) {
        super(parent, SWT.TITLE | SWT.RESIZE | SWT.APPLICATION_MODAL,
                CAVE.DO_NOT_BLOCK);
        this.textChoices = textChoices;
        this.singleSelect = singleSelect;

        if (singleSelect) {
            setText("Select One");
        } else {
            setText("Select Items");
        }
    }

    /**
     * Single select list.
     * 
     * @param parent
     *            Parent shell
     * @param textChoices
     *            Items for the list
     */
    public ListSelectionDlg(Shell parent, String[] textChoices) {
        this(parent, textChoices, true);
    }

    @Override
    protected Layout constructShellLayout() {
        // Create the main layout for the shell.
        GridLayout mainLayout = new GridLayout(1, false);
        mainLayout.marginHeight = 2;
        mainLayout.marginWidth = 2;
        mainLayout.verticalSpacing = 2;
        return mainLayout;
    }

    @Override
    protected Object constructShellLayoutData() {
        return new GridData(SWT.FILL, SWT.DEFAULT, true, false);
    }

    @Override
    protected void initializeComponents(Shell shell) {
        GridData gd = new GridData(SWT.FILL, SWT.FILL, true, true);
        Composite mainComp = new Composite(shell, SWT.NONE);
        mainComp.setLayout(new GridLayout(1, false));
        mainComp.setLayoutData(gd);

        int style = SWT.SINGLE;
        if (!this.singleSelect) {
            style = SWT.MULTI;
        }

        style |= SWT.V_SCROLL | SWT.H_SCROLL;

        gd = new GridData(SWT.FILL, SWT.FILL, true, true);
        selectList = new List(mainComp, SWT.BORDER | style);
        selectList.setLayoutData(gd);
        selectList.addMouseListener(new MouseListener() {

            @Override
            public void mouseUp(MouseEvent e) {
                // No op
            }

            @Override
            public void mouseDown(MouseEvent e) {
                // No op
            }

            @Override
            public void mouseDoubleClick(MouseEvent e) {
                action();
            }
        });

        gd = new GridData(SWT.CENTER, SWT.DEFAULT, true, false);
        Composite btnComp = new Composite(shell, SWT.NONE);
        btnComp.setLayout(new GridLayout(2, false));
        btnComp.setLayoutData(gd);

        Button selectBtn = new Button(btnComp, SWT.PUSH);
        selectBtn.setLayoutData(new GridData(75, SWT.DEFAULT));
        selectBtn.setText("Select");
        selectBtn.addSelectionListener(new SelectionAdapter() {
            @Override
            public void widgetSelected(SelectionEvent e) {
                action();
            }
        });

        Button cancelBtn = new Button(btnComp, SWT.PUSH);
        cancelBtn.setLayoutData(new GridData(75, SWT.DEFAULT));
        cancelBtn.setText("Cancel");
        cancelBtn.addSelectionListener(new SelectionAdapter() {
            @Override
            public void widgetSelected(SelectionEvent e) {
                close();
            }
        });

        selectList.setItems(textChoices);
        this.shell.setMinimumSize(225, 275);
    }

    /**
     * Action handler.
     */
    private void action() {
        setReturnValue(selectList.getSelection());
        close();
    }
}
