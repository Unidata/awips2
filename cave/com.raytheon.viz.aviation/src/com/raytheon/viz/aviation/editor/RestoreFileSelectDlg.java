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
package com.raytheon.viz.aviation.editor;

import java.util.Calendar;
import java.util.TimeZone;

import org.eclipse.swt.SWT;
import org.eclipse.swt.events.SelectionAdapter;
import org.eclipse.swt.events.SelectionEvent;
import org.eclipse.swt.graphics.Font;
import org.eclipse.swt.layout.GridData;
import org.eclipse.swt.layout.GridLayout;
import org.eclipse.swt.widgets.Button;
import org.eclipse.swt.widgets.Composite;
import org.eclipse.swt.widgets.Dialog;
import org.eclipse.swt.widgets.Display;
import org.eclipse.swt.widgets.Label;
import org.eclipse.swt.widgets.List;
import org.eclipse.swt.widgets.Shell;

import com.raytheon.uf.common.dataplugin.text.db.StdTextProduct;
import com.raytheon.viz.texteditor.TextWorkstationNotStartedException;

public class RestoreFileSelectDlg extends Dialog {
    /**
     * Dialog shell.
     */
    private Shell shell;

    /**
     * The display control.
     */
    private Display display;

    private Font controlFont;

    private List fileList;

    private java.util.List<Object> list;

    /**
     * Return object when the shell is disposed.
     */
    private int returnIdx = -1;

    public RestoreFileSelectDlg(Shell parent) {
        super(parent, 0);
    }

    /**
     * Open method used to display the GHG Color dialog.
     * 
     * @return True/False.
     * @throws TextWorkstationNotStartedException
     */
    public int open(java.util.List<Object> list)
            throws TextWorkstationNotStartedException {
        Shell parent = getParent();
        display = parent.getDisplay();
        shell = new Shell(parent, SWT.TITLE);
        this.list = list;

        // Create the main layout for the shell.
        GridLayout mainLayout = new GridLayout(1, false);
        mainLayout.marginHeight = 2;
        mainLayout.marginWidth = 2;
        mainLayout.verticalSpacing = 2;
        shell.setLayout(mainLayout);

        shell.setText("Restore File");

        // Initialize all of the controls and layouts
        initializeComponents();

        shell.pack();
        shell.open();

        while (!shell.isDisposed()) {
            if (!display.readAndDispatch()) {
                display.sleep();
            }
        }

        controlFont.dispose();

        return returnIdx;
    }

    /**
     * Initialize the controls on the display.
     */
    private void initializeComponents() {
        controlFont = new Font(shell.getDisplay(), "Monospace", 10, SWT.NORMAL);

        createListControl();
        // Create the buttons at the bottom of the display.
        createBottomButtons();
    }

    private void createListControl() {
        GridData gd = new GridData(SWT.FILL, SWT.DEFAULT, true, false);
        Composite listComp = new Composite(shell, SWT.NONE);
        listComp.setLayout(new GridLayout(1, false));
        listComp.setLayoutData(gd);

        Label listLbl = new Label(listComp, SWT.NONE);
        listLbl.setText("Available files to restore:");

        gd = new GridData(SWT.FILL, SWT.FILL, true, true);
        gd.widthHint = 275;
        gd.heightHint = 300;
        gd.horizontalSpan = 2;
        fileList = new List(listComp, SWT.BORDER | SWT.SINGLE | SWT.V_SCROLL
                | SWT.H_SCROLL);
        fileList.setLayoutData(gd);
        fileList.setFont(controlFont);

        for (Object obj : list) {
            StdTextProduct tmp = ((StdTextProduct) (obj));
            Calendar createTime = Calendar.getInstance(TimeZone
                    .getTimeZone("GMT"));
            createTime.setTimeInMillis(tmp.getRefTime());
            String label = tmp.getCccid() + tmp.getNnnid() + tmp.getXxxid()
                    + " - " + createTime.getTime().toString();
            fileList.add(label);
        }
    }

    private void createBottomButtons() {
        GridData gd = new GridData(SWT.FILL, SWT.DEFAULT, true, false);
        Composite mainButtonComp = new Composite(shell, SWT.NONE);
        mainButtonComp.setLayout(new GridLayout(1, false));
        mainButtonComp.setLayoutData(gd);

        gd = new GridData(SWT.CENTER, SWT.DEFAULT, false, false);
        Composite buttonComp = new Composite(shell, SWT.NONE);
        buttonComp.setLayout(new GridLayout(2, false));
        buttonComp.setLayoutData(gd);

        gd = new GridData(100, SWT.DEFAULT);
        Button restoreBtn = new Button(buttonComp, SWT.PUSH);
        restoreBtn.setText("Restore");
        restoreBtn.setLayoutData(gd);
        restoreBtn.addSelectionListener(new SelectionAdapter() {
            @Override
            public void widgetSelected(SelectionEvent event) {
                int idx = fileList.getSelectionIndex();
                returnIdx = idx;
                shell.dispose();
            }
        });

        gd = new GridData(100, SWT.DEFAULT);
        Button cancelBtn = new Button(buttonComp, SWT.PUSH);
        cancelBtn.setText("Cancel");
        cancelBtn.setLayoutData(gd);
        cancelBtn.addSelectionListener(new SelectionAdapter() {
            @Override
            public void widgetSelected(SelectionEvent event) {
                returnIdx = -1;
                shell.dispose();
            }
        });
    }
}
