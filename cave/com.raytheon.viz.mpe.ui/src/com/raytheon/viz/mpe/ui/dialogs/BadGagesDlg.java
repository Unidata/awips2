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
package com.raytheon.viz.mpe.ui.dialogs;

import java.util.ArrayList;

import org.eclipse.swt.SWT;
import org.eclipse.swt.events.DisposeEvent;
import org.eclipse.swt.events.DisposeListener;
import org.eclipse.swt.events.SelectionAdapter;
import org.eclipse.swt.events.SelectionEvent;
import org.eclipse.swt.graphics.GC;
import org.eclipse.swt.layout.GridData;
import org.eclipse.swt.layout.GridLayout;
import org.eclipse.swt.widgets.Button;
import org.eclipse.swt.widgets.Composite;
import org.eclipse.swt.widgets.Display;
import org.eclipse.swt.widgets.Label;
import org.eclipse.swt.widgets.List;
import org.eclipse.swt.widgets.Shell;

import com.raytheon.viz.mpe.core.MPEDataManager;
import com.raytheon.viz.ui.dialogs.DialogUtil;
import com.raytheon.viz.ui.perspectives.AbstractVizPerspectiveManager;
import com.raytheon.viz.ui.perspectives.VizPerspectiveListener;

/**
 * Bad Gage dialog. This will allow the user to remove the Bad Gages from the
 * list of bad gages.
 *
 * <pre>
 *
 * SOFTWARE HISTORY
 *
 * Date          Ticket#  Engineer  Description
 * ------------- -------- --------- --------------------------------------------
 * May 04, 2011  ?        lvenable  Initial creation
 * Apr 05, 2016  5504     bkowal    Fix GUI sizing issues.
 * Sep 21, 2016  5901     randerso  Fix dialog centering issue introduced in
 *                                  Eclipse 4
 *
 * </pre>
 *
 * @author lvenable
 * @version 1.0
 */

public class BadGagesDlg extends AbstractMPEDialog {

    /**
     * Gage list control.
     */
    private List gageList;

    /**
     * Dete selected items button.
     */
    private Button deleteSelectedBtn;

    /**
     * List of gages to be permanently deleted.
     */
    private ArrayList<String> deleteGages = new ArrayList<String>();

    /**
     * Constructor.
     *
     * @param parent
     */
    public BadGagesDlg(Shell parent) {
        super(parent);
    }

    /**
     * Open the dialog.
     *
     * @return Return value (null).
     */
    public Object open() {
        Shell parent = this.getParent();
        Display display = parent.getDisplay();

        // Set up the shell and allow it to resize.
        shell = new Shell(parent, SWT.DIALOG_TRIM | SWT.MODELESS | SWT.RESIZE);
        shell.setText("Bad Gages");

        // Create the main layout for the shell.
        GridLayout mainLayout = new GridLayout(1, true);
        mainLayout.marginHeight = 1;
        mainLayout.marginWidth = 1;
        shell.setLayout(mainLayout);

        shell.addDisposeListener(new DisposeListener() {
            @Override
            public void widgetDisposed(DisposeEvent e) {
                AbstractVizPerspectiveManager mgr = VizPerspectiveListener
                        .getCurrentPerspectiveManager();
                if (mgr != null) {
                    mgr.removePespectiveDialog(BadGagesDlg.this);
                }
            }
        });

        // Initialize all of the controls and layouts
        initializeComponents();

        shell.pack();

        DialogUtil.centerOnParentShell(parent, shell);

        shell.open();

        // Set the minimum size of the dialog so resizing does not hide
        // the buttons.
        shell.setMinimumSize(shell.getBounds().width, shell.getBounds().height);

        while (!shell.isDisposed()) {
            if (!display.readAndDispatch()) {
                display.sleep();
            }
        }

        return null;
    }

    /**
     * Initialize the widget components.
     */
    private void initializeComponents() {
        createGageList();
        addSeparator(shell);
        createBottomButtons();

        populateGageList();
        gageList.deselectAll();
    }

    /**
     * Create the gage list and the delete button.
     */
    private void createGageList() {
        Composite listComp = new Composite(shell, SWT.NONE);
        listComp.setLayout(new GridLayout(1, false));
        listComp.setLayoutData(new GridData(SWT.FILL, SWT.FILL, true, true));

        gageList = new List(listComp, SWT.BORDER | SWT.MULTI | SWT.V_SCROLL
                | SWT.H_SCROLL);
        GridData gd = new GridData(SWT.FILL, SWT.FILL, true, true);
        GC gc = new GC(gageList);
        /*
         * Ensures that a minimum of approximately 38 characters will be
         * displayed.
         */
        gd.minimumWidth = gc.getFontMetrics().getAverageCharWidth() * 38;
        gc.dispose();
        // Ensures that a minimum of ten rows will be displayed.
        gd.heightHint = gageList.getItemHeight() * 10;
        gageList.setLayoutData(gd);
        gageList.addSelectionListener(new SelectionAdapter() {
            @Override
            public void widgetSelected(SelectionEvent event) {
                if (gageList.getSelectionCount() > 0) {
                    deleteSelectedBtn.setEnabled(true);
                } else {
                    deleteSelectedBtn.setEnabled(false);
                }
            }
        });

        deleteSelectedBtn = new Button(listComp, SWT.PUSH);
        gd = new GridData(SWT.CENTER, SWT.DEFAULT, true, false);
        gd.minimumWidth = deleteSelectedBtn.getDisplay().getDPI().x;
        deleteSelectedBtn.setText("Delete Selected Item");
        deleteSelectedBtn.setEnabled(false);
        deleteSelectedBtn.setLayoutData(gd);
        deleteSelectedBtn.addSelectionListener(new SelectionAdapter() {
            @Override
            public void widgetSelected(SelectionEvent event) {
                deleteGageItemsFromList();
            }
        });
    }

    /**
     * Create the bottom OK and Cancel buttons.
     */
    private void createBottomButtons() {
        Composite buttonComp = new Composite(shell, SWT.NONE);
        buttonComp.setLayout(new GridLayout(2, true));
        buttonComp.setLayoutData(new GridData(SWT.CENTER, SWT.DEFAULT, true,
                false));

        int minimumButtonWidth = buttonComp.getDisplay().getDPI().x;

        GridData gd = new GridData(SWT.FILL, SWT.DEFAULT, true, false);
        gd.minimumWidth = minimumButtonWidth;
        Button okBtn = new Button(buttonComp, SWT.PUSH);
        okBtn.setText("OK");
        okBtn.setLayoutData(gd);
        okBtn.addSelectionListener(new SelectionAdapter() {
            @Override
            public void widgetSelected(SelectionEvent event) {
                okAction();
                close();
            }
        });

        gd = new GridData(SWT.FILL, SWT.DEFAULT, true, false);
        gd.minimumWidth = minimumButtonWidth;
        Button cancelBtn = new Button(buttonComp, SWT.PUSH);
        cancelBtn.setText("Cancel");
        cancelBtn.setLayoutData(gd);
        cancelBtn.addSelectionListener(new SelectionAdapter() {
            @Override
            public void widgetSelected(SelectionEvent event) {
                close();
            }
        });
    }

    /**
     * Add a separator to the display.
     *
     * @param parentComp
     *            Parent Composite
     */
    private void addSeparator(Composite parentComp) {
        GridLayout gl = (GridLayout) parentComp.getLayout();

        GridData gd = new GridData(SWT.FILL, SWT.DEFAULT, true, false);
        gd.horizontalSpan = gl.numColumns;
        Label sepLbl = new Label(parentComp, SWT.SEPARATOR | SWT.HORIZONTAL);
        sepLbl.setLayoutData(gd);
    }

    /**
     * Populate the Bad Gages list.
     */
    private void populateGageList() {
        ArrayList<String> dataArray = MPEDataManager.getInstance()
                .readBadGageList();
        for (String str : dataArray) {
            gageList.add(str);
        }
    }

    /**
     * Delete the selected gages from the list.
     */
    private void deleteGageItemsFromList() {

        if (gageList.getSelectionCount() == 0) {
            return;
        }

        // Add the selected gages to the deleted gages array.
        String[] selectedGages = gageList.getSelection();
        for (String s : selectedGages) {
            deleteGages.add(s);
        }

        gageList.remove(gageList.getSelectionIndices());

        if (gageList.getSelectionCount() == 0) {
            deleteSelectedBtn.setEnabled(false);
        }
    }

    /**
     * When the OK button is pressed, permanently remove the bad gages.
     */
    private void okAction() {
        for (String s : deleteGages) {
            MPEDataManager.getInstance().removeBadGage(s);
        }

        MPEDataManager.getInstance().writeBadGageList();
    }
}
