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
package com.raytheon.viz.aviation.climatedata;

import java.io.File;
import java.text.SimpleDateFormat;
import java.util.Calendar;
import java.util.Date;
import java.util.TimeZone;

import org.eclipse.swt.SWT;
import org.eclipse.swt.events.SelectionAdapter;
import org.eclipse.swt.events.SelectionEvent;
import org.eclipse.swt.graphics.Font;
import org.eclipse.swt.layout.GridData;
import org.eclipse.swt.layout.GridLayout;
import org.eclipse.swt.widgets.Button;
import org.eclipse.swt.widgets.Composite;
import org.eclipse.swt.widgets.Group;
import org.eclipse.swt.widgets.Label;
import org.eclipse.swt.widgets.Layout;
import org.eclipse.swt.widgets.Shell;

import com.raytheon.uf.common.status.IUFStatusHandler;
import com.raytheon.uf.common.status.UFStatus;
import com.raytheon.uf.common.status.UFStatus.Priority;
import com.raytheon.uf.viz.core.exception.VizException;
import com.raytheon.viz.ui.dialogs.CaveSWTDialog;
import com.raytheon.viz.ui.dialogs.ICloseCallback;

/**
 * This class displays the NCDC Inventory/History dialog.
 * 
 * <pre>
 * 
 * SOFTWARE HISTORY
 * Date         Ticket#    Engineer    Description
 * ------------ ---------- ----------- --------------------------
 * Nov 16, 2009 #3438      lvenable     Initial creation
 * Oct 08, 2012 #1229      rferrel     Changes for non-blocking GenScriptsDlg.
 * Oct 08, 2012 #1229      rferrel     Make dialog non-blocking.
 * 
 * </pre>
 * 
 * @author lvenable
 * @version 1.0
 */
public class NCDCInvHistDlg extends CaveSWTDialog {
    private static final transient IUFStatusHandler statusHandler = UFStatus
            .getHandler(NCDCInvHistDlg.class);

    /**
     * Label font.
     */
    private Font labelFont;

    /**
     * Inventory name label.
     */
    private Label invNameLbl;

    /**
     * Inventory size label.
     */
    private Label invSizeLbl;

    /**
     * Inventory last modification label.
     */
    private Label invLastModLbl;

    /**
     * Inventory script button.
     */
    private Button invScriptBtn;

    /**
     * History name label.
     */
    private Label histNameLbl;

    /**
     * History size label.
     */
    private Label histSizeLbl;

    /**
     * History last modification label.
     */
    private Label histLastModLbl;

    /**
     * History script button.
     */
    private Button histScriptBtn;

    /**
     * Script button width.
     */
    private int scriptButtonWidth = 300;

    /**
     * Generate scripts dialog.
     */
    private GenScriptsDlg generateScriptsDlg;

    private String invFilename = "ish-inventory.txt";

    private String histFilename = "ish-history.txt";

    private File invFile;

    private File histFile;

    /**
     * Constructor.
     * 
     * @param parentShell
     *            Parent shell.
     */
    public NCDCInvHistDlg(Shell parentShell) {
        super(parentShell, SWT.DIALOG_TRIM, CAVE.PERSPECTIVE_INDEPENDENT
                | CAVE.DO_NOT_BLOCK);
        setText("NCDC Inventory/History");
    }

    @Override
    protected Layout constructShellLayout() {
        return new GridLayout(1, false);
    }

    @Override
    protected void disposed() {
        labelFont.dispose();
    }

    @Override
    protected void initializeComponents(Shell shell) {
        // Initialize all of the data, controls, and layouts
        labelFont = new Font(getDisplay(), "Sans", 10, SWT.BOLD);

        createInventoryControls();
        createHistoryCOntrols();
        createCloseButton();

        updateLabels();
    }

    /**
     * Create the inventory controls.
     */
    private void createInventoryControls() {
        GridData gd = new GridData(SWT.FILL, SWT.DEFAULT, true, false);
        Group inventoryGroup = new Group(shell, SWT.NONE);
        inventoryGroup.setLayout(new GridLayout(2, false));
        inventoryGroup.setLayoutData(gd);
        inventoryGroup.setText(" Inventory ");

        /*
         * Name
         */
        gd = new GridData(SWT.FILL, SWT.DEFAULT, false, false);
        Label nameLbl = new Label(inventoryGroup, SWT.RIGHT);
        nameLbl.setText("Name:");
        nameLbl.setFont(labelFont);
        nameLbl.setLayoutData(gd);

        gd = new GridData(SWT.FILL, SWT.DEFAULT, true, false);
        invNameLbl = new Label(inventoryGroup, SWT.NONE);
        invNameLbl.setText(invFilename);
        invNameLbl.setFont(labelFont);
        invNameLbl.setLayoutData(gd);

        /*
         * Size
         */
        gd = new GridData(SWT.FILL, SWT.DEFAULT, false, false);
        Label sizeLbl = new Label(inventoryGroup, SWT.RIGHT);
        sizeLbl.setText("Size:");
        sizeLbl.setFont(labelFont);
        sizeLbl.setLayoutData(gd);

        gd = new GridData(SWT.FILL, SWT.DEFAULT, true, false);
        invSizeLbl = new Label(inventoryGroup, SWT.NONE);
        invSizeLbl.setText("**************");
        invSizeLbl.setFont(labelFont);
        invSizeLbl.setLayoutData(gd);

        /*
         * Last Modification
         */
        gd = new GridData(SWT.FILL, SWT.DEFAULT, false, false);
        Label lastModLbl = new Label(inventoryGroup, SWT.RIGHT);
        lastModLbl.setText("Last Mod:");
        lastModLbl.setFont(labelFont);
        lastModLbl.setLayoutData(gd);

        gd = new GridData(SWT.FILL, SWT.DEFAULT, true, false);
        invLastModLbl = new Label(inventoryGroup, SWT.NONE);
        invLastModLbl.setText("**************");
        invLastModLbl.setFont(labelFont);
        invLastModLbl.setLayoutData(gd);

        /*
         * Generate script button
         */
        gd = new GridData(scriptButtonWidth, SWT.DEFAULT);
        gd.horizontalSpan = 2;
        invScriptBtn = new Button(inventoryGroup, SWT.PUSH);
        invScriptBtn.setText("Generate Script");
        invScriptBtn.setLayoutData(gd);
        invScriptBtn.addSelectionListener(new SelectionAdapter() {
            @Override
            public void widgetSelected(SelectionEvent e) {
                if (generateScriptsDlg == null
                        || generateScriptsDlg.getShell() == null
                        || generateScriptsDlg.isDisposed()) {
                    histScriptBtn.setEnabled(false);

                    generateScriptsDlg = new GenScriptsDlg(shell, "inv");
                    generateScriptsDlg.setCloseCallback(new ICloseCallback() {

                        @Override
                        public void dialogClosed(Object returnValue) {
                            histScriptBtn.setEnabled(true);
                        }
                    });
                    generateScriptsDlg.open();
                } else {
                    generateScriptsDlg.bringToTop();
                }
            }
        });
    }

    /**
     * Create the history controls.
     */
    private void createHistoryCOntrols() {
        GridData gd = new GridData(SWT.FILL, SWT.DEFAULT, true, false);
        Group historyGroup = new Group(shell, SWT.NONE);
        historyGroup.setLayout(new GridLayout(2, false));
        historyGroup.setLayoutData(gd);
        historyGroup.setText(" History ");

        /*
         * Name
         */
        gd = new GridData(SWT.FILL, SWT.DEFAULT, false, false);
        Label nameLbl = new Label(historyGroup, SWT.RIGHT);
        nameLbl.setText("Name:");
        nameLbl.setFont(labelFont);
        nameLbl.setLayoutData(gd);

        gd = new GridData(SWT.FILL, SWT.DEFAULT, true, false);
        histNameLbl = new Label(historyGroup, SWT.NONE);
        histNameLbl.setText(histFilename);
        histNameLbl.setFont(labelFont);
        histNameLbl.setLayoutData(gd);

        /*
         * Size
         */
        gd = new GridData(SWT.FILL, SWT.DEFAULT, false, false);
        Label sizeLbl = new Label(historyGroup, SWT.RIGHT);
        sizeLbl.setText("Size:");
        sizeLbl.setFont(labelFont);
        sizeLbl.setLayoutData(gd);

        gd = new GridData(SWT.FILL, SWT.DEFAULT, true, false);
        histSizeLbl = new Label(historyGroup, SWT.NONE);
        histSizeLbl.setText("**************");
        histSizeLbl.setFont(labelFont);
        histSizeLbl.setLayoutData(gd);

        /*
         * Last Modification
         */
        gd = new GridData(SWT.FILL, SWT.DEFAULT, false, false);
        Label lastModLbl = new Label(historyGroup, SWT.RIGHT);
        lastModLbl.setText("Last Mod:");
        lastModLbl.setFont(labelFont);
        lastModLbl.setLayoutData(gd);

        gd = new GridData(SWT.FILL, SWT.DEFAULT, true, false);
        histLastModLbl = new Label(historyGroup, SWT.NONE);
        histLastModLbl.setText("**************");
        histLastModLbl.setFont(labelFont);
        histLastModLbl.setLayoutData(gd);

        /*
         * Generate script button
         */
        gd = new GridData(scriptButtonWidth, SWT.DEFAULT);
        gd.horizontalSpan = 2;
        histScriptBtn = new Button(historyGroup, SWT.PUSH);
        histScriptBtn.setText("Generate Script");
        histScriptBtn.setLayoutData(gd);
        histScriptBtn.addSelectionListener(new SelectionAdapter() {
            @Override
            public void widgetSelected(SelectionEvent e) {
                if (generateScriptsDlg == null
                        || generateScriptsDlg.getShell() == null
                        || generateScriptsDlg.isDisposed()) {
                    invScriptBtn.setEnabled(false);
                    generateScriptsDlg = new GenScriptsDlg(shell, "his");
                    generateScriptsDlg.setCloseCallback(new ICloseCallback() {

                        @Override
                        public void dialogClosed(Object returnValue) {
                            invScriptBtn.setEnabled(true);
                        }
                    });
                    generateScriptsDlg.open();
                } else {
                    generateScriptsDlg.bringToTop();
                }
            }
        });
    }

    /**
     * Create the close button at the bottom of the display.
     */
    private void createCloseButton() {
        Composite buttonComp = new Composite(shell, SWT.NONE);
        buttonComp.setLayout(new GridLayout(1, false));
        buttonComp.setLayoutData(new GridData(SWT.FILL, SWT.DEFAULT, true,
                false));

        GridData gd = new GridData(SWT.CENTER, SWT.DEFAULT, true, false);
        gd.widthHint = 80;
        Button closeBtn = new Button(buttonComp, SWT.PUSH);
        closeBtn.setText("Close");
        closeBtn.setLayoutData(gd);
        closeBtn.addSelectionListener(new SelectionAdapter() {
            @Override
            public void widgetSelected(SelectionEvent e) {
                shell.dispose();
            }
        });
    }

    private void updateLastMod(Label lmLbl, Date date) {
        SimpleDateFormat dateFmt = new SimpleDateFormat(
                "E MMM dd HH:mm:ss yyyy");
        dateFmt.setTimeZone(TimeZone.getTimeZone("GMT"));
        lmLbl.setText(dateFmt.format(date));
    }

    private void getIshFiles() {
        try {
            invFile = new File(ClimateDataPython.getIshFilePath() + "/"
                    + invFilename);
            histFile = new File(ClimateDataPython.getIshFilePath() + "/"
                    + histFilename);
        } catch (VizException e) {
            statusHandler.handle(Priority.PROBLEM, e.getMessage(), e);
        }
    }

    private void updateLabels() {
        getIshFiles();

        if (invFile != null) {
            long lastModified = invFile.lastModified();
            long size = invFile.length();
            Date d = Calendar.getInstance().getTime();
            d.setTime(lastModified);
            updateLastMod(invLastModLbl, d);
            invSizeLbl.setText(String.format("%d MB", size / 1024 / 1000));
        }

        if (histFile != null) {
            long lastModified = histFile.lastModified();
            long size = histFile.length();
            Date d = Calendar.getInstance().getTime();
            d.setTime(lastModified);
            updateLastMod(histLastModLbl, d);
            histSizeLbl.setText(String.format("%d MB", size / 1024 / 1000));
        }
    }
}
