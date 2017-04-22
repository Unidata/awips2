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

import java.util.Arrays;

import org.eclipse.swt.SWT;
import org.eclipse.swt.events.SelectionAdapter;
import org.eclipse.swt.events.SelectionEvent;
import org.eclipse.swt.graphics.GC;
import org.eclipse.swt.layout.GridData;
import org.eclipse.swt.layout.GridLayout;
import org.eclipse.swt.widgets.Button;
import org.eclipse.swt.widgets.Composite;
import org.eclipse.swt.widgets.Group;
import org.eclipse.swt.widgets.Layout;
import org.eclipse.swt.widgets.List;
import org.eclipse.swt.widgets.Shell;

import com.raytheon.uf.common.status.IUFStatusHandler;
import com.raytheon.uf.common.status.UFStatus;
import com.raytheon.uf.common.status.UFStatus.Priority;
import com.raytheon.uf.viz.core.exception.VizException;
import com.raytheon.viz.ui.dialogs.CaveSWTDialog;

/**
 * This class displays sites with hdf5 files the user can select to filter out
 * unwanted data.
 * 
 * <pre>
 * 
 * SOFTWARE HISTORY
 * Date         Ticket#    Engineer    Description
 * ------------ ---------- ----------- --------------------------
 * Jun 15, 2016 #5693      rferrel     Initial creation
 * 
 * </pre>
 * 
 * @author rferrel
 * @version 1.0
 */
public class FixClimateFilesDlg extends CaveSWTDialog {
    private static final transient IUFStatusHandler statusHandler = UFStatus
            .getHandler(FixClimateFilesDlg.class);

    private List siteList;

    private IClimateDataMenuDlg cdmDlg;

    /**
     * Constructor.
     * 
     * @param parentShell
     *            Parent shell.
     */
    public FixClimateFilesDlg(Shell parentShell, IClimateDataMenuDlg cdmDlg) {
        super(parentShell, SWT.DIALOG_TRIM, CAVE.PERSPECTIVE_INDEPENDENT
                | CAVE.DO_NOT_BLOCK);
        this.cdmDlg = cdmDlg;
        setText("Fix Site hdf5 files");
    }

    @Override
    protected Layout constructShellLayout() {
        return new GridLayout(1, false);
    }

    @Override
    protected void initializeComponents(Shell shell) {
        createSiteList();
        createButtons();
    }

    /**
     * Create the inventory controls.
     */
    private void createSiteList() {
        GridData gd = new GridData(SWT.FILL, SWT.DEFAULT, true, false);
        Group sitesGroup = new Group(shell, SWT.NONE);
        sitesGroup.setLayout(new GridLayout(1, false));
        sitesGroup.setLayoutData(gd);
        sitesGroup.setText(" Sites ");

        siteList = new List(sitesGroup, SWT.MULTI | SWT.V_SCROLL);
        gd = new GridData(SWT.FILL, SWT.DEFAULT, true, true);
        gd.heightHint = siteList.getItemHeight() * 10;
        siteList.setLayoutData(gd);
        try {
            String[] sites = ClimateDataPython.getSiteList();
            Arrays.sort(sites);
            for (String site : sites) {
                siteList.add(site);
            }
        } catch (VizException e) {
            statusHandler.handle(Priority.PROBLEM, e.getLocalizedMessage(), e);
        }
    }

    /**
     * Create the history controls.
     */

    /**
     * Create the buttons at the bottom of the display.
     */
    private void createButtons() {
        Composite buttonComp = new Composite(shell, SWT.NONE);
        buttonComp.setLayout(new GridLayout(2, true));
        buttonComp.setLayoutData(new GridData(SWT.FILL, SWT.DEFAULT, true,
                false));

        // Assume this is the widest text for the buttons.
        String closeText = "Close";

        GridData gd = new GridData(SWT.CENTER, SWT.DEFAULT, true, false);
        Button updateBtn = new Button(buttonComp, SWT.PUSH);
        gd.widthHint = calculateButtonWidth(updateBtn, closeText);
        updateBtn.setText("Fix");
        updateBtn.setLayoutData(gd);
        updateBtn.addSelectionListener(new SelectionAdapter() {

            @Override
            public void widgetSelected(SelectionEvent e) {
                fixClimateFiles();
                close();
            }
        });

        gd = new GridData(SWT.CENTER, SWT.DEFAULT, true, false);
        Button closeBtn = new Button(buttonComp, SWT.PUSH);
        closeBtn.setText(closeText);
        gd.widthHint = calculateButtonWidth(closeBtn, closeText);
        closeBtn.setLayoutData(gd);
        closeBtn.addSelectionListener(new SelectionAdapter() {
            @Override
            public void widgetSelected(SelectionEvent e) {
                close();
            }
        });

    }

    /**
     * Calculate the button width based on the text to be displayed.
     * 
     * @param btn
     *            Button control.
     * @param text
     *            Text to be displayed.
     * @return The calculated button width.
     */
    private int calculateButtonWidth(Button btn, String text) {
        int rv = 0;
        int extentLength = 0;
        int defaultButtonWidth = 60;
        int textBufferWidth = 15;

        // Get the length of the text in pixels that will be displayed in the
        // button.
        GC gc = new GC(btn);
        extentLength = gc.stringExtent(text).x;
        rv = (defaultButtonWidth > extentLength) ? defaultButtonWidth
                : extentLength;
        gc.dispose();

        /*
         * Return the length of the text and the added buffer that accounts for
         * the button edges.
         */
        return rv + textBufferWidth;
    }

    private void fixClimateFiles() {
        if (siteList.getSelectionCount() > 0) {
            String[] fixSites = siteList.getSelection();

            ClimateDataManager.getInstance().fixClimateFiles(fixSites, cdmDlg);
        }
    }

}
