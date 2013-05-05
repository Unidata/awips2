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

import java.util.HashMap;
import java.util.Map;

import org.eclipse.swt.SWT;
import org.eclipse.swt.events.SelectionAdapter;
import org.eclipse.swt.events.SelectionEvent;
import org.eclipse.swt.layout.GridData;
import org.eclipse.swt.layout.GridLayout;
import org.eclipse.swt.widgets.Button;
import org.eclipse.swt.widgets.Composite;
import org.eclipse.swt.widgets.Group;
import org.eclipse.swt.widgets.Layout;
import org.eclipse.swt.widgets.Shell;

import com.raytheon.viz.aviation.resource.ResourceConfigMgr;
import com.raytheon.viz.ui.dialogs.CaveSWTDialog;

/**
 * QCDialog class displays the Quality Control dialog for AvnFPS.
 * 
 * <pre>
 * SOFTWARE HISTORY
 * Date         Ticket#     Engineer    Description
 * ------------ ----------  ----------- ------------------------------------
 * 28 FEB 2008  938         lvenable    Initial creation.
 * 9/19/2008    1444        grichard    Add Taf wx quality check capability.
 * 1/17/2011    7782        rferrel     Dialog now has open return null or
 *                                      the desired check list; removed the use
 *                                      of sites to be like OB9.2.X.
 * 10/09/2012   1229        rferrel     Made dialog non-blocking.
 * 
 * </pre>
 * 
 * @author lvenable
 * @version 1.0
 * 
 */
public class QcDialog extends CaveSWTDialog {

    /**
     * Current weather check box.
     */
    private Button currentWxChk;

    /**
     * Climate weather check box.
     */
    private Button climateChk;

    /**
     * Impact check box.
     */
    private Button impactChk;

    /**
     * Main composite.
     */
    private Composite mainComp;

    /**
     * A map of the checkbox statuses
     */
    private Map<String, String> items;

    /**
     * Constructor.
     * 
     * @param parent
     *            - parent composite
     */
    public QcDialog(Shell parent, Map<String, String> items) {
        super(parent, SWT.DIALOG_TRIM, CAVE.PERSPECTIVE_INDEPENDENT
                | CAVE.DO_NOT_BLOCK);
        setText("AvnFPS QC");

        this.items = items;
    }

    @Override
    protected Layout constructShellLayout() {
        // Create the main layout for the shell.
        GridLayout gl = new GridLayout(1, false);
        gl.marginHeight = 0;
        gl.marginWidth = 0;
        return gl;
    }

    @Override
    protected void initializeComponents(Shell shell) {
        setReturnValue(false);

        GridLayout gl = new GridLayout(1, false);
        mainComp = new Composite(shell, SWT.NONE);
        mainComp.setLayout(gl);

        // Initialize all of the controls and layouts
        ResourceConfigMgr configMgr = ResourceConfigMgr.getInstance();
        configMgr.setDefaultColors(mainComp);

        createMainControls(configMgr);

        createBottomButtons(configMgr);
    }

    /**
     * Create the main controls on the display.
     */
    private void createMainControls(ResourceConfigMgr configMgr) {
        Composite topComp = new Composite(mainComp, SWT.NONE);
        GridLayout gl = new GridLayout(2, false);
        topComp.setLayout(gl);
        GridData gd = new GridData(SWT.CENTER, SWT.DEFAULT, true, false);
        topComp.setLayoutData(gd);
        configMgr.setDefaultColors(topComp);

        // ----------------------------------------
        // Create the QC Checks check buttons
        // ----------------------------------------
        gd = new GridData(SWT.FILL, SWT.CENTER, true, true);
        Group checksGroup = new Group(topComp, SWT.NONE);
        gl = new GridLayout(1, false);
        checksGroup.setLayout(gl);
        checksGroup.setLayoutData(gd);
        checksGroup.setText(" Checks ");
        configMgr.setDefaultFontAndColors(checksGroup);

        gd = new GridData(120, SWT.DEFAULT);
        currentWxChk = new Button(checksGroup, SWT.CHECK);
        currentWxChk.setSelection(true);
        configMgr.setDefaultFontAndColors(currentWxChk, "Current Wx", gd);

        gd = new GridData(120, SWT.DEFAULT);
        climateChk = new Button(checksGroup, SWT.CHECK);
        climateChk.setSelection(true);
        configMgr.setDefaultFontAndColors(climateChk, "Climate", gd);

        gd = new GridData(120, SWT.DEFAULT);
        impactChk = new Button(checksGroup, SWT.CHECK);
        impactChk.setSelection(true);
        configMgr.setDefaultFontAndColors(impactChk, "Impact", gd);

        if (items != null) {
            if (items.get("currentwx").equals("0")) {
                currentWxChk.setSelection(false);
            }
            if (items.get("climate").equals("0")) {
                climateChk.setSelection(false);
            }
            if (items.get("impact").equals("0")) {
                impactChk.setSelection(false);
            }
        }

        // ----------------------------------------
        // Create the TAFs label and list controls
        // ----------------------------------------
        Composite tafComp = new Composite(topComp, SWT.NONE);
        gl = new GridLayout(1, false);
        tafComp.setLayout(gl);
        gd = new GridData(SWT.DEFAULT, SWT.FILL, false, true);
        tafComp.setLayoutData(gd);
        configMgr.setDefaultColors(tafComp);
    }

    /**
     * Create the OK and Close buttons.
     */
    private void createBottomButtons(ResourceConfigMgr configMgr) {
        Composite centeredComp = new Composite(mainComp, SWT.NONE);
        GridLayout gl = new GridLayout(3, false);
        centeredComp.setLayout(gl);
        GridData gd = new GridData(SWT.CENTER, SWT.DEFAULT, true, false);
        gd.horizontalSpan = 2;
        centeredComp.setLayoutData(gd);
        configMgr.setDefaultColors(centeredComp);

        gd = new GridData(90, SWT.DEFAULT);
        Button okBtn = new Button(centeredComp, SWT.NONE);
        okBtn.setToolTipText("Perform selected QC checks on slected TAFs");
        configMgr.setDefaultFontAndColors(okBtn, "Apply", gd);
        okBtn.addSelectionListener(new SelectionAdapter() {
            @Override
            public void widgetSelected(SelectionEvent event) {
                HashMap<String, String> qcItems = new HashMap<String, String>();
                if (currentWxChk.getSelection()) {
                    qcItems.put("currentwx", "1");
                } else {
                    qcItems.put("currentwx", "0");
                }
                if (climateChk.getSelection()) {
                    qcItems.put("climate", "1");
                } else {
                    qcItems.put("climate", "0");
                }
                if (impactChk.getSelection()) {
                    qcItems.put("impact", "1");
                } else {
                    qcItems.put("impact", "0");
                }
                setReturnValue(qcItems);
                close();
            }
        });

        gd = new GridData(90, SWT.DEFAULT);
        Button closeBtn = new Button(centeredComp, SWT.NONE);
        closeBtn.setToolTipText("Close window without applying checks.");
        configMgr.setDefaultFontAndColors(closeBtn, "Close", gd);
        closeBtn.addSelectionListener(new SelectionAdapter() {
            @Override
            public void widgetSelected(SelectionEvent event) {
                setReturnValue(null);
                close();
            }
        });
    }

    public void preOpened() {
        super.preOpened();
        setReturnValue(null);
    }
}
