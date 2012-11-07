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
package com.raytheon.viz.aviation.utility;

import org.eclipse.swt.SWT;
import org.eclipse.swt.custom.StyledText;
import org.eclipse.swt.events.SelectionAdapter;
import org.eclipse.swt.events.SelectionEvent;
import org.eclipse.swt.graphics.Cursor;
import org.eclipse.swt.layout.GridData;
import org.eclipse.swt.layout.GridLayout;
import org.eclipse.swt.widgets.Button;
import org.eclipse.swt.widgets.Composite;
import org.eclipse.swt.widgets.Label;
import org.eclipse.swt.widgets.Layout;
import org.eclipse.swt.widgets.Shell;

import com.raytheon.viz.aviation.resource.ResourceConfigMgr;
import com.raytheon.viz.aviation.resource.ResourceConfigMgr.ResourceTag;
import com.raytheon.viz.ui.dialogs.CaveSWTDialog;

/**
 * A dialog to display the forecast(s) details.
 * 
 * <pre>
 * 
 * SOFTWARE HISTORY
 * 
 * Date         Ticket#    Engineer    Description
 * ------------ ---------- ----------- --------------------------
 *                                     Initial creation
 * Oct 10, 2012 1229       rferrel     Made dialog non-blocking
 * 
 * </pre>
 * 
 * @author rferrel
 * @version 1.0
 */
public class TransmissionViewerDlg extends CaveSWTDialog {

    /**
     * Main composite for the controls.
     */
    private Composite mainComp;

    /**
     * TAF text control.
     */
    private StyledText stText;

    private Cursor textCursor;

    private String tafText;

    public TransmissionViewerDlg(Shell parentShell, String tafText,
            String tafInfo) {
        super(parentShell, SWT.SHELL_TRIM | SWT.RESIZE,
                CAVE.PERSPECTIVE_INDEPENDENT | CAVE.MODE_INDEPENDENT
                        | CAVE.DO_NOT_BLOCK);
        setText(tafInfo);

        this.tafText = tafText;
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
    protected void disposed() {
        textCursor.dispose();
    }

    @Override
    protected void initializeComponents(Shell shell) {
        mainComp = new Composite(shell, SWT.NONE);
        mainComp.setLayout(new GridLayout(1, false));
        GridData gd = new GridData(SWT.FILL, SWT.FILL, true, true);
        mainComp.setLayoutData(gd);

        // Initialize all of the controls and layouts
        initializeComponents();
    }

    @Override
    protected void preOpened() {
        shell.setMinimumSize(400, 200);
    }

    private void initializeComponents() {
        ResourceConfigMgr configMgr = ResourceConfigMgr.getInstance();

        configMgr.setDefaultColors(mainComp);

        Label titleLbl = new Label(mainComp, SWT.CENTER);
        titleLbl.setText("TAF Information:");
        titleLbl.setLayoutData(new GridData(SWT.CENTER, SWT.DEFAULT, true,
                false));
        configMgr.setDefaultFontAndColors(titleLbl);

        GridData gd = new GridData(SWT.FILL, SWT.FILL, true, true);
        gd.widthHint = 550;
        gd.heightHint = 500;
        stText = new StyledText(mainComp, SWT.BORDER | SWT.MULTI | SWT.H_SCROLL
                | SWT.V_SCROLL);
        stText.setLayoutData(gd);
        stText.setEditable(false);
        configMgr.setTextFontAndColors(stText);

        String cursorStr = configMgr
                .getResourceAsString(ResourceTag.TextCursor);
        int cursorInt = configMgr.getCursorAsInt(cursorStr);
        textCursor = new Cursor(getDisplay(), cursorInt);
        stText.setCursor(textCursor);

        stText.setText(tafText);

        gd = new GridData(SWT.CENTER, SWT.DEFAULT, true, false);
        gd.minimumWidth = 80;
        gd.widthHint = 80;
        Button closeBtn = new Button(mainComp, SWT.PUSH);
        closeBtn.setText("Close");
        closeBtn.setLayoutData(gd);
        configMgr.setDefaultFontAndColors(closeBtn);
        closeBtn.addSelectionListener(new SelectionAdapter() {
            @Override
            public void widgetSelected(SelectionEvent e) {
                shell.dispose();
            }
        });
    }
}
