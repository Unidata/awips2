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
package com.raytheon.viz.aviation.resource;

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

import com.raytheon.viz.aviation.resource.ResourceConfigMgr.ResourceTag;
import com.raytheon.viz.ui.dialogs.CaveSWTDialog;

/**
 * Display help text for the ResourceEditor dialog within the AvnFPS
 * TAFMonitor dialog.
 * 
 * <pre>
 * 
 * SOFTWARE HISTORY
 * 
 * Date         Ticket#    Engineer    Description
 * ------------ ---------- ----------- --------------------------
 * ************ ********** *********** No previous software history.
 * Oct 11, 2012 1229       jkorman     Make dialog non-blocking.
 * 
 * </pre>
 * 
 * @author 
 * @version 1.0
 */

public class ResourceEditorHelpDlg extends CaveSWTDialog {
    /**
     * Main composite for the controls.
     */
    private Composite mainComp;

    /**
     * Help text control.
     */
    private StyledText stText;

    private Cursor textCursor;

    /**
     * Construct the ResourceEditor help dialog. 
     * @param parentShell The shell containing this dialog.
     */
    public ResourceEditorHelpDlg(Shell parentShell) {
        super(parentShell, SWT.SHELL_TRIM | SWT.RESIZE,
                CAVE.PERSPECTIVE_INDEPENDENT | CAVE.MODE_INDEPENDENT
                        | CAVE.DO_NOT_BLOCK);
        setText("AvnFPS Help");
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

        // Initialize all of the controls and layouts
        initializeComponents();
    }

    @Override
    protected void preOpened() {
        shell.setMinimumSize(400, 400);
    }

    private void initializeComponents() {
        ResourceConfigMgr configMgr = ResourceConfigMgr.getInstance();

        configMgr.setDefaultColors(mainComp);

        Label titleLbl = new Label(mainComp, SWT.CENTER);
        titleLbl.setText("Resource Editor Help");
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

        gd = new GridData(SWT.CENTER, SWT.DEFAULT, true, false);
        gd.minimumWidth = 80;
        gd.widthHint = 80;
        Button closeBtn = new Button(mainComp, SWT.PUSH);
        configMgr.setDefaultFontAndColors(closeBtn, "Close", gd);
        closeBtn.addSelectionListener(new SelectionAdapter() {
            @Override
            public void widgetSelected(SelectionEvent e) {
                shell.dispose();
            }
        });

        setHelpText();
    }

    private void setHelpText() {
        StringBuilder sb = new StringBuilder();

        sb.append(
                "This is an editor for the private resources file for AvnFPS.")
                .append("\n\n");

        sb.append(
                "The name of the resource is listed first. Placing the mouse over")
                .append("\n");
        sb.append("the resource name will display a description. The value of")
                .append("\n");
        sb.append(
                "the resource is displayed on the right.  Depending on the resource")
                .append("\n");
        sb.append("type it is:").append("\n\n");

        sb.append("Toggle button:  selected or not").append("\n");
        sb.append("Option menu:    allows selection from predefined values")
                .append("\n");
        sb.append("Entry/Spinner:  type in values, such as width and height")
                .append("\n");
        sb.append("Dialogs:").append("\n");
        sb.append(
                "    Audio selection dialog: to select audio file, resource playFile")
                .append("\n");
        sb.append("    Color chooser dialog:   to define colors").append("\n");
        sb.append("    Font chooser dialog:    to define fonts").append("\n\n");

        sb.append(
                "When the editor starts, it tries to access the user localization")
                .append("\n");
        sb.append(
                "resource file. If the file does not exist, the base configuration file")
                .append("\n");
        sb.append("is read in.").append("\n\n");

        sb.append(
                "Press 'Save' to write displayed values into your localization configuration")
                .append("\n");
        sb.append(
                "file. To restore default configuration, press 'Restore' for all resources,")
                .append("\n");
        sb.append(
                "or the 'Reset' button to the left of each resource name to reset")
                .append("\n");
        sb.append("them individually.").append("\n");

        stText.setText(sb.toString());
    }

    public void showDialog() {
        shell.setFocus();
    }
}
