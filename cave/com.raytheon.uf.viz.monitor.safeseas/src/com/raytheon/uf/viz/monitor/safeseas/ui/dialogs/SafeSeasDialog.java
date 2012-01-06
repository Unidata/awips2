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
package com.raytheon.uf.viz.monitor.safeseas.ui.dialogs;

import org.eclipse.swt.SWT;
import org.eclipse.swt.custom.StyledText;
import org.eclipse.swt.events.SelectionAdapter;
import org.eclipse.swt.events.SelectionEvent;
import org.eclipse.swt.events.ShellAdapter;
import org.eclipse.swt.events.ShellEvent;
import org.eclipse.swt.graphics.Color;
import org.eclipse.swt.graphics.Font;
import org.eclipse.swt.layout.GridData;
import org.eclipse.swt.layout.GridLayout;
import org.eclipse.swt.widgets.Button;
import org.eclipse.swt.widgets.Composite;
import org.eclipse.swt.widgets.Label;
import org.eclipse.swt.widgets.Layout;
import org.eclipse.swt.widgets.Shell;

import com.raytheon.uf.common.monitor.data.ObConst;
import com.raytheon.uf.viz.monitor.listeners.IGuardianListener;
import com.raytheon.uf.viz.monitor.safeseas.SafeSeasMonitor;
import com.raytheon.uf.viz.monitor.safeseas.SafeSeasReportModel;
import com.raytheon.uf.viz.monitor.util.MonitorThresholdConfiguration;
import com.raytheon.viz.ui.dialogs.CaveSWTDialog;

/**
 * The SAFESEAS Dialog Stand-in
 * 
 * <pre>
 * 
 * SOFTWARE HISTORY
 * Date         Ticket#    Engineer    Description
 * ------------ ---------- ----------- --------------------------
 * Feb 20, 2009 1999       grichard    Initial creation.
 * 
 * </pre>
 * 
 * @author grichard
 * @version 1.0
 */

public class SafeSeasDialog extends CaveSWTDialog implements IGuardianListener {

    /**
     * Guardian threat message viewer text control.
     */
    private StyledText threatMessageViewerStTxt;

    /**
     * Guardian threat priority label.
     */
    private Label guardianLbl;

    /**
     * Large font.
     */
    private Font largeFont;

    /**
     * Text editor background color.
     */
    private Color editorColor;

    /**
     * Instance variable for Safe Seas Monitor
     */
    private SafeSeasMonitor safeSeas;

    /**
     * Constructor.
     * 
     * @param parent
     *            Parent Shell.
     */
    public SafeSeasDialog(Shell parent) {
        super(parent, SWT.DIALOG_TRIM | SWT.RESIZE);
        setText("SAFESEAS");
    }

    @Override
    protected Layout constructShellLayout() {
        // Create the main layout for the shell.
        GridLayout mainLayout = new GridLayout(1, false);
        mainLayout.marginHeight = 5;
        mainLayout.marginWidth = 5;
        mainLayout.verticalSpacing = 5;
        return mainLayout;
    }

    @Override
    protected void disposed() {
        largeFont.dispose();
        editorColor.dispose();
    }

    @Override
    protected void initializeComponents(Shell shell) {
        largeFont = new Font(shell.getDisplay(), "Monospace", 10, SWT.NORMAL);

        editorColor = new Color(getDisplay(), 82, 107, 129);

        // Initialize all of the controls and layouts
        createGuardianControl();
        createBottomButtons();

        shell.addShellListener(new ShellAdapter() {
            @Override
            public void shellClosed(ShellEvent event) {
                closeDisplay();
            }
        });
    }

    /**
     * Create the Guardian control.
     */
    private void createGuardianControl() {
        Composite guardianComp = new Composite(shell, SWT.NONE);
        GridData gd = new GridData(SWT.FILL, SWT.DEFAULT, true, false);
        guardianComp.setLayout(new GridLayout(1, false));
        guardianComp.setLayoutData(gd);

        gd = new GridData(SWT.FILL, SWT.DEFAULT, true, false);
        gd.heightHint = 25;
        gd.widthHint = 450;

        guardianLbl = new Label(guardianComp, SWT.CENTER);
        guardianLbl
                .setText("The color coded result of this decision assistance tool");
        guardianLbl.setLayoutData(gd);

        gd = new GridData(SWT.FILL, SWT.DEFAULT, true, false);
        gd.heightHint = 150;
        gd.widthHint = 450;

        threatMessageViewerStTxt = new StyledText(guardianComp, SWT.BORDER
                | SWT.MULTI | SWT.V_SCROLL | SWT.H_SCROLL);
        threatMessageViewerStTxt.setWordWrap(true);
        threatMessageViewerStTxt.setFont(largeFont);
        threatMessageViewerStTxt.setEditable(false);
        threatMessageViewerStTxt.setLayoutData(gd);
        threatMessageViewerStTxt.setBackground(editorColor);
        threatMessageViewerStTxt.setForeground(getDisplay().getSystemColor(
                SWT.COLOR_WHITE));

    }

    /**
     * Create the OK and Cancel buttons.
     */
    private void createBottomButtons() {
        Composite buttonComp = new Composite(shell, SWT.NONE);
        GridData gd = new GridData(SWT.CENTER, SWT.DEFAULT, true, false);
        buttonComp.setLayout(new GridLayout(2, false));
        buttonComp.setLayoutData(gd);

        gd = new GridData(80, SWT.DEFAULT);
        Button okBtn = new Button(buttonComp, SWT.PUSH);
        okBtn.setText("Start");
        okBtn.setLayoutData(gd);
        okBtn.addSelectionListener(new SelectionAdapter() {
            @Override
            public void widgetSelected(SelectionEvent event) {
                SafeSeasReportModel.getInstance().setSafeSeasDisplay(
                        SafeSeasDialog.this);
                safeSeas = SafeSeasMonitor.getInstance();
                safeSeas
                        .readTableConfig(MonitorThresholdConfiguration.SAFESEAS_THRESHOLD_CONFIG);
            }
        });

        gd = new GridData(80, SWT.DEFAULT);
        Button cancelBtn = new Button(buttonComp, SWT.PUSH);
        cancelBtn.setText("Stop");
        cancelBtn.setLayoutData(gd);
        cancelBtn.addSelectionListener(new SelectionAdapter() {
            @Override
            public void widgetSelected(SelectionEvent event) {
                closeDisplay();
            }
        });
    }

    /**
     * Close the display.
     */
    public void closeDisplay() {
        try {
            // Unregister for all alert notifications on this monitor
            for (String p : SafeSeasMonitor.pluginName) {
                safeSeas.stopObserver(p, safeSeas);
            }
            // Kill the monitor
            safeSeas.nullifyMonitor();
        } catch (NullPointerException e) {
            // Dispose the dialog
            shell.dispose();
        }
        // Dispose the dialog
        shell.dispose();
    }

    /**
     * This method puts the guardian threat priority in the label
     */
    @Override
    public void putPriority(ObConst.ThreatLevel threatPriority) {
        try {
            switch (threatPriority) {
            case BLACK:
                guardianLbl.setBackground(getDisplay().getSystemColor(
                        SWT.COLOR_BLACK));
                break;
            case GRAY:
                guardianLbl.setBackground(getDisplay().getSystemColor(
                        SWT.COLOR_GRAY));
                break;
            case GREEN:
                guardianLbl.setBackground(getDisplay().getSystemColor(
                        SWT.COLOR_GREEN));
                break;
            case YELLOW:
                guardianLbl.setBackground(getDisplay().getSystemColor(
                        SWT.COLOR_YELLOW));
                break;
            case RED:
                guardianLbl.setBackground(getDisplay().getSystemColor(
                        SWT.COLOR_RED));
                break;
            default:
                guardianLbl.setBackground(getDisplay().getSystemColor(
                        SWT.COLOR_GRAY));
                break;
            }
        } catch (Exception e) {

        }

    }

    /**
     * This method puts the guardian threat message in the text control
     */
    @Override
    public void putMessage(String threatMessage) {
        try {
            threatMessageViewerStTxt.setText(threatMessage);
        } catch (Exception e) {

        }

    }

}
