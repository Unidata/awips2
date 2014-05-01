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
import org.eclipse.swt.custom.ScrolledComposite;
import org.eclipse.swt.events.ControlAdapter;
import org.eclipse.swt.events.ControlEvent;
import org.eclipse.swt.events.SelectionAdapter;
import org.eclipse.swt.events.SelectionEvent;
import org.eclipse.swt.layout.GridData;
import org.eclipse.swt.layout.GridLayout;
import org.eclipse.swt.widgets.Button;
import org.eclipse.swt.widgets.Composite;
import org.eclipse.swt.widgets.Label;
import org.eclipse.swt.widgets.Layout;
import org.eclipse.swt.widgets.MessageBox;
import org.eclipse.swt.widgets.Shell;

import com.raytheon.viz.aviation.AviationDialog;
import com.raytheon.viz.ui.dialogs.CaveSWTDialog;

/**
 * This is a dialog changing the values for the AvnFPS resources.
 * 
 * <pre>
 * 
 * SOFTWARE HISTORY
 * 
 * Date         Ticket#    Engineer    Description
 * ------------ ---------- ----------- --------------------------
 *                                     Initial creation
 * Oct 10, 2012 1229       rferrel     Make dialog non-blocking.
 * 
 * </pre>
 * 
 * @author rferrel
 * @version 1.0
 */
public class ResourceEditorDlg extends CaveSWTDialog implements IResize {

    private ScrolledComposite scrolledComp;

    private Composite resourceComp;

    private final int SCROLLED_COMP_WIDTH = 400;

    private final int SCROLLED_COMP_HEIGHT = 500;

    private ResourceItemComp resItemComp;

    private ResourceEditorHelpDlg helpDlg;

    public ResourceEditorDlg(Shell parent) {
        super(parent, SWT.SHELL_TRIM, CAVE.PERSPECTIVE_INDEPENDENT
                | CAVE.DO_NOT_BLOCK);
        setText("AvnFPS Resource Editor");
    }

    @Override
    protected void preOpened() {
        shell.setMinimumSize(500, 550);
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
    protected void initializeComponents(Shell shell) {
        createTopLabelAndButtons();
        createScrolledComposite();

        createResourceItems();
    }

    private void createTopLabelAndButtons() {
        String userName = AviationDialog.USERNAME;

        GridData gd = new GridData(SWT.FILL, SWT.DEFAULT, true, false);
        Label title = new Label(shell, SWT.CENTER);
        title.setText("Resource configuration for: " + userName);
        title.setLayoutData(gd);

        Composite buttonComp = new Composite(shell, SWT.NONE);
        buttonComp.setLayout(new GridLayout(4, true));
        buttonComp.setLayoutData(new GridData(SWT.FILL, SWT.DEFAULT, true,
                false));

        gd = new GridData(SWT.CENTER, SWT.DEFAULT, true, false);
        gd.widthHint = 100;
        Button closeBtn = new Button(buttonComp, SWT.PUSH);
        closeBtn.setText("Close");
        closeBtn.setLayoutData(gd);
        closeBtn.addSelectionListener(new SelectionAdapter() {
            @Override
            public void widgetSelected(SelectionEvent e) {
                shell.dispose();
            }
        });

        gd = new GridData(SWT.CENTER, SWT.DEFAULT, true, false);
        gd.widthHint = 100;
        Button saveBtn = new Button(buttonComp, SWT.PUSH);
        saveBtn.setText("Save");
        saveBtn.setLayoutData(gd);
        saveBtn.addSelectionListener(new SelectionAdapter() {
            @Override
            public void widgetSelected(SelectionEvent e) {
                resItemComp.saveResources();

                MessageBox mb = new MessageBox(shell, SWT.ICON_INFORMATION
                        | SWT.OK);
                mb.setText("Restart Notice");
                mb.setMessage("Restart GUI for change to take effect.");
                mb.open();

                shell.dispose();
            }
        });

        gd = new GridData(SWT.CENTER, SWT.DEFAULT, true, false);
        gd.widthHint = 100;
        Button restoreBtn = new Button(buttonComp, SWT.PUSH);
        restoreBtn.setText("Restore");
        restoreBtn.setLayoutData(gd);
        restoreBtn.addSelectionListener(new SelectionAdapter() {
            @Override
            public void widgetSelected(SelectionEvent e) {
                resItemComp.restoreAllResources();
            }
        });

        gd = new GridData(SWT.CENTER, SWT.DEFAULT, true, false);
        gd.widthHint = 100;
        Button helpBtn = new Button(buttonComp, SWT.PUSH);
        helpBtn.setText("Help");
        helpBtn.setLayoutData(gd);
        helpBtn.addSelectionListener(new SelectionAdapter() {
            @Override
            public void widgetSelected(SelectionEvent e) {
                if (mustCreate(helpDlg)) {
                    helpDlg = new ResourceEditorHelpDlg(shell);
                    helpDlg.open();
                } else {
                    helpDlg.bringToTop();
                }
            }
        });
    }

    private void createScrolledComposite() {
        scrolledComp = new ScrolledComposite(shell, SWT.V_SCROLL | SWT.H_SCROLL
                | SWT.BORDER);
        GridLayout gl = new GridLayout(1, false);
        gl.verticalSpacing = 1;
        scrolledComp.setLayout(gl);
        GridData gd = new GridData(SWT.FILL, SWT.FILL, true, true);
        gd.heightHint = SCROLLED_COMP_HEIGHT;
        gd.widthHint = SCROLLED_COMP_WIDTH;
        scrolledComp.setLayoutData(gd);

        gl = new GridLayout(1, false);
        gl.verticalSpacing = 1;
        resourceComp = new Composite(scrolledComp, SWT.NONE);
        resourceComp.setLayout(gl);
        resourceComp.layout();

        scrolledComp.setContent(resourceComp);
        scrolledComp.setExpandHorizontal(true);
        scrolledComp.setExpandVertical(true);

        scrolledComp.addControlListener(new ControlAdapter() {
            @Override
            public void controlResized(ControlEvent e) {
                scrolledComp.setMinSize(resourceComp.computeSize(SWT.DEFAULT,
                        SWT.DEFAULT));
            }
        });

        scrolledComp.layout();
    }

    private void createResourceItems() {
        resItemComp = new ResourceItemComp(resourceComp, this);
    }

    @Override
    public void resizeAction() {
        scrolledComp.layout();
        scrolledComp.setMinSize(resourceComp.computeSize(SWT.DEFAULT,
                SWT.DEFAULT));
    }
}
