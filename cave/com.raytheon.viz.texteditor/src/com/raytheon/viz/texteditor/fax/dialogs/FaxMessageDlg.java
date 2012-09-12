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
package com.raytheon.viz.texteditor.fax.dialogs;

import org.eclipse.swt.SWT;
import org.eclipse.swt.custom.StyledText;
import org.eclipse.swt.events.SelectionAdapter;
import org.eclipse.swt.events.SelectionEvent;
import org.eclipse.swt.layout.GridData;
import org.eclipse.swt.layout.GridLayout;
import org.eclipse.swt.widgets.Button;
import org.eclipse.swt.widgets.Composite;
import org.eclipse.swt.widgets.Label;
import org.eclipse.swt.widgets.Layout;
import org.eclipse.swt.widgets.Menu;
import org.eclipse.swt.widgets.MenuItem;
import org.eclipse.swt.widgets.Shell;
import org.eclipse.swt.widgets.Text;

import com.raytheon.uf.common.dataplugin.text.request.SendFaxRequest;
import com.raytheon.uf.common.status.IUFStatusHandler;
import com.raytheon.uf.common.status.UFStatus;
import com.raytheon.uf.common.status.UFStatus.Priority;
import com.raytheon.uf.viz.core.exception.VizException;
import com.raytheon.uf.viz.core.requests.ThriftClient;
import com.raytheon.viz.texteditor.TextWorkstationConstants;
import com.raytheon.viz.ui.dialogs.CaveSWTDialog;

/**
 * TODO Add Description
 * 
 * <pre>
 * 
 * SOFTWARE HISTORY
 * 
 * Date         Ticket#    Engineer    Description
 * ------------ ---------- ----------- --------------------------
 * Oct 31, 2010            lvenable     Initial creation
 * 
 * </pre>
 * 
 * @author lvenable
 * @version 1.0
 */

// TODO - need to replace CaveSWTDialogStub with CaveSWTDialog

public class FaxMessageDlg extends CaveSWTDialog {
    private static final transient IUFStatusHandler statusHandler = UFStatus.getHandler(FaxMessageDlg.class);
    private Text faxNumberTF;

    private Text recipientTF;

    private Text companyTF;

    private StyledText stText;

    private String initialText;

    public void setInitialText(String initialText) {
        this.initialText = initialText;
    }

    public FaxMessageDlg(Shell parent) {
        super(parent, SWT.DIALOG_TRIM | SWT.RESIZE);
        setText("Fax Message");
    }

    @Override
    protected Layout constructShellLayout() {
        // Create the main layout for the shell.
        GridLayout mainLayout = new GridLayout(1, false);
        mainLayout.marginHeight = 0;
        mainLayout.marginWidth = 2;
        mainLayout.verticalSpacing = 2;
        return mainLayout;
    }

    @Override
    protected Object constructShellLayoutData() {
        return new GridData(SWT.FILL, SWT.FILL, true, true);
    }

    /*
     * (non-Javadoc)
     * 
     * @see
     * com.raytheon.viz.ui.dialogs.CaveSWTDialog#initializeComponents(org.eclipse
     * .swt.widgets.Shell)
     */
    @Override
    protected void initializeComponents(Shell shell) {
        // Create the menus
        createMenus();

        // Create the controls on the display
        createControls();
    }

    private void createMenus() {
        Menu menuBar = new Menu(shell, SWT.BAR);

        createFileMenu(menuBar);
        createHelpMenu(menuBar);

        shell.setMenuBar(menuBar);
    }

    private void createFileMenu(Menu menuBar) {
        // -------------------------------------
        // Create the file menu
        // -------------------------------------
        MenuItem fileMenuItem = new MenuItem(menuBar, SWT.CASCADE);
        fileMenuItem.setText("&File");

        // Create the File menu item with a File "dropdown" menu
        Menu fileMenu = new Menu(menuBar);
        fileMenuItem.setMenu(fileMenu);

        /*
         * Exit
         */
        MenuItem exitMI = new MenuItem(fileMenu, SWT.NONE);
        exitMI.setText("Exit");
        exitMI.addSelectionListener(new SelectionAdapter() {
            public void widgetSelected(SelectionEvent event) {
                shell.dispose();
            }
        });
    }

    private void createHelpMenu(Menu menuBar) {
        // -------------------------------------
        // Create the file menu
        // -------------------------------------
        MenuItem helpMenuItem = new MenuItem(menuBar, SWT.CASCADE);
        helpMenuItem.setText("&Help");

        // Create the File menu item with a Help "dropdown" menu
        Menu helpMenu = new Menu(menuBar);
        helpMenuItem.setMenu(helpMenu);

        /*
         * Contents and About
         */
        MenuItem contentsMI = new MenuItem(helpMenu, SWT.NONE);
        contentsMI.setText("&Contents");
        contentsMI.addSelectionListener(new SelectionAdapter() {
            public void widgetSelected(SelectionEvent event) {

            }
        });

        MenuItem aboutMI = new MenuItem(helpMenu, SWT.NONE);
        aboutMI.setText("&About");
        aboutMI.addSelectionListener(new SelectionAdapter() {
            public void widgetSelected(SelectionEvent event) {

            }
        });
    }

    private void createControls() {
        Composite controlComp = new Composite(shell, SWT.NONE);
        controlComp.setLayout(new GridLayout(2, false));
        controlComp.setLayoutData(new GridData(SWT.FILL, SWT.FILL, true, true));

        GridData gd = new GridData(SWT.CENTER, SWT.DEFAULT, true, false);
        gd.horizontalSpan = ((GridLayout) controlComp.getLayout()).numColumns;
        Label ldadLbl = new Label(controlComp, SWT.NONE);
        ldadLbl.setText("LDAD Fax Recipient");
        ldadLbl.setLayoutData(gd);

        Label faxNumLbl = new Label(controlComp, SWT.NONE);
        faxNumLbl.setText("Fax Number: ");

        gd = new GridData(180, SWT.DEFAULT);
        faxNumberTF = new Text(controlComp, SWT.BORDER);
        faxNumberTF.setLayoutData(gd);

        // Add a separator bar
        addSeparator(controlComp);

        Label recipLbl = new Label(controlComp, SWT.NONE);
        recipLbl.setText("Recipient: ");

        gd = new GridData(275, SWT.DEFAULT);
        recipientTF = new Text(controlComp, SWT.BORDER);
        recipientTF.setLayoutData(gd);

        Label companyLbl = new Label(controlComp, SWT.NONE);
        companyLbl.setText("Company: ");

        gd = new GridData(275, SWT.DEFAULT);
        companyTF = new Text(controlComp, SWT.BORDER);
        companyTF.setLayoutData(gd);

        // Add a separator bar
        addSeparator(controlComp);

        /*
         * Send and Cancel buttons
         */
        Composite buttonComp = new Composite(controlComp, SWT.NONE);
        buttonComp.setLayout(new GridLayout(2, true));
        gd = new GridData(SWT.FILL, SWT.DEFAULT, true, false);
        gd.horizontalSpan = ((GridLayout) controlComp.getLayout()).numColumns;
        buttonComp.setLayoutData(gd);

        gd = new GridData(SWT.FILL, SWT.DEFAULT, true, false);
        Button sendBtn = new Button(buttonComp, SWT.PUSH);
        sendBtn.setText("Send");
        sendBtn.setLayoutData(gd);
        sendBtn.addSelectionListener(new SelectionAdapter() {
            @Override
            public void widgetSelected(SelectionEvent e) {
                sendAction();
            }
        });

        gd = new GridData(SWT.FILL, SWT.DEFAULT, true, false);
        Button cancelBtn = new Button(buttonComp, SWT.PUSH);
        cancelBtn.setText("Cancel");
        cancelBtn.setLayoutData(gd);
        cancelBtn.addSelectionListener(new SelectionAdapter() {
            @Override
            public void widgetSelected(SelectionEvent e) {
                shell.dispose();
            }
        });

        /*
         * Styled text control
         */
        gd = new GridData(SWT.FILL, SWT.FILL, true, true);
        gd.widthHint = 500; // TODO : may need to be removed
        gd.heightHint = 350; // TODO : may need to be removed
        gd.horizontalSpan = ((GridLayout) controlComp.getLayout()).numColumns;

        stText = new StyledText(controlComp, SWT.BORDER | SWT.V_SCROLL
                | SWT.WRAP);
        stText.setLayoutData(gd);
        if (initialText != null) {
            stText.setText(initialText);
        }
    }

    private void addSeparator(Composite parentComp) {
        GridLayout gl = (GridLayout) parentComp.getLayout();

        GridData gd = new GridData(SWT.FILL, SWT.DEFAULT, true, false);
        gd.horizontalSpan = gl.numColumns;
        Label sepLbl = new Label(parentComp, SWT.SEPARATOR | SWT.HORIZONTAL);
        sepLbl.setLayoutData(gd);
    }

    private void sendAction() {
        String faxNumber = faxNumberTF.getText();
        String faxTitle = TextWorkstationConstants.getHostName();
        String faxCompany = companyTF.getText();
        String faxText = stText.getText();
        String faxRecipient = recipientTF.getText();
        if (null != faxNumber && null != faxTitle && null != faxCompany
                && null != faxText && null != faxRecipient) {
            SendFaxRequest faxReq = new SendFaxRequest(faxNumber, faxTitle,
                    faxCompany, faxText, faxRecipient);
            try {
                Object retval = ThriftClient.sendRequest(faxReq);
                if (retval instanceof String && !"Success".equals(retval)) {
                    statusHandler.handle(Priority.SIGNIFICANT,
 (String) retval);
                }
            } catch (VizException e) {
                statusHandler.handle(Priority.SIGNIFICANT,
                        "Exception while sending fax to edex.", e);
            }
        }
    }
}
