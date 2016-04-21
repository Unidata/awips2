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
package com.raytheon.viz.xdat;

import org.eclipse.swt.SWT;
import org.eclipse.swt.events.SelectionAdapter;
import org.eclipse.swt.events.SelectionEvent;
import org.eclipse.swt.graphics.Font;
import org.eclipse.swt.layout.GridData;
import org.eclipse.swt.layout.GridLayout;
import org.eclipse.swt.widgets.Button;
import org.eclipse.swt.widgets.Composite;
import org.eclipse.swt.widgets.Label;
import org.eclipse.swt.widgets.Layout;
import org.eclipse.swt.widgets.List;
import org.eclipse.swt.widgets.MessageBox;
import org.eclipse.swt.widgets.Shell;
import org.eclipse.swt.widgets.Spinner;
import org.eclipse.swt.widgets.Text;

import com.raytheon.viz.hydrocommon.HydroConstants;
import com.raytheon.viz.ui.dialogs.CaveSWTDialog;

/**
 * This class displays the Insert Data dialog.
 * 
 * <pre>
 * SOFTWARE HISTORY
 * Date         Ticket#    Engineer    Description
 * ------------ ---------- ----------- --------------------------
 * 10 Nov 2008             lvenable    Initial creation.
 * 10 Feb 2009             wkwock      Added functions.
 * 22 May 2015             skorolev    Corrected shefParamCode. Got rid of Vector.
 * 
 * </pre>
 * 
 * @author lvenable
 * @version 1.0
 * 
 */
public class InsertDataDlg extends CaveSWTDialog {

    /**
     * Control font.
     */
    private Font controlFont;

    /**
     * ID text control.
     */
    private Text idTF;

    /**
     * Physical Element text control.
     */
    private Text peTypeTF;

    /**
     * Choose SHEF label.
     */
    private Label chooseShefLbl;

    /**
     * SHEF parameter list control.
     */
    private List shefParamList;

    /**
     * Enter data value label.
     */
    private Label enterDataLbl;

    /**
     * Enter data value text control.
     */
    private Text enterDataTF;

    /**
     * Choose hour label.
     */
    private Label chooseHourLbl;

    /**
     * Choose minute label.
     */
    private Label chooseMinLbl;

    /**
     * Post to Database button.
     */
    private Button postToDbBtn;

    /**
     * Hours spinner.
     */
    private Spinner hourSpnr;

    /**
     * Minute spinner.
     */
    private Spinner minuteSpnr;

    /**
     * PE from main menu
     */
    private String pe;

    /**
     * object for access Database
     */
    private XdatDB databaseMgr;

    /**
     * Callback interface used to display the data on the screen.
     */
    private ITextDisplay displayCB;

    /**
     * Constructor.
     * 
     * @param parentShell
     *            Parent shell.
     */
    public InsertDataDlg(Shell parentShell, String peType, XdatDB database,
            ITextDisplay displayCB) {
        super(parentShell, SWT.DIALOG_TRIM | SWT.RESIZE);
        setText("Insert Data");

        pe = peType;
        this.databaseMgr = database;
        this.displayCB = displayCB;
    }

    @Override
    protected Layout constructShellLayout() {
        return new GridLayout(1, false);
    }

    @Override
    protected void disposed() {
        controlFont.dispose();
    }

    @Override
    protected void preOpened() {
        shell.setMinimumSize(shell.getBounds().width, shell.getBounds().height);
    }

    @Override
    protected void initializeComponents(Shell shell) {
        setReturnValue(false);
        // Initialize all of the controls and layouts
        controlFont = new Font(shell.getDisplay(), "Monospace", 10, SWT.NORMAL);

        createTopControls();

        // Add a horizontal separator bar
        GridData gd = new GridData(GridData.FILL_HORIZONTAL);
        Label sepLbl = new Label(shell, SWT.SEPARATOR | SWT.HORIZONTAL);
        sepLbl.setLayoutData(gd);

        createBottomControls();

        // Add a horizontal separator bar
        gd = new GridData(GridData.FILL_HORIZONTAL);
        Label sepLbl2 = new Label(shell, SWT.SEPARATOR | SWT.HORIZONTAL);
        sepLbl2.setLayoutData(gd);

        createCloseButton();

        enableShefControls(true);
        idTF.setText(displayCB.getSelectedSite());
    }

    /**
     * Create the top set of controls.
     */
    private void createTopControls() {
        GridData gd = new GridData(SWT.FILL, SWT.DEFAULT, true, false);
        Composite topCtrlComp = new Composite(shell, SWT.NONE);
        GridLayout gl = new GridLayout(2, false);
        gl.verticalSpacing = 10;
        topCtrlComp.setLayout(gl);
        topCtrlComp.setLayoutData(gd);

        gd = new GridData(SWT.FILL, SWT.CENTER, false, true);
        Label idLbl = new Label(topCtrlComp, SWT.RIGHT);
        idLbl.setText("ID: ");
        idLbl.setLayoutData(gd);

        gd = new GridData(100, SWT.DEFAULT);
        idTF = new Text(topCtrlComp, SWT.BORDER);
        idTF.setLayoutData(gd);

        gd = new GridData(SWT.FILL, SWT.CENTER, false, true);
        Label peTypeLbl = new Label(topCtrlComp, SWT.RIGHT);
        peTypeLbl.setText("PE Type: ");
        peTypeLbl.setLayoutData(gd);

        gd = new GridData(50, SWT.DEFAULT);
        peTypeTF = new Text(topCtrlComp, SWT.BORDER);
        peTypeTF.setLayoutData(gd);
        peTypeTF.setTextLimit(2);
        peTypeTF.setText(pe);

        gd = new GridData(SWT.CENTER, SWT.DEFAULT, true, false);
        gd.horizontalSpan = 2;
        Button shefParamBtn = new Button(topCtrlComp, SWT.PUSH);
        shefParamBtn.setText(" Get SHEF Parameter Code Choices ");
        shefParamBtn.setLayoutData(gd);
        shefParamBtn.addSelectionListener(new SelectionAdapter() {
            @Override
            public void widgetSelected(SelectionEvent event) {
                String ID = idTF.getText().trim();
                if (ID.compareTo("") == 0) {
                    MessageBox mb = new MessageBox(getParent(),
                            SWT.ICON_WARNING | SWT.OK);
                    mb.setMessage("Please enter a ID.");
                    mb.open();
                    return;
                }

                String PE = peTypeTF.getText().trim();
                if (PE.compareTo("") == 0) {
                    MessageBox mb = new MessageBox(getParent(),
                            SWT.ICON_WARNING | SWT.OK);
                    mb.setMessage("Please enter a PE Type.");
                    mb.open();
                    return;
                }

                java.util.List<String> shefParamCode = databaseMgr
                        .getShefParamCode(ID, PE);
                if (shefParamCode != null) {
                    for (int i = 0; i < shefParamCode.size(); i++) {
                        shefParamList.add(shefParamCode.get(i));
                    }
                }
            }
        });
    }

    /**
     * Create the bottom set of controls (can be disabled).
     */
    private void createBottomControls() {
        GridData gd = new GridData(SWT.FILL, SWT.DEFAULT, true, false);
        Composite bottomCtrlComp = new Composite(shell, SWT.NONE);
        GridLayout gl = new GridLayout(2, false);
        gl.verticalSpacing = 10;
        bottomCtrlComp.setLayout(gl);
        bottomCtrlComp.setLayoutData(gd);

        gd = new GridData();
        gd.horizontalSpan = 2;
        chooseShefLbl = new Label(bottomCtrlComp, SWT.NONE);
        chooseShefLbl.setText("Choose SHEF Parameter Code:");
        chooseShefLbl.setLayoutData(gd);

        gd = new GridData(SWT.FILL, SWT.FILL, true, true);
        gd.widthHint = 300;
        gd.heightHint = 200;
        gd.horizontalSpan = 2;
        shefParamList = new List(bottomCtrlComp, SWT.BORDER | SWT.SINGLE
                | SWT.V_SCROLL);
        shefParamList.setLayoutData(gd);
        shefParamList.setFont(controlFont);

        enterDataLbl = new Label(bottomCtrlComp, SWT.NONE);
        enterDataLbl.setText("Enter Data Value: ");

        gd = new GridData(120, SWT.DEFAULT);
        enterDataTF = new Text(bottomCtrlComp, SWT.BORDER);
        enterDataTF.setLayoutData(gd);

        chooseHourLbl = new Label(bottomCtrlComp, SWT.NONE);
        chooseHourLbl.setText("Choose Hour (GMT): ");

        gd = new GridData(40, SWT.DEFAULT);
        hourSpnr = new Spinner(bottomCtrlComp, SWT.BORDER);
        hourSpnr.setDigits(0);
        hourSpnr.setIncrement(1);
        hourSpnr.setPageIncrement(5);
        hourSpnr.setMinimum(0);
        hourSpnr.setMaximum(23);
        hourSpnr.setSelection(12);
        hourSpnr.setLayoutData(gd);

        chooseMinLbl = new Label(bottomCtrlComp, SWT.NONE);
        chooseMinLbl.setText("Choose Minute: ");

        gd = new GridData(40, SWT.DEFAULT);
        minuteSpnr = new Spinner(bottomCtrlComp, SWT.BORDER);
        minuteSpnr.setDigits(0);
        minuteSpnr.setIncrement(1);
        minuteSpnr.setPageIncrement(5);
        minuteSpnr.setMinimum(0);
        minuteSpnr.setMaximum(59);
        minuteSpnr.setSelection(0);
        minuteSpnr.setLayoutData(gd);

        gd = new GridData(SWT.CENTER, SWT.DEFAULT, true, false);
        gd.widthHint = 120;
        gd.horizontalSpan = 2;
        postToDbBtn = new Button(bottomCtrlComp, SWT.PUSH);
        postToDbBtn.setText("Post to DB");
        postToDbBtn.setLayoutData(gd);
        postToDbBtn.addSelectionListener(new SelectionAdapter() {
            @Override
            public void widgetSelected(SelectionEvent event) {
                postToDB();
            }
        });
    }

    /**
     * Create the close button.
     */
    private void createCloseButton() {
        GridData gd = new GridData(SWT.FILL, SWT.DEFAULT, true, false);
        Composite buttonComp = new Composite(shell, SWT.NONE);
        buttonComp.setLayout(new GridLayout(1, false));
        buttonComp.setLayoutData(gd);

        gd = new GridData(SWT.CENTER, SWT.DEFAULT, true, false);
        gd.widthHint = 90;
        Button closeBtn = new Button(buttonComp, SWT.PUSH);
        closeBtn.setText("Close");
        closeBtn.setLayoutData(gd);
        closeBtn.addSelectionListener(new SelectionAdapter() {
            @Override
            public void widgetSelected(SelectionEvent event) {
                shell.dispose();
            }
        });
    }

    /**
     * Enable/Disable the SHEF controls.
     * 
     * @param flag
     *            Enable flag.
     */
    private void enableShefControls(boolean flag) {
        chooseShefLbl.setEnabled(flag);
        shefParamList.setEnabled(flag);
        enterDataLbl.setEnabled(flag);
        enterDataTF.setEnabled(flag);
        chooseHourLbl.setEnabled(flag);
        chooseMinLbl.setEnabled(flag);
        postToDbBtn.setEnabled(flag);
        hourSpnr.setEnabled(flag);
        minuteSpnr.setEnabled(flag);
    }

    private void postToDB() {
        String id = idTF.getText().trim();
        if (id.compareTo("") == 0) {
            MessageBox mb = new MessageBox(getParent(), SWT.ICON_ERROR | SWT.OK);
            mb.setText("Error");
            mb.setMessage("ERROR:  cannot post because ID is blank.");
            mb.open();
            return;
        }

        String shefParamCode[] = shefParamList.getSelection();
        if (shefParamCode.length != 1) {
            MessageBox mb = new MessageBox(getParent(), SWT.ICON_ERROR | SWT.OK);
            mb.setText("Error");
            mb.setMessage("ERROR: cannot post because PE Type is blank.");
            mb.open();
            return;
        }

        String pe = shefParamCode[0].split(" ")[0];

        double value = HydroConstants.MISSING_VALUE;
        try {
            value = Double.parseDouble(enterDataTF.getText().trim());
        } catch (NumberFormatException nfe) {
            MessageBox mb = new MessageBox(getParent(), SWT.ICON_ERROR | SWT.OK);
            mb.setText("Error");
            mb.setMessage("ERROR: cannot post because data value is blank or invalid.");
            mb.open();
            return;
        }

        int hour = hourSpnr.getSelection();
        int minute = minuteSpnr.getSelection();

        XdatShefUtil shefUtil = new XdatShefUtil();
        shefUtil.createInsertFile(id, pe, value, hour, minute,
                displayCB.getStartDate());
        shefUtil.sendFile();

        shell.dispose();
    }
}
