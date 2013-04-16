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
package com.raytheon.viz.hydro.timeseries;

import java.text.ParseException;
import java.util.ArrayList;
import java.util.Iterator;

import org.eclipse.swt.SWT;
import org.eclipse.swt.events.SelectionAdapter;
import org.eclipse.swt.events.SelectionEvent;
import org.eclipse.swt.events.ShellAdapter;
import org.eclipse.swt.events.ShellEvent;
import org.eclipse.swt.layout.GridData;
import org.eclipse.swt.layout.GridLayout;
import org.eclipse.swt.layout.RowLayout;
import org.eclipse.swt.widgets.Button;
import org.eclipse.swt.widgets.Composite;
import org.eclipse.swt.widgets.Label;
import org.eclipse.swt.widgets.Layout;
import org.eclipse.swt.widgets.List;
import org.eclipse.swt.widgets.MessageBox;
import org.eclipse.swt.widgets.Shell;
import org.eclipse.swt.widgets.Text;

import com.raytheon.viz.hydro.timeseries.table.ForecastDataAttribute;
import com.raytheon.viz.hydrocommon.HydroConstants;
import com.raytheon.viz.ui.dialogs.CaveSWTDialog;

/**
 * Dialog to insert forecast data attributes.
 * 
 * <pre>
 * 
 * SOFTWARE HISTORY
 * Date         Ticket#    Engineer    Description
 * ------------ ---------- ----------- --------------------------
 * Mar 26, 2009            mpduff     Initial creation
 * April 16, 2013 1790     rferrel     Make non-blocking dialog.
 * 
 * </pre>
 * 
 * @author mpduff
 * @version 1.0
 */

public class ForecastAttributeDlg extends CaveSWTDialog {

    /** Dialog title */
    private static final String TITLE = "Insert Forecast Data Attributes";

    /** Product ID label */
    private static final String PROD_ID_LBL = "Product ID/Time:";

    /** Basis time label */
    private static final String BASIS_TIME_LBL = "Basis Time:";

    /** Type source label */
    private static final String TYPE_SOURCE_LBL = "Type\n Source:";

    /** Product ID text field */
    private Text prodIdTF;

    /** Time text field */
    private Text timeTF;

    /** Basis time text field */
    private Text basisTimeTF;

    /** Type Source list */
    private List tsList;

    /** Forecast data attribute data object */
    private ForecastDataAttribute fcstDataAtt = null;

    /** List of listeners. */
    private ArrayList<ForecastDataAttributeListener> listenerList = new ArrayList<ForecastDataAttributeListener>();

    /**
     * Non-blocking constructor.
     * 
     * @param parentShell
     * @param fcstDataAtt
     */
    public ForecastAttributeDlg(Shell parentShell,
            ForecastDataAttribute fcstDataAtt) {
        super(parentShell, SWT.DIALOG_TRIM, CAVE.DO_NOT_BLOCK);
        setText(TITLE);

        this.fcstDataAtt = fcstDataAtt;
    }

    /*
     * (non-Javadoc)
     * 
     * @see com.raytheon.viz.ui.dialogs.CaveSWTDialogBase#constructShellLayout()
     */
    @Override
    protected Layout constructShellLayout() {
        // Create the main layout for the shell.
        GridLayout mainLayout = new GridLayout(1, true);
        mainLayout.marginHeight = 1;
        mainLayout.marginWidth = 1;
        return mainLayout;
    }

    /*
     * (non-Javadoc)
     * 
     * @see
     * com.raytheon.viz.ui.dialogs.CaveSWTDialogBase#initializeComponents(org
     * .eclipse.swt.widgets.Shell)
     */
    @Override
    protected void initializeComponents(Shell shell) {
        setReturnValue(false);
        // Initialize all of the controls and layouts
        initializeComponents();

        shell.addShellListener(new ShellAdapter() {
            @Override
            public void shellClosed(ShellEvent event) {
                // Block the disposal of this dialog by way of the "X" button.
                ForecastAttributeDlg.this.shell.setVisible(false);
                event.doit = false;
            }
        });
    }

    /**
     * Initialize the dialog components.
     */
    private void initializeComponents() {
        createTextAreas();

        createBtns();
    }

    /**
     * Create the text and list widgets.
     */
    private void createTextAreas() {
        Composite prodIdLblComp = new Composite(shell, SWT.NONE);
        RowLayout layout = new RowLayout();
        prodIdLblComp.setLayout(layout);

        Label prodIdLbl = new Label(prodIdLblComp, SWT.NONE);
        prodIdLbl.setText(PROD_ID_LBL);

        Composite prodIdComp = new Composite(shell, SWT.NONE);
        GridLayout prodIdTFLayout = new GridLayout(2, false);
        prodIdComp.setLayout(prodIdTFLayout);

        Label spacer = new Label(prodIdComp, SWT.NONE);
        spacer.setText(" ");

        prodIdTF = new Text(prodIdComp, SWT.BORDER);
        GridData gd = new GridData(275, SWT.DEFAULT);
        prodIdTF.setLayoutData(gd);
        prodIdTF.setText(fcstDataAtt.getProductId());

        Composite timeComp = new Composite(shell, SWT.NONE);
        GridLayout timeTFLayout = new GridLayout(2, false);
        timeComp.setLayout(timeTFLayout);

        Label spacer2 = new Label(timeComp, SWT.NONE);
        spacer2.setText(" ");

        timeTF = new Text(timeComp, SWT.BORDER);
        timeTF.setLayoutData(gd);
        timeTF.setText(fcstDataAtt.getTime());

        Composite basisTimeLblComp = new Composite(shell, SWT.NONE);
        RowLayout basisTimeLblLayout = new RowLayout();
        basisTimeLblComp.setLayout(basisTimeLblLayout);

        Label basisTimeLbl = new Label(basisTimeLblComp, SWT.NONE);
        basisTimeLbl.setText(BASIS_TIME_LBL);

        Composite basisTimeComp = new Composite(shell, SWT.NONE);
        GridLayout basisTimeTFLayout = new GridLayout(2, false);
        basisTimeComp.setLayout(basisTimeTFLayout);

        Label spacer3 = new Label(basisTimeComp, SWT.NONE);
        spacer3.setText(" ");

        basisTimeTF = new Text(basisTimeComp, SWT.BORDER);
        basisTimeTF.setLayoutData(gd);
        basisTimeTF.setText(fcstDataAtt.getBasisTime());

        Composite tsLblComp = new Composite(shell, SWT.NONE);
        GridLayout tsLblLayout = new GridLayout(2, false);
        tsLblComp.setLayout(tsLblLayout);

        Label tsLbl = new Label(tsLblComp, SWT.NONE);
        tsLbl.setText(TYPE_SOURCE_LBL);

        tsList = new List(tsLblComp, SWT.BORDER | SWT.SINGLE);
        gd = new GridData(SWT.BEGINNING, SWT.BEGINNING, true, true, 1, 1);
        gd.widthHint = 228;
        gd.heightHint = 100;
        tsList.setLayoutData(gd);
        for (String s : fcstDataAtt.getTypeSource()) {
            tsList.add(s);
        }
    }

    /**
     * Create the button widgets.
     */
    private void createBtns() {
        Composite btnComp = new Composite(shell, SWT.NONE);
        GridData gd = new GridData(SWT.FILL, SWT.DEFAULT, true, true);
        GridLayout buttonGl = new GridLayout(3, false);
        btnComp.setLayoutData(gd);
        btnComp.setLayout(buttonGl);

        int buttonWidth = 125;

        gd = new GridData(buttonWidth, SWT.DEFAULT);
        Button cancelBtn = new Button(btnComp, SWT.PUSH);
        cancelBtn.setText("Cancel");
        cancelBtn.setLayoutData(gd);
        cancelBtn.addSelectionListener(new SelectionAdapter() {
            @Override
            public void widgetSelected(SelectionEvent event) {
                shell.setVisible(false);
                reset();
            }
        });

        gd = new GridData(SWT.END, SWT.DEFAULT, true, false);
        gd.widthHint = buttonWidth;
        Button saveBtn = new Button(btnComp, SWT.PUSH);
        saveBtn.setText("Save");
        saveBtn.setLayoutData(gd);
        saveBtn.addSelectionListener(new SelectionAdapter() {
            @Override
            public void widgetSelected(SelectionEvent event) {
                shell.setVisible(false);
                String errMessage = "Invalid Product Time.";
                try {
                    HydroConstants.DATE_FORMAT.parse(timeTF.getText());
                    errMessage = "Invalid Basis Time.";
                    HydroConstants.DATE_FORMAT.parse(basisTimeTF.getText());
                    errMessage = "You must select a Type Source.";
                    if (tsList.getSelectionCount() == 0) {
                        MessageBox mb = new MessageBox(getParent(),
                                SWT.ICON_WARNING | SWT.OK);
                        mb.setText("Invalid Selection");
                        mb.setMessage(errMessage);
                        mb.open();
                        return;
                    }
                    fcstDataAtt.setBasisTime(basisTimeTF.getText());
                    fcstDataAtt.setSelectedTS(tsList.getItem(tsList
                            .getSelectionIndex()));
                    fcstDataAtt.setProductId(prodIdTF.getText());
                    fcstDataAtt.setTime(timeTF.getText());
                    fireUpdateEvent(new FcstAttUpdateEvent(event, fcstDataAtt));
                } catch (ParseException e) {
                    MessageBox mb = new MessageBox(getParent(),
                            SWT.ICON_WARNING | SWT.OK);
                    mb.setText("Invalid Date Format");
                    mb.setMessage(errMessage);
                    mb.open();
                }
            }
        });
    }

    /**
     * Reset the dialog.
     */
    private void reset() {
        prodIdTF.setText(fcstDataAtt.getProductId());
        timeTF.setText(fcstDataAtt.getTime());
        basisTimeTF.setText(fcstDataAtt.getBasisTime());
        tsList.deselectAll();
    }

    /**
     * Make the dialog visible.
     */
    public void showDialog() {
        shell.setVisible(true);
        shell.setFocus();
    }

    /**
     * Add a ForecastDataAttributeListener.
     * 
     * @param fdal
     */
    public void addListener(ForecastDataAttributeListener fdal) {
        listenerList.add(fdal);
    }

    /**
     * Fire a StationDisplayUpdateEvent.
     */
    public void fireUpdateEvent(FcstAttUpdateEvent event) {
        Iterator<ForecastDataAttributeListener> iter = listenerList.iterator();
        while (iter.hasNext()) {
            (iter.next()).notifyUpdate(event);
        }
    }
}