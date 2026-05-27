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

import java.util.ArrayList;
import java.util.Calendar;
import java.util.Iterator;

import org.eclipse.swt.SWT;
import org.eclipse.swt.events.SelectionAdapter;
import org.eclipse.swt.events.SelectionEvent;
import org.eclipse.swt.events.ShellAdapter;
import org.eclipse.swt.events.ShellEvent;
import org.eclipse.swt.layout.GridData;
import org.eclipse.swt.layout.GridLayout;
import org.eclipse.swt.widgets.Button;
import org.eclipse.swt.widgets.Composite;
import org.eclipse.swt.widgets.Label;
import org.eclipse.swt.widgets.List;
import org.eclipse.swt.widgets.MessageBox;
import org.eclipse.swt.widgets.Shell;
import org.eclipse.swt.widgets.Text;

import com.raytheon.uf.common.time.util.TimeUtil;
import com.raytheon.viz.hydro.timeseries.table.ForecastDataAttribute;
import com.raytheon.viz.hydrocommon.HydroConstants;
import com.raytheon.viz.ui.dialogs.CaveSWTDialog;
import com.raytheon.viz.ui.widgets.DateTimeSpinner;

/**
 * Dialog to insert forecast data attributes.
 * 
 * <pre>
 * 
 * SOFTWARE HISTORY
 * Date         Ticket#    Engineer    Description
 * ------------ ---------- ----------- --------------------------
 * Mar 26, 2009            mpduff      Initial creation
 * April 16, 2013 1790     rferrel     Make non-blocking dialog.
 * May 11, 2016   5483     bkowal      Fix GUI sizing issues. Cleanup.
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

    /** Product Time Calendar */
    private Calendar productTime;

    /** Calendar to use when resetting the Product Time */
    private Calendar resetProductTime;

    private DateTimeSpinner productTimeSpinner;

    /** Basis Time Calendar */
    private Calendar basisTime;

    /** Calendar to use when resetting the Basis Time */
    private Calendar resetBasisTime;

    private DateTimeSpinner basisTimeSpinner;

    /** Type Source list */
    private List tsList;

    /** Forecast data attribute data object */
    private ForecastDataAttribute fcstDataAtt = null;

    /** List of listeners. */
    private final java.util.List<ForecastDataAttributeListener> listenerList = new ArrayList<>();

    /**
     * Non-blocking constructor.
     * 
     * @param parentShell
     * @param fcstDataAtt
     */
    public ForecastAttributeDlg(Shell parentShell,
            ForecastDataAttribute fcstDataAtt, Calendar productTime,
            Calendar basisTime) {
        super(parentShell, SWT.DIALOG_TRIM, CAVE.DO_NOT_BLOCK);
        setText(TITLE);

        this.fcstDataAtt = fcstDataAtt;
        this.productTime = productTime;
        this.basisTime = basisTime;
        /*
         * Need to clone based on the current {@link DateTimeSpinner}
         * implementation so that it will be possible to reset the tracked
         * date/times.
         */
        this.resetProductTime = TimeUtil.newCalendar(productTime);
        this.resetBasisTime = TimeUtil.newCalendar(basisTime);
    }

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
        Composite mainComp = new Composite(shell, SWT.NONE);
        GridLayout gl = new GridLayout(1, false);
        GridData gd = new GridData(SWT.FILL, SWT.FILL, true, true);
        gd.minimumWidth = mainComp.getDisplay().getDPI().x * 3;
        mainComp.setLayout(gl);
        mainComp.setLayoutData(gd);

        Label prodIdLbl = new Label(mainComp, SWT.NONE);
        prodIdLbl.setText(PROD_ID_LBL);

        prodIdTF = new Text(mainComp, SWT.BORDER);
        gd = new GridData(SWT.FILL, SWT.DEFAULT, true, false);
        prodIdTF.setLayoutData(gd);
        prodIdTF.setText(fcstDataAtt.getProductId());

        productTimeSpinner = new DateTimeSpinner(mainComp, productTime, 6);

        Label basisTimeLbl = new Label(mainComp, SWT.NONE);
        basisTimeLbl.setText(BASIS_TIME_LBL);

        basisTimeSpinner = new DateTimeSpinner(mainComp, basisTime, 6);

        Composite tsLblComp = new Composite(shell, SWT.NONE);
        GridLayout tsLblLayout = new GridLayout(2, false);
        gd = new GridData(SWT.FILL, SWT.FILL, true, true);
        tsLblComp.setLayout(tsLblLayout);
        tsLblComp.setLayoutData(gd);

        Label tsLbl = new Label(tsLblComp, SWT.NONE);
        tsLbl.setText(TYPE_SOURCE_LBL);

        tsList = new List(tsLblComp, SWT.BORDER | SWT.SINGLE);
        gd = new GridData(SWT.FILL, SWT.FILL, true, true);
        gd.heightHint = tsList.getItemHeight() * 5;

        tsList.setLayoutData(gd);
        tsList.setItems(fcstDataAtt.getTypeSource());
    }

    /**
     * Create the button widgets.
     */
    private void createBtns() {
        Composite btnComp = new Composite(shell, SWT.NONE);
        GridData gd = new GridData(SWT.CENTER, SWT.DEFAULT, true, false);
        GridLayout buttonGl = new GridLayout(2, true);
        btnComp.setLayoutData(gd);
        btnComp.setLayout(buttonGl);

        final int minimumButtonWidth = btnComp.getDisplay().getDPI().x;

        gd = new GridData(SWT.DEFAULT, SWT.DEFAULT, true, false);
        gd.minimumWidth = minimumButtonWidth;
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

        gd = new GridData(SWT.DEFAULT, SWT.DEFAULT, true, false);
        gd.minimumWidth = minimumButtonWidth;
        Button saveBtn = new Button(btnComp, SWT.PUSH);
        saveBtn.setText("Save");
        saveBtn.setLayoutData(gd);
        saveBtn.addSelectionListener(new SelectionAdapter() {
            @Override
            public void widgetSelected(SelectionEvent event) {
                if (tsList.getSelectionCount() == 0) {
                    MessageBox mb = new MessageBox(getParent(),
                            SWT.ICON_WARNING | SWT.OK);
                    mb.setText("Invalid Selection");
                    mb.setMessage("You must select a Type Source.");
                    mb.open();
                    return;
                }
                shell.setVisible(false);
                fcstDataAtt.setBasisTime(HydroConstants.DATE_FORMAT
                        .format(basisTime.getTime()));
                fcstDataAtt.setSelectedTS(tsList.getItem(tsList
                        .getSelectionIndex()));
                fcstDataAtt.setProductId(prodIdTF.getText());
                fcstDataAtt.setTime(HydroConstants.DATE_FORMAT
                        .format(productTime.getTime()));

                resetProductTime = TimeUtil.newCalendar(productTime);
                resetBasisTime = TimeUtil.newCalendar(basisTime);

                fireUpdateEvent(new FcstAttUpdateEvent(event, fcstDataAtt));
            }
        });
    }

    /**
     * Reset the dialog.
     */
    private void reset() {
        prodIdTF.setText(fcstDataAtt.getProductId());
        productTime.setTimeInMillis(resetProductTime.getTimeInMillis());
        productTimeSpinner.redraw();
        basisTime.setTimeInMillis(resetBasisTime.getTimeInMillis());
        basisTimeSpinner.redraw();
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