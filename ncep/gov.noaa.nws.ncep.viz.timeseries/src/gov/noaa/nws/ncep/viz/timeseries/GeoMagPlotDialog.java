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
package gov.noaa.nws.ncep.viz.timeseries;

import gov.noaa.nws.ncep.viz.localization.NcPathManager;
import gov.noaa.nws.ncep.viz.timeseries.rsc.GeoMagResourceData;

import java.io.File;
import java.util.Calendar;
import java.util.HashMap;
import java.util.Map;
import java.util.TimeZone;

import org.eclipse.jface.dialogs.IDialogConstants;
import org.eclipse.swt.SWT;
import org.eclipse.swt.events.SelectionEvent;
import org.eclipse.swt.events.SelectionListener;
import org.eclipse.swt.layout.GridData;
import org.eclipse.swt.layout.GridLayout;
import org.eclipse.swt.layout.RowLayout;
import org.eclipse.swt.widgets.Button;
import org.eclipse.swt.widgets.Combo;
import org.eclipse.swt.widgets.Composite;
import org.eclipse.swt.widgets.Control;
import org.eclipse.swt.widgets.DateTime;
import org.eclipse.swt.widgets.Label;
import org.eclipse.swt.widgets.Shell;

import com.raytheon.uf.common.status.IUFStatusHandler;
import com.raytheon.uf.common.status.UFStatus;
import com.raytheon.uf.common.status.UFStatus.Priority;
import com.raytheon.uf.common.time.DataTime;
import com.raytheon.uf.viz.core.DescriptorMap;
import com.raytheon.uf.viz.core.drawables.IDescriptor;
import com.raytheon.uf.viz.core.drawables.IRenderableDisplay;
import com.raytheon.uf.viz.core.drawables.ResourcePair;
import com.raytheon.uf.viz.core.exception.VizException;
import com.raytheon.uf.viz.core.procedures.Bundle;
import com.raytheon.viz.ui.BundleLoader;
import com.raytheon.viz.ui.UiUtil;
import com.raytheon.viz.ui.dialogs.CaveJFACEDialog;
import com.raytheon.viz.ui.editor.AbstractEditor;

//import org.eclipse.ui.commands.Priority;

/**
 * A dialog to select GeoMag Plot
 * 
 */

public class GeoMagPlotDialog extends CaveJFACEDialog {
    private static final transient IUFStatusHandler statusHandler = UFStatus
            .getHandler(GeoMagPlotDialog.class);

    private static final int LOAD_NEW_ID = IDialogConstants.CLIENT_ID + 3841;

    public static final String STATION = "stationCode";

    public static final String SOURCE = "sourceId";

    private Composite comp;

    private Combo stationCombo;

    private Combo sourceCombo;

    private Combo scaleCombo;

    private Button durations[] = new Button[6];

    private Button fixedButton, updatingButton;

    private DateTime startDate;

    private Combo startHour;

    /**
     * Constructor.
     * 
     * @param parent
     */
    public GeoMagPlotDialog(Shell parent) {
        super(parent);
    }

    /**
     * Sets the title for the dialog.
     * 
     * @see org.eclipse.jface.window.Window#configureShell(org.eclipse.swt.widgets
     *      .Shell)
     */
    @Override
    protected void configureShell(Shell shell) {
        super.configureShell(shell);
        shell.setText("Magnetometer Plots");
    }

    /*
     * (non-Javadoc)
     * 
     * @see
     * com.raytheon.viz.ui.dialogs.CaveJFACEDialog#createDialogArea(org.eclipse
     * .swt.widgets.Composite)
     */
    @Override
    protected Control createDialogArea(Composite parent) {
        comp = (Composite) super.createDialogArea(parent);

        // GridLayout layout = new GridLayout(1, false);
        // comp.setLayout(layout);
        GridLayout mainLayout = new GridLayout(1, true);
        // mainLayout.marginHeight = 1;
        // mainLayout.marginWidth = 1;
        comp.setLayout(mainLayout);

        Label label0 = new Label(comp, SWT.NONE);
        label0.setText("Real-Time Ground-based Magnetometers");
        label0.getFont().getFontData()[0].setStyle(SWT.BOLD);
        GridData data = new GridData(SWT.CENTER, SWT.CENTER, true, false);
        label0.setLayoutData(data);

        Composite row1 = new Composite(comp, SWT.NONE);
        RowLayout rowLayout1 = new RowLayout();
        rowLayout1.center = true;
        row1.setLayout(rowLayout1);

        Label stationLabel = new Label(row1, SWT.NONE);
        stationLabel.setText("Station: ");
        stationCombo = new Combo(row1, SWT.NONE);
        stationCombo.setItems(new String[] { "BOU", "CL2", "CNB", "FRD", "FRN",
                "JEJ", "NGK", "OTT" });
        stationCombo.select(0);

        Label sourceLabel = new Label(row1, SWT.NONE);
        sourceLabel.setText("Data Source: ");
        sourceCombo = new Combo(row1, SWT.NONE);
        sourceCombo.setItems(new String[] { "101", "102", "103" });
        sourceCombo.select(0);

        Label scaleLabel = new Label(row1, SWT.NONE);
        scaleLabel.setText("Scale: ");
        scaleCombo = new Combo(row1, SWT.NONE);
        scaleCombo.setItems(new String[] { "Auto" });
        scaleCombo.select(0);

        Composite row2 = new Composite(comp, SWT.NONE);
        RowLayout rowLayout2 = new RowLayout();
        rowLayout2.center = true;
        row2.setLayout(rowLayout2);

        Label durationLabel = new Label(row2, SWT.NONE);
        durationLabel.setText("Duration: ");

        durations[0] = new Button(row2, SWT.RADIO);
        durations[0].setText("3 hr");
        durations[0].setData(new Integer(3));

        durations[1] = new Button(row2, SWT.RADIO);
        durations[1].setText("6 hr");
        durations[1].setData(new Integer(6));

        durations[2] = new Button(row2, SWT.RADIO);
        durations[2].setText("12 hr");
        durations[2].setData(new Integer(12));
        durations[2].setSelection(true);

        durations[3] = new Button(row2, SWT.RADIO);
        durations[3].setText("24 hr");
        durations[3].setData(new Integer(24));

        durations[4] = new Button(row2, SWT.RADIO);
        durations[4].setText("2 day");
        durations[4].setData(new Integer(48));

        durations[5] = new Button(row2, SWT.RADIO);
        durations[5].setText("3 day");
        durations[5].setData(new Integer(72));

        Composite row3 = new Composite(comp, SWT.NONE);
        RowLayout rowLayout3 = new RowLayout();
        rowLayout3.center = true;
        row3.setLayout(rowLayout3);

        updatingButton = new Button(row3, SWT.RADIO);
        updatingButton.setText("Updating Display");
        updatingButton.setSelection(true);
        updatingButton.addSelectionListener(new SelectionListener() {

            @Override
            public void widgetSelected(SelectionEvent e) {
                startDate.setEnabled(false);
                startHour.setEnabled(false);
            }

            @Override
            public void widgetDefaultSelected(SelectionEvent e) {
            }
        });

        fixedButton = new Button(row3, SWT.RADIO);
        fixedButton.setText("Fixed display beginning");
        fixedButton.addSelectionListener(new SelectionListener() {

            @Override
            public void widgetSelected(SelectionEvent e) {
                startDate.setEnabled(true);
                startHour.setEnabled(true);
            }

            @Override
            public void widgetDefaultSelected(SelectionEvent e) {
            }
        });

        startDate = new DateTime(row3, SWT.DATE | SWT.LONG | SWT.DROP_DOWN
                | SWT.BORDER);
        startDate.setEnabled(false);

        Label hourLabel = new Label(row3, SWT.NONE);
        hourLabel.setText("Hour");

        startHour = new Combo(row3, SWT.NONE);
        startHour.setItems(new String[] { "00", "03", "06", "09", "12", "15",
                "18", "21" });
        startHour.select(0);
        startHour.setEnabled(false);

        return comp;
    }

    /*
     * (non-Javadoc)
     * 
     * @see org.eclipse.jface.dialogs.Dialog#buttonPressed(int)
     */
    @Override
    protected void buttonPressed(int buttonId) {
        if (buttonId == LOAD_NEW_ID) {
            loadPlot();
        } else
            super.buttonPressed(buttonId);
    }

    private void loadPlot() {
        File bundleFile = NcPathManager.getInstance().getStaticFile(
                "ncep/Bundles/GeoMagPlot.xml");

        DataTime startTime = null;
        Map<String, String> vars = new HashMap<String, String>();
        vars.put(STATION, stationCombo.getText());
        vars.put(SOURCE, sourceCombo.getText());
        int duration = getDuration();
        boolean setDate = fixedButton.getSelection();
        if (setDate) {
            startTime = getStartTime();
        }

        Bundle b;
        try {
            b = Bundle.unmarshalBundle(bundleFile, vars);

            IRenderableDisplay renderableDisplay = b.getDisplays()[0];
            IDescriptor bundleDescriptor = renderableDisplay.getDescriptor();
            if (bundleDescriptor instanceof GeoMagDescriptor) {
                GeoMagDescriptor geo = (GeoMagDescriptor) bundleDescriptor;
                for (ResourcePair pair : geo.getSerializableResources()) {
                    if (pair.getResourceData() instanceof GeoMagResourceData) {
                        GeoMagResourceData rscData = (GeoMagResourceData) pair
                                .getResourceData();
                        rscData.setPlotLengthInHours(duration);
                        if (setDate) {
                            rscData.setStartTime(startTime);
                            rscData.setUpdating(false);
                        }
                    }
                }
            }
            String bundleEditorId = DescriptorMap.getEditorId(bundleDescriptor
                    .getClass().getName());
            // AbstractEditor editor = UiUtil.createOrOpenEditor(bundleEditorId,
            AbstractEditor editor = UiUtil.createEditor(bundleEditorId,
                    b.getDisplays());

            BundleLoader.loadTo(editor, b);
        } catch (VizException e) {
            // TODO Auto-generated catch block. Please revise as appropriate.
            statusHandler.handle(Priority.PROBLEM, e.getLocalizedMessage(), e);
        }
    }

    private DataTime getStartTime() {
        Calendar time = Calendar.getInstance(TimeZone.getTimeZone("GMT"));
        time.set(Calendar.YEAR, startDate.getYear());
        time.set(Calendar.MONTH, startDate.getMonth());
        time.set(Calendar.DAY_OF_MONTH, startDate.getDay());
        time.set(Calendar.HOUR_OF_DAY, Integer.parseInt(startHour.getText()));
        time.set(Calendar.MINUTE, 0);
        time.set(Calendar.SECOND, 0);
        time.set(Calendar.MILLISECOND, 0);
        return new DataTime(time);
    }

    private int getDuration() {
        int value = 0;
        for (Button opt : durations) {
            if (opt.getSelection()) {
                value = (Integer) opt.getData();
                break;
            }
        }
        return value;
    }

    /**
     * 
     * @see org.eclipse.jface.dialogs.Dialog#createButtonsForButtonBar(org.eclipse.swt.widgets.Composite)
     */
    @Override
    protected void createButtonsForButtonBar(Composite parent) {
        createButton(parent, LOAD_NEW_ID, "Create Plot", true);
        createButton(parent, IDialogConstants.CANCEL_ID, "Cancel", false);
    }
}
