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

import java.util.Arrays;
import java.util.Collections;

import org.eclipse.swt.SWT;
import org.eclipse.swt.events.SelectionAdapter;
import org.eclipse.swt.events.SelectionEvent;
import org.eclipse.swt.layout.GridData;
import org.eclipse.swt.layout.GridLayout;
import org.eclipse.swt.widgets.Button;
import org.eclipse.swt.widgets.Combo;
import org.eclipse.swt.widgets.Composite;
import org.eclipse.swt.widgets.Label;
import org.eclipse.swt.widgets.Layout;
import org.eclipse.swt.widgets.Shell;

import com.raytheon.viz.aviation.climatology.WeatherPlotDataManager;
import com.raytheon.viz.aviation.resource.ResourceConfigMgr;
import com.raytheon.viz.aviation.xml.PlotViewerCfg;
import com.raytheon.viz.aviation.xml.WxPlotCfg;
import com.raytheon.viz.ui.dialogs.CaveSWTDialog;

/**
 * TimeSelectorDialog class displays the Time Selector dialog for AvnFPS.
 * 
 * <pre>
 * SOFTWARE HISTORY
 * Date         Ticket#    Engineer    Description
 * ------------ ---------- ----------- --------------------------
 * 28 FEB 2008  938        lvenable    Initial creation.
 * 3/27/2008    1033       grichard    Added ETA-MOS label.
 * 10/10/2012   1229       rferrel     Make dialog non-blocking.
 * 
 * </pre>
 * 
 * @author lvenable
 * @version 1.0
 * 
 */
public class TimeSelectorDialog extends CaveSWTDialog {

    /**
     * TAF combo control.
     */
    private Combo tafCbo;

    /**
     * GFS-MOS combo control.
     */
    private Combo gfsmosCbo;

    /**
     * GFS-LAMP combo control.
     */
    private Combo gfslampCbo;

    /**
     * ETNAM-MOS combo control.
     */
    private Combo nammosCbo;

    /**
     * NAM-WRF combo control.
     */
    private Combo namWrfCbo;

    private Composite mainComp;

    private WxPlotCfg wxPlotCfg;

    /**
     * Constructor.
     * 
     * @param parent
     *            Parent Shell.
     */
    public TimeSelectorDialog(Shell parent, WxPlotCfg wxPlotCfg) {
        super(parent, SWT.DIALOG_TRIM | SWT.RESIZE,
                CAVE.PERSPECTIVE_INDEPENDENT | CAVE.MODE_INDEPENDENT
                        | CAVE.DO_NOT_BLOCK);
        setText("AvnFPS Time Selector");
        this.wxPlotCfg = wxPlotCfg;
    }

    @Override
    protected Layout constructShellLayout() {
        // Create the main layout for the shell.
        GridLayout gl = new GridLayout(1, false);
        gl.marginHeight = 0;
        gl.marginWidth = 0;
        gl.verticalSpacing = 0;
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

        createTimeControls(configMgr);
        addSeparator(configMgr);
        createBottomButtons(configMgr);
        populateLists();
    }

    /**
     * Create the time controls.
     */
    private void createTimeControls(ResourceConfigMgr configMgr) {
        Composite timeComp = new Composite(mainComp, SWT.NONE);
        timeComp.setLayout(new GridLayout(2, false));
        configMgr.setDefaultColors(timeComp);

        Label tafLbl = new Label(timeComp, SWT.NONE);
        tafLbl.setText("TAF");
        configMgr.setDefaultFontAndColors(tafLbl);

        tafCbo = new Combo(timeComp, SWT.DROP_DOWN | SWT.READ_ONLY);
        configMgr.setDefaultFont(tafCbo);

        Label gfsmosLbl = new Label(timeComp, SWT.NONE);
        gfsmosLbl.setText(getPlotLabel(PlotViewerCfg.ClassNames.GFS_MOS));
        configMgr.setDefaultFontAndColors(gfsmosLbl);

        gfsmosCbo = new Combo(timeComp, SWT.DROP_DOWN | SWT.READ_ONLY);
        configMgr.setDefaultFont(gfsmosCbo);

        Label gfslampLbl = new Label(timeComp, SWT.NONE);
        gfslampLbl.setText(getPlotLabel(PlotViewerCfg.ClassNames.GFSLAMP));
        configMgr.setDefaultFontAndColors(gfslampLbl);

        gfslampCbo = new Combo(timeComp, SWT.DROP_DOWN | SWT.READ_ONLY);
        configMgr.setDefaultFont(gfslampCbo);

        Label etamosLbl = new Label(timeComp, SWT.NONE);
        etamosLbl.setText(getPlotLabel(PlotViewerCfg.ClassNames.NAM_MOS));
        configMgr.setDefaultFontAndColors(etamosLbl);

        nammosCbo = new Combo(timeComp, SWT.DROP_DOWN | SWT.READ_ONLY);
        configMgr.setDefaultFont(nammosCbo);

        Label namWrfLbl = new Label(timeComp, SWT.NONE);
        namWrfLbl.setText(getPlotLabel(PlotViewerCfg.ClassNames.NAM_WRF));
        configMgr.setDefaultFontAndColors(namWrfLbl);

        namWrfCbo = new Combo(timeComp, SWT.DROP_DOWN | SWT.READ_ONLY);
        configMgr.setDefaultFont(namWrfCbo);
    }

    private String getPlotLabel(PlotViewerCfg.ClassNames className) {
        String name = className.getName();
        for (PlotViewerCfg plotViewer : wxPlotCfg.getPlotViewers()) {
            if (name.endsWith(plotViewer.getClassName())) {
                return plotViewer.getLabelName();
            }
        }
        return "Unknown";
    }

    /**
     * Add a horizontal separator to the display.
     */
    private void addSeparator(ResourceConfigMgr configMgr) {
        GridData gd = new GridData(GridData.FILL_HORIZONTAL);
        gd.horizontalSpan = 4;
        Label sepLbl = new Label(mainComp, SWT.SEPARATOR | SWT.HORIZONTAL);
        sepLbl.setLayoutData(gd);
        configMgr.setDefaultColors(sepLbl);
    }

    /**
     * Create the Reset and Close buttons at the bottom of the display.
     */
    private void createBottomButtons(ResourceConfigMgr configMgr) {
        Composite buttonComp = new Composite(mainComp, SWT.NONE);
        GridData gd = new GridData(SWT.CENTER, SWT.DEFAULT, true, false);
        buttonComp.setLayout(new GridLayout(2, false));
        buttonComp.setLayoutData(gd);
        configMgr.setDefaultColors(buttonComp);

        gd = new GridData(80, SWT.DEFAULT);
        Button resetBtn = new Button(buttonComp, SWT.PUSH);
        configMgr.setDefaultFontAndColors(resetBtn, "Reset", gd);
        resetBtn.addSelectionListener(new SelectionAdapter() {
            @Override
            public void widgetSelected(SelectionEvent event) {
                performReset();
            }
        });

        gd = new GridData(80, SWT.DEFAULT);
        Button closeBtn = new Button(buttonComp, SWT.PUSH);
        configMgr.setDefaultFontAndColors(closeBtn, "Close", gd);
        closeBtn.addSelectionListener(new SelectionAdapter() {
            @Override
            public void widgetSelected(SelectionEvent event) {
                updateSelections();
                setReturnValue(true);
                shell.dispose();
            }
        });
    }

    private void performReset() {
        if (tafCbo.getItemCount() > 0) {
            tafCbo.select(0);
        }

        if (gfsmosCbo.getItemCount() > 0) {
            gfsmosCbo.select(0);
        }

        if (gfslampCbo.getItemCount() > 0) {
            gfslampCbo.select(0);
        }

        if (nammosCbo.getItemCount() > 0) {
            nammosCbo.select(0);
        }

        if (namWrfCbo.getItemCount() > 0) {
            namWrfCbo.select(0);
        }
    }

    private void populateLists() {
        WeatherPlotDataManager dataMgr = WeatherPlotDataManager.getInstance();
        int comboWidth = 200;

        String[] keys = dataMgr.getTafHeaders();
        for (String key : keys) {
            tafCbo.add(key);
        }
        if (comboWidth < tafCbo.computeSize(SWT.DEFAULT, SWT.DEFAULT).x) {
            comboWidth = tafCbo.computeSize(SWT.DEFAULT, SWT.DEFAULT).x;
        }

        tafCbo.select(tafCbo.indexOf(dataMgr.getSelectedTaf()));

        keys = dataMgr.getNamMosTimes();
        if (keys.length > 0) {
            for (String key : keys) {
                nammosCbo.add(key);
            }
            nammosCbo.select(nammosCbo.indexOf(dataMgr.getSelectedNamMos()));
            if (comboWidth < nammosCbo.computeSize(SWT.DEFAULT, SWT.DEFAULT).x) {
                comboWidth = nammosCbo.computeSize(SWT.DEFAULT, SWT.DEFAULT).x;
            }
        }

        keys = dataMgr.getGfsMosTimes();
        if (keys.length > 0) {
            for (String key : keys) {
                gfsmosCbo.add(key);
            }
            gfsmosCbo.select(gfsmosCbo.indexOf(dataMgr.getSelectedGfsMos()));
            if (comboWidth < gfsmosCbo.computeSize(SWT.DEFAULT, SWT.DEFAULT).x) {
                comboWidth = gfsmosCbo.computeSize(SWT.DEFAULT, SWT.DEFAULT).x;
            }
        }

        keys = dataMgr.getGfsLampTimes();
        if (keys.length > 0) {
            Arrays.sort(keys, Collections.reverseOrder());
            for (String key : keys) {
                gfslampCbo.add(key);
            }
            gfslampCbo.select(gfslampCbo.indexOf(dataMgr.getSelectedGfsLamp()));
            if (comboWidth < gfslampCbo.computeSize(SWT.DEFAULT, SWT.DEFAULT).x) {
                comboWidth = gfslampCbo.computeSize(SWT.DEFAULT, SWT.DEFAULT).x;
            }
        }

        keys = dataMgr.getNamWrfTimes();
        if (keys.length > 0) {
            for (String key : keys) {
                namWrfCbo.add(key);
            }
            namWrfCbo.select(namWrfCbo.indexOf(dataMgr.getselectedNamWrf()));
            if (comboWidth < namWrfCbo.computeSize(SWT.DEFAULT, SWT.DEFAULT).x) {
                comboWidth = namWrfCbo.computeSize(SWT.DEFAULT, SWT.DEFAULT).x;
            }
        }

        tafCbo.setLayoutData(new GridData(comboWidth, SWT.DEFAULT));
        gfsmosCbo.setLayoutData(new GridData(comboWidth, SWT.DEFAULT));
        gfslampCbo.setLayoutData(new GridData(comboWidth, SWT.DEFAULT));
        nammosCbo.setLayoutData(new GridData(comboWidth, SWT.DEFAULT));
        namWrfCbo.setLayoutData(new GridData(comboWidth, SWT.DEFAULT));
    }

    private void updateSelections() {
        WeatherPlotDataManager dataMgr = WeatherPlotDataManager.getInstance();
        String taf = "";
        String namMos = null;
        String gfsMos = null;
        String gfsLamp = null;
        String namWrf = null;

        if (tafCbo.getSelectionIndex() >= 0) {
            taf = tafCbo.getItem(tafCbo.getSelectionIndex());
        }

        if (nammosCbo.getSelectionIndex() >= 0) {
            namMos = nammosCbo.getItem(nammosCbo.getSelectionIndex());
        }

        if (gfsmosCbo.getSelectionIndex() >= 0) {
            gfsMos = gfsmosCbo.getItem(gfsmosCbo.getSelectionIndex());
        }

        if (gfslampCbo.getSelectionIndex() >= 0) {
            gfsLamp = gfslampCbo.getItem(gfslampCbo.getSelectionIndex());
        }

        if (namWrfCbo.getSelectionIndex() >= 0) {
            namWrf = namWrfCbo.getItem(namWrfCbo.getSelectionIndex());
        }

        dataMgr.updateSelections(taf, namMos, gfsMos, gfsLamp, namWrf);
    }
}
