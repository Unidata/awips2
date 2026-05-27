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
package com.raytheon.uf.viz.monitor.ffmp.ffti;

import java.util.Arrays;
import java.util.HashSet;
import java.util.Set;

import org.eclipse.swt.SWT;
import org.eclipse.swt.events.DisposeEvent;
import org.eclipse.swt.events.DisposeListener;
import org.eclipse.swt.events.SelectionAdapter;
import org.eclipse.swt.events.SelectionEvent;
import org.eclipse.swt.graphics.Font;
import org.eclipse.swt.layout.GridData;
import org.eclipse.swt.layout.GridLayout;
import org.eclipse.swt.widgets.Button;
import org.eclipse.swt.widgets.Combo;
import org.eclipse.swt.widgets.Composite;
import org.eclipse.swt.widgets.Label;
import org.eclipse.swt.widgets.List;
import org.eclipse.swt.widgets.TabFolder;

import com.raytheon.uf.common.monitor.config.FFMPRunConfigurationManager;
import com.raytheon.uf.common.monitor.config.FFMPSourceConfigurationManager;
import com.raytheon.uf.common.monitor.xml.FFTIAttributeXML;
import com.raytheon.uf.common.monitor.xml.FFTISettingXML;
import com.raytheon.uf.common.monitor.xml.ProductRunXML;
import com.raytheon.uf.common.monitor.xml.ProductXML;
import com.raytheon.uf.common.monitor.xml.SourceXML;

/**
 * FFTI Setting Composite.
 *
 * <pre>
 * SOFTWARE HISTORY
 * Date         Ticket#    Engineer    Description
 * ------------ ---------- ----------- --------------------------
 * 08/07/2012   578        mpduff      FFTI now only a single selection and populates
 *                                     correctly.
 * Jun 21, 2018 6641       njensen     Size qpfDurCbo based on char width
 * Jul 30, 2018 6720       njensen     Update for changed method names
 * Aug 31, 2018 6588       dgilling    Support refactored TotalDurScaleComp.
 *
 * </pre>
 */
public class SettingComp extends Composite implements DurationInterface {

    private static final String DUR_HOURS_STR = "Duration (hrs): ";

    /**
     * Parent tab folder.
     */
    private TabFolder parent;

    private Font labelFont;

    private Button accumRdo;

    private Button ratioRdo;

    private Button diffRdo;

    private FFTISliderCanvas sliderCanvas;

    private Label sliderCanvasLabel;

    private ToggleCanvas qpeToggle;

    private ToggleCanvas guidToggle;

    private ToggleCanvas qpfToggle;

    private List qpeList;

    private List guidList;

    private List qpfList;

    private Label qpeDurLbl;

    // duration hour for QPE
    private double qpeDurHr;

    private DurHoursScaleComp guidDurSlider;

    private Combo qpfDurCbo;

    private TotalDurScaleComp totalDurScale;

    // attributes with default values
    private FFTIAttribute accumAttrib = new FFTIAttribute(0.05, 0, 1, 3, 4);

    private FFTIAttribute ratioAttrib = new FFTIAttribute(0.05, 0, 0.5, 0.8,
            1.25);

    private FFTIAttribute diffAttrib = new FFTIAttribute(0.1, -3, 1, 2.5, 3.5);

    // temporary storage for qpf
    private String selectedQpfVal = "0";

    private FFTISettingXML fftiSetting;

    public SettingComp(TabFolder parent) {
        super(parent, 0);

        this.parent = parent;

        init();
    }

    public SettingComp(TabFolder parent, FFTISettingXML fftiSetting) {
        super(parent, 0);

        this.parent = parent;
        this.fftiSetting = fftiSetting;

        init();

        // set the attributes
        FFTIAttributeXML attr = fftiSetting.getAttribute();
        if ("accum".equalsIgnoreCase(attr.getAttributeName())) {
            accumAttrib.setRedThreshold(attr.getRedThrshld());
            accumAttrib.setYellowThreshold(attr.getYellowThrshld());
            accumRdo.setSelection(true);
            ratioRdo.setSelection(false);
            diffRdo.setSelection(false);
            accumAction(accumAttrib);
        } else if ("ratio".equalsIgnoreCase(attr.getAttributeName())) {
            ratioAttrib.setRedThreshold(attr.getRedThrshld());
            ratioAttrib.setYellowThreshold(attr.getYellowThrshld());
            ratioRdo.setSelection(true);
            accumRdo.setSelection(false);
            diffRdo.setSelection(false);
            ratioAction(ratioAttrib);
        } else {
            // must be diff
            diffAttrib.setRedThreshold(attr.getRedThrshld());
            diffAttrib.setYellowThreshold(attr.getYellowThrshld());
            diffRdo.setSelection(true);
            accumRdo.setSelection(false);
            ratioRdo.setSelection(false);
            diffAction(diffAttrib);
        }

        // set qpe
        setQPEDurHr(fftiSetting.getQpeSource().getDurationHour());

        // set guid
        guidToggle.setToggleState(
                fftiSetting.getGuidSource().getDurationHour() != 0);
        guidDurSlider
                .setTimeDuration(fftiSetting.getGuidSource().getDurationHour());

        // set qpf
        double qpfVal = fftiSetting.getQpfSource().getDurationHour();
        if (qpfVal == -999.0) {
            qpfVal = 0.0;
        }
        this.qpfToggle.setToggleState(qpfVal != 0);

        for (int i = 0; i < qpfDurCbo.getItemCount(); i++) {
            double qpfCboDur = Double.parseDouble(qpfDurCbo.getItem(i));
            if (qpfCboDur == fftiSetting.getQpfSource().getDurationHour()) {
                qpfDurCbo.select(i);
                selectedQpfVal = qpfDurCbo.getItem(i);
            }
        }

        // the total duration slider
        double totalDur = 0;
        if (fftiSetting.getQpeSource().getDurationHour() > 0) {
            totalDur = fftiSetting.getQpeSource().getDurationHour();
        }
        if (fftiSetting.getQpfSource().getDurationHour() > 0) {
            totalDur += fftiSetting.getQpfSource().getDurationHour();
        }

        totalDurScale.setTimeDuration(totalDur);
        updateAccumAttrib(totalDur);
        if (qpfToggle.getToggleState()) {
            totalDurScale.setScaleToYellow();
        } else {
            totalDurScale.setScaleToGrey();
        }
    }

    private void init() {
        labelFont = new Font(this.getDisplay(), "Sans", 10, SWT.BOLD);

        GridData gd = new GridData(SWT.FILL, SWT.FILL, true, true);
        GridLayout gl = new GridLayout(1, false);
        gl.verticalSpacing = 2;
        gl.marginHeight = 2;
        gl.marginWidth = 2;
        this.setLayout(gl);
        this.setLayoutData(gd);

        this.addDisposeListener(new DisposeListener() {
            @Override
            public void widgetDisposed(DisposeEvent arg0) {
                labelFont.dispose();
            }
        });

        /*
         * Add the controls to the display.
         */
        createAttributeControls();

        createPrecipitationControls();

        createTotalDurationControls();

        accumRdo.setEnabled(true);
        accumAction(accumAttrib);

        setSettings();
    }

    private void createAttributeControls() {
        createHeaderLabel("Attribute that will be monitored");

        GridData gd = new GridData(SWT.FILL, SWT.DEFAULT, true, false);
        GridLayout gl = new GridLayout(3, false);
        gl.horizontalSpacing = 20;
        Composite attrComp = new Composite(this, SWT.NONE);
        attrComp.setLayout(gl);
        attrComp.setLayoutData(gd);

        /*
         * Radio buttons
         */
        gd = new GridData();
        gd.horizontalIndent = 10;
        Composite radioComp = new Composite(attrComp, SWT.NONE);
        GridLayout radGl = new GridLayout(1, false);
        radGl.verticalSpacing = 1;
        radioComp.setLayout(radGl);
        radioComp.setLayoutData(gd);

        accumRdo = new Button(radioComp, SWT.RADIO);
        accumRdo.setText("Accum");
        accumRdo.setSelection(true);
        accumRdo.addSelectionListener(new SelectionAdapter() {
            @Override
            public void widgetSelected(SelectionEvent e) {
                accumAction(accumAttrib);
            }
        });

        ratioRdo = new Button(radioComp, SWT.RADIO);
        ratioRdo.setText("Ratio");
        ratioRdo.addSelectionListener(new SelectionAdapter() {
            @Override
            public void widgetSelected(SelectionEvent e) {
                ratioAction(ratioAttrib);
            }
        });

        diffRdo = new Button(radioComp, SWT.RADIO);
        diffRdo.setText("Diff");
        diffRdo.addSelectionListener(new SelectionAdapter() {
            @Override
            public void widgetSelected(SelectionEvent e) {
                diffAction(diffAttrib);
            }
        });

        /*
         * Slider control and label
         */
        sliderCanvas = new FFTISliderCanvas(attrComp, accumAttrib);

        gd = new GridData(SWT.DEFAULT, SWT.CENTER, false, true);
        gd.widthHint = 60;
        sliderCanvasLabel = new Label(attrComp, SWT.NONE);
        sliderCanvasLabel.setText("inches");
        sliderCanvasLabel.setLayoutData(gd);
    }

    private void createPrecipitationControls() {
        int listWidth = 225;
        int listHeight = 125;

        createHeaderLabel("Precipitation Sources");

        GridData gd = new GridData(SWT.FILL, SWT.DEFAULT, true, false);
        GridLayout gl = new GridLayout(5, false);
        gl.horizontalSpacing = 20;
        Composite precipSrcComp = new Composite(this, SWT.NONE);
        precipSrcComp.setLayout(gl);
        precipSrcComp.setLayoutData(gd);

        /*
         * Toggles *************************
         */

        // QPE
        qpeToggle = new ToggleCanvas(precipSrcComp, "QPE", true);

        // GUID
        guidToggle = new ToggleCanvas(precipSrcComp, "GUID", true, 2);

        // QPF
        qpfToggle = new ToggleCanvas(precipSrcComp, "QPF", true, 2);
        qpfToggle.setOwner(this);
        qpeToggle.setOwner(this);

        /*
         * List controls *************************
         */

        // QPE
        gd = new GridData(SWT.CENTER, SWT.DEFAULT, true, false);
        gd.widthHint = listWidth;
        gd.heightHint = listHeight;
        qpeList = new List(precipSrcComp, SWT.BORDER | SWT.V_SCROLL);
        qpeList.setLayoutData(gd);
        fillQpeList();

        // GUID
        gd = new GridData(SWT.CENTER, SWT.DEFAULT, true, false);
        gd.horizontalSpan = 2;
        gd.widthHint = listWidth - 75;
        gd.heightHint = listHeight;
        guidList = new List(precipSrcComp, SWT.BORDER | SWT.V_SCROLL);
        guidList.setLayoutData(gd);
        fillGuidList();

        // QPF
        gd = new GridData(SWT.CENTER, SWT.DEFAULT, true, false);
        gd.horizontalSpan = 2;
        gd.widthHint = listWidth;
        gd.heightHint = listHeight;
        qpfList = new List(precipSrcComp, SWT.BORDER | SWT.V_SCROLL);
        qpfList.setLayoutData(gd);
        fillQpfList();

        /*
         * Duration controls **********************
         */

        // QPE
        gd = new GridData(SWT.CENTER, SWT.CENTER, true, true);
        qpeDurLbl = new Label(precipSrcComp, SWT.NONE);
        setQPEDurHr(1);
        qpeDurLbl.setLayoutData(gd);
        qpeDurLbl.setFont(labelFont);

        // GUID
        gd = new GridData();
        Label guidDurLbl = new Label(precipSrcComp, SWT.NONE);
        guidDurLbl.setText(DUR_HOURS_STR);
        guidDurLbl.setLayoutData(gd);
        guidDurLbl.setFont(labelFont);

        guidDurSlider = new DurHoursScaleComp(precipSrcComp);
        guidDurSlider.setOwner(this);

        // QPF
        gd = new GridData();
        Label qpfDurLbl = new Label(precipSrcComp, SWT.NONE);
        qpfDurLbl.setText(DUR_HOURS_STR);
        qpfDurLbl.setLayoutData(gd);
        qpfDurLbl.setFont(labelFont);

        gd = new GridData(SWT.DEFAULT, SWT.DEFAULT);
        qpfDurCbo = new Combo(precipSrcComp, SWT.DROP_DOWN | SWT.READ_ONLY);
        qpfDurCbo.setLayoutData(gd);
        fillqpfDurCbo();
    }

    /**
     * fill in the GUID names for GUID from FFMPSourceConfig.xml
     */
    private void fillGuidList() {
        FFMPSourceConfigurationManager fscm = FFMPSourceConfigurationManager
                .getInstance();
        java.util.List<String> guidSources = fscm.getGuidanceDisplayNames();
        if (guidSources == null) {
            return;
        }

        // use HashSet just to make an unique guidList
        Set<String> guidSet = new HashSet<>();
        for (String sourceName : guidSources) {
            guidSet.add(sourceName);
            guidList.add(sourceName);
        }

        guidList.addSelectionListener(new SelectionAdapter() {

            @Override
            public void widgetSelected(SelectionEvent e) {
                int index = ((List) e.getSource()).getSelectionIndex();
                String source = ((List) e.getSource()).getItem(index);
                FFMPSourceConfigurationManager srcConfigMgr = FFMPSourceConfigurationManager
                        .getInstance();
                SourceXML mysource = srcConfigMgr
                        .getSourceByDisplayName(source);
                guidDurSlider.setTimeDuration(mysource.getDurationHour());
            }
        });
    }

    private void fillQpeList() {
        FFMPRunConfigurationManager runConfigMgr = FFMPRunConfigurationManager
                .getInstance();
        java.util.List<ProductRunXML> products = runConfigMgr.getProducts();

        FFMPSourceConfigurationManager srcConfigMgr = FFMPSourceConfigurationManager
                .getInstance();

        if (products == null) {
            return;
        }

        Set<String> qpeSet = new HashSet<>();

        for (ProductRunXML product : products) {
            for (ProductXML pr : srcConfigMgr.getProducts()) {
                String str = null;
                SourceXML source = srcConfigMgr.getSource(pr.getQpe());
                String displayName = source.getDisplayName();

                if (source.isMosaic()) {
                    if (!qpeSet.contains(product.getProductKey())) {
                        if (!qpeSet.contains(displayName)) {
                            qpeSet.add(displayName);
                            qpeList.add(displayName);
                        }
                        break;
                    }
                } else {
                    if (pr.getPrimarySource()
                            .equals(product.getProductName())) {
                        str = product.getProductKey() + "-"
                                + displayName;
                        if (!qpeSet.contains(product.getProductKey())) {
                            if (!qpeSet.contains(str)) {
                                if (!qpeSet.contains(displayName)) {
                                    qpeSet.add(str);
                                    qpeList.add(str);
                                }
                            }
                        }
                    }
                }
            }
        }
    }

    /**
     * fill in the QPF names for QPF from FFMPSourceConfig.xml
     */
    private void fillQpfList() {
        FFMPRunConfigurationManager runConfigMgr = FFMPRunConfigurationManager
                .getInstance();
        java.util.List<ProductRunXML> products = runConfigMgr.getProducts();

        FFMPSourceConfigurationManager srcConfigMgr = FFMPSourceConfigurationManager
                .getInstance();

        if (products == null) {
            return;
        }

        Set<String> qpfSet = new HashSet<>();

        for (ProductRunXML product : products) {
            for (ProductXML pr : srcConfigMgr.getProducts()) {
                String str = null;
                for (int i = 0; i < pr.getQpfList().size(); i++) {
                    SourceXML source = srcConfigMgr.getSource(pr.getQpf(i));
                    String displayName = source.getDisplayName();
                    if (source.isMosaic()) {
                        if (!qpfSet.contains(product.getProductKey())) {
                            if (!qpfSet.contains(displayName)) {
                                qpfSet.add(displayName);
                                qpfList.add(displayName);
                                break;
                            }
                        }
                    } else {
                        if (pr.getPrimarySource()
                                .equals(product.getProductName())) {
                            str = product.getProductKey() + "-"
                                    + displayName;
                            if (!qpfSet.contains(product.getProductKey())) {
                                if (!qpfSet.contains(str)) {
                                    if (!qpfSet.contains(
                                            displayName)) {
                                        qpfSet.add(str);
                                        qpfList.add(str);
                                        break;
                                    }
                                }
                            }
                        }
                    }
                }
            }
        }
    }

    /**
     * fill in the duration hours for QPF from FFMPSourceConfig.xml
     */
    private void fillqpfDurCbo() {
        FFMPSourceConfigurationManager fscm = FFMPSourceConfigurationManager
                .getInstance();
        java.util.List<String> qpfSourceNames = fscm.getQPFSourceNames();
        if (qpfSourceNames == null) {
            return;
        }

        // HashSet to make a unique List
        Set<String> qpfSet = new HashSet<>();
        for (String sourceName : qpfSourceNames) {
            SourceXML source = fscm.getSource(sourceName);
            String durHour = String.valueOf(source.getDurationHour());
            if (!qpfSet.contains(durHour)) {
                qpfSet.add(durHour);
                qpfDurCbo.add(durHour);
            }
        }
        if (qpfDurCbo.getItemCount() > 0) {
            qpfDurCbo.select(0);
            selectedQpfVal = qpfDurCbo.getItem(0);
        }
    }

    private void createTotalDurationControls() {
        createHeaderLabel("Total Duration across All Sources");

        GridData gd = new GridData(SWT.FILL, SWT.DEFAULT, true, false);
        GridLayout gl = new GridLayout(2, false);
        gl.horizontalSpacing = 10;
        Composite totalDurComp = new Composite(this, SWT.NONE);
        totalDurComp.setLayout(gl);
        totalDurComp.setLayoutData(gd);

        totalDurScale = new TotalDurScaleComp(totalDurComp, this);
        totalDurScale.setTimeDuration(getQpeDurHr() + getQpfDurHr());
        if (qpfToggle.getToggleState()) {
            totalDurScale.setScaleToYellow();
        } else {
            totalDurScale.setScaleToGrey();
        }
        totalDurScale.setLayoutData(
                new GridData(SWT.FILL, SWT.DEFAULT, true, false));

        gd = new GridData(SWT.LEFT, SWT.CENTER, false, true);
        Label hoursLbl = new Label(totalDurComp, SWT.NONE);
        hoursLbl.setText("Hours");
        hoursLbl.setFont(labelFont);
        hoursLbl.setLayoutData(gd);

        updateAccumAttrib(getQpeDurHr() + getQpfDurHr());
    }

    private void createHeaderLabel(String text) {
        GridData gd = new GridData(SWT.FILL, SWT.DEFAULT, true, false);
        GridLayout gl = new GridLayout(1, false);
        gl.marginHeight = 0;
        Composite lblComp = new Composite(this, SWT.NONE);
        lblComp.setLayout(gl);
        lblComp.setLayoutData(gd);
        lblComp.setBackground(
                parent.getDisplay().getSystemColor(SWT.COLOR_DARK_GRAY));

        gd = new GridData(SWT.CENTER, SWT.DEFAULT, true, false);
        Label attrLbl = new Label(lblComp, SWT.NONE);
        attrLbl.setText(text);
        attrLbl.setFont(labelFont);
        attrLbl.setBackground(
                parent.getDisplay().getSystemColor(SWT.COLOR_DARK_GRAY));
        attrLbl.setForeground(
                parent.getDisplay().getSystemColor(SWT.COLOR_WHITE));
        attrLbl.setLayoutData(gd);
    }

    /**
     * Set the dialog to reflect the saved configuration.
     */
    private void setSettings() {
        // Select the configured items, otherwise select the first

        if (this.fftiSetting != null) {
            // QPE
            if (fftiSetting.getQpeSource().getDisplayNameList() == null
                    || fftiSetting.getQpeSource().getDisplayNameList()
                            .isEmpty()) {
                qpeList.setSelection(0);
            } else {
                // Only using the first one in the list to match A1
                java.util.List<String> items = Arrays
                        .asList(qpeList.getItems());
                String name = fftiSetting.getQpeSource().getDisplayNameList()
                        .get(0);
                int idx = items.indexOf(name);
                qpeList.select(idx);
                qpeList.showSelection();
            }

            // GUID
            if (fftiSetting.getGuidSource().getDisplayNameList() == null
                    || fftiSetting.getGuidSource().getDisplayNameList()
                            .isEmpty()) {
                guidList.setSelection(0);
            } else {
                // Only using the first one in the list to match A1
                java.util.List<String> items = Arrays
                        .asList(guidList.getItems());
                String name = fftiSetting.getGuidSource().getDisplayNameList()
                        .get(0);
                int idx = items.indexOf(name);
                guidList.select(idx);
                guidList.showSelection();
            }

            // QPF
            if (fftiSetting.getQpfSource().getDisplayNameList() == null
                    || fftiSetting.getQpfSource().getDisplayNameList()
                            .isEmpty()) {
                qpfList.setSelection(0);
            } else {
                // Only using the first one in the list to match A1
                java.util.List<String> items = Arrays
                        .asList(qpfList.getItems());
                String name = fftiSetting.getQpfSource().getDisplayNameList()
                        .get(0);
                int idx = items.indexOf(name);
                qpfList.select(idx);
                qpfList.showSelection();
            }

        }
    }

    private void accumAction(FFTIAttribute attribVal) {
        // change attribute values
        sliderCanvasLabel.setText("inches");
        this.sliderCanvas.setValues(attribVal);

        // deactivate guid
        guidToggle.setToggleState(false);
        guidDurSlider.setTimeDuration(guidDurSlider.getLowerVal());
        guidDurSlider.enableScale(false);
        guidList.setEnabled(false);
    }

    private void ratioAction(FFTIAttribute attribVal) {
        // change attribute values
        sliderCanvasLabel.setText("x100%");
        this.sliderCanvas.setValues(attribVal);

        // reset guid, qpe and total duartion hour if GUID was off
        if (!guidToggle.getToggleState()) {
            this.updateGuidDurHour(1.0);
            this.updateQPEDurHour(1.0);
            this.updateTotalDurHour(1.0);
        }

        // activate guid
        guidToggle.setToggleState(true);
        guidDurSlider.enableScale(true);
        guidList.setEnabled(true);

    }

    private void diffAction(FFTIAttribute attribVal) {
        // change attribute value
        sliderCanvasLabel.setText("inches");
        this.sliderCanvas.setValues(attribVal);

        if (!guidToggle.getToggleState()) {
            this.updateGuidDurHour(1.0);
            this.updateQPEDurHour(1.0);
            this.updateTotalDurHour(1.0);
        }

        // activate guid
        guidToggle.setToggleState(true);
        guidDurSlider.enableScale(true);
        guidList.setEnabled(true);
    }

    public ToggleCanvas getQpfToggle() {
        return qpfToggle;
    }

    public ToggleCanvas getGuidToggle() {
        return guidToggle;
    }

    public ToggleCanvas getqpeToggle() {
        return qpeToggle;
    }

    public void setQpeToggleState(boolean state) {
        qpeToggle.setToggleState(state);
    }

    public void qpeSrcToggled(boolean qpeState) {
        if (this.isAccumRdoSelected() && qpeState) {
            // toggle the QPE src
            qpeToggle.setToggleState(!qpeState);

            // duration value to 0.0
            setQPEDurHr(0.0);

            // update the total duration
            if (this.qpfToggle.getToggleState()) {
                double durHr = Double.parseDouble(
                        qpfDurCbo.getItem(qpfDurCbo.getSelectionIndex()));
                updateTotalDurHour(durHr);
            } else {
                updateTotalDurHour(0.0);
            }
        }
    }

    public void qpfSrcToggled(boolean qpfState) {
        // update the QPF duration hour combo
        int index = qpfDurCbo.getSelectionIndex();
        if (qpfState) {
            // change back to original value
            qpfDurCbo.setItem(index, selectedQpfVal);
        } else {
            // change selected value to '0'
            selectedQpfVal = qpfDurCbo.getItem(index);
            qpfDurCbo.setItem(index, "0");
        }
        qpfDurCbo.select(index);

        // update total slider
        if (isAccumRdoSelected()) {
            double totalDurHr = getQpeDurHr() + getQpfDurHr();
            updateTotalDurHour(totalDurHr);
            updateAccumAttrib(totalDurHr);
        } else {
            // update QPE
            updateQPEDurHour(getTotalDurHr());
        }
        if (qpfState) {
            totalDurScale.setScaleToYellow();
            double qpfDurHr = Double.parseDouble(
                    qpfDurCbo.getItem(qpfDurCbo.getSelectionIndex()));
            if (getTotalDurHr() < qpfDurHr) {
                updateTotalDurHour(qpfDurHr);
            }
        } else {
            totalDurScale.setScaleToGrey();
        }
    }

    public boolean isAccumRdoSelected() {
        return accumRdo.getSelection();
    }

    public String getSelectedAttribType() {
        if (accumRdo.getSelection()) {
            return "Accum";
        } else if (this.ratioRdo.getSelection()) {
            return "Ratio";
        }

        return "Diff";
    }

    public double getYellowThreshold() {
        return sliderCanvas.getLowerValue();
    }

    public double getRedThreshold() {
        return sliderCanvas.getUpperValue();
    }

    /**
     * update QPE duration hour
     */
    @Override
    public void updateQPEDurHour(double durHour) {
        if (this.qpfToggle.getToggleState()) {
            double qpfDurHr = Double.parseDouble(
                    this.qpfDurCbo.getItem(qpfDurCbo.getSelectionIndex()));
            durHour = durHour - qpfDurHr;
        }

        setQPEDurHr(durHour);
    }

    /**
     * update QPE duration hour
     */
    public void setQPEDurHr(double durHour) {
        qpeDurHr = durHour;
        if (durHour < 0.0) {
            qpeDurHr = 0;
        }

        qpeToggle.setToggleState(qpeDurHr != 0);

        this.qpeDurLbl
                .setText(DUR_HOURS_STR + String.format("%2.2f", qpeDurHr));
        qpeDurLbl.setSize(160, 20);
    }

    /**
     * update total duration hour
     */
    @Override
    public void updateTotalDurHour(double guidDurHour) {
        double totalDurHr = guidDurHour;
        double qpfDurHr = Double.parseDouble(
                this.qpfDurCbo.getItem(qpfDurCbo.getSelectionIndex()));
        if (totalDurHr < qpfDurHr) {
            totalDurHr = qpfDurHr;
        }

        if (totalDurHr < guidDurSlider.getLowerVal()) {
            totalDurHr = guidDurSlider.getLowerVal();
        }

        if (totalDurHr > guidDurSlider.getUpperVal()) {
            totalDurHr = guidDurSlider.getUpperVal();
        }

        this.totalDurScale.setTimeDuration(guidDurHour);
        updateAccumAttrib(guidDurHour);
    }

    /**
     * update GUID duration hour (from total scale)
     */
    @Override
    public void updateGuidDurHour(double durHour) {
        if (!this.guidDurSlider.getEnabled()) {
            return;
        }

        double totalDurHr = durHour;
        if (totalDurHr < guidDurSlider.getLowerVal()) {
            totalDurHr = guidDurSlider.getLowerVal();
        }

        if (totalDurHr > guidDurSlider.getUpperVal()) {
            totalDurHr = guidDurSlider.getUpperVal();
        }

        this.guidDurSlider.setTimeDuration(totalDurHr);
    }

    @Override
    public boolean getGuidEnabled() {
        return this.guidDurSlider.getEnabled();
    }

    public String[] getQpfSrc() {
        return qpfList.getSelection();
    }

    public double getQpfDurHr() {
        if (!this.qpfToggle.getToggleState()) {
            return 0;
        }

        return Double
                .parseDouble(qpfDurCbo.getItem(qpfDurCbo.getSelectionIndex()));
    }

    public String[] getGuidSrc() {
        return guidList.getSelection();
    }

    public double getGuidDurHr() {
        if (!this.guidToggle.getToggleState()) {
            return 0.0;
        }

        return this.guidDurSlider.getSelectedValue();
    }

    public String[] getQpeSrc() {
        return qpeList.getSelection();
    }

    public double getQpeDurHr() {
        if (!this.qpeToggle.getToggleState()) {
            return 0.0;
        }

        return qpeDurHr;
    }

    @Override
    public void updateAccumAttrib(double totalDurHr) {
        double accumMax = 1.0;
        if (totalDurHr <= 0.25) {
            accumMax = 1.0;
            accumAttrib.setInc(0.05);
        } else if (totalDurHr <= 4) {
            accumMax = totalDurHr * 4;
            accumAttrib.setInc(0.05);
        } else if (totalDurHr <= 12) {
            accumMax = totalDurHr * 3;
            accumAttrib.setInc(0.1);
        } else {
            accumMax = totalDurHr * 2;
            accumAttrib.setInc(0.25);
        }
        this.accumAttrib.setMax(accumMax);

        if (this.isAccumRdoSelected()) {
            this.accumAction(accumAttrib);
        }
    }

    @Override
    public double getTotalDurHr() {
        return totalDurScale.getDoubleValue();
    }

    @Override
    public double adjustTotalDurationHr(double durHr) {
        double totalDurHr = durHr;
        if (!isAccumRdoSelected()) {
            if (totalDurHr > guidDurSlider.getUpperVal()) {
                totalDurHr = guidDurSlider.getUpperVal();
            }
        }

        if (totalDurHr < getQpfDurHr()) {
            totalDurHr = getQpfDurHr();
        }

        return totalDurHr;
    }
}