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
package com.raytheon.viz.gfe.dialogs;

import java.util.ArrayList;
import java.util.List;

import org.eclipse.swt.SWT;
import org.eclipse.swt.events.SelectionAdapter;
import org.eclipse.swt.events.SelectionEvent;
import org.eclipse.swt.graphics.Point;
import org.eclipse.swt.layout.GridData;
import org.eclipse.swt.layout.GridLayout;
import org.eclipse.swt.widgets.Button;
import org.eclipse.swt.widgets.Combo;
import org.eclipse.swt.widgets.Composite;
import org.eclipse.swt.widgets.Label;
import org.eclipse.swt.widgets.Text;

import com.raytheon.uf.common.dataplugin.gfe.weather.WeatherKey;
import com.raytheon.uf.common.dataplugin.gfe.weather.WeatherSubKey;
import com.raytheon.uf.common.dataplugin.gfe.weather.WxDefinition;
import com.raytheon.uf.viz.core.VizApp;
import com.raytheon.viz.gfe.Activator;
import com.raytheon.viz.gfe.PythonPreferenceStore;
import com.raytheon.viz.gfe.core.msgs.ICombineModeChangedListener;
import com.raytheon.viz.gfe.core.parm.Parm;
import com.raytheon.viz.gfe.core.parm.ParmState.CombineMode;
import com.raytheon.viz.gfe.core.wxvalue.WeatherWxValue;
import com.raytheon.viz.gfe.core.wxvalue.WxValue;

/**
 * Provides UI to set a weather wx value for the set value dialog
 * 
 * <pre>
 * 
 * SOFTWARE HISTORY
 * Date         Ticket#    Engineer    Description
 * ------------ ---------- ----------- --------------------------
 * Jun 18, 2009 #1318           randerso     Initial creation
 * 
 * </pre>
 * 
 * @author randerso
 * @version 1.0
 */

public class WxSetValue extends AbstractSetValue implements
        ICombineModeChangedListener {

    private WeatherWxValue weatherValue;

    private boolean showDescription;

    private Composite topFrame;

    private Text label;

    private Button combineButton;

    private Composite subKeyFrame;

    private ArrayList<SubKey> subKeyUI;

    private boolean setPickUpValueEachTime;

    /**
     * Constructor
     * 
     * @param parent
     *            composite to contain the controls
     * @param parm
     *            the parm to be acted on
     */
    public WxSetValue(Composite parent, Parm parm, boolean showCombine,
            boolean showAddToSession, boolean setPickupValueEachTime) {
        super(parent, parm);
        GridLayout layout = (GridLayout) getLayout();
        layout.marginWidth = 0;
        layout.marginHeight = 0;
        layout.verticalSpacing = 0;

        this.setPickUpValueEachTime = setPickupValueEachTime;

        parm.getListeners().addCombineModeChangedListener(this);

        WeatherWxValue pickupValue = (WeatherWxValue) parm.getParmState()
                .getPickUpValue();
        WeatherKey key = new WeatherKey(pickupValue.getWeatherKey());
        weatherValue = new WeatherWxValue(key, pickupValue.getParm());

        showDescription = dataManager.getSpatialDisplayManager()
                .getShowDescription();

        // create the top frame containing the current value and an ADD button
        topFrame = new Composite(this, SWT.NONE);
        GridData layoutData = new GridData(SWT.FILL, SWT.FILL, true, true);
        topFrame.setLayoutData(layoutData);
        layout = new GridLayout(3, false);
        layout.marginWidth = 0;
        topFrame.setLayout(layout);

        label = new Text(topFrame, SWT.BORDER | SWT.READ_ONLY);
        layoutData = new GridData(SWT.FILL, SWT.CENTER, true, false);
        label.setLayoutData(layoutData);

        Button addButton = new Button(topFrame, SWT.PUSH);
        addButton.setText("Add");
        addButton.addSelectionListener(new SelectionAdapter() {
            @Override
            public void widgetSelected(SelectionEvent e) {
                addSubKey();
            }
        });

        Button resetButton = new Button(topFrame, SWT.PUSH);
        resetButton.setText("Reset");
        resetButton.addSelectionListener(new SelectionAdapter() {
            @Override
            public void widgetSelected(SelectionEvent e) {
                reset();
            }
        });

        // create the frame around all subkeys uis
        Composite frame = new Composite(this, SWT.NONE);
        layoutData = new GridData(SWT.FILL, SWT.DEFAULT, true, false);
        frame.setLayoutData(layoutData);
        layout = new GridLayout(2, false);
        layout.marginWidth = 0;
        layout.marginHeight = 0;
        layout.horizontalSpacing = 0;
        frame.setLayout(layout);
        subKeyUI = new ArrayList<SubKey>();

        // label the entries in the sub key frame
        createSubKeyLabels(frame);

        subKeyFrame = new Composite(frame, SWT.NONE);
        layoutData = new GridData(SWT.FILL, SWT.DEFAULT, true, false);
        subKeyFrame.setLayoutData(layoutData);
        layout = new GridLayout(0, false);
        layout.horizontalSpacing = 0;
        layout.marginWidth = 0;
        layout.marginHeight = 0;
        subKeyFrame.setLayout(layout);

        // create the bottom frame, which contains the combine button and
        // the Set PickUp Value button
        Composite bottomFrame = new Composite(this, SWT.NONE);
        layoutData = new GridData(SWT.CENTER, SWT.DEFAULT, false, false);
        bottomFrame.setLayoutData(layoutData);
        layout = new GridLayout(2, false);
        layout.marginHeight = 0;
        layout.marginWidth = 0;
        bottomFrame.setLayout(layout);

        CombineMode combineModeVar = parm.getParmState().getCombineMode();

        if (showCombine) {
            combineButton = new Button(bottomFrame, SWT.CHECK);
            combineButton.setText("Combine");
            combineButton.setSelection(combineModeVar == CombineMode.COMBINE);
            combineButton.addSelectionListener(new SelectionAdapter() {
                @Override
                public void widgetSelected(SelectionEvent e) {
                    cModeChanged();
                }
            });
        }

        if (showAddToSession) {
            Button svb = new Button(bottomFrame, SWT.PUSH);
            svb.setText("Add to session");
            svb.addSelectionListener(new SelectionAdapter() {
                @Override
                public void widgetSelected(SelectionEvent e) {
                    addToSession();
                }
            });
        }

        // set initial weather key
        setInitialDisplay();

    }

    /*
     * (non-Javadoc)
     * 
     * @see org.eclipse.swt.widgets.Widget#dispose()
     */
    @Override
    public void dispose() {
        parm.getListeners().removeCombineModeChangedListener(this);
        super.dispose();
    }

    protected void cModeChanged() {
        // callback when weather mode changes from button
        CombineMode state = CombineMode.REPLACE;
        if (combineButton.getSelection()) {
            state = CombineMode.COMBINE;
        }

        // LogStream.logUse("Combine change: ", self._combineModeVar.get())
        dataManager.getParmOp().setCombineMode(state);
    }

    /*
     * (non-Javadoc)
     * 
     * @see
     * com.raytheon.viz.gfe.core.msgs.ICombineModeChangedListener#combineModeChanged
     * (com.raytheon.viz.gfe.core.parm.Parm,
     * com.raytheon.viz.gfe.core.parm.ParmState.CombineMode)
     */
    @Override
    public void combineModeChanged(Parm parm, final CombineMode mode) {
        // Parm Client notification
        VizApp.runAsync(new Runnable() {

            @Override
            public void run() {
                combineButton.setSelection(mode.equals(CombineMode.COMBINE));
            }
        });
    }

    protected void addToSession() {
        // LogStream.logUse("Add to session")
        parm.getParmState().addSessionPickupValue(weatherValue);
    }

    protected void reset() {
        // LogStream.logUse("Reset")
        for (SubKey key : subKeyUI) {
            key.dispose();
        }
        subKeyUI.clear();

        weatherValue = new WeatherWxValue(new WeatherKey(siteId,
                "<NoCov>:<NoWx>:<NoInten>:<NoVis>:"), parm);

        if (setPickUpValueEachTime) {
            setWxPickup();
        }
        setInitialDisplay();

        subKeyFrame.layout();
        subKeyFrame.pack(true);
        getShell().pack(true);
    }

    /**
     * Set the pickupValue of the associated parm to the WeatherWxValue defined
     * by this control
     */
    public void setWxPickup() {
        if (parm != null) {
            parm.getParmState().setPickUpValue(weatherValue);
        }
    }

    /**
     * @return the DiscreteWxValue defined by this UI
     */
    public WxValue getWxPickup() {
        return new WeatherWxValue(weatherValue.getWeatherKey(), parm);
    }

    protected void addSubKey() {
        // check to see if the last entry is a none type
        WeatherSubKey addedKey;
        if (subKeyUI.size() == 0) {
            addedKey = new WeatherSubKey(siteId,
                    "<NoCov>:<NoWx>:<NoInten>:<NoVis>:<NoAttr>");
        } else if (!subKeyUI.get(subKeyUI.size() - 1).subkey().getType()
                .equals("<NoWx>")) {
            WeatherSubKey subkey = subKeyUI.get(subKeyUI.size() - 1).subkey();
            addedKey = new WeatherSubKey(subkey);
        } else { // Do not add another empty key
            return;
        }
        ((GridLayout) subKeyFrame.getLayout()).numColumns++;
        SubKey sk = new SubKey(subKeyFrame, addedKey);
        sk.setLayoutData(new GridData(SWT.DEFAULT, SWT.FILL, false, true));
        subKeyUI.add(sk);
        subKeyFrame.layout();
        subKeyFrame.pack(true);
        getShell().pack(true);
    }

    private void setInitialDisplay() {
        for (SubKey subKey : subKeyUI) {
            subKey.dispose();
        }
        subKeyUI.clear();
        ((GridLayout) subKeyFrame.getLayout()).numColumns = 0;

        // get the subkeys to be displayed initially
        // reset all entries and create new ones as necessary
        for (WeatherSubKey sk : weatherValue.getWeatherKey().getSubKeys()) {
            // make the subkey
            ((GridLayout) subKeyFrame.getLayout()).numColumns++;
            SubKey subk = new SubKey(subKeyFrame, sk);
            subk.setLayoutData(new GridData(SWT.DEFAULT, SWT.FILL, false, true));
            subKeyUI.add(subk);
        }
        subKeyFrame.layout();
        subKeyFrame.pack(true);

        // display the current weather key
        label.setText(weatherValue.getWeatherKey().toPrettyString());
    }

    private void createSubKeyLabels(Composite comp) {
        Composite f = new Composite(comp, SWT.BORDER);
        GridLayout layout = new GridLayout(1, false);
        f.setLayout(layout);
        GridData layoutData = new GridData(SWT.DEFAULT, SWT.FILL, false, true);
        f.setLayoutData(layoutData);

        Combo dummy = new Combo(f, SWT.DROP_DOWN | SWT.READ_ONLY);
        Point comboSize = dummy.computeSize(SWT.DEFAULT, SWT.DEFAULT);
        dummy.dispose();

        Label typeLabel = new Label(f, SWT.CENTER);
        typeLabel.setText("Type:");
        layoutData = new GridData(SWT.FILL, SWT.DEFAULT, true, false);
        Point labelSize = typeLabel.computeSize(SWT.DEFAULT, SWT.DEFAULT);
        int vIndent = (comboSize.y - labelSize.y);
        layoutData.verticalIndent = vIndent / 2;
        typeLabel.setLayoutData(layoutData);

        Label covLabel = new Label(f, SWT.CENTER);
        covLabel.setText("Cov/Prob:");
        layoutData = new GridData(SWT.FILL, SWT.DEFAULT, true, false);
        layoutData.verticalIndent = vIndent;
        covLabel.setLayoutData(layoutData);

        Label intenLabel = new Label(f, SWT.CENTER);
        intenLabel.setText("Inten:");
        layoutData = new GridData(SWT.FILL, SWT.DEFAULT, true, false);
        layoutData.verticalIndent = vIndent;
        intenLabel.setLayoutData(layoutData);

        Label visLabel = new Label(f, SWT.CENTER);
        visLabel.setText("Vis:");
        layoutData = new GridData(SWT.FILL, SWT.DEFAULT, true, false);
        layoutData.verticalIndent = vIndent;
        visLabel.setLayoutData(layoutData);
    }

    private void subKeyChanged() {
        ArrayList<WeatherSubKey> wxsubkeys = new ArrayList<WeatherSubKey>();
        for (SubKey key : subKeyUI) {
            if (key.subkey().isValid()) {
                wxsubkeys.add(key.subkey());
            }
        }

        WeatherKey key = new WeatherKey(siteId, wxsubkeys);
        weatherValue = new WeatherWxValue(key, parm);

        if (setPickUpValueEachTime) {
            setWxPickup();
        }

        updateLabel();
    }

    private void updateLabel() {
        label.setText(weatherValue.getWeatherKey().toPrettyString());
    }

    private class SubKey extends Composite {

        private Combo typeCombo, covCombo, intenCombo, visCombo;

        private ArrayList<Button> attributeCheckBoxes = new ArrayList<Button>();

        private WxDefinition wxDef;

        private PythonPreferenceStore prefs;

        private String type;

        private String coverage;

        private String intensity;

        private String visibility;

        private List<String> attributes;

        public SubKey(Composite parent, WeatherSubKey subKey) {
            super(parent, SWT.BORDER);

            this.wxDef = subKey.wxDef();
            this.prefs = Activator.getDefault().getPreferenceStore();
            GridLayout layout = new GridLayout(1, false);
            setLayout(layout);

            // initial value - as text strings
            this.type = subKey.getType();
            this.coverage = subKey.getCoverage();
            this.intensity = subKey.getIntensity();
            this.visibility = subKey.getVisibility();
            this.attributes = subKey.getAttributes();

            // create the lists
            createTypeCombo();
            createCoverageCombo();
            createIntensityCombo();
            createVisibilityCombo();
            createAttributeButtons();

            pack();
        }

        private void createTypeCombo() {
            List<String> types = WeatherSubKey.availableWxTypes(siteId);

            typeCombo = new Combo(this, SWT.DROP_DOWN | SWT.READ_ONLY);
            typeCombo.setLayoutData(new GridData(SWT.FILL, SWT.DEFAULT, true,
                    false));

            for (String t : types) {
                typeCombo.add(labelType(t));
            }

            typeCombo.setText(labelType(this.type));

            typeCombo.addSelectionListener(new SelectionAdapter() {
                @Override
                public void widgetSelected(SelectionEvent e) {
                    typeSelected(typeCombo.getText());
                }
            });
        }

        private void createCoverageCombo() {
            List<String> coverages = WeatherSubKey.availableCoverages(siteId,
                    this.type);

            // create a new one if we have valid coverages
            if (coverages.size() > 0) {
                // Choose the appropriate coverage for the current type
                if (!coverages.contains(this.coverage)) {
                    this.coverage = prefs.getString(this.type
                            + "_defaultCoverage");
                    if (coverage.isEmpty()) {
                        this.coverage = coverages.get(0);
                    }
                }

                covCombo = new Combo(this, SWT.DROP_DOWN | SWT.READ_ONLY);
                covCombo.setLayoutData(new GridData(SWT.FILL, SWT.DEFAULT,
                        true, false));

                for (String c : coverages) {
                    covCombo.add(labelCoverage(this.type, c));
                }

                covCombo.setText(labelCoverage(this.type, this.coverage));

                covCombo.addSelectionListener(new SelectionAdapter() {
                    @Override
                    public void widgetSelected(SelectionEvent e) {
                        covSelected(covCombo.getText());
                    }

                });

                // if only one choice, then disable selections
                covCombo.setEnabled(coverages.size() > 1);
            }

        }

        private void createIntensityCombo() {
            List<String> intensities = WeatherSubKey.availableIntensities(
                    siteId, this.type);

            // create a new one if we have valid intensities
            if (intensities.size() > 0) {
                // Choose the appropriate coverage for the current type
                if (!intensities.contains(this.intensity)) {
                    this.intensity = prefs.getString(this.type
                            + "_defaultIntensity");
                    if (this.intensity.isEmpty()) {
                        this.intensity = intensities.get(0);
                    }
                }

                intenCombo = new Combo(this, SWT.DROP_DOWN | SWT.READ_ONLY);
                intenCombo.setLayoutData(new GridData(SWT.FILL, SWT.DEFAULT,
                        true, false));
                for (String i : intensities) {
                    intenCombo.add(labelIntensity(this.type, i));
                }

                intenCombo.setText(labelIntensity(this.type, this.intensity));

                intenCombo.addSelectionListener(new SelectionAdapter() {
                    @Override
                    public void widgetSelected(SelectionEvent e) {
                        intenSelected(intenCombo.getText());
                    }

                });

                // if only one choice, then disable selections
                intenCombo.setEnabled(intensities.size() > 1);
            }
        }

        private void createVisibilityCombo() {
            List<String> visibilities = WeatherSubKey
                    .availableVisibilities(siteId);

            // create a new one if we have valid visibilities
            if (visibilities.size() > 0) {
                if (!visibilities.contains(this.visibility)) {
                    this.visibility = visibilities.get(0);
                }

                visCombo = new Combo(this, SWT.DROP_DOWN | SWT.READ_ONLY);
                visCombo.setLayoutData(new GridData(SWT.FILL, SWT.DEFAULT,
                        true, false));

                for (String v : visibilities) {
                    visCombo.add(v);
                }

                visCombo.setText(this.visibility);

                visCombo.addSelectionListener(new SelectionAdapter() {
                    @Override
                    public void widgetSelected(SelectionEvent e) {
                        visSelected(visCombo.getText());
                    }
                });

                // if only one choice, then disable selections
                visCombo.setEnabled(visibilities.size() > 1);
            }
        }

        private void createAttributeButtons() {
            // available attributes
            List<String> availAttributes = WeatherSubKey.availableAttributes(
                    siteId, this.type);

            // create a new one if we have valid attribute
            if (availAttributes.size() > 0) {
                for (String a : availAttributes) {
                    Button b = new Button(this, SWT.CHECK);
                    b.setText(labelAttribute(this.type, a));
                    b.setSelection(this.attributes.contains(a));
                    b.addSelectionListener(new SelectionAdapter() {
                        @Override
                        public void widgetSelected(SelectionEvent e) {
                            Button b = (Button) e.widget;
                            attrSelected(b.getText(), b.getSelection());
                        }
                    });
                    this.attributeCheckBoxes.add(b);
                }
            }
        }

        // callback when a new wxType is selected.
        private void typeSelected(String value) {
            this.type = this.valueFromLabel(value);
            this.coverage = " ";
            this.intensity = " ";
            this.visibility = " ";
            this.attributes.clear();
            this.intenCombo.dispose();
            this.covCombo.dispose();
            this.visCombo.dispose();

            for (Button b : attributeCheckBoxes) {
                b.dispose();
            }
            this.attributeCheckBoxes.clear();

            this.createCoverageCombo();
            this.createIntensityCombo();
            this.createVisibilityCombo();
            this.createAttributeButtons();

            layout();
            pack();
            getShell().pack();

            WxSetValue.this.subKeyChanged();
        }

        // callback when a new coverage is selected.
        private void covSelected(String value) {
            this.coverage = this.valueFromLabel(value);
            WxSetValue.this.subKeyChanged();
        }

        // callback when a new intensity is selected
        private void intenSelected(String value) {
            this.intensity = this.valueFromLabel(value);
            WxSetValue.this.subKeyChanged();
        }

        // callback when a new visibility is selected
        private void visSelected(String value) {
            this.visibility = value;
            WxSetValue.this.subKeyChanged();
        }

        // callback when a new attribute is selected
        private void attrSelected(String value, boolean selected) {
            String attr = this.valueFromLabel(value);
            if (selected) {
                this.attributes.add(attr);
            } else {
                this.attributes.remove(attr);
            }
            WxSetValue.this.subKeyChanged();
        }

        // translate to label from value
        private String labelType(String stype) {
            if (showDescription) {
                return stype + " (" + this.wxDef.typeDesc(stype) + ")";
            } else {
                return stype;
            }
        }

        private String labelCoverage(String stype, String coverage) {
            if (showDescription) {
                return coverage + " ("
                        + this.wxDef.coverageDesc(stype, coverage) + ")";
            } else {
                return coverage;
            }
        }

        private String labelIntensity(String stype, String inten) {
            if (showDescription) {
                return inten + " (" + this.wxDef.intensityDesc(stype, inten)
                        + ")";
            } else {
                return inten;
            }
        }

        private String labelAttribute(String stype, String attr) {
            if (showDescription) {
                return attr + " (" + this.wxDef.attributeDesc(stype, attr)
                        + ")";
            } else {
                return attr;
            }
        }

        private String valueFromLabel(String label) {
            if (!showDescription) {
                return label;
            } else {
                int index = label.indexOf(" (");
                if (index != -1) {
                    return label.substring(0, index);
                } else {
                    return label;
                }
            }
        }

        // accessor for subkey
        public WeatherSubKey subkey() {
            String stringV = this.coverage + ':' + this.type + ':'
                    + this.intensity + ':' + this.visibility + ':';

            String attrList = "";

            if (this.attributes.size() > 0) {
                int i = 0;
                for (String attr : this.attributes) {
                    attrList = attrList + attr;

                    if (i < (this.attributes.size() - 1)) {
                        attrList = attrList + ',';
                    }
                }
            } else {
                attrList = "<NoAttr>";
            }

            stringV = stringV + attrList;

            return new WeatherSubKey(siteId, stringV);
        }
    }
}
