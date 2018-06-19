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
import org.eclipse.swt.events.ModifyEvent;
import org.eclipse.swt.events.ModifyListener;
import org.eclipse.swt.events.SelectionAdapter;
import org.eclipse.swt.events.SelectionEvent;
import org.eclipse.swt.layout.GridData;
import org.eclipse.swt.layout.GridLayout;
import org.eclipse.swt.widgets.Button;
import org.eclipse.swt.widgets.Combo;
import org.eclipse.swt.widgets.Composite;
import org.eclipse.swt.widgets.Text;

import com.raytheon.uf.common.dataplugin.gfe.DiscreteTerm;
import com.raytheon.uf.common.dataplugin.gfe.discrete.DiscreteDefinition;
import com.raytheon.uf.common.dataplugin.gfe.discrete.DiscreteKey;
import com.raytheon.uf.common.dataplugin.gfe.discrete.DiscreteKeyDef;
import com.raytheon.uf.viz.core.VizApp;
import com.raytheon.viz.gfe.core.msgs.ICombineModeChangedListener;
import com.raytheon.viz.gfe.core.parm.Parm;
import com.raytheon.viz.gfe.core.parm.ParmState.CombineMode;
import com.raytheon.viz.gfe.core.wxvalue.DiscreteWxValue;

/**
 * Provides UI to set a discrete wx value for the set value dialog
 * 
 * <pre>
 * 
 * SOFTWARE HISTORY
 * Date         Ticket#    Engineer    Description
 * ------------ ---------- ----------- --------------------------
 * Jun 22, 2009 #1318      randerso    Initial creation
 * Aug 20, 2014 #1664      randerso    Fixed invalid thread access
 * 
 * </pre>
 * 
 * @author randerso
 * @version 1.0
 */

public class DiscreteSetValue extends AbstractSetValue implements
        ICombineModeChangedListener {

    private DiscreteKey discreteKey;

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
    public DiscreteSetValue(Composite parent, Parm parm, boolean showCombine,
            boolean showAddToSession, boolean setPickupValueEachTime) {
        super(parent, parm);
        GridLayout layout = (GridLayout) getLayout();
        layout.marginWidth = 0;
        layout.marginHeight = 0;
        layout.verticalSpacing = 0;

        this.setPickUpValueEachTime = setPickupValueEachTime;

        parm.getListeners().addCombineModeChangedListener(this);

        DiscreteWxValue pickupValue = (DiscreteWxValue) parm.getParmState()
                .getPickUpValue();
        discreteKey = new DiscreteKey(pickupValue.getDiscreteKey());

        showDescription = dataManager.getSpatialDisplayManager()
                .getShowDescription();

        // create the top frame containing the current value and an ADD button
        topFrame = new Composite(this, SWT.NONE);
        GridData layoutData = new GridData(SWT.FILL, SWT.FILL, true, true);
        topFrame.setLayoutData(layoutData);
        topFrame.setLayout(new GridLayout(3, false));

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
        layout = new GridLayout(1, false);
        layout.marginWidth = 0;
        layout.marginHeight = 0;
        layout.horizontalSpacing = 0;
        frame.setLayout(layout);
        subKeyUI = new ArrayList<SubKey>();

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
        parm.getParmState().addSessionPickupValue(
                new DiscreteWxValue(discreteKey, parm));
    }

    protected void addSubKey() {
        // check to see if the last entry is a none type
        String addedKey = DiscreteKey.defaultKey(siteId, parm.getParmID())
                .getSubKeys().get(0);
        ((GridLayout) subKeyFrame.getLayout()).numColumns++;
        SubKey sk = new SubKey(subKeyFrame, addedKey);
        sk.setLayoutData(new GridData(SWT.DEFAULT, SWT.FILL, false, true));
        subKeyUI.add(sk);
        subKeyFrame.layout();
        subKeyFrame.pack(true);
        getShell().pack(true);
    }

    protected void reset() {
        // LogStream.logUse("Reset")
        for (SubKey key : subKeyUI) {
            key.dispose();
        }
        subKeyUI.clear();

        discreteKey = DiscreteKey.defaultKey(siteId, parm.getParmID());

        if (setPickUpValueEachTime) {
            setWxPickup();
        }
        setInitialDisplay();

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
        List<String> subkeys = discreteKey.getSubKeys();

        // reset all entries and create new ones as necessary
        for (String sk : subkeys) {
            // make the subkey
            ((GridLayout) subKeyFrame.getLayout()).numColumns++;
            SubKey subk = new SubKey(subKeyFrame, sk);
            subk.setLayoutData(new GridData(SWT.DEFAULT, SWT.FILL, false, true));
            subKeyUI.add(subk);
        }
        subKeyFrame.layout();
        subKeyFrame.pack(true);

        // display the current weather key
        label.setText(discreteKey.toString());
    }

    /**
     * Set the pickupValue of the associated parm to the DiscreteWxValue defined
     * by this control
     */
    public void setWxPickup() {
        parm.getParmState().setPickUpValue(
                new DiscreteWxValue(discreteKey, parm));
    }

    /**
     * Returns the DiscreteWxValue defined by this UI
     * 
     * @return
     */
    public DiscreteWxValue getWxPickup() {
        return new DiscreteWxValue(discreteKey, parm);
    }

    private void subKeyChanged() {
        ArrayList<String> wxsubkeys = new ArrayList<String>();
        for (SubKey key : subKeyUI) {
            wxsubkeys.add(key.getSubKey());
        }

        discreteKey = new DiscreteKey(siteId, wxsubkeys, parm.getParmID());

        if (setPickUpValueEachTime) {
            setWxPickup();
        }

        updateLabel();
    }

    private void updateLabel() {
        // display the current discrete key
        label.setText(discreteKey.toString());
    }

    private class SubKey extends Composite {
        private Combo typeList;

        private Text auxEntryField;

        private int auxDataLength;

        public SubKey(Composite parent, String subKey) {
            super(parent, SWT.BORDER);

            setLayout(new GridLayout(3, false));
            GridData data = new GridData(SWT.FILL, SWT.DEFAULT, true, true);
            setLayoutData(data);

            data = new GridData(GridData.FILL_HORIZONTAL);
            typeList = new Combo(this, SWT.DROP_DOWN | SWT.READ_ONLY);
            typeList.setLayoutData(data);

            DiscreteDefinition discreteDef = DiscreteKey
                    .discreteDefinition(siteId);
            String compositeName = parm.getParmID().getCompositeName();
            List<DiscreteKeyDef> keydef = discreteDef.keys(compositeName);

            updateCombo(typeList, keydef);

            typeList.addSelectionListener(new SelectionAdapter() {

                @Override
                public void widgetSelected(SelectionEvent arg0) {
                    typeSelected();
                }
            });

            auxDataLength = discreteDef.auxDataLength(compositeName);
            if (auxDataLength > 0) {
                auxEntryField = new Text(this, SWT.BORDER);
                auxEntryField.setTextLimit(auxDataLength);
                auxEntryField.addModifyListener(new ModifyListener() {
                    @Override
                    public void modifyText(ModifyEvent arg0) {
                        auxFieldChanged();
                    }
                });
            }

            if (subKey == null) {
                typeList.select(0);
            } else {
                setComboSelection(typeList, subKey);
            }

            getShell().pack(true);
        }

        private void typeSelected() {
            if (auxEntryField != null) {
                setAuxField("");
            }
            subKeyChanged();
        }

        private void setAuxField(String value) {
            auxEntryField.setText(value);
        }

        private void auxFieldChanged() {
            subKeyChanged();
        }

        public String getSubKey() {
            String s = valueFromLabel(typeList.getText());

            String auxData = null;
            if (auxEntryField != null) {
                auxData = auxEntryField.getText();
            }
            if (auxData == null) {
                auxData = "";
            }
            if (auxData.length() > 0) {
                s += DiscreteKey.AUXDATA_SEPARATOR + auxData;
            }
            return s;
        }

        public String valueFromLabel(String label) {
            if (!showDescription) {
                return label;
            } else {
                int index = label.indexOf(" (");
                if (index > 0) {
                    return label.substring(0, index);
                } else {
                    return label;
                }
            }
        }

        public String labelFromValue(DiscreteTerm term) {
            String sym = term.getSymbol();
            String desc = term.getDescription();
            String s = sym;
            if (showDescription && !sym.equals(desc)) {
                s += " (" + desc + ")";
            }
            return s;
        }

        private void updateCombo(Combo combo, List<? extends DiscreteTerm> list) {
            combo.removeAll();

            for (DiscreteTerm term : list) {
                combo.add(labelFromValue(term));
            }
            combo.setText(combo.getItem(0));
            combo.setEnabled(combo.getItemCount() > 1);
        }
    }

    private void setComboSelection(Combo combo, String term) {
        for (String item : combo.getItems()) {
            if (item.startsWith(term)) {
                combo.setText(item);
                return;
            }
        }

        // if we got here choose first item
        combo.setText(combo.getItem(0));
    }

}
