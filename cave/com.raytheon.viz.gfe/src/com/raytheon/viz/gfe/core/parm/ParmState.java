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
package com.raytheon.viz.gfe.core.parm;

import java.util.ArrayList;

import org.eclipse.jface.preference.IPreferenceStore;

import com.raytheon.uf.common.status.IUFStatusHandler;
import com.raytheon.uf.common.status.UFStatus;
import com.raytheon.uf.common.status.UFStatus.Priority;
import com.raytheon.uf.common.time.TimeRange;
import com.raytheon.viz.gfe.Activator;
import com.raytheon.viz.gfe.core.DataManager;
import com.raytheon.viz.gfe.core.wxvalue.WxValue;
import com.raytheon.viz.gfe.interpolation.Interpolator.Algorithm;

/**
 * The ParmState class contains state information about the parm that pertains
 * to editing modes and values.
 * 
 * <pre>
 * SOFTWARE HISTORY
 * Date         Ticket#    Engineer    Description
 * ------------ ---------- ----------- --------------------------
 * 03/01/2008              chammack    Initial skeleton creation.	
 * 07/28/2008              wdougherty  Fix null pointer exception in getCurrentVectorMode()
 * 06/10/2009   2159       rjpeter     Fixed recent and session values to not contain duplicates.
 * </pre>
 * 
 * @author chammack
 * @version 1.0
 */
public class ParmState {
    private static final transient IUFStatusHandler statusHandler = UFStatus
            .getHandler(ParmState.class);

    public static enum CombineMode {
        COMBINE("Combine"), REPLACE("Replace");

        private String stringValue;

        private CombineMode(String value) {
            this.stringValue = value;
        }

        /*
         * (non-Javadoc)
         * 
         * @see java.lang.Enum#toString()
         */
        @Override
        public String toString() {
            return stringValue;
        }
    }

    public static enum VectorMode {

        MAGNITUDE("Magnitude Only"), DIRECTION("Direction Only"), BOTH("Both");

        private String stringValue;

        private VectorMode(String value) {
            this.stringValue = value;
        }

        /*
         * (non-Javadoc)
         * 
         * @see java.lang.Enum#toString()
         */
        @Override
        public String toString() {
            return stringValue;
        }

    }

    /**
     * Interpolation mode
     * 
     */
    public static enum InterpMode {
        GAPS("Gaps"), EDITED("Based on Edited Data");

        private String displayString;

        private InterpMode(String displayString) {
            this.displayString = displayString;
        }

        /**
         * Get the human readable string to be displayed in dialogs, etc.
         * 
         * @return display string
         */
        public String getDisplayString() {
            return displayString;
        }
    }

    /**
     * Get the current Combine Mode.
     * 
     * @return
     */
    public static CombineMode getCurrentCombineMode() {
        CombineMode currentMode = CombineMode.REPLACE;
        IPreferenceStore prefs = Activator.getDefault().getPreferenceStore();
        if (prefs.contains("WeatherDiscreteCombineMode")) {
            if (prefs.getBoolean("WeatherDiscreteCombineMode")) {
                currentMode = CombineMode.COMBINE;
            }
        }

        DataManager dm = DataManager.getCurrentInstance();
        if (dm != null) {
            Parm[] parms = dm.getParmManager().getAllParms();
            if (parms != null && parms.length > 0) {
                currentMode = parms[0].getParmState().getCombineMode();
            }
        }
        return currentMode;
    }

    /**
     * Get the current Vector Editor Mode.
     * 
     * @return
     */
    public static VectorMode getCurrentVectorMode() {
        VectorMode currentMode = VectorMode.BOTH;

        DataManager dm = DataManager.getCurrentInstance();
        if (dm != null) {
            Parm[] parms = dm.getParmManager().getAllParms();
            if (parms != null && parms.length > 0) {
                currentMode = parms[0].getParmState().getVectorMode();
            }
        }
        return currentMode;
    }

    private boolean temporary;

    private boolean selected;

    private float deltaValue;

    private float fuzzValue;

    private int pencilWidth;

    private int smoothSize;

    private WxValue pickupValue;

    private TimeRange selectedTimeRange;

    private VectorMode vectorMode;

    private CombineMode combineMode;

    private InterpMode interpMode;

    private final Parm parm;

    private boolean iscParm;

    private Algorithm algorithm;

    /**
     * A list of the last 15 WxValues, used from the set pickup value menu, to
     * display the last values.
     */
    private ArrayList<WxValue> sessionPickupValues;

    /**
     * A list of recently added WxValues. Shown in "Set to Recent Values" menu
     * item.
     */
    private ArrayList<WxValue> recentPickupValues;

    public ParmState(Parm parm) {
        this.parm = parm;
        this.combineMode = CombineMode.REPLACE;
        this.vectorMode = VectorMode.BOTH;
        this.selected = false;
        this.deltaValue = 1.0f;
        this.fuzzValue = 1.0f;
        this.iscParm = false;
        this.temporary = false;
        this.selectedTimeRange = new TimeRange();

        // set the pick up value to a default value
        this.pickupValue = WxValue.defaultValue(this.parm);

        IPreferenceStore prefStore = Activator.getDefault()
                .getPreferenceStore();
        String uiName = this.parm.getParmID().compositeNameUI();

        if (prefStore.contains(uiName + "_deltaValue")) {
            this.deltaValue = prefStore.getFloat(uiName + "_deltaValue");
        } else {
            this.deltaValue = (float) Math.pow(10.0, -this.parm.getGridInfo()
                    .getPrecision());
        }

        if (prefStore.contains(uiName + "_fuzzValue")) {
            this.fuzzValue = prefStore.getFloat(uiName + "_fuzzValue");
        } else {
            this.fuzzValue = (this.parm.getGridInfo().getMaxValue() - this.parm
                    .getGridInfo().getMinValue()) / 100;
        }

        if (prefStore.contains(uiName + "_pencilWidth")) {
            this.pencilWidth = prefStore.getInt(uiName + "_pencilWidth");
        } else {
            this.pencilWidth = 4; // default to 4 grid cells
        }

        if (prefStore.contains(uiName + "_smoothSize")) {
            this.smoothSize = prefStore.getInt(uiName + "_smoothSize");
        } else {
            this.smoothSize = 3; // default to 3 grid cells
        }

        if (prefStore.contains(uiName + "_interpolateAlgorithm")) {
            try {
                this.algorithm = Algorithm.valueOf(prefStore.getString(uiName
                        + "_interpolateAlgorithm"));
            } catch (Exception e) {
                this.algorithm = Algorithm.CUBIC_ADVECT;
                statusHandler.handle(Priority.PROBLEM,
                        "Invalid value specified for " + uiName
                                + "_interpolateAlgorithm", e);
            }
        } else {
            String pn = this.parm.getParmID().getParmName();
            if (pn.equals("QPF") || pn.equals("Sky") || pn.equals("PoP")) {
                this.algorithm = Algorithm.CUBIC_ADVECT;
            } else {
                this.algorithm = Algorithm.CUBIC_NOADVECT;
            }
        }

        if (prefStore.contains("WindEditMode")) {
            String windEditMode = prefStore.getString("WindEditMode")
                    .toUpperCase();
            if (windEditMode.startsWith("MAG")) {
                this.vectorMode = VectorMode.MAGNITUDE;
            } else if (windEditMode.startsWith("DIR")) {
                this.vectorMode = VectorMode.DIRECTION;
            } else if ("BOTH".equals(windEditMode)) {
                this.vectorMode = VectorMode.BOTH;
            } else {
                statusHandler.handle(Priority.PROBLEM, "Invalid value (\""
                        + windEditMode + "\" specified for WindEditMode");
            }
        }

        if (prefStore.contains("WeatherDiscreteCombineMode")) {
            if (prefStore.getBoolean("WeatherDiscreteCombineMode")) {
                combineMode = CombineMode.COMBINE;
            }
        }
    }

    /**
     * Set the vector mode
     * 
     * @param vectorMode
     */
    public void setVectorMode(VectorMode vectorMode) {
        this.vectorMode = vectorMode;
        parm.getListeners().fireVectorModeChangedListener(parm, vectorMode);
    }

    /**
     * Set the combine mode
     * 
     * @param combineMode
     */
    public void setCombineMode(CombineMode combineMode) {
        this.combineMode = combineMode;
        parm.getListeners().fireCombineModeChangedListener(parm, combineMode);
    }

    public void setSelected(boolean select) {
        if (parm.getGridInfo().isTimeIndependentParm()) {
            return; // can never change the state of time indep parms
        }

        if (select == this.selected) {
            return; // same state - nothing to do
        }

        this.selected = select;

        // notify the parm clients
        parm.getListeners().fireParameterSelectionChangedListener(parm,
                this.selected);
    }

    /**
     * Set the pickup value
     * 
     * @param pickupValue
     */
    public void setPickUpValue(WxValue pickupValue) {
        this.pickupValue = pickupValue;
        addRecentPickupValue(pickupValue);
        parm.getListeners().firePickupValueChangedListener(parm, pickupValue);
    }

    /**
     * Set the delta value
     * 
     * @param deltaValue
     */
    public void setDeltaValue(float deltaValue) {
        this.deltaValue = deltaValue;
    }

    /**
     * Set the fuzz value
     * 
     * @param fuzzValue
     */
    public void setFuzzValue(float fuzzValue) {
        this.fuzzValue = fuzzValue;
    }

    /**
     * Command received from the ParmManager to update the parm's selected time
     * range based on a new global selection time range.
     * 
     * Verifies that the global selection time range isn't for all time, if so,
     * then sets this parm's selection time range to the same value. Else we use
     * the split boundary's expandTRToQuantum() routine to expand the global
     * time range to one that corresponds to this parm's requirements. Notifies
     * the parm that the selectedTR changed.
     * 
     * @param globalSelectionTimeRange
     */
    public void updateSelectedTimeRange(TimeRange globalSelectionTimeRange) {
        if (parm.getGridInfo().isTimeIndependentParm()) {
            return; // can never change the state of time indep parms
        }

        if (globalSelectionTimeRange.equals(TimeRange.allTimes())) {
            selectedTimeRange = globalSelectionTimeRange;
        } else {
            selectedTimeRange = parm.getGridInfo().getTimeConstraints()
                    .expandTRToQuantum(globalSelectionTimeRange);
        }

        parm.getListeners().fireSelectionTimeRangeChanged(parm,
                selectedTimeRange);
    }

    /**
     * Set the pencil width
     * 
     * @param aWidth
     */
    public void setPencilWidth(int aWidth) {
        this.pencilWidth = aWidth;
    }

    public void setInterpolateAlgorithm(Algorithm algorithm) {
        this.algorithm = algorithm;
    }

    /**
     * Set the smooth size
     * 
     * @param smoothSize
     */
    public void setSmoothSize(int smoothSize) {
        this.smoothSize = smoothSize;
    }

    /**
     * Return the delta value
     * 
     * @return
     */
    public float getDeltaValue() {
        return this.deltaValue;
    }

    /**
     * Return the fuzz value
     * 
     * @return
     */
    public float getFuzzValue() {
        return this.fuzzValue;
    }

    /**
     * Return the pickup value
     * 
     * @return
     */
    public WxValue getPickUpValue() {
        return this.pickupValue;
    }

    /**
     * Return the selected time range
     * 
     * @return
     */
    public TimeRange getSelectedTimeRange() {
        return this.selectedTimeRange;
    }

    /**
     * Return whether the parm is selected
     * 
     * @return
     */
    public boolean isSelected() {
        return this.selected;
    }

    /**
     * Return the vector mode
     * 
     * @return
     */
    public VectorMode getVectorMode() {
        return vectorMode;
    }

    /**
     * Return the combine mode
     * 
     * @return
     */
    public CombineMode getCombineMode() {
        return combineMode;
    }

    /**
     * Return the pencil width
     * 
     * @return
     */
    public int getPencilWidth() {
        return pencilWidth;
    }

    public Algorithm getInterpolateAlgorithm() {
        return this.algorithm;
    }

    /**
     * Gets the smooth size.
     * 
     * @return the smooth size
     */
    public int getSmoothSize() {
        return this.smoothSize;
    }

    public WxValue[] getRecentPickUpValues() {
        throw new UnsupportedOperationException();
    }

    /**
     * @return the temporary
     */
    public boolean isTemporary() {
        return temporary;
    }

    /**
     * @param temporary
     *            the temporary to set
     */
    public void setTemporary(boolean temporary) {
        this.temporary = temporary;
    }

    /**
     * @return the interpMode
     */
    public InterpMode getInterpMode() {
        return interpMode;
    }

    /**
     * @param interpMode
     *            the interpMode to set
     */
    public void setInterpMode(InterpMode interpMode) {
        this.interpMode = interpMode;
    }

    /**
     * @return the iscParm
     */
    public boolean isIscParm() {
        return iscParm;
    }

    /**
     * @param iscParm
     *            the iscParm to set
     */
    public void setIscParm(boolean iscParm) {
        this.iscParm = iscParm;
    }

    public void addRecentPickupValue(WxValue value) {
        ArrayList<WxValue> recent = getRecentPickupValues();

        if (recent.contains(value)) {
            recent.remove(value);
        }

        if (recent.size() < 1) {
            recent.add(value);
        } else {
            recent.add(0, value);
        }
    }

    public void addSessionPickupValue(WxValue value) {
        ArrayList<WxValue> session = getSessionPickupValues();

        if (session.contains(value)) {
            session.remove(value);
        }

        if (session.size() < 1) {
            session.add(value);
        } else {
            session.add(0, value);
        }

        if (session.size() > 15) {
            session.remove(15);
        }

    }

    public ArrayList<WxValue> getSessionPickupValues() {
        if (sessionPickupValues == null) {
            sessionPickupValues = new ArrayList<WxValue>(15);
        }
        return sessionPickupValues;
    }

    public ArrayList<WxValue> getRecentPickupValues() {
        if (recentPickupValues == null) {
            recentPickupValues = new ArrayList<WxValue>(15);
        }
        return recentPickupValues;
    }

}
