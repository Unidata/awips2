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
import java.util.List;

import com.raytheon.uf.common.status.IUFStatusHandler;
import com.raytheon.uf.common.status.UFStatus;
import com.raytheon.uf.common.status.UFStatus.Priority;
import com.raytheon.uf.common.time.TimeRange;
import com.raytheon.viz.gfe.GFEPreference;
import com.raytheon.viz.gfe.core.DataManager;
import com.raytheon.viz.gfe.core.DataManagerUIFactory;
import com.raytheon.viz.gfe.core.wxvalue.DiscreteWxValue;
import com.raytheon.viz.gfe.core.wxvalue.ScalarWxValue;
import com.raytheon.viz.gfe.core.wxvalue.VectorWxValue;
import com.raytheon.viz.gfe.core.wxvalue.WeatherWxValue;
import com.raytheon.viz.gfe.core.wxvalue.WxValue;
import com.raytheon.viz.gfe.interpolation.Interpolator.Algorithm;

/**
 * The ParmState class contains state information about the parm that pertains
 * to editing modes and values.
 *
 * <pre>
 * SOFTWARE HISTORY
 *
 * Date          Ticket#  Engineer    Description
 * ------------- -------- ----------- ------------------------------------------
 * Mar 01, 2008           chammack    Initial skeleton creation.
 * Jul 28, 2008           wdougherty  Fix null pointer exception in
 *                                    getCurrentVectorMode()
 * Jun 10, 2009  2159     rjpeter     Fixed recent and session values to not
 *                                    contain duplicates.
 * Jan 24, 2018  7153     randerso    Changes to allow new GFE config file to be
 *                                    selected when perspective is re-opened.
 * May 08, 2018  7301     dgilling    Fix handling of smooth size preference.
 *
 * </pre>
 *
 * @author chammack
 */
public class ParmState {
    private static final int LIST_VALUES_MAX = 15;

    private static final IUFStatusHandler statusHandler = UFStatus
            .getHandler(ParmState.class);

    /** Wx/Discrete Combine Mode */
    public static enum CombineMode {
        /** Combine Wx/Discretes */
        COMBINE("Combine"),

        /** Replace Wx/Discretes */
        REPLACE("Replace");

        private String stringValue;

        private CombineMode(String value) {
            this.stringValue = value;
        }

        @Override
        public String toString() {
            return stringValue;
        }
    }

    /** Vector Edit Mode */
    public static enum VectorMode {

        /** Edit magnitude */
        MAGNITUDE("Magnitude Only"),

        /** Edit direction */
        DIRECTION("Direction Only"),

        /** Edit both magnitude and direction */
        BOTH("Both");

        private String stringValue;

        private VectorMode(String value) {
            this.stringValue = value;
        }

        @Override
        public String toString() {
            return stringValue;
        }

    }

    /** Interpolation mode */
    public static enum InterpMode {
        /** Interpolate based on gaps */
        GAPS("Gaps"),

        /** Interpolate based on edited data */
        EDITED("Based on Edited Data");

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
     * @return the current Combine mode
     */
    public static CombineMode getCurrentCombineMode() {
        boolean combine = GFEPreference.getBoolean("WeatherDiscreteCombineMode",
                false);
        CombineMode currentMode = (combine ? CombineMode.COMBINE
                : CombineMode.REPLACE);

        DataManager dm = DataManagerUIFactory.getCurrentInstance();
        if (dm != null) {
            Parm[] parms = dm.getParmManager().getAllParms();
            if ((parms != null) && (parms.length > 0)) {
                currentMode = parms[0].getParmState().getCombineMode();
            }
        }
        return currentMode;
    }

    /**
     * @return the current Vector Editor mode.
     */
    public static VectorMode getCurrentVectorMode() {
        VectorMode currentMode = VectorMode.BOTH;

        DataManager dm = DataManagerUIFactory.getCurrentInstance();
        if (dm != null) {
            Parm[] parms = dm.getParmManager().getAllParms();
            if ((parms != null) && (parms.length > 0)) {
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
     * A list of the last LIST_VALUES_MAX WxValues, used from the set pickup
     * value menu, to display the last values.
     */
    private List<WxValue> sessionPickUpValues = new ArrayList<>(
            LIST_VALUES_MAX);

    /**
     * A list of recently added WxValues. Shown in "Set to Recent Values" menu
     * item.
     */
    private List<WxValue> recentPickUpValues = new ArrayList<>(LIST_VALUES_MAX);

    /**
     * Constructor
     *
     * @param parm
     */
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

        String uiName = this.parm.getParmID().compositeNameUI();

        this.deltaValue = GFEPreference.getFloat(uiName + "_deltaValue",
                (float) Math.pow(10.0,
                        -this.parm.getGridInfo().getPrecision()));

        this.fuzzValue = GFEPreference.getFloat(uiName + "_fuzzValue",
                (this.parm.getGridInfo().getMaxValue()
                        - this.parm.getGridInfo().getMinValue()) / 100);

        // default to 4 grid cells
        this.pencilWidth = GFEPreference.getInt(uiName + "_pencilWidth", 4);

        // default to 3 grid cells
        this.smoothSize = GFEPreference.getInt(uiName + "_smoothSize",
                GFEPreference.getInt("SmoothSize", 3));

        String s = GFEPreference.getString(uiName + "_interpolateAlgorithm",
                null);
        if (s != null) {
            try {
                this.algorithm = Algorithm.valueOf(s);
            } catch (Exception e) {
                this.algorithm = null;
                statusHandler.handle(Priority.PROBLEM,
                        "Invalid value specified for " + uiName
                                + "_interpolateAlgorithm",
                        e);
            }
        }

        if (this.algorithm == null) {
            String pn = this.parm.getParmID().getParmName();
            if ("QPF".equals(pn) || "Sky".equals(pn) || "PoP".equals(pn)) {
                this.algorithm = Algorithm.CUBIC_ADVECT;
            } else {
                this.algorithm = Algorithm.CUBIC_NOADVECT;
            }
        }

        String windEditMode = GFEPreference.getString("WindEditMode", "BOTH")
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

        boolean combine = GFEPreference.getBoolean("WeatherDiscreteCombineMode",
                false);
        combineMode = (combine ? CombineMode.COMBINE : CombineMode.REPLACE);
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

    /**
     * Set the parm's selection state
     *
     * @param select
     */
    public void setSelected(boolean select) {
        if (parm.getGridInfo().isTimeIndependentParm()) {
            // can never change the state of time indep parms
            return;
        }

        if (select == this.selected) {
            // same state - nothing to do
            return;
        }

        this.selected = select;

        // notify the parm clients
        parm.getListeners().fireParameterSelectionChangedListener(parm,
                this.selected);
    }

    /**
     * Set the pickUp value
     *
     * @param pickUpValue
     */
    public void setPickUpValue(WxValue pickUpValue) {
        this.pickupValue = pickUpValue;
        addRecentPickUpValue(pickUpValue);
        parm.getListeners().firePickupValueChangedListener(parm, pickUpValue);
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
            // can never change the state of time indep parms
            return;
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

    /**
     * Set the interpolation algorithm
     *
     * @param algorithm
     */
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
     * @return the delta value
     */
    public float getDeltaValue() {
        return this.deltaValue;
    }

    /**
     * @return the fuzz value
     */
    public float getFuzzValue() {
        return this.fuzzValue;
    }

    /**
     * @return the pickup value
     */
    public WxValue getPickUpValue() {
        return this.pickupValue;
    }

    /**
     * @return the selected time range
     */
    public TimeRange getSelectedTimeRange() {
        return this.selectedTimeRange;
    }

    /**
     * @return true if the parm is selected
     */
    public boolean isSelected() {
        return this.selected;
    }

    /**
     * @return the vector mode
     */
    public VectorMode getVectorMode() {
        return vectorMode;
    }

    /**
     * @return the combine mode
     */
    public CombineMode getCombineMode() {
        return combineMode;
    }

    /**
     * @return the pencil width
     */
    public int getPencilWidth() {
        return pencilWidth;
    }

    /**
     * @return the interpolation algorithm
     */
    public Algorithm getInterpolateAlgorithm() {
        return this.algorithm;
    }

    /**
     * @return the smooth size
     */
    public int getSmoothSize() {
        return this.smoothSize;
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

    /**
     * Adds the given pickup value to the list of recently-defined values.
     *
     * @param pickUpValue
     */
    public void addRecentPickUpValue(WxValue pickUpValue) {
        if (!validatePickUpValue(pickUpValue)) {
            return;
        }

        updatePickUpValueList(this.recentPickUpValues, pickUpValue);
    }

    /**
     * Adds the given pickup value to the list of session-defined values.
     *
     * @param pickUpValue
     */
    public void addSessionPickUpValue(WxValue pickUpValue) {
        if (!validatePickUpValue(pickUpValue)) {
            return;
        }

        updatePickUpValueList(this.sessionPickUpValues, pickUpValue);
    }

    private boolean validatePickUpValue(WxValue pickUpValue) {
        if (pickupValue == null) {
            // null (no pickup value set) is valid for all grid types
            return true;
        }

        // ensure pick up value is of same type as parm
        switch (this.parm.getGridInfo().getGridType()) {
        case SCALAR:
            if (pickUpValue instanceof ScalarWxValue) {
                return true;
            }
            break;

        case VECTOR:
            if (pickUpValue instanceof VectorWxValue) {
                return true;
            }
            break;

        case WEATHER:
            if (pickUpValue instanceof WeatherWxValue) {
                return true;
            }
            break;

        case DISCRETE:
            if (pickUpValue instanceof DiscreteWxValue) {
                return true;
            }
            break;

        default:
            break;
        }
        statusHandler.error(String.format(
                "Attempt to define pick up value of wrong type. Parmtype=%s pickUpValueType=%s",
                parm.getGridInfo().getGridType(),
                pickUpValue.getClass().getSimpleName()));
        return false;
    }

    private void updatePickUpValueList(List<WxValue> list,
            WxValue pickUpValue) {
        // remove any matching entry
        list.remove(pickUpValue);

        // remove entries if list will be too long
        if (list.size() == LIST_VALUES_MAX) {
            list.remove(LIST_VALUES_MAX - 1);
        }

        // prepend the new entry
        list.add(0, pickUpValue);
    }

    /**
     * @return the session pick up values for this parm.
     */
    public List<WxValue> getSessionPickUpValues() {
        return sessionPickUpValues;
    }

    /**
     * @return the recent pick up values for this parm.
     */
    public List<WxValue> getRecentPickuUpValues() {
        return recentPickUpValues;
    }

}
