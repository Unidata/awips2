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
package com.raytheon.uf.viz.datadelivery.subscription.subset.presenter;

import java.util.ArrayList;
import java.util.Collections;
import java.util.List;

import com.google.common.annotations.VisibleForTesting;
import com.raytheon.uf.common.datadelivery.registry.GriddedDataSet;
import com.raytheon.uf.viz.datadelivery.utils.DataDeliveryGUIUtils;
import com.raytheon.viz.ui.presenter.components.ButtonConf;
import com.raytheon.viz.ui.presenter.components.CheckBoxConf;
import com.raytheon.viz.ui.presenter.components.ComboBoxConf;
import com.raytheon.viz.ui.presenter.components.ListConf;

/**
 * Gridded data timing selection dialog presenter.
 * 
 * <pre>
 * 
 * SOFTWARE HISTORY
 * 
 * Date         Ticket#    Engineer    Description
 * ------------ ---------- ----------- --------------------------
 * Sep 12, 2012            mpduff     Initial creation
 * Sep 27, 2012  1202      bgonzale   Set selectionDate to date and cycle.
 * Oct 11, 2012  1263      jpiatt     Modified for cancel flag.
 * Jan 04, 2013  1420      mpduff     Add the dataset object.
 * 
 * </pre>
 * 
 * @author mpduff
 * @version 1.0
 */

public class GriddedTimingSelectionPresenter {

    /** Popup title */
    @VisibleForTesting
    final String POPUP_TITLE = "Notice";

    /** Popup message for no data selected */
    @VisibleForTesting
    final String VALID_DATE_MUST_BE_SELECTED = "A valid date must be selected.";

    /** The dialog view class */
    private final IGriddedTimingSelectionDlgView view;

    /** Latest data check box conf object */
    @VisibleForTesting
    final CheckBoxConf latestDataChkConf;

    /** Latest data check box action handler */
    private final Runnable latestDataChkAction = new Runnable() {
        @Override
        public void run() {
            toggleUseLatestData();
        }
    };

    /** Ok button action handler */
    @VisibleForTesting
    final Runnable okBtnAction = new Runnable() {
        @Override
        public void run() {
            if (handleOk()) {
                view.closeDlg();
            }
        }
    };

    /** Cancel button action handler */
    @VisibleForTesting
    final Runnable cancelBtnAction = new Runnable() {
        @Override
        public void run() {
            cancel = true;
        }
    };

    /** Cancel flag */
    private boolean cancel = false;

    /** Date/cycle list conf object */
    @VisibleForTesting
    final ListConf dateCycleListConf;

    /** Ok button conf object */
    @VisibleForTesting
    final ButtonConf okBtnConf;

    /** Cancel button conf object */
    @VisibleForTesting
    ButtonConf cancelBtnConf;

    /** The selected cycle */
    private int cycle = -999;

    /** The selected date */
    private String selectedDate;

    /** The Gridded dataset obj */
    private final GriddedDataSet dataSet;

    /**
     * Constructor.
     * 
     * @param view
     *            The view
     * @param dataSet
     *            The dataset
     * @param dateCycleList
     *            The date/cycle list values
     */
    public GriddedTimingSelectionPresenter(IGriddedTimingSelectionDlgView view,
            GriddedDataSet dataSet, List<String> dateCycleList) {
        this.view = view;
        this.dataSet = dataSet;

        latestDataChkConf = new CheckBoxConf("Get Latest Data", true,
                "Use the latest time", latestDataChkAction);

        int width = 175;
        int height = 225;
        dateCycleListConf = new ListConf("Date/Cycle options",
                dateCycleList.toArray(new String[dateCycleList.size()]), true,
                width, height);

        okBtnConf = new ButtonConf(true, "OK", null, okBtnAction);
        cancelBtnConf = new ButtonConf(true, "Cancel", null, cancelBtnAction);
    }

    /**
     * Initialize the view.
     */
    public void init() {
        view.setLatestDataCheckBox(latestDataChkConf);
        view.setDateCycleList(dateCycleListConf);
        view.setOkButton(okBtnConf);
        view.setCancelButton(cancelBtnConf);
    }

    /**
     * OK Button action method.
     * 
     * @return true if everything ok
     */
    protected boolean handleOk() {
        if (!view.isLatestDataEnabled()) {
            String selection = view.getSelection();
            if (ComboBoxConf.SELECT_ONE.equals(selection)
                    || ComboBoxConf.NONE_AVAILABLE.equals(selection)) {
                view.displayPopup(POPUP_TITLE, VALID_DATE_MUST_BE_SELECTED);
                return false;
            }

            DataDeliveryGUIUtils.latencyValidChk(view.getLatency(),
                    getMaxLatency());

            // parse off the date/cycle time selected
            String[] parts = selection.split(" - ");
            this.selectedDate = selection;
            String cycleStr = parts[1];
            cycleStr = cycleStr.substring(0, cycleStr.indexOf(" Z"));
            this.cycle = Integer.parseInt(cycleStr);
        } else {
            this.cycle = -999;
        }

        return true;
    }

    /**
     * Max latency value in minutes.
     * 
     * @return
     */
    private int getMaxLatency() {
        List<Integer> cycleList = new ArrayList<Integer>(dataSet.getCycles());
        Collections.sort(cycleList);

        int max = 0;

        for (int i = 0; i < cycleList.size(); i++) {
            if (i + 1 <= cycleList.size()) {
                int tempMax = cycleList.get(i + 1) - cycleList.get(i);
                if (tempMax > max) {
                    max = tempMax;
                }
            }
        }

        return max * 60;
    }

    /**
     * This method is called via the "Use Latest Data" checkbox being
     * selected/unselected.
     */
    protected void toggleUseLatestData() {
        view.setDateCycleListEnabled();
    }

    /**
     * Open the dialog.
     * 
     * @return The selected cycle
     */
    public Integer open() {
        Runnable callback = new Runnable() {
            @Override
            public void run() {
                init();
            }
        };
        this.view.setPreOpenCallback(callback);
        this.view.openDlg();

        return cycle;
    }

    /**
     * Get the selected date.
     * 
     * @return the selected date string
     */
    public String getDate() {
        return this.selectedDate;
    }

    /**
     * Get cancel flag.
     * 
     * @return true if cancel selected
     */
    public boolean isCancel() {
        return cancel;
    }
}
