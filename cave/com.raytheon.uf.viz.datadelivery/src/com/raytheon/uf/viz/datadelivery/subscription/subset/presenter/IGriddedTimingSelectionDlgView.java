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

import com.raytheon.viz.ui.presenter.IPresenterView;
import com.raytheon.viz.ui.presenter.components.ButtonConf;
import com.raytheon.viz.ui.presenter.components.CheckBoxConf;
import com.raytheon.viz.ui.presenter.components.ListConf;

/**
 * IGriddedTimingSelectionDlgView interface
 * 
 * <pre>
 * 
 * SOFTWARE HISTORY
 * 
 * Date         Ticket#    Engineer    Description
 * ------------ ---------- ----------- --------------------------
 * Sep 12, 2012            mpduff     Initial creation.
 * Oct 11, 2012   1263     jpiatt     Modified for cancel flag.
 * Jan 04, 2013   1420     mpduff     Add getters for Latency and Priority.
 * 
 * </pre>
 * 
 * @author mpduff
 * @version 1.0
 */

public interface IGriddedTimingSelectionDlgView extends IPresenterView {

    /**
     * Open the view dialog.
     * 
     * @return the selection
     */
    Integer openDlg();

    /**
     * Set the config object for the latest data check box.
     * 
     * @param checkBoxConf
     */
    void setLatestDataCheckBox(CheckBoxConf checkBoxConf);

    /**
     * Set the date/cycle list config object.
     * 
     * @param dateCycleListConf
     */
    void setDateCycleList(ListConf dateCycleListConf);

    /**
     * Set the config object for the ok button.
     * 
     * @param okBtnConf
     */
    void setOkButton(ButtonConf okBtnConf);

    /**
     * Set the callback to be called at preopen.
     * 
     * @param preOpenCallback
     */
    void setPreOpenCallback(Runnable preOpenCallback);

    /**
     * Check if the latest data checkbox is enabled.
     * 
     * @return true if enabled.
     */
    boolean isLatestDataEnabled();

    /**
     * Set the date/cycle list enabled.
     */
    void setDateCycleListEnabled();

    /**
     * Get the list selection.
     * 
     * @return the selected item in the list
     */
    String getSelection();

    /**
     * Close the dialog.
     */
    void closeDlg();

    /**
     * Set the cancel button.
     * 
     * @param cancelBtnConf
     */
    void setCancelButton(ButtonConf cancelBtnConf);

    /**
     * Get the latency value.
     * 
     * @return Latency value
     */
    int getLatency();

    /**
     * Get the priority value.
     * 
     * @return priority value
     */
    int getPriority();
}
