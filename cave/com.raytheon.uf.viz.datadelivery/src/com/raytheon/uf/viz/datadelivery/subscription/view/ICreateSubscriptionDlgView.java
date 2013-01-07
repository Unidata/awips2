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
package com.raytheon.uf.viz.datadelivery.subscription.view;

import java.util.Date;
import java.util.List;

import org.eclipse.swt.widgets.Shell;

import com.raytheon.viz.ui.presenter.IPresenterView;
import com.raytheon.viz.ui.presenter.components.ButtonConf;
import com.raytheon.viz.ui.presenter.components.CheckBoxConf;
import com.raytheon.viz.ui.presenter.components.ComboBoxConf;

/**
 * Create Subscription Dialog View Interface.
 * 
 * <pre>
 * 
 * SOFTWARE HISTORY
 * 
 * Date         Ticket#    Engineer    Description
 * ------------ ---------- ----------- --------------------------
 * Aug 24, 2012  0223      mpduff      Initial creation
 * Dec 13, 2012  1391      bgonzale    Added status methods.
 * Jan 02, 2013  1441      djohnson    Add isGroupSelected.
 * 
 * </pre>
 * 
 * @author mpduff
 * @version 1.0
 */

public interface ICreateSubscriptionDlgView extends IPresenterView {
    /**
     * Get the subscription Name.
     * 
     * @return
     */
    String getSubscriptionName();

    /**
     * Set the subscription name.
     * 
     * @param subscriptionName
     */
    void setSubscriptionName(String subscriptionName);

    /**
     * Get the subscription description
     * 
     * @return
     */
    String getSubscriptionDescription();

    /**
     * Set the subscription description.
     * 
     * @param subscriptionDescription
     */
    void setSubscriptionDescription(String subscriptionDescription);

    /**
     * Get the group name.
     * 
     * @return
     */
    String getGroupName();

    /**
     * Set the group name.
     * 
     * @param groupName
     */
    void setGroupName(String groupName);

    /**
     * Get the delivery selection
     * 
     * @return
     */
    int getDeliverySelection();

    /**
     * Set the delivery selection.
     * 
     * @param idx
     */
    void setDeliverySelection(int idx);

    /**
     * Is the no expiration date checkbox checked?
     * 
     * @return
     */
    boolean isNoExpirationDate();

    /**
     * Set the no expiration date checkbox
     * 
     * @param noExpiration
     */
    void setNoExpiration(boolean noExpiration);

    /**
     * Get the start text.
     * 
     * @return
     */
    String getStartText();

    /**
     * Get the selected status.
     * 
     * @return Status.OK if Ok selected; Status.CANCEL if cancel selected.
     */
    int getStatus();

    /**
     * Set the selected status. Set to Status.OK if Ok selected; Status.CANCEL
     * if cancel selected.
     */
    void setStatus(int status);

    /**
     * Set the start date
     * 
     * @param startDate
     */
    void setStartDate(Date startDate);

    /**
     * Get the expiration time text
     * 
     * @return
     */
    String getExpirationText();

    /**
     * Set the expiration date
     * 
     * @param expDate
     */
    void setExpirationDate(Date expDate);

    /**
     * Get the is always active selection.
     * 
     * @return
     */
    boolean isAlwaysActive();

    /**
     * Set the always active selection.
     * 
     * @param active
     */
    void setAlwaysActive(boolean active);

    /**
     * Get the active start text
     * 
     * @return
     */
    String getActiveStartText();

    /**
     * Set the active start date
     * 
     * @param activeStartDate
     */
    void setActiveStartDate(Date activeStartDate);

    /**
     * Get the active end text
     * 
     * @return
     */
    String getActiveEndText();

    /**
     * Set the active end date
     * 
     * @param activeEndDate
     */
    void setActiveEndDate(Date activeEndDate);

    /**
     * Get the selected priority
     * 
     * @return
     */
    int getPriority();

    /**
     * Set the priority selection
     * 
     * @param i
     */
    void setPriority(int i);

    /**
     * Open the dialog
     */
    void openDlg();

    /**
     * Is view disposed check
     * 
     * @return
     */
    boolean isDisposed();

    /**
     * Bring the view to the top
     */
    void bringToTop();

    /**
     * Set the subscription date fields enabled/disabled
     * 
     * @param b
     */
    void setSubscriptionDatesEnabled(boolean b);

    /**
     * Set the start date button enabled/disabled
     * 
     * @param b
     */
    void setStartDateBtnEnabled(boolean b);

    /**
     * Set the end date button enabled/disabled
     * 
     * @param b
     */
    void setEndDateBtnEnabled(boolean b);

    /**
     * Set the active date fields enabled/disabled
     * 
     * @param b
     */
    void setActiveDatesEnabled(boolean b);

    /**
     * Set the active end date button enabled/disabled
     * 
     * @param b
     */
    void setActiveEndDateBtnEnabled(boolean b);

    /**
     * Set the active start date button enabled/disabled
     * 
     * @param b
     */
    void setActiveStartDateBtnEnabled(boolean b);

    /**
     * Set the delivery options combo config object
     * 
     * @param deliveryComboConf
     */
    void setDeliveryOptionsComboConf(ComboBoxConf deliveryComboConf);

    /**
     * Set the delivery options Strings
     * 
     * @param deliveryOptions
     */
    void setDeliveryOptions(String[] deliveryOptions);

    /**
     * Set the OK button config object.
     * 
     * @param okConf
     */
    void setOkConf(ButtonConf okConf);

    /**
     * Get the create flag.
     * 
     * @return
     */
    boolean isCreate();

    /**
     * Select all text in the subscription name field.
     */
    void selectAllSubscriptionName();

    /**
     * Get the change reason.
     * 
     * @return
     */
    String getChangeReason();

    /**
     * Close the view dialog
     */
    void closeDlg();

    /**
     * Get list of selected cycle times
     * 
     * @return
     */
    List<Integer> getCycleTimes();

    /**
     * Set the cycle checkbox config list
     * 
     * @param checkboxConfList
     */
    void setCycleConf(List<CheckBoxConf> checkboxConfList);

    /**
     * Set the select all button config object
     * 
     * @param selectAllConf
     */
    void setSelectAllButton(ButtonConf selectAllConf);

    /**
     * Set the deselect all button config object
     * 
     * @param deselectAllConf
     */
    void setDeselectAllButton(ButtonConf deselectAllConf);

    /**
     * Set the date text fields enabled/disabled.
     * 
     * @param flag
     */
    void setDateTxtFieldsEnabled(boolean flag);

    /**
     * Set the active text fields enabled/disabled
     * 
     * @param flag
     */
    void setActiveTextFieldsEnabled(boolean flag);

    /**
     * Set the cycle times to select.
     * 
     * @param cycleStrings
     */
    void selectCycles(List<String> cycleStrings);

    /**
     * The callback that should be performed when preOpened() is called.
     * 
     * @param callback
     *            the callback
     */
    void setPreOpenCallback(Runnable callback);

    /**
     * Create the cycle group portion of the subscription view.
     */
    void createCycleGroup();

    /**
     * Return the shell used for display content.
     * 
     * @return the shell
     */
    Shell getShell();

    /**
     * Return true if a valid group is selected.
     * 
     * @return
     */
    boolean isGroupSelected();
}
