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
package com.raytheon.viz.gfe.dialogs.formatterlauncher;

/**
 * PRoduct tab interface.
 * 
 * <pre>
 * SOFTWARE HISTORY
 * Date         Ticket#    Engineer    Description
 * ------------ ---------- ----------- --------------------------
 * 18 APR 2008  ###        lvenable    Initial creation
 * 20 APR 2015  4027       randerso    Renamed ProductStateEnum with an initial capital
 * 
 * </pre>
 * 
 * @author lvenable
 * @version 1.0
 * 
 */
public interface IProductTab {
    /**
     * Remove a product tab.
     * 
     * @param tabName
     *            Tab name.
     */
    void removeProductTab(String tabName);

    /**
     * Set the state of the product tab.
     * 
     * @param state
     *            Product state.
     * @param productName
     */
    void setTabState(ConfigData.ProductStateEnum state, String productName);

    /**
     * Update the status from the component.
     * 
     * @param status
     */
    void updateStatus(String significance, String status);
}
