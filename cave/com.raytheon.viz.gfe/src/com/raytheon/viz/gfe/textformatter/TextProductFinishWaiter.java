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
package com.raytheon.viz.gfe.textformatter;

import com.raytheon.viz.gfe.dialogs.formatterlauncher.ConfigData;
import com.raytheon.viz.gfe.dialogs.formatterlauncher.ConfigData.ProductStateEnum;

/**
 * Listens for a text product to finish and then returns the product with
 * waitAndGetProduct(). Intended for command line/automated test running of the
 * text formatter.
 * 
 * <pre>
 * 
 * SOFTWARE HISTORY
 * Date         Ticket#    Engineer    Description
 * ------------ ---------- ----------- --------------------------
 * Apr 30, 2009            njensen     Initial creation
 * Apr 20, 2015  4027      randerso    Renamed ProductStateEnum with an initial capital
 * 
 * </pre>
 * 
 * @author njensen
 * @version 1.0
 */

public class TextProductFinishWaiter implements TextProductFinishListener {

    private String product;

    private ProductStateEnum state;

    /*
     * (non-Javadoc)
     * 
     * @see
     * com.raytheon.viz.gfe.textformatter.TextProductFinishListener#startProgressBar
     * ()
     */
    @Override
    public void startProgressBar(ConfigData.ProductStateEnum state) {
        this.state = state;
    }

    /*
     * (non-Javadoc)
     * 
     * @see
     * com.raytheon.viz.gfe.textformatter.TextProductFinishListener#stopProgressBar
     * ()
     */
    @Override
    public void stopProgressBar(ConfigData.ProductStateEnum state) {
        this.state = state;
    }

    /*
     * (non-Javadoc)
     * 
     * @see com.raytheon.viz.gfe.textformatter.TextProductFinishListener#
     * textProductQueued()
     */
    @Override
    public void textProductQueued(ConfigData.ProductStateEnum state) {
        this.state = state;
    }

    /*
     * (non-Javadoc)
     * 
     * @seecom.raytheon.viz.gfe.textformatter.TextProductFinishListener#
     * textProductFinished(java.lang.String)
     */
    @Override
    public void textProductFinished(String productText,
            ConfigData.ProductStateEnum state) {
        this.product = productText;
        this.state = state;
    }

    public String waitAndGetProduct() throws InterruptedException {
        while (!state.isComplete()) {
            Thread.sleep(100);
        }
        if (state == ConfigData.ProductStateEnum.Failed) {
            product = "";
        }
        return product;
    }

    public ConfigData.ProductStateEnum getState() {
        return this.state;
    }
}
