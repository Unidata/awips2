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
 * 
 * </pre>
 * 
 * @author njensen
 * @version 1.0
 */

public class TextProductFinishWaiter implements TextProductFinishListener {

    private String product;

    private boolean wait = true;

    /*
     * (non-Javadoc)
     * 
     * @see
     * com.raytheon.viz.gfe.textformatter.TextProductFinishListener#startProgressBar
     * ()
     */
    @Override
    public void startProgressBar(ConfigData.productStateEnum state) {
        wait = true;
    }

    /*
     * (non-Javadoc)
     * 
     * @see
     * com.raytheon.viz.gfe.textformatter.TextProductFinishListener#stopProgressBar
     * ()
     */
    @Override
    public void stopProgressBar(ConfigData.productStateEnum state) {
        wait = false;
    }

    /*
     * (non-Javadoc)
     * 
     * @see com.raytheon.viz.gfe.textformatter.TextProductFinishListener#
     * textProductQueued()
     */
    @Override
    public void textProductQueued() {
        wait = true;
    }

    /*
     * (non-Javadoc)
     * 
     * @seecom.raytheon.viz.gfe.textformatter.TextProductFinishListener#
     * textProductFinished(java.lang.String)
     */
    @Override
    public void textProductFinished(String productText,
            ConfigData.productStateEnum state) {
        this.product = productText;
        wait = false;
    }

    public String waitAndGetProduct() throws InterruptedException {
        while (wait) {
            Thread.sleep(50);
        }
        return product;
    }

}
