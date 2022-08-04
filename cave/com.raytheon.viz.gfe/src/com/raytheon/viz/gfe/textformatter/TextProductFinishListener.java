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
 * Listener for when a text formatter script finishes
 * 
 * <pre>
 * SOFTWARE HISTORY
 * Date         Ticket#    Engineer    Description
 * ------------ ---------- ----------- --------------------------
 * Oct 15, 2008            njensen     Initial creation
 * Apr 20, 2015  4027      randerso    Renamed ProductStateEnum with an initial capital
 * 
 * </pre>
 * 
 * @author njensen
 * @version 1.0
 */

public interface TextProductFinishListener {

    public void textProductFinished(String productText,
            ConfigData.ProductStateEnum state);

    public void startProgressBar(ConfigData.ProductStateEnum state);

    public void stopProgressBar(ConfigData.ProductStateEnum state);

    public void textProductQueued(ConfigData.ProductStateEnum state);

}
