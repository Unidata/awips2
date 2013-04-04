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
package com.raytheon.uf.viz.datadelivery.browser;

import java.util.List;

import com.raytheon.uf.viz.datadelivery.common.ui.IDataLoad;
import com.raytheon.uf.viz.datadelivery.filter.MetaDataManager;

/**
 * Thread used to load in data types.
 * 
 * <pre>
 * 
 * SOFTWARE HISTORY
 * 
 * Date         Ticket#    Engineer    Description
 * ------------ ---------- ----------- --------------------------
 * Mar 16, 2012            lvenable     Initial creation
 * 
 * </pre>
 * 
 * @author lvenable
 * @version 1.0
 */

public class BrowserDataTypeThread implements Runnable {

    private IDataLoad callback = null;

    /**
     * Constructor.
     */
    public BrowserDataTypeThread(IDataLoad callback) {
        this.callback = callback;
    }

    /*
     * (non-Javadoc)
     * 
     * @see java.lang.Runnable#run()
     */
    @Override
    public void run() {
        List<String> dataTypes = MetaDataManager.getInstance().getAvailableDataTypes();
        callback.loadDataTypeComplete(dataTypes);
    }

}
