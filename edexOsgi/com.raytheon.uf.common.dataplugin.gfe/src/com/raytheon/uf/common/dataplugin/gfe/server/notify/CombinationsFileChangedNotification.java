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
package com.raytheon.uf.common.dataplugin.gfe.server.notify;

import com.raytheon.uf.common.message.WsId;
import com.raytheon.uf.common.serialization.annotations.DynamicSerialize;
import com.raytheon.uf.common.serialization.annotations.DynamicSerializeElement;

/**
 * Combinations File Changed Notification
 * 
 * <pre>
 * 
 * SOFTWARE HISTORY
 * 
 * Date         Ticket#    Engineer    Description
 * ------------ ---------- ----------- --------------------------
 * Feb 5, 2014   #2591     randerso     Initial creation
 * 
 * </pre>
 * 
 * @author randerso
 * @version 1.0
 */

@DynamicSerialize
public class CombinationsFileChangedNotification extends GfeNotification {

    @DynamicSerializeElement
    private String combinationsFileName;

    @DynamicSerializeElement
    private WsId whoChanged;

    /**
     * default constructor for serialization
     */
    public CombinationsFileChangedNotification() {
        super();
    }

    public CombinationsFileChangedNotification(String combinationsFileName,
            WsId whoChanged, String siteID) {
        super();
        this.siteID = siteID;
        this.combinationsFileName = combinationsFileName;
        this.whoChanged = whoChanged;
    }

    public String getCombinationsFileName() {
        return combinationsFileName;
    }

    public void setCombinationsFileName(String combinationsFileName) {
        this.combinationsFileName = combinationsFileName;
    }

    public WsId getWhoChanged() {
        return whoChanged;
    }

    public void setWhoChanged(WsId whoChanged) {
        this.whoChanged = whoChanged;
    }

}
