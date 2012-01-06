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
package com.raytheon.uf.common.dataplugin.gfe.request;

import com.raytheon.uf.common.serialization.annotations.DynamicSerialize;
import com.raytheon.uf.common.serialization.annotations.DynamicSerializeElement;

/**
 * TODO Add Description
 * 
 * <pre>
 * 
 * SOFTWARE HISTORY
 * 
 * Date         Ticket#    Engineer    Description
 * ------------ ---------- ----------- --------------------------
 * Jan 21, 2011            randerso     Initial creation
 * 
 * </pre>
 * 
 * @author randerso
 * @version 1.0
 */

@DynamicSerialize
public class GetIscSendStatusRequest extends AbstractGfeRequest {

    @DynamicSerialize
    public static class IscSendStatus {
        @DynamicSerializeElement
        private boolean sendISConSave;

        @DynamicSerializeElement
        private boolean sendISConPublish;

        @DynamicSerializeElement
        private boolean requestISC;

        public IscSendStatus() {
        }

        public IscSendStatus(boolean sendISConSave, boolean sendISConPublish,
                boolean requestISC) {
            super();
            this.sendISConSave = sendISConSave;
            this.sendISConPublish = sendISConPublish;
            this.requestISC = requestISC;
        }

        /**
         * @return the sendISConSave
         */
        public boolean isSendISConSave() {
            return sendISConSave;
        }

        /**
         * @param sendISConSave
         *            the sendISConSave to set
         */
        public void setSendISConSave(boolean sendISConSave) {
            this.sendISConSave = sendISConSave;
        }

        /**
         * @return the sendISConPublish
         */
        public boolean isSendISConPublish() {
            return sendISConPublish;
        }

        /**
         * @param sendISConPublish
         *            the sendISConPublish to set
         */
        public void setSendISConPublish(boolean sendISConPublish) {
            this.sendISConPublish = sendISConPublish;
        }

        /**
         * @return the requestISC
         */
        public boolean isRequestISC() {
            return requestISC;
        }

        /**
         * @param requestISC
         *            the requestISC to set
         */
        public void setRequestISC(boolean requestISC) {
            this.requestISC = requestISC;
        }

    }

    private String siteID;

    /**
     * @return the siteID
     */
    @Override
    public String getSiteID() {
        return siteID;
    }

    /**
     * @param siteID
     *            the siteID to set
     */
    @Override
    public void setSiteID(String siteID) {
        this.siteID = siteID;
    }
}
