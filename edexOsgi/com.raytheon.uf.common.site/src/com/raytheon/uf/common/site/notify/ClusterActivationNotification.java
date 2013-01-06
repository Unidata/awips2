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
package com.raytheon.uf.common.site.notify;

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
 * Aug 9, 2011            bphillip     Initial creation
 * 
 * </pre>
 * 
 * @author bphillip
 * @version 1.0
 */

@DynamicSerialize
public class ClusterActivationNotification extends SiteActivationNotification {

    @DynamicSerializeElement
    private boolean clusterActive;

    public ClusterActivationNotification() {
    }

    public ClusterActivationNotification(String primarySite,
            String modifiedSite, String pluginName, ACTIVATIONTYPE type,
            ACTIVATIONSTATUS status, boolean clusterActive) {
        super(primarySite, modifiedSite, pluginName, type, status);
        this.clusterActive = clusterActive;
    }

    /**
     * @return the clusterActive
     */
    public boolean isClusterActive() {
        return clusterActive;
    }

    /**
     * @param clusterActive
     *            the clusterActive to sTet
     */
    public void setClusterActive(boolean clusterActive) {
        this.clusterActive = clusterActive;
    }

    public String toString() {
        StringBuilder buffer = new StringBuilder();

        if (isActivation()) {
            if (isFailure()) {
                buffer.append(this.getModifiedSite());
                buffer.append(" has failed to activate on some or all cluster members.  See logs for details");
            } else {
                buffer.append(this.getModifiedSite());
                buffer.append(" has been successfully activated on all cluster members");
            }
        } else{
            if (isFailure()) {
                buffer.append(this.getModifiedSite());
                buffer.append(" has failed to deactivate on some or all cluster members.  See logs for details");
            } else {
                buffer.append(this.getModifiedSite());
                buffer.append(" has been successfully deactivated on all cluster members");
            }
        }
        return buffer.toString();
    }

}
