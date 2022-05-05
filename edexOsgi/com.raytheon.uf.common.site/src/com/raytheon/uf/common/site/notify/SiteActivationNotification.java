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
public class SiteActivationNotification {

    public enum ACTIVATIONTYPE {
        ACTIVATE, DEACTIVATE
    }

    public enum ACTIVATIONSTATUS {
        BEGIN, SUCCESS, FAILURE
    }

    @DynamicSerializeElement
    protected ACTIVATIONTYPE type;

    @DynamicSerializeElement
    protected ACTIVATIONSTATUS status;

    @DynamicSerializeElement
    protected String primarySite;

    @DynamicSerializeElement
    protected String modifiedSite;

    @DynamicSerializeElement
    protected String runMode;

    @DynamicSerializeElement
    protected String serverName;

    @DynamicSerializeElement
    protected String pluginName;

    public SiteActivationNotification() {
    }

    public SiteActivationNotification(String primarySite, String modifiedSite,
            String pluginName, ACTIVATIONTYPE type, ACTIVATIONSTATUS status) {
        this.primarySite = primarySite;
        this.modifiedSite = modifiedSite;
        this.pluginName = pluginName;
        this.type = type;
        this.status = status;
    }

    public SiteActivationNotification(String primarySite, String modifiedSite,
            String runMode, String serverName, String pluginName,
            ACTIVATIONTYPE type, ACTIVATIONSTATUS status) {
        this.primarySite = primarySite;
        this.modifiedSite = modifiedSite;
        this.runMode = runMode;
        this.serverName = serverName;
        this.pluginName = pluginName;
        this.type = type;
        this.status = status;
    }

    public String toString() {
        StringBuffer buffer = new StringBuffer();
        buffer.append(this.pluginName.toUpperCase()).append(":");
        buffer.append(this.status).append(":");
        buffer.append(this.type).append(" ");
        buffer.append(this.modifiedSite.toUpperCase()).append(" on ");
        buffer.append(this.getServerAndRunMode());
        return buffer.toString();
    }

    public boolean isActivation() {
        return this.type.equals(ACTIVATIONTYPE.ACTIVATE);
    }

    public boolean isDeactivation() {
        return this.type.equals(ACTIVATIONTYPE.DEACTIVATE);
    }

    public boolean isBegin() {
        return this.status.equals(ACTIVATIONSTATUS.BEGIN);
    }

    public boolean isSuccess() {
        return this.status.equals(ACTIVATIONSTATUS.SUCCESS);
    }

    public boolean isFailure() {
        return this.status.equals(ACTIVATIONSTATUS.FAILURE);
    }

    public String getServerAndRunMode() {
        return this.serverName + ":" + this.runMode;
    }

    /**
     * @return the type
     */
    public ACTIVATIONTYPE getType() {
        return type;
    }

    /**
     * @param type
     *            the type to set
     */
    public void setType(ACTIVATIONTYPE type) {
        this.type = type;
    }

    /**
     * @return the primarySite
     */
    public String getPrimarySite() {
        return primarySite;
    }

    /**
     * @param primarySite
     *            the primarySite to set
     */
    public void setPrimarySite(String primarySite) {
        this.primarySite = primarySite;
    }

    /**
     * @return the modifiedSite
     */
    public String getModifiedSite() {
        return modifiedSite;
    }

    /**
     * @param modifiedSite
     *            the modifiedSite to set
     */
    public void setModifiedSite(String modifiedSite) {
        this.modifiedSite = modifiedSite;
    }

    /**
     * @return the runMode
     */
    public String getRunMode() {
        return runMode;
    }

    /**
     * @param runMode
     *            the runMode to set
     */
    public void setRunMode(String runMode) {
        this.runMode = runMode;
    }

    /**
     * @return the serverName
     */
    public String getServerName() {
        return serverName;
    }

    /**
     * @param serverName
     *            the serverName to set
     */
    public void setServerName(String serverName) {
        this.serverName = serverName;
    }

    /**
     * @return the pluginName
     */
    public String getPluginName() {
        return pluginName;
    }

    /**
     * @param pluginName
     *            the pluginName to set
     */
    public void setPluginName(String pluginName) {
        this.pluginName = pluginName;
    }

    /**
     * @return the status
     */
    public ACTIVATIONSTATUS getStatus() {
        return status;
    }

    /**
     * @param status
     *            the status to set
     */
    public void setStatus(ACTIVATIONSTATUS status) {
        this.status = status;
    }
}
