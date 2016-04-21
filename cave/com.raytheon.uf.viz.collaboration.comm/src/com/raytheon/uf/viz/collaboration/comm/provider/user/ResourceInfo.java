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
package com.raytheon.uf.viz.collaboration.comm.provider.user;

import org.jivesoftware.smack.packet.Presence;
import org.jivesoftware.smack.packet.Presence.Mode;
import org.jivesoftware.smack.packet.Presence.Type;

/**
 * Information for an XMPP resource (client ie pidgin)
 * 
 * <pre>
 * 
 * SOFTWARE HISTORY
 * 
 * Date         Ticket#    Engineer    Description
 * ------------ ---------- ----------- --------------------------
 * Apr 22, 2014 2822       bclement     Initial creation
 * 
 * </pre>
 * 
 * @author bclement
 * @version 1.0
 */
public class ResourceInfo {

    public static final String VERSION_KEY = "urn:uf:viz:collaboration:version";

    private final String resourceName;

    private String collaborationVersion;

    private Type lastType;

    private Mode lastMode;

    /**
     * @param resourceName
     * @param presence
     */
    public ResourceInfo(String resourceName, Presence presence) {
        this.resourceName = resourceName;
        updateInfo(presence);
    }

    /**
     * Update resource information from presence
     * 
     * @param presence
     */
    public void updateInfo(Presence presence) {
        Object version = presence.getProperty(VERSION_KEY);
        if (version != null) {
            this.collaborationVersion = version.toString();
        }
        this.lastType = presence.getType();
        this.lastMode = presence.getMode();
    }

    /**
     * @return true if this resource supports shared displays
     */
    public boolean supportsSharedDisplays() {
        return this.collaborationVersion != null;
    }

    /**
     * @return the resourceName
     */
    public String getResourceName() {
        return resourceName;
    }

    /**
     * @return the collaborationVersion
     */
    public String getCollaborationVersion() {
        return collaborationVersion;
    }

    /**
     * @param collaborationVersion
     *            the collaborationVersion to set
     */
    public void setCollaborationVersion(String collaborationVersion) {
        this.collaborationVersion = collaborationVersion;
    }

    /**
     * @return the lastType
     */
    public Type getLastType() {
        return lastType;
    }

    /**
     * @param lastType
     *            the lastType to set
     */
    public void setLastType(Type lastType) {
        this.lastType = lastType;
    }

    /**
     * @return the lastMode
     */
    public Mode getLastMode() {
        return lastMode;
    }

    /**
     * @param lastMode
     *            the lastMode to set
     */
    public void setLastMode(Mode lastMode) {
        this.lastMode = lastMode;
    }

}
