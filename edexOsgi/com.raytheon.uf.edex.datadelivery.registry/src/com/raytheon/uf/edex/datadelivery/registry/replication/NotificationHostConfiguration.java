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
package com.raytheon.uf.edex.datadelivery.registry.replication;

import javax.xml.bind.annotation.XmlAccessType;
import javax.xml.bind.annotation.XmlAccessorType;
import javax.xml.bind.annotation.XmlElement;

/**
 * 
 * Host configuration information
 * 
 * <pre>
 * 
 * SOFTWARE HISTORY
 * 
 * Date         Ticket#     Engineer    Description
 * ------------ ----------  ----------- --------------------------
 * 4/9/2013     1802        bphillip    Initial implementation
 * 6/4/2013     1707        bphillip    Renamed and changed fields for clarity
 * 11/20/2013   2534        bphillip    Added reciprocate field
 * </pre>
 * 
 * @author bphillip
 * @version 1
 */
@XmlAccessorType(XmlAccessType.NONE)
public class NotificationHostConfiguration {

    /** The site that this registry is running at */
    @XmlElement
    private String registrySiteName;

    /** Descripion of the host */
    @XmlElement
    private String registryDescription;

    /** The name of the host */
    @XmlElement
    private String registryBaseURL;

    /** True if subscriptions should be reciprocated */
    @XmlElement
    private boolean reciprocate = false;

    public NotificationHostConfiguration() {

    }

    public NotificationHostConfiguration(String registrySiteName,
            String registryDescription, String registryBaseURL) {
        this.registrySiteName = registrySiteName;
        this.registryDescription = registryDescription;
        this.registryBaseURL = registryBaseURL;
    }

    public String getRegistrySiteName() {
        return registrySiteName;
    }

    public void setRegistrySiteName(String registrySiteName) {
        this.registrySiteName = registrySiteName;
    }

    public String getRegistryDescription() {
        return registryDescription;
    }

    public void setRegistryDescription(String registryDescription) {
        this.registryDescription = registryDescription;
    }

    public String getRegistryBaseURL() {
        return registryBaseURL;
    }

    public void setRegistryBaseURL(String registryBaseURL) {
        this.registryBaseURL = registryBaseURL;
    }

    public boolean isReciprocate() {
        return reciprocate;
    }

    public void setReciprocate(boolean reciprocate) {
        this.reciprocate = reciprocate;
    }
}
