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
package com.raytheon.uf.viz.collaboration.comm.identity.info;

import java.util.List;

import javax.xml.bind.annotation.XmlAccessType;
import javax.xml.bind.annotation.XmlAccessorType;
import javax.xml.bind.annotation.XmlElement;
import javax.xml.bind.annotation.XmlRootElement;

/**
 * Persisted site configuration information
 * 
 * <pre>
 * 
 * SOFTWARE HISTORY
 * 
 * Date         Ticket#    Engineer    Description
 * ------------ ---------- ----------- --------------------------
 * Jun 12, 2012            mnash       Initial creation
 * Jan 08, 2014 2563       bclement    added format/parse methods to HostConfig
 * Oct 10, 2014 3708       bclement    moved HostConfig and SiteConfig to separate files
 * 
 * </pre>
 * 
 * @author mnash
 * @version 1.0
 */

@XmlRootElement
@XmlAccessorType(XmlAccessType.NONE)
public class SiteConfigInformation {

    public static final String ROLE_NAME = "Role";

    public static final String SITE_NAME = "Site";

    @XmlElement
    private List<HostConfig> server;

    @XmlElement
    private List<SiteConfig> config;

    /**
     * @return the hosts
     */
    public List<HostConfig> getServer() {
        return server;
    }

    /**
     * @param hosts
     *            the hosts to set
     */
    public void setServer(List<HostConfig> server) {
        this.server = server;
    }

    /**
     * @return the infos
     */
    public List<SiteConfig> getConfig() {
        return config;
    }

    /**
     * @param infos
     *            the infos to set
     */
    public void setConfig(List<SiteConfig> config) {
        this.config = config;
    }

}