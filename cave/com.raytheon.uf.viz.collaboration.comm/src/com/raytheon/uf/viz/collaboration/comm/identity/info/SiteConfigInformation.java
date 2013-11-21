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
import javax.xml.bind.annotation.XmlAttribute;
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
 * Jun 12, 2012            mnash     Initial creation
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

    @XmlAccessorType(XmlAccessType.NONE)
    public static class HostConfig {
        @XmlAttribute
        private String hostname;

        @XmlAttribute
        private String prettyName;

        /**
         * @return the hostname
         */
        public String getHostname() {
            return hostname;
        }

        /**
         * @param hostname
         *            the hostname to set
         */
        public void setHostname(String hostname) {
            this.hostname = hostname;
        }

        /**
         * @return the prettyName
         */
        public String getPrettyName() {
            return prettyName;
        }

        /**
         * @param prettyName
         *            the prettyName to set
         */
        public void setPrettyName(String prettyName) {
            this.prettyName = prettyName;
        }
    }

    @XmlAccessorType(XmlAccessType.NONE)
    public static class SiteConfig {

        @XmlAttribute
        private String site;

        @XmlElement
        private String[] subscribedSites;

        @XmlElement
        private String[] roles;

        /**
         * @return the site
         */
        public String getSite() {
            return site;
        }

        /**
         * @param site
         *            the site to set
         */
        public void setSite(String site) {
            this.site = site;
        }

        /**
         * @return the subscribedSites
         */
        public String[] getSubscribedSites() {
            return subscribedSites;
        }

        /**
         * @param subscribedSites
         *            the subscribedSites to set
         */
        public void setSubscribedSites(String[] subscribedSites) {
            this.subscribedSites = subscribedSites;
        }

        /**
         * @return the role
         */
        public String[] getRoles() {
            return roles;
        }

        /**
         * @param role
         *            the role to set
         */
        public void setRoles(String[] roles) {
            this.roles = roles;
        }
    }
}