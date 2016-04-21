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

import javax.xml.bind.annotation.XmlAccessType;
import javax.xml.bind.annotation.XmlAccessorType;
import javax.xml.bind.annotation.XmlAttribute;
import javax.xml.bind.annotation.XmlElement;
import javax.xml.bind.annotation.XmlValue;

/**
 * Collaboration site configuration object. Stores site specific white/black
 * lists used for message filtering and forecaster roles. Roles are not
 * restricted by an enum, but usually are short term or long term forecaster.
 * 
 * <pre>
 * 
 * SOFTWARE HISTORY
 * 
 * Date         Ticket#    Engineer    Description
 * ------------ ---------- ----------- --------------------------
 * Oct 9, 2014  3708      bclement     moved from SiteConfigurationInformation
 *                                      added blacklist support
 * 
 * </pre>
 * 
 * @author bclement
 * @version 1.0
 */
@XmlAccessorType(XmlAccessType.NONE)
public class SiteConfig {

    public static enum ListType {
        WHITELIST, BLACKLIST
    };

    @XmlAccessorType(XmlAccessType.NONE)
    public static class ListEntry {
        @XmlValue
        private String value;

        @XmlAttribute
        private boolean removed = false;

        /**
         * 
         */
        public ListEntry() {
        }

        public ListEntry(String value, boolean removed) {
            this.value = value;
            this.removed = removed;
        }

        /**
         * @return the value
         */
        public String getValue() {
            return value;
        }

        /**
         * @param value
         *            the value to set
         */
        public void setValue(String value) {
            this.value = value;
        }

        /**
         * @return the removed
         */
        public boolean isRemoved() {
            return removed;
        }

        /**
         * @param removed
         *            the removed to set
         */
        public void setRemoved(boolean removed) {
            this.removed = removed;
        }

    }

    @XmlAttribute
    private String site;

    @XmlAttribute
    private ListType listType;

    @XmlElement(name = "listEntry")
    private ListEntry[] listEntries;

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

    /**
     * @return the listType
     */
    public ListType getListType() {
        return listType;
    }

    /**
     * @param listType
     *            the listType to set
     */
    public void setListType(ListType listType) {
        this.listType = listType;
    }

    /**
     * @return the listEntries
     */
    public ListEntry[] getListEntries() {
        return listEntries;
    }

    /**
     * @param listEntries
     *            the listEntries to set
     */
    public void setListEntries(ListEntry[] listEntries) {
        this.listEntries = listEntries;
    }

}
