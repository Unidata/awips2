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
package com.raytheon.uf.viz.collaboration.ui;

import java.util.Collections;
import java.util.HashSet;
import java.util.Map;
import java.util.Set;

import com.raytheon.uf.viz.collaboration.comm.identity.info.SiteConfig;
import com.raytheon.uf.viz.collaboration.comm.identity.info.SiteConfig.ListEntry;
import com.raytheon.uf.viz.collaboration.comm.identity.info.SiteConfig.ListType;

/**
 * Configuration that determines if the user should see messages from other
 * sites
 * 
 * <pre>
 * 
 * SOFTWARE HISTORY
 * 
 * Date         Ticket#    Engineer    Description
 * ------------ ---------- ----------- --------------------------
 * Oct 9, 2014  3708      bclement     Initial creation
 * 
 * </pre>
 * 
 * @author bclement
 * @version 1.0
 */
public class SiteVisiblityConfig {

    private final String actingSite;

    private final Set<String> sites;
    
    private final Map<String, ListEntry> userSpecificConfigs;

    private final ListType listType;
    
    /**
     * @param config
     * @param userSpecificConfigs
     */
    public SiteVisiblityConfig(SiteConfig config,
            Map<String, ListEntry> userSpecificConfigs) {
        ListType lt = config.getListType();
        if (lt == null) {
            lt = ListType.WHITELIST;
        }
        this.listType = lt;
        this.actingSite = config.getSite();
        this.userSpecificConfigs = userSpecificConfigs;
        ListEntry[] listEntries = config.getListEntries();
        if (listEntries != null) {
            this.sites = new HashSet<String>(listEntries.length);
            for (ListEntry entry : listEntries) {
                this.sites.add(entry.getValue());
            }
        } else {
            this.sites = Collections.emptySet();
        }
    }

    /**
     * @param actingSite
     * @param sites
     * @param listType
     * @param userSpecificConfigs
     */
    public SiteVisiblityConfig(String actingSite, Set<String> sites,
            ListType listType, Map<String, ListEntry> userSpecificConfigs) {
        this.sites = sites;
        this.listType = listType;
        this.actingSite = actingSite;
        this.userSpecificConfigs = userSpecificConfigs;
    }

    /**
     * @param site
     * @return true if messages from site should be shown to user
     */
    public boolean isVisible(String site) {
        ListEntry userSettings = userSpecificConfigs.get(site);
        if (userSettings != null) {
            return !userSettings.isRemoved();
        } else {
            if (isWhitelist()) {
                return sites.contains(site);
            } else {
                return !sites.contains(site);
            }
        }
    }

    /**
     * Allow messages from site to be seen
     * 
     * @param site
     */
    public void show(String site) {
        userSpecificConfigs.put(site, new ListEntry(site, false));
    }

    /**
     * Don't show messages from site
     * 
     * @param site
     */
    public void hide(String site) {
        userSpecificConfigs.put(site, new ListEntry(site, true));
    }

    /**
     * @return filter list for sites. see {@link #isWhitelist()}
     */
    public String[] getSites() {
        return sites.toArray(new String[0]);
    }

    /**
     * @return true if filter list is a whitelist, false if it is a blacklist
     */
    public boolean isWhitelist() {
        return listType.equals(ListType.WHITELIST);
    }

    /**
     * @return the actingSite
     */
    public String getActingSite() {
        return actingSite;
    }

    /**
     * @return the userSpecificConfigs
     */
    public Map<String, ListEntry> getUserSpecificConfigs() {
        return userSpecificConfigs;
    }

}
