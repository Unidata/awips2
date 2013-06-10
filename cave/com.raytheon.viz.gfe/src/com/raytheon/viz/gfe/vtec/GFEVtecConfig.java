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
package com.raytheon.viz.gfe.vtec;

import java.util.Collections;
import java.util.Set;

/**
 * Class to hold exceptions for GFE ETN assignment logic in HazardsTable.py and
 * GFEVtecUtil. Values are intended to be injected via spring from
 * com.raytheon.viz.gfe/res/spring/gfe.xml.
 * 
 * <pre>
 * 
 * SOFTWARE HISTORY
 * 
 * Date         Ticket#    Engineer    Description
 * ------------ ---------- ----------- --------------------------
 * May 14, 2013  #1842     dgilling     Initial creation
 * 
 * </pre>
 * 
 * @author dgilling
 * @version 1.0
 */

public class GFEVtecConfig {

    private static final GFEVtecConfig INSTANCE = new GFEVtecConfig();

    private Set<String> sitesIgnoreNationalEtn = Collections.emptySet();

    private Set<String> nationalEtnPhensigs = Collections.emptySet();

    private Set<String> tropicalEtnPhensigs = Collections.emptySet();

    private GFEVtecConfig() {
    }

    public static GFEVtecConfig getInstance() {
        return INSTANCE;
    }

    /*
     * (non-Javadoc)
     * 
     * @see java.lang.Object#toString()
     */
    @Override
    public String toString() {
        StringBuilder builder = new StringBuilder();
        builder.append("GFEVtecConfig [sitesIgnoreNationalEtn=");
        builder.append(sitesIgnoreNationalEtn);
        builder.append(", nationalEtnPhensigs=");
        builder.append(nationalEtnPhensigs);
        builder.append(", tropicalEtnPhensigs=");
        builder.append(tropicalEtnPhensigs);
        builder.append("]");
        return builder.toString();
    }

    public Set<String> getSitesIgnoreNationalEtn() {
        return sitesIgnoreNationalEtn;
    }

    public Set<String> getNationalEtnPhensigs() {
        return nationalEtnPhensigs;
    }

    public Set<String> getTropicalEtnPhensigs() {
        return tropicalEtnPhensigs;
    }

    public void setSitesIgnoreNationalEtn(Set<String> sitesIgnoreNationalEtn) {
        this.sitesIgnoreNationalEtn = sitesIgnoreNationalEtn;
    }

    public void setNationalEtnPhensigs(Set<String> nationalEtnPhensigs) {
        this.nationalEtnPhensigs = nationalEtnPhensigs;
    }

    public void setTropicalEtnPhensigs(Set<String> tropicalEtnPhensigs) {
        this.tropicalEtnPhensigs = tropicalEtnPhensigs;
    }
}
