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
package com.raytheon.uf.common.monitor.xml;

import java.util.ArrayList;

import javax.xml.bind.annotation.XmlAccessType;
import javax.xml.bind.annotation.XmlAccessorType;
import javax.xml.bind.annotation.XmlElement;
import javax.xml.bind.annotation.XmlElements;
import javax.xml.bind.annotation.XmlRootElement;

import com.raytheon.uf.common.dataplugin.binlightning.LightningConstants;
import com.raytheon.uf.common.dataplugin.radar.util.RadarsInUseUtil;

/**
 * Scan site run config
 *
 * <pre>
 *
 * SOFTWARE HISTORY
 *
 * Date         Ticket#    Engineer    Description
 * ------------ ---------- ----------- --------------------------
 * Jan 20, 2015 3949       nabowle     Add lightning source.
 *
 * </pre>
 *
 * @author nabowle
 * @version 1.0
 */
@XmlRootElement(name = "SCANSiteRunConfig")
@XmlAccessorType(XmlAccessType.NONE)
public class SCANSiteRunConfigXML {

    @XmlElement(name = "lightningSource")
    private String lightningSource = LightningConstants.DEFAULT_SOURCE;

    @XmlElements({ @XmlElement(name = "site") })
    private ArrayList<SCANSiteXML> sites;

    public ArrayList<SCANSiteXML> getSites() {
        return sites;
    }

    public void setSite(ArrayList<SCANSiteXML> sites) {
        this.sites = sites;
    }

    public ArrayList<String> getSiteNames() {
        ArrayList<String> isites = new ArrayList<String>();
        if (sites != null) {
            for (SCANSiteXML site : getSites()) {
                isites.add(site.getScanSite());
            }
        }

        return isites;
    }

    public ArrayList<String> getLocalSiteNames() {
        ArrayList<String> isites = new ArrayList<String>();
        if (sites != null) {
            for (SCANSiteXML site : getSites()) {
                if (site.getMenuLocation().equals(
                        RadarsInUseUtil.LOCAL_CONSTANT)) {
                    isites.add(site.getScanSite());
                }
            }
        }

        return isites;
    }

    public ArrayList<String> getDialSiteNames() {
        ArrayList<String> isites = new ArrayList<String>();
        if (sites != null) {
            for (SCANSiteXML site : getSites()) {
                if (site.getMenuLocation()
                        .equals(RadarsInUseUtil.MOSAIC_CONSTANT)) {
                    isites.add(site.getScanSite());
                }
            }
        }

        return isites;
    }

    public void addSite(SCANSiteXML site) {
        if (sites == null) {
            sites = new ArrayList<SCANSiteXML>();
        }

        sites.add(site);
    }

    public String getLightningSource() {
        return this.lightningSource;
    }

    public void setLightningSource(String lightningSource) {
        this.lightningSource = lightningSource;
    }

}
