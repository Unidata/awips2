package com.raytheon.uf.common.monitor.xml;

import java.util.ArrayList;

import javax.xml.bind.annotation.XmlAccessType;
import javax.xml.bind.annotation.XmlAccessorType;
import javax.xml.bind.annotation.XmlElement;
import javax.xml.bind.annotation.XmlElements;
import javax.xml.bind.annotation.XmlRootElement;

import com.raytheon.uf.common.dataplugin.radar.util.RadarsInUseUtil;
import com.raytheon.uf.common.serialization.ISerializableObject;

@XmlRootElement(name = "SCANSiteRunConfig")
@XmlAccessorType(XmlAccessType.NONE)
public class SCANSiteRunConfigXML implements ISerializableObject {

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
                        .equals(RadarsInUseUtil.DIAL_CONSTANT)) {
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

}
