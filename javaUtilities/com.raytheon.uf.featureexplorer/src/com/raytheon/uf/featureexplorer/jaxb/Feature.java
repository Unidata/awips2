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
package com.raytheon.uf.featureexplorer.jaxb;

import java.util.ArrayList;
import java.util.List;

import javax.xml.bind.annotation.XmlAccessType;
import javax.xml.bind.annotation.XmlAccessorType;
import javax.xml.bind.annotation.XmlAttribute;
import javax.xml.bind.annotation.XmlElement;
import javax.xml.bind.annotation.XmlElements;
import javax.xml.bind.annotation.XmlRootElement;

/**
 * JAXB-able POJO representation of an Eclipse feature.
 * 
 * <pre>
 * SOFTWARE HISTORY
 * Date         Ticket#    Engineer    Description
 * ------------ ---------- ----------- --------------------------
 * ?            ?          ?           Initial creation
 * Jun 16, 2016 5694       bkowal      Added {@link #requires}.
 * </pre>
 */
@XmlRootElement
@XmlAccessorType(XmlAccessType.NONE)
public class Feature {

    @XmlAttribute
    private String id;

    @XmlAttribute
    private String label;

    @XmlAttribute
    private String version;

    @XmlAttribute(name = "provider-name")
    private String providerName;

    @XmlElement(name = "requires", required = false)
    private Requires requires;

    @XmlElements({ @XmlElement(name = "includes", type = Includes.class) })
    private List<Includes> includes;

    @XmlElements({ @XmlElement(name = "plugin", type = Plugin.class) })
    private List<Plugin> plugins;

    public Feature() {
        this.includes = new ArrayList<Includes>();
        this.plugins = new ArrayList<Plugin>();
    }

    public Feature(String anId, String aLabel, String aVersion,
            String aProviderName, List<Includes> includesList,
            List<Plugin> pluginsList) {
        this.includes = new ArrayList<Includes>(includesList);
        this.plugins = new ArrayList<Plugin>(pluginsList);
        this.id = anId;
        this.label = aLabel;
        this.version = aVersion;
        this.providerName = aProviderName;
    }

    public List<Includes> getIncludes() {
        return this.includes;
    }

    public List<Plugin> getPlugins() {
        return this.plugins;
    }

    public String getId() {
        return this.id;
    }

    public String getLabel() {
        return this.label;
    }

    public String getVersion() {
        return this.version;
    }

    public String getProviderName() {
        return this.providerName;
    }

    public void setId(String anId) {
        this.id = anId;
    }

    public void setLabel(String aLabel) {
        this.label = aLabel;
    }

    public void setVersion(String aVersion) {
        this.version = aVersion;
    }

    public void setProviderName(String aProviderName) {
        this.providerName = aProviderName;
    }

    public void setPlugins(List<Plugin> pluginsList) {
        this.plugins = new ArrayList<Plugin>(pluginsList);
    }

    public Requires getRequires() {
        return requires;
    }

    public void setRequires(Requires requires) {
        this.requires = requires;
    }

}
