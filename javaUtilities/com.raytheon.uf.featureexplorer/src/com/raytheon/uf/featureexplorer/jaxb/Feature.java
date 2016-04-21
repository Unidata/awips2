package com.raytheon.uf.featureexplorer.jaxb;

import java.util.ArrayList;
import java.util.List;

import javax.xml.bind.annotation.XmlAccessType;
import javax.xml.bind.annotation.XmlAccessorType;
import javax.xml.bind.annotation.XmlAttribute;
import javax.xml.bind.annotation.XmlElement;
import javax.xml.bind.annotation.XmlElements;
import javax.xml.bind.annotation.XmlRootElement;

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

    @XmlElements( { @XmlElement(name = "includes", type = Includes.class) })
    private List<Includes> includes;

    @XmlElements( { @XmlElement(name = "plugin", type = Plugin.class) })
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

}
