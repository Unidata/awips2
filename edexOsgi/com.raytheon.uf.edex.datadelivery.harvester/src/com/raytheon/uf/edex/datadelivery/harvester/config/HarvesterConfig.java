package com.raytheon.uf.edex.datadelivery.harvester.config;

import javax.xml.bind.annotation.XmlAccessType;
import javax.xml.bind.annotation.XmlAccessorType;
import javax.xml.bind.annotation.XmlElement;
import javax.xml.bind.annotation.XmlRootElement;

import com.raytheon.uf.common.datadelivery.registry.Provider;
import com.raytheon.uf.common.serialization.ISerializableObject;
import com.raytheon.uf.common.serialization.annotations.DynamicSerializeElement;

/**
 * HarveterConfig
 * 
 * <pre>
 * 
 * SOFTWARE HISTORY
 * Date         Ticket#    Engineer    Description
 * ------------ ---------- ----------- --------------------------
 * 12 Sept, 2012   1038      dhladky     Initial creation
 * 
 * </pre>
 * 
 * @author dhladky
 * @version 1.0
 */

@XmlRootElement(name = "harvester")
@XmlAccessorType(XmlAccessType.NONE)
public class HarvesterConfig implements ISerializableObject {

    @XmlElement(name = "provider", type = Provider.class)
    @DynamicSerializeElement
    private Provider provider;

    @XmlElement(name = "primaryRegistryHost")
    @DynamicSerializeElement
    private String primaryRegistryHost;

    @XmlElement(name = "secondaryRegistryHost")
    @DynamicSerializeElement
    private String secondaryRegistryHost;

    @XmlElement(name = "tertiaryRegistryHost")
    @DynamicSerializeElement
    private String tertiaryRegistryHost;

    @XmlElement(name = "agent")
    @DynamicSerializeElement
    private Agent agent;

    public HarvesterConfig() {

    }

    public Agent getAgent() {
        return agent;
    }

    public String getPrimaryRegistryHost() {
        return primaryRegistryHost;
    }

    public Provider getProvider() {
        return provider;
    }

    public String getSecondaryRegistryHost() {
        return secondaryRegistryHost;
    }

    public String getTertiaryRegistryHost() {
        return tertiaryRegistryHost;
    }

    public void setAgent(Agent agent) {
        this.agent = agent;
    }

    public void setPrimaryRegistryHost(String primaryRegistryHost) {
        this.primaryRegistryHost = primaryRegistryHost;
    }

    public void setProvider(Provider provider) {
        this.provider = provider;
    }

    public void setSecondaryRegistryHost(String secondaryRegistryHost) {
        this.secondaryRegistryHost = secondaryRegistryHost;
    }

    public void setTertiaryRegistryHost(String tertiaryRegistryHost) {
        this.tertiaryRegistryHost = tertiaryRegistryHost;
    }

}
