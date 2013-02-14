package com.raytheon.uf.edex.datadelivery.retrieval.mapping;

import java.util.List;

import javax.xml.bind.annotation.XmlAccessType;
import javax.xml.bind.annotation.XmlAccessorType;
import javax.xml.bind.annotation.XmlElement;
import javax.xml.bind.annotation.XmlElements;
import javax.xml.bind.annotation.XmlRootElement;

import com.raytheon.uf.common.serialization.ISerializableObject;
import com.raytheon.uf.common.serialization.annotations.DynamicSerialize;
import com.raytheon.uf.common.serialization.annotations.DynamicSerializeElement;

/**
 * List of plugin Route overrides
 * 
 * <pre>
 * 
 * SOFTWARE HISTORY
 * Date         Ticket#    Engineer    Description
 * ------------ ---------- ----------- --------------------------
 * Nov 26, 2011            dhladky     Initial creation
 * 
 * </pre>
 * 
 * @author dhladky
 * @version 1.0
 */

@XmlRootElement(name = "pluginRouteList")
@XmlAccessorType(XmlAccessType.NONE)
@DynamicSerialize
public class PluginRouteList implements ISerializableObject {

    public PluginRouteList() {

    }

    @XmlElements({ @XmlElement(name = "pluginRoute", type = PluginRoute.class) })
    @DynamicSerializeElement
    private List<PluginRoute> pluginRoute;

    public List<PluginRoute> getPluginRoute() {
        return pluginRoute;
    }

    public void setPluginRoute(List<PluginRoute> pluginRoute) {
        this.pluginRoute = pluginRoute;
    }

}
