package com.raytheon.uf.common.datadelivery.harvester;

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

import java.util.List;

import javax.xml.bind.annotation.XmlAccessType;
import javax.xml.bind.annotation.XmlAccessorType;
import javax.xml.bind.annotation.XmlAttribute;
import javax.xml.bind.annotation.XmlElement;
import javax.xml.bind.annotation.XmlElements;
import javax.xml.bind.annotation.XmlRootElement;

import com.raytheon.uf.common.datadelivery.registry.Parameter;
import com.raytheon.uf.common.serialization.ISerializableObject;

/**
 * 
 * Configuration for an individual layer running a LayerCollector instance
 * 
 * <pre>
 * SOFTWARE HISTORY
 * Date         Ticket#    Engineer    Description
 * ------------ ---------- ----------- --------------------------
 * 08/21/2012   754       dhladky      initial
 * </pre>
 * 
 * @author dhladky
 * @version 1.0
 */

@XmlRootElement(name = "layer")
@XmlAccessorType(XmlAccessType.NONE)
public class ConfigLayer implements ISerializableObject {

    // corresponds to the feature name/type
    @XmlAttribute(name = "name", required = true)
    private String name;
   
    // corresponds to the feature namespace
    @XmlAttribute(name = "namespace", required = true)
    private String namespace;

    @XmlElement(name = "minx", required = true)
    private Double minx;

    @XmlElement(name = "maxx", required = true)
    private Double maxx;

    @XmlElement(name = "miny", required = true)
    private Double miny;

    @XmlElement(name = "maxy", required = true)
    private Double maxy;
    
    @XmlElement(name = "crs", required = true)
    private String crs;
    
    @XmlElements({ @XmlElement(name = "parameter", type = Parameter.class, required = true) })
    private List<Parameter> parameters;

    public String getName() {
        return name;
    }

    public void setName(String name) {
        this.name = name;
    }

    public Double getMinx() {
        return minx;
    }

    public void setMinx(Double minx) {
        this.minx = minx;
    }

    public Double getMaxx() {
        return maxx;
    }

    public void setMaxx(Double maxx) {
        this.maxx = maxx;
    }

    public Double getMiny() {
        return miny;
    }

    public void setMiny(Double miny) {
        this.miny = miny;
    }

    public Double getMaxy() {
        return maxy;
    }

    public void setMaxy(Double maxy) {
        this.maxy = maxy;
    }

    public void setParameters(List<Parameter> parameters) {
        this.parameters = parameters;
    }

    public List<Parameter> getParameters() {
        return parameters;
    }

    public Parameter getParameter(String name) {
        Parameter parm = null;
        for (Parameter lparm : getParameters()) {
            if (lparm.getName().equals(name)) {
                parm = lparm;
                break;
            }
        }
        return parm;
    }

    public void setCrs(String crs) {
        this.crs = crs;
    }

    public String getCrs() {
        return crs;
    }

    public void setNamespace(String namespace) {
        this.namespace = namespace;
    }

    public String getNamespace() {
        return namespace;
    }
}
