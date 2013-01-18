package com.raytheon.uf.common.datadelivery.retrieval.xml;

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

import java.io.Serializable;

import javax.xml.bind.annotation.XmlAccessType;
import javax.xml.bind.annotation.XmlAccessorType;
import javax.xml.bind.annotation.XmlElement;
import javax.xml.bind.annotation.adapters.XmlJavaTypeAdapter;

import com.raytheon.uf.common.datadelivery.registry.Coverage;
import com.raytheon.uf.common.datadelivery.registry.CoverageAdapter;
import com.raytheon.uf.common.datadelivery.registry.Ensemble;
import com.raytheon.uf.common.datadelivery.registry.Parameter;
import com.raytheon.uf.common.datadelivery.registry.Time;
import com.raytheon.uf.common.serialization.ISerializableObject;
import com.raytheon.uf.common.serialization.annotations.DynamicSerialize;
import com.raytheon.uf.common.serialization.annotations.DynamicSerializeElement;

/**
 * Attribute, product from provider XML
 * 
 * <pre>
 * 
 * SOFTWARE HISTORY
 * Date         Ticket#    Engineer    Description
 * ------------ ---------- ----------- --------------------------
 * Jan 17, 2011    191      dhladky     Initial creation
 * 
 * </pre>
 * 
 * @author dhladky
 * @version 1.0
 */

@XmlAccessorType(XmlAccessType.NONE)
@DynamicSerialize
public class RetrievalAttribute implements ISerializableObject, Serializable {

    /**
     * 
     * Enumeration of the data types
     * 
     * @author dhladky
     * @version 1.0
     */
    public enum DATA_TYPE {

        GRID("GRID"), RADAR("RADAR"), POINT_DATA("POINT_DATA");

        private final String type;

        private DATA_TYPE(String name) {
            type = name;
        }

        public String getAttributeType() {
            return type;
        }
    }

    private static final long serialVersionUID = -4810121582849653605L;

    @XmlElement
    @DynamicSerializeElement
    private Parameter parameter;

    @XmlElement
    @DynamicSerializeElement
    private String subName;

    @XmlElement(name = "plugin")
    @DynamicSerializeElement
    private String plugin;

    @XmlElement(name = "coverage")
    @XmlJavaTypeAdapter(value = CoverageAdapter.class)
    @DynamicSerializeElement
    private Coverage coverage;

    @XmlElement(name = "time", type = Time.class)
    @DynamicSerializeElement
    private Time time;

    @XmlElement
    @DynamicSerializeElement
    private Ensemble ensemble;

    @XmlElement(name = "provider")
    @DynamicSerializeElement
    private String provider;

    public RetrievalAttribute() {

    }

    public Coverage getCoverage() {
        return coverage;
    }

    public Parameter getParameter() {
        return parameter;
    }

    public String getPlugin() {
        return plugin;
    }

    public String getProvider() {
        return provider;
    }

    public Time getTime() {
        return time;
    }

    public void setCoverage(Coverage coverage) {
        this.coverage = coverage;
    }

    public void setParameter(Parameter parameter) {
        this.parameter = parameter;
    }

    public void setPlugin(String plugin) {
        this.plugin = plugin;
    }

    public void setProvider(String provider) {
        this.provider = provider;
    }

    public void setTime(Time time) {
        this.time = time;
    }

    public void setSubName(String subName) {
        this.subName = subName;
    }

    public String getSubName() {
        return subName;
    }

    public Ensemble getEnsemble() {
        return ensemble;
    }

    public void setEnsemble(Ensemble ensemble) {
        this.ensemble = ensemble;
    }

}
