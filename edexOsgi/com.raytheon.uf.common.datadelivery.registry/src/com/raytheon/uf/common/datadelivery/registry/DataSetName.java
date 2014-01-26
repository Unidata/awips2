package com.raytheon.uf.common.datadelivery.registry;

import java.util.Map;

import javax.xml.bind.annotation.XmlAccessType;
import javax.xml.bind.annotation.XmlAccessorType;
import javax.xml.bind.annotation.XmlAttribute;
import javax.xml.bind.annotation.XmlRootElement;
import javax.xml.bind.annotation.adapters.XmlJavaTypeAdapter;

import com.raytheon.uf.common.registry.annotations.RegistryObject;
import com.raytheon.uf.common.registry.annotations.RegistryObjectAssociation;
import com.raytheon.uf.common.registry.annotations.RegistryObjectDescription;
import com.raytheon.uf.common.registry.annotations.RegistryObjectName;
import com.raytheon.uf.common.registry.annotations.RegistryObjectOwner;
import com.raytheon.uf.common.registry.annotations.RegistryObjectVersion;
import com.raytheon.uf.common.registry.annotations.SlotAttribute;
import com.raytheon.uf.common.registry.ebxml.MapValuesResolver;
import com.raytheon.uf.common.serialization.XmlGenericMapAdapter;
import com.raytheon.uf.common.serialization.annotations.DynamicSerialize;
import com.raytheon.uf.common.serialization.annotations.DynamicSerializeElement;

/**
 * Abstract Meta Data object
 * 
 * <pre>
 * 
 * SOFTWARE HISTORY
 * Date         Ticket#    Engineer    Description
 * ------------ ---------- ----------- --------------------------
 * Feb 20, 2011 218        dhladky     Initial creation
 * May 15, 2012 455        jspinks     Added Registry annotations 
 * 8/3/2012     724        bphillip    Added more registry annotations
 * Aug 22, 2012 0743       djohnson    Store data type as an enum.
 * Sep 07, 2012 1102       djohnson    Add {@code @XmlRootElement}.
 * Nov 19, 2012 1166       djohnson    Clean up JAXB representation of registry objects.
 * jan 23, 2013   2584     dhladky     Versions.
 * 
 * </pre>
 * 
 * @author dhladky
 * @version 1.0
 */

@XmlRootElement
@XmlAccessorType(XmlAccessType.NONE)
@DynamicSerialize
@RegistryObject(value = { "providerName", "dataSetType", "dataSetName" }, storeContent = false)
@RegistryObjectVersion(value = 1.0f)
public class DataSetName {

    @RegistryObjectOwner
    @XmlAttribute
    @DynamicSerializeElement
    @SlotAttribute
    protected String providerName;

    @RegistryObjectDescription
    @RegistryObjectName
    @XmlAttribute
    @DynamicSerializeElement
    @SlotAttribute
    protected String dataSetName;

    @XmlAttribute
    @DynamicSerializeElement
    @SlotAttribute
    protected DataType dataSetType;

    @DynamicSerializeElement
    @RegistryObjectAssociation(MapValuesResolver.class)
    @XmlJavaTypeAdapter(value = XmlGenericMapAdapter.class, type = Map.class)
    protected Map<String, Parameter> parameters;

    public DataSetName() {

    }

    public Map<String, Parameter> getParameters() {
        return parameters;
    }

    public void setParameters(Map<String, Parameter> map) {
        this.parameters = map;
    }

    public String getDataSetName() {
        return dataSetName;
    }

    public void setDataSetName(String dataSetName) {
        this.dataSetName = dataSetName;
    }

    public DataType getDataSetType() {
        return dataSetType;
    }

    public void setDataSetType(DataType dataSetType) {
        this.dataSetType = dataSetType;
    }

    public String getProviderName() {
        return providerName;
    }

    public void setProviderName(String providerName) {
        this.providerName = providerName;
    }

}
