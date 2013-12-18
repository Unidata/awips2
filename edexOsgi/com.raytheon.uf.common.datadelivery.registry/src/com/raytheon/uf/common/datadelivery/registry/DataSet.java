package com.raytheon.uf.common.datadelivery.registry;

import java.util.HashMap;
import java.util.Map;

import javax.xml.bind.annotation.XmlAccessType;
import javax.xml.bind.annotation.XmlAccessorType;
import javax.xml.bind.annotation.XmlAttribute;
import javax.xml.bind.annotation.XmlElement;
import javax.xml.bind.annotation.adapters.XmlJavaTypeAdapter;

import org.apache.commons.lang.builder.EqualsBuilder;
import org.apache.commons.lang.builder.HashCodeBuilder;

import com.raytheon.uf.common.datadelivery.registry.Provider.ServiceType;
import com.raytheon.uf.common.registry.annotations.RegistryObject;
import com.raytheon.uf.common.registry.annotations.RegistryObjectAssociation;
import com.raytheon.uf.common.registry.annotations.RegistryObjectDescription;
import com.raytheon.uf.common.registry.annotations.RegistryObjectName;
import com.raytheon.uf.common.registry.annotations.RegistryObjectOwner;
import com.raytheon.uf.common.registry.annotations.SlotAttribute;
import com.raytheon.uf.common.registry.annotations.SlotAttributeConverter;
import com.raytheon.uf.common.registry.ebxml.MapValuesResolver;
import com.raytheon.uf.common.registry.ebxml.slots.KeySetSlotConverter;
import com.raytheon.uf.common.serialization.XmlGenericMapAdapter;
import com.raytheon.uf.common.serialization.annotations.DynamicSerialize;
import com.raytheon.uf.common.serialization.annotations.DynamicSerializeElement;

/**
 * DataSet, wraps up the DataSetMetaData objects.
 * 
 * <pre>
 * 
 * SOFTWARE HISTORY
 * Date         Ticket#    Engineer    Description
 * ------------ ---------- ----------- --------------------------
 * Feb 28, 2011 218        dhladky      Initial creation
 * Aug 02, 2012 955        djohnson     Renamed to DataSet.
 * Aug 10, 2012 1022       djohnson     Move grid specific code to {@link GriddedDataSet}.
 * Aug 22, 2012 0743       djohnson     Store data type as an enum.
 * Sep 07, 2012 1102       djohnson     Remove invalid {@code @XmlRootElement}.
 * Nov 19, 2012 1166       djohnson     Clean up JAXB representation of registry objects.
 * Dec 18, 2013 2636       mpduff       Add a data availability delay for the dataset.
 * 
 * </pre>
 * 
 * @author dhladky
 * @version 1.0
 */
@XmlAccessorType(XmlAccessType.NONE)
@DynamicSerialize
@RegistryObject({ "providerName", "collectionName", "dataSetName" })
public abstract class DataSet<T extends Time, C extends Coverage> {

    @RegistryObjectOwner
    @XmlAttribute
    @DynamicSerializeElement
    @SlotAttribute
    protected String providerName;

    @XmlAttribute
    @DynamicSerializeElement
    @SlotAttribute
    protected String collectionName;

    @RegistryObjectDescription
    @RegistryObjectName
    @XmlAttribute
    @DynamicSerializeElement
    @SlotAttribute
    protected String dataSetName;

    /**
     * Map of parameters and their descriptions.
     */
    @DynamicSerializeElement
    @RegistryObjectAssociation(MapValuesResolver.class)
    @SlotAttribute
    @SlotAttributeConverter(KeySetSlotConverter.class)
    @XmlJavaTypeAdapter(type = Map.class, value = XmlGenericMapAdapter.class)
    protected Map<String, Parameter> parameters = new HashMap<String, Parameter>();

    @XmlElement(name = "coverage")
    @DynamicSerializeElement
    protected C coverage;

    @XmlAttribute
    @DynamicSerializeElement
    @SlotAttribute
    protected DataType dataSetType;

    @XmlElement
    @DynamicSerializeElement
    @SlotAttribute
    @SlotAttributeConverter(TimeSlotConverter.class)
    protected T time;

    @XmlElement
    @DynamicSerializeElement
    @SlotAttribute
    protected int availabilityOffset;

    public Map<String, Parameter> getParameters() {
        return parameters;
    }

    public void setParameters(Map<String, Parameter> parameters) {
        this.parameters = parameters;
    }

    public String getCollectionName() {
        return collectionName;
    }

    public void setCollectionName(String collectionName) {
        this.collectionName = collectionName;
    }

    public String getDataSetName() {
        return dataSetName;
    }

    public void setDataSetName(String dataSetName) {
        this.dataSetName = dataSetName;
    }

    public void setCoverage(C coverage) {
        this.coverage = coverage;
    }

    public C getCoverage() {
        return coverage;
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

    public void setTime(T time) {
        this.time = time;
    }

    public T getTime() {
        return time;
    }

    /**
     * Retrieve the service type for this instance of DataSet.
     * 
     * @return the serviceType
     */
    public abstract ServiceType getServiceType();

    /**
     * @return the availabilityOffset
     */
    public int getAvailabilityOffset() {
        return availabilityOffset;
    }

    /**
     * @param availabilityOffset
     *            the availabilityOffset to set
     */
    public void setAvailabilityOffset(int availabilityOffset) {
        this.availabilityOffset = availabilityOffset;
    }

    @Override
    public boolean equals(Object obj) {
        if (obj instanceof DataSet) {
            @SuppressWarnings("rawtypes")
            DataSet other = (DataSet) obj;
            EqualsBuilder eqBuilder = new EqualsBuilder();
            eqBuilder.append(this.getProviderName(), other.getProviderName());
            eqBuilder.append(this.getCollectionName(),
                    other.getCollectionName());
            eqBuilder.append(this.getDataSetName(), other.getDataSetName());
            return eqBuilder.isEquals();
        }
        return super.equals(obj);
    }

    @Override
    public int hashCode() {
        HashCodeBuilder hcBuilder = new HashCodeBuilder();
        hcBuilder.append(this.getProviderName());
        hcBuilder.append(this.getCollectionName());
        hcBuilder.append(this.getDataSetName());
        return hcBuilder.toHashCode();
    }

    /**
     * Combine the important information from another dataset into this one.
     * Used when a data set update is occuring.
     * 
     * @param result
     *            the combined dataset
     */
    public void combine(DataSet<T, C> toCombine) {
        this.getParameters().putAll(toCombine.getParameters());
    }
}
