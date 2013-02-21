package com.raytheon.uf.common.datadelivery.registry;

import static com.google.common.base.Preconditions.checkNotNull;

import java.util.Comparator;

import javax.xml.bind.annotation.XmlAccessType;
import javax.xml.bind.annotation.XmlAccessorType;
import javax.xml.bind.annotation.XmlAttribute;
import javax.xml.bind.annotation.XmlElement;
import javax.xml.bind.annotation.XmlSeeAlso;

import org.apache.commons.lang.builder.EqualsBuilder;
import org.apache.commons.lang.builder.HashCodeBuilder;

import com.raytheon.uf.common.registry.annotations.RegistryObject;
import com.raytheon.uf.common.registry.annotations.RegistryObjectDescription;
import com.raytheon.uf.common.registry.annotations.RegistryObjectName;
import com.raytheon.uf.common.registry.annotations.RegistryObjectOwner;
import com.raytheon.uf.common.registry.annotations.SlotAttribute;
import com.raytheon.uf.common.registry.annotations.SlotAttributeConverter;
import com.raytheon.uf.common.registry.ebxml.slots.DateSlotConverter;
import com.raytheon.uf.common.serialization.ISerializableObject;
import com.raytheon.uf.common.serialization.annotations.DynamicSerializeElement;
import com.raytheon.uf.common.time.util.ImmutableDate;

/**
 * Abstract Meta Data object
 * 
 * <pre>
 * 
 * SOFTWARE HISTORY
 * Date         Ticket#    Engineer    Description
 * ------------ ---------- ----------- --------------------------
 * Feb 20, 2011 218        dhladky      Initial creation
 * May 15, 2012 455        jspinks      Added Registry annotations
 * Jul 24, 2012 955        djohnson     Add {@link #equals(Object)} and {@link #hashCode()}.
 * 8/3/2012     724        bphillip     Added more registry annotations
 * Aug 10, 2012 1022       djohnson     Requires provider name for {@link DataSet}.
 * Aug 15, 2012 0743       djohnson     Add date attribute.
 * Sep 06, 2012 1102       djohnson     Implement comparable.
 * Oct 16, 2012 0726       djohnson     Override {@link #toString()}.
 * Nov 19, 2012 1166       djohnson     Clean up JAXB representation of registry objects.
 * </pre>
 * 
 * @author dhladky
 * @version 1.0
 */
@XmlAccessorType(XmlAccessType.NONE)
@XmlSeeAlso({ GriddedDataSetMetaData.class, OpenDapGriddedDataSetMetaData.class })
@RegistryObject({ "url" })
public abstract class DataSetMetaData implements ISerializableObject {
    public static final String DATE_SLOT = "date";

    public static final String DATA_SET_NAME_SLOT = "dataSetName";

    public static final String PROVIDER_NAME_SLOT = "providerName";
    
    /**
     * Compares the two instances of {@link DataSetMetaData} by their applicable
     * date fields.
     */
    public static Comparator<? super DataSetMetaData> DATE_COMPARATOR = new Comparator<DataSetMetaData>()
    {
        @Override
        public int compare(DataSetMetaData o1, DataSetMetaData o2) {

            checkNotNull(o1, "Cannot compare this object with null!");
            checkNotNull(o2, "Cannot compare this object with null!");

            if (o1.date != null && o2.date != null) {
                return o1.date.compareTo(o2.date);
            }

            return 0;
        }
    };

    @RegistryObjectDescription
    @XmlAttribute
    @DynamicSerializeElement
    protected String dataSetDescription;

    @RegistryObjectOwner
    @RegistryObjectName
    @XmlAttribute
    @DynamicSerializeElement
    protected String url;

    @XmlElement
    @DynamicSerializeElement
    @SlotAttribute
    @SlotAttributeConverter(TimeSlotConverter.class)
    protected Time time;

    @XmlAttribute
    @SlotAttribute(DATA_SET_NAME_SLOT)
    @DynamicSerializeElement
    private String dataSetName;

    @XmlAttribute
    @SlotAttribute(PROVIDER_NAME_SLOT)
    @DynamicSerializeElement
    private String providerName;

    @XmlAttribute
    @SlotAttribute(DATE_SLOT)
    @SlotAttributeConverter(DateSlotConverter.class)
    @DynamicSerializeElement
    private ImmutableDate date;

    public DataSetMetaData() {

    }

    public String getUrl() {
        return url;
    }

    public void setUrl(String url) {
        this.url = url;
    }

    public void setTime(Time time) {
        this.time = time;
    }

    public Time getTime() {
        return time;
    }

    public String getDataSetDescription() {
        return dataSetDescription;
    }

    public void setDataSetDescription(String dataSetDescription) {
        this.dataSetDescription = dataSetDescription;
    }

    /**
     * @return
     */
    public String getDataSetName() {
        return dataSetName;
    }

    /**
     * @param dataSetName
     *            the dataSetName to set
     */
    public void setDataSetName(String dataSetName) {
        this.dataSetName = dataSetName;
    }

    public String getProviderName() {
        return providerName;
    }

    public void setProviderName(String providerName) {
        this.providerName = providerName;
    }
    

    /**
     * Get the date this object starts on. In the gridded world, this would
     * correspond to the base reference time.
     * 
     * @return
     */
    public ImmutableDate getDate() {
        return date;
    }

    /**
     * Set the date this object starts on. In the gridded world, this would
     * correspond to the base reference time.
     * 
     * @param date
     *            the date
     */
    public void setDate(ImmutableDate date) {
        this.date = date;
    }

    @Override
    public boolean equals(Object obj) {
        if (obj instanceof DataSetMetaData) {
            DataSetMetaData other = (DataSetMetaData) obj;
            EqualsBuilder eqBuilder = new EqualsBuilder();
            eqBuilder.append(this.getUrl(), other.getUrl());
            return eqBuilder.isEquals();
        }

        return super.equals(obj);
    }

    @Override
    public int hashCode() {
        HashCodeBuilder hcBuilder = new HashCodeBuilder();
        hcBuilder.append(this.getUrl());
        return hcBuilder.toHashCode();
    }

    @Override
    public String toString() {
        return url;
    }

    /**
     * Accepts a {@link IDataSetMetaDataVisitor} which can perform arbitrary
     * processing on this {@link DataSetMetaData} instance. Should be defined by
     * each concrete class instance.
     * 
     * @param visitor
     *            the visitor
     */
    public abstract void accept(IDataSetMetaDataVisitor visitor);
}
