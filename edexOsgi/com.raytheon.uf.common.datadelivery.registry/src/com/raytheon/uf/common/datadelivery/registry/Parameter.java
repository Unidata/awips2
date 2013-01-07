package com.raytheon.uf.common.datadelivery.registry;

import java.io.Serializable;
import java.util.ArrayList;
import java.util.List;

import javax.xml.bind.annotation.XmlAccessType;
import javax.xml.bind.annotation.XmlAccessorType;
import javax.xml.bind.annotation.XmlAttribute;
import javax.xml.bind.annotation.XmlElement;
import javax.xml.bind.annotation.XmlElements;
import javax.xml.bind.annotation.XmlRootElement;
import javax.xml.bind.annotation.XmlType;

import org.apache.commons.lang.builder.EqualsBuilder;
import org.apache.commons.lang.builder.HashCodeBuilder;

import com.raytheon.uf.common.datadelivery.registry.DataLevelType.LevelType;
import com.raytheon.uf.common.registry.annotations.RegistryObject;
import com.raytheon.uf.common.registry.annotations.RegistryObjectAssociation;
import com.raytheon.uf.common.registry.annotations.RegistryObjectDescription;
import com.raytheon.uf.common.registry.annotations.RegistryObjectName;
import com.raytheon.uf.common.registry.annotations.SlotAttribute;
import com.raytheon.uf.common.registry.annotations.SlotAttributeConverter;
import com.raytheon.uf.common.serialization.ISerializableObject;
import com.raytheon.uf.common.serialization.annotations.DynamicSerialize;
import com.raytheon.uf.common.serialization.annotations.DynamicSerializeElement;

/**
 * Parameter object
 * 
 * <pre>
 * 
 * SOFTWARE HISTORY
 * Date         Ticket#    Engineer    Description
 * ------------ ---------- ----------- --------------------------
 * Feb 17, 2011 218        dhladky     Initial creation
 * May 14, 2012 455        jspinks     Added registry annotations. 
 * 8/3/2012     724        bphillip    Added more registry annotations
 * Aug 22, 2012 0743       djohnson    Store data type as an enum.
 * Sep 06, 2012 1121       mpduff      Added toString().
 * Sep 07, 2012 1102       djohnson    Add {@code @XmlRootElement}.
 * Nov 19, 2012 1166       djohnson    Clean up JAXB representation of registry objects.
 * </pre>
 * 
 * @author dhladky
 * @version 1.0
 */

@XmlRootElement(namespace = "com.raytheon.uf.common.datadelivery.registry")
@XmlType(name = "parameter", namespace = "com.raytheon.uf.common.datadelivery.registry")
@XmlAccessorType(XmlAccessType.NONE)
@DynamicSerialize
@RegistryObject({ "name", "dataType" })
public class Parameter implements ISerializableObject, Serializable {

	private static final long serialVersionUID = -2332611624661834210L;

	public Parameter() {

    }

    @RegistryObjectName
    @XmlAttribute
    @DynamicSerializeElement
    @SlotAttribute
    private String name;

    @XmlAttribute
    @DynamicSerializeElement
    private String providerName;

    @RegistryObjectDescription
    @XmlAttribute
    @DynamicSerializeElement
    private String definition;

    @XmlAttribute
    @DynamicSerializeElement
    private String units;

    @XmlAttribute
    @DynamicSerializeElement
    @SlotAttribute
    private DataType dataType;

    @XmlAttribute
    @DynamicSerializeElement
    private String missingValue;

    @XmlAttribute
    @DynamicSerializeElement
    private String fillValue;

    @XmlAttribute
    @DynamicSerializeElement
    private Integer ensemble;

    @XmlAttribute
    @DynamicSerializeElement
    private String baseType;

    @XmlElements({ @XmlElement(name = "levelType", type = DataLevelType.class) })
    @DynamicSerializeElement
    private List<DataLevelType> levelType;

    @XmlElement(name = "levels", type = Levels.class)
    @DynamicSerializeElement
    @SlotAttribute
    @SlotAttributeConverter(ParameterLevelsConverter.class)
    @RegistryObjectAssociation(ParameterLevelsResolver.class)
    private Levels levels;

    public String getName() {
        return name;
    }

    public void setName(String name) {
        this.name = name;
    }

    public String getProviderName() {
        return providerName;
    }

    public void setProviderName(String providerName) {
        this.providerName = providerName;
    }

    public String getDefinition() {
        return definition;
    }

    public void setDefinition(String definition) {
        this.definition = definition;
    }

    public String getUnits() {
        return units;
    }

    public void setUnits(String units) {
        this.units = units;
    }

    public DataType getDataType() {
        return dataType;
    }

    public void setDataType(DataType dataType) {
        this.dataType = dataType;
    }

    public String getMissingValue() {
        return missingValue;
    }

    public void setMissingValue(String missingValue) {
        this.missingValue = missingValue;
    }

    public void setBaseType(String baseType) {
        this.baseType = baseType;
    }

    public String getBaseType() {
        return baseType;
    }

    public void setLevelType(List<DataLevelType> list) {
        this.levelType = list;
    }

    public List<DataLevelType> getLevelType() {
        return levelType;
    }

    public void addLevelType(DataLevelType type) {
        if (levelType == null) {
            levelType = new ArrayList<DataLevelType>();
        }

        levelType.add(type);
    }

    /**
     * get the level by type
     * 
     * @param type
     * @return
     */
    public DataLevelType getDataLevelByType(LevelType type) {
        for (DataLevelType dlt : levelType) {
            if (dlt.getType().equals(type)) {
                return dlt;
            }
        }

        return null;
    }

    /**
     * get the level by id
     * 
     * @param type
     * @return
     */
    public DataLevelType getDataLevelById(int id) {
        for (DataLevelType dlt : levelType) {
            if (dlt.getId() == id) {
                return dlt;
            }
        }

        return null;
    }

    /**
     * get the level by description
     * 
     * @param type
     * @return
     */
    public DataLevelType getDataLevelByDescription(String desc) {
        for (DataLevelType dlt : levelType) {
            if (dlt.getDescription().equals(desc)) {
                return dlt;
            }
        }

        return null;
    }

    public void setLevels(Levels levels) {
        this.levels = levels;
    }

    public Levels getLevels() {
        return levels;
    }

    public void setEnsemble(Integer ensemble) {
        this.ensemble = ensemble;
    }

    public Integer getEnsemble() {
        return ensemble;
    }

    public void setFillValue(String fillValue) {
        this.fillValue = fillValue;
    }

    public String getFillValue() {
        return fillValue;
    }

    @Override
    public String toString() {
        return this.providerName;
    }

    @Override
    public boolean equals(Object obj) {
        if (obj instanceof Parameter) {
            Parameter other = (Parameter) obj;

            EqualsBuilder eqBuilder = new EqualsBuilder();
            eqBuilder.append(name, other.name);
            eqBuilder.append(dataType, other.dataType);

            return eqBuilder.isEquals();
        }
        return super.equals(obj);
    }

    @Override
    public int hashCode() {
        HashCodeBuilder hcBuilder = new HashCodeBuilder();
        hcBuilder.append(name);
        hcBuilder.append(dataType);

        return hcBuilder.toHashCode();
    }
}
