package com.raytheon.uf.common.datadelivery.registry;

import java.io.Serializable;
import java.util.ArrayList;
import java.util.HashMap;
import java.util.Map;

import javax.xml.bind.annotation.XmlAccessType;
import javax.xml.bind.annotation.XmlAccessorType;
import javax.xml.bind.annotation.XmlAttribute;
import javax.xml.bind.annotation.XmlElement;
import javax.xml.bind.annotation.XmlElements;
import javax.xml.bind.annotation.XmlEnum;
import javax.xml.bind.annotation.XmlType;

import com.raytheon.uf.common.serialization.ISerializableObject;
import com.raytheon.uf.common.serialization.annotations.DynamicSerialize;
import com.raytheon.uf.common.serialization.annotations.DynamicSerializeElement;

/**
 * Level Type, lev type name
 * 
 * <pre>
 * 
 * SOFTWARE HISTORY
 * 
 * Date         Ticket#    Engineer    Description
 * ------------ ---------- ----------- --------------------------
 * Feb 24, 2012            dhladky     Initial creation.
 * Jun  8, 2012            jpiatt      Fixed level type description &
 *                                        Code clean up.
 * Set 06, 2012   1121     mpduff      Added a unique key.
 * Nov 19, 2012 1166       djohnson    Clean up JAXB representation of registry objects.
 * 
 * </pre>
 * 
 * @author dhladky
 * @version 1.0
 */
@XmlAccessorType(XmlAccessType.NONE)
@DynamicSerialize
public class DataLevelType implements ISerializableObject, Serializable {

    private static final long serialVersionUID = -6953092339309118122L;
    
    /**
     * Unique key for the object.
     */
    private transient String key;

    /**
     * Constructor.
     */
    public DataLevelType() {

    }

    /**
     * Constructor.
     * 
     * @param type
     * @param id
     */
    public DataLevelType(LevelType levelType) {
        this.type = levelType;
        layer = new ArrayList<Double>();
    }

    @XmlAttribute
    @DynamicSerializeElement
    private LevelType type;

    @XmlAttribute
    @DynamicSerializeElement
    private String unit;

    @XmlElements({ @XmlElement(name = "layer") })
    @DynamicSerializeElement
    private ArrayList<Double> layer;

    /**
     * LevelType enumeration
     */
    @DynamicSerialize
    @XmlEnum
    @XmlType(namespace = "com.raytheon.uf.common.datadelivery.registry")
    public enum LevelType {

        MB("pressure", "Pressure Levels", 100), LYRBL("layer",
                "Difference Between Two Levels", 101), SFC("surface",
                "Surface", 1), MAXW("max", "Maximum Level", 102), TROP(
                "tropopause", "Tropopause Level", 105), FHAG("height",
                "Height Level", 104), TEMP("temp", "Temperature Level", 103), HSCLW(
                "highest", "Highest Level", 106), LSCLW("lowest",
                "Lowest Level", 107), EL("equilibrium", "Equlibrium Level", 108), CCL(
                "convective", "Convective Level", 109), CBL("cloud",
                "Cloud Level", 110), SIGL("sigma", "Sigma Level", 111), PVL(
                "pv", "PV Level", 111), CTL("top", "Top Level", 112), MSL(
                "mean", "Mean Sea Level", 113), EA("entire",
                "Entire Atmosphere (As Single Layer)", 114), ODEG("0c",
                "0c isotherm", 115), LCY("low", "Low Cloud Bottom Level", 116), MCY(
                "middle", "Middle Cloud Level", 117), HCY("high",
                "High Cloud Level", 118), PBL("planetary",
                "Planetary Boundary Layer", 119), MWSL("hourly",
                "Hourly Maximum", 120), U("u-component",
                "U-Component of Hourly Maximum", 121), V("v-component",
                "V-Component of Hourly Maximum", 122), SEAB("sea_",
                "Sea Ice, Elevation/Area/Thickness/Movement", 123), UNKNOWN(
                "unknown", "UNKNOWN", 0);

        private final String levelType;

        private final String description;

        private final int id;

        private LevelType(String name, String description, int id) {
            levelType = name;
            this.description = description;
            this.id = id;
        }

        private static final Map<String, LevelType> DESCRIPTION_LOOKUP = new HashMap<String, LevelType>();
        static {
            for (LevelType levelType : LevelType.values()) {
                DESCRIPTION_LOOKUP.put(levelType.getDescription(), levelType);
            }
        }

        public String getLevelType() {
            return levelType;
        }

        /**
         * @return
         */
        public String getDescription() {
            return description;
        }

        /**
         * Get level type id.
         * 
         * @return level type id
         */
        public int getLevelTypeId() {
            return id;
        }

        /**
         * Returns the level name based on the level type Id. However, this
         * method has some special cases because there is currently no unified
         * mapping yet.
         * 
         * @return the level name
         */
        public static String getLevelTypeIdName(int levelTypeId) {
            LevelType rval = LevelType.UNKNOWN;
            for (LevelType val : LevelType.values()) {
                if (levelTypeId == val.getLevelTypeId()) {
                    rval = val;
                    break;
                }
            }

            // special case since enums can't start with integers
            if (rval == LevelType.ODEG) {
                return "0DEG";
            } else if (rval == LevelType.U || rval == LevelType.V) {
                return LevelType.MAXW.toString();
            }

            return rval.toString();
        }

        /**
         * Look up the enum instance via its description.
         * 
         * @param description
         *            the description
         * @return the enum instance
         * @throws IllegalArgumentException
         *             if there isn't an enum instance with the description
         */
        public static LevelType fromDescription(String description) {
            LevelType retVal = DESCRIPTION_LOOKUP.get(description);
            if (retVal == null) {
                throw new IllegalArgumentException(
                        "No enum instance with the specified description ["
                                + description + "]");
            }
            return retVal;
        }
    };

    /**
     * Set type.
     * 
     * @param type
     */
    public void setType(LevelType type) {
        this.type = type;
    }

    /**
     * Get type.
     * 
     * @return type
     */
    public LevelType getType() {
        return type;
    }

    /**
     * Set layer.
     * 
     * @param layer
     */
    public void setLayer(ArrayList<Double> layer) {
        this.layer = layer;
    }

    /**
     * Get layer
     * 
     * @return layer
     */
    public ArrayList<Double> getLayer() {
        return layer;
    }

    /**
     * Add layer.
     * 
     * @param l
     */
    public void addLayer(Double l) {
        if (layer == null) {
            layer = new ArrayList<Double>();
        }

        layer.add(l);
    }

    /**
     * Set unit.
     * 
     * @param unit
     */
    public void setUnit(String unit) {
        this.unit = unit;
    }

    /**
     * Get unit.
     * 
     * @return unit
     */
    public String getUnit() {
        return unit;
    }

    /**
     * Get the level type description
     * 
     * @return description of the level type
     */
    public String getDescription() {
        return (type != null) ? type.getDescription() : null;
    }
    
    /**
     * A unique key for this object.
     * 
     * @return String the unique key
     */
    public String getKey() {
        if (this.key == null) {
            this.key = this.type + this.unit;
        }
        
        return key;
    }

    /*
     * (non-Javadoc)
     * 
     * @see java.lang.Object#hashCode()
     */
    @Override
    public int hashCode() {
        final int prime = 31;
        int result = 1;
        result = prime * result + ((layer == null) ? 0 : layer.hashCode());
        result = prime * result + ((type == null) ? 0 : type.hashCode());
        result = prime * result + ((unit == null) ? 0 : unit.hashCode());
        return result;
    }

    /*
     * (non-Javadoc)
     * 
     * @see java.lang.Object#equals(java.lang.Object)
     */
    @Override
    public boolean equals(Object obj) {
        if (this == obj)
            return true;
        if (obj == null)
            return false;
        if (getClass() != obj.getClass())
            return false;
        DataLevelType other = (DataLevelType) obj;
        if (layer == null) {
            if (other.layer != null)
                return false;
        } else if (!layer.equals(other.layer))
            return false;
        if (type == null) {
            if (other.type != null)
                return false;
        } else if (!type.equals(other.type))
            return false;
        if (unit == null) {
            if (other.unit != null)
                return false;
        } else if (!unit.equals(other.unit))
            return false;
        return true;
    }

    /**
     * Delegate
     * 
     * @return
     */
    public int getId() {
        return type.getLevelTypeId();
    }
}
