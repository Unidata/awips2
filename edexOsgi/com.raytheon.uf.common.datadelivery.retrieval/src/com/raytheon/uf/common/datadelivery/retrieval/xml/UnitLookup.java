package com.raytheon.uf.common.datadelivery.retrieval.xml;

import java.util.HashMap;
import java.util.List;
import java.util.Map;

import javax.xml.bind.annotation.XmlAccessType;
import javax.xml.bind.annotation.XmlAccessorType;
import javax.xml.bind.annotation.XmlElement;
import javax.xml.bind.annotation.XmlElements;
import javax.xml.bind.annotation.XmlRootElement;

import com.raytheon.uf.common.serialization.ISerializableObject;
import com.raytheon.uf.common.serialization.annotations.DynamicSerialize;
import com.raytheon.uf.common.serialization.annotations.DynamicSerializeElement;

/**
 * Unit Config Lookup
 * 
 * <pre>
 * 
 * SOFTWARE HISTORY
 * Date         Ticket#    Engineer    Description
 * ------------ ---------- ----------- --------------------------
 * 20 Oct, 2012   1163      dhladky     Initial creation
 * 
 * </pre>
 * 
 * @author dhladky
 * @version 1.0
 */

@XmlRootElement(name = "unitLookup")
@XmlAccessorType(XmlAccessType.NONE)
@DynamicSerialize
public class UnitLookup implements ISerializableObject {

    @XmlElements({ @XmlElement(name = "unitConfig", type = UnitConfig.class) })
    @DynamicSerializeElement
    private List<UnitConfig> unitConfigs;

    /**
     * Map of the entries by provider name
     */
    private Map<String, UnitConfig> units = null;

    /**
     * Creates the units for speed
     */
    private void createUnitsMap() {
        if (units == null) {
            units = new HashMap<String, UnitConfig>();
            List<UnitConfig> configs = getUnitConfigs();
            if (configs != null) {
                for (UnitConfig con : configs) {
                    units.put(con.getProviderName(), con);
                }
            }
        }
    }

    /**
     * Gets the unit by the name
     * 
     * @param awipsName
     * @return
     */
    public UnitConfig getUnitByName(String name) {
        List<UnitConfig> configs = getUnitConfigs();
        if (configs != null) {
            for (UnitConfig unit : configs) {
                if (unit.getName().equals(name)) {
                    return unit;
                }
            }
        }

        return null;
    }

    /**
     * Gets the unit by the name
     * 
     * @param awipsName
     * @return
     */
    public UnitConfig getUnitByProviderName(String name) {

        if (units == null) {
            createUnitsMap();
        }

        return units.get(name);
    }

    public List<UnitConfig> getUnitConfigs() {
        return unitConfigs;
    }

    public void setUnitConfigs(List<UnitConfig> unitConfigs) {
        this.unitConfigs = unitConfigs;
    }

}
