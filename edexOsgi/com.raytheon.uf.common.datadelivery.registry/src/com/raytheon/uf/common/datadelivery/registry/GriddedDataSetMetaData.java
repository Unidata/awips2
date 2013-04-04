package com.raytheon.uf.common.datadelivery.registry;

import java.util.HashMap;
import java.util.Map;

import javax.xml.bind.annotation.XmlAccessType;
import javax.xml.bind.annotation.XmlAccessorType;
import javax.xml.bind.annotation.XmlAttribute;
import javax.xml.bind.annotation.XmlSeeAlso;
import javax.xml.bind.annotation.adapters.XmlJavaTypeAdapter;

import com.raytheon.uf.common.registry.annotations.RegistryObject;
import com.raytheon.uf.common.registry.annotations.SlotAttribute;
import com.raytheon.uf.common.serialization.XmlGenericMapAdapter;
import com.raytheon.uf.common.serialization.annotations.DynamicSerializeElement;

/**
 * Gridded Meta Data object
 * 
 * <pre>
 * 
 * SOFTWARE HISTORY
 * Date         Ticket#    Engineer    Description
 * ------------ ---------- ----------- --------------------------
 * Feb 20, 2011    218      dhladky     Initial creation
 * Jul 24, 2012    955      djohnson    Add {@link RegistryObject}.
 * Aug 20, 2012   0743      djohnson    Store cycle in a slot.
 * Nov 19, 2012 1166        djohnson    Clean up JAXB representation of registry objects.
 * 
 * </pre>
 * 
 * @author dhladky
 * @version 1.0
 */
@XmlAccessorType(XmlAccessType.NONE)
@XmlSeeAlso(OpenDapGriddedDataSetMetaData.class)
public abstract class GriddedDataSetMetaData extends
        DataSetMetaData {

    public static final String CYCLE_SLOT = "cycle";

    public static final int NO_CYCLE = -1;

    public GriddedDataSetMetaData() {
    }

    /**
     * map of the level types available in set
     */
    @DynamicSerializeElement
    @XmlJavaTypeAdapter(type = Map.class, value = XmlGenericMapAdapter.class)
    private Map<DataLevelType, Levels> levelTypes = new HashMap<DataLevelType, Levels>();

    /**
     * map of the level types available in set
     */
    @DynamicSerializeElement
    @XmlAttribute
    @SlotAttribute(CYCLE_SLOT)
    private int cycle = NO_CYCLE;

    public void setLevelTypes(Map<DataLevelType, Levels> levelTypes) {
        this.levelTypes = levelTypes;
    }

    public Map<DataLevelType, Levels> getLevelTypes() {
        return levelTypes;
    }

    public void addLevelType(DataLevelType type, Levels levels) {
        if (levelTypes == null) {
            levelTypes = new HashMap<DataLevelType, Levels>();
        }
        if (!levelTypes.containsKey(type)) {
            levelTypes.put(type, levels);
        }
    }

    public void setCycle(int cycle) {
        this.cycle = cycle;
    }

    public int getCycle() {
        return cycle;
    }

}
