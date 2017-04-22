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
package com.raytheon.uf.edex.plugin.satellite.mcidas.util;

import java.io.File;
import java.util.Arrays;
import java.util.Collections;
import java.util.HashMap;
import java.util.List;
import java.util.Map;

import javax.xml.bind.JAXBContext;
import javax.xml.bind.Unmarshaller;
import javax.xml.bind.annotation.XmlAccessType;
import javax.xml.bind.annotation.XmlAccessorType;
import javax.xml.bind.annotation.XmlAttribute;
import javax.xml.bind.annotation.XmlRootElement;

import com.raytheon.uf.common.localization.IPathManager;
import com.raytheon.uf.common.localization.LocalizationContext;
import com.raytheon.uf.common.localization.LocalizationContext.LocalizationType;
import com.raytheon.uf.common.localization.PathManagerFactory;
import com.raytheon.uf.common.status.IUFStatusHandler;
import com.raytheon.uf.common.status.UFStatus;

/**
 * Class for looking up physical elements, creating entities, and area names for
 * McIDAS satellite data based on localization config files.
 * 
 * 
 * <pre>
 * 
 * SOFTWARE HISTORY
 * 
 * Date          Ticket#  Engineer  Description
 * ------------- -------- --------- --------------------------------------------
 * Jul 29, 2011           dfriedma  Initial creation
 * Jul 14, 2016  5744     mapeters  Initial javadoc creation, config files moved
 *                                  from edex_static to common_static
 * 
 * </pre>
 * 
 * @author dfriedma
 */
public class McidasSatelliteLookups {
    private static final IUFStatusHandler theHandler = UFStatus
            .getHandler(McidasSatelliteLookups.class);

    private static final String LOOKUP_DIR = "satellite"
            + IPathManager.SEPARATOR + "mcidas";

    public static class PhysicalElementKey {
        public PhysicalElementKey() {
        }

        public PhysicalElementKey(int sensorSource, int bandIndex) {
            this.sensorSource = sensorSource;
            this.bandIndex = bandIndex;
        }

        @XmlAttribute(name = "ss")
        public int sensorSource;

        @XmlAttribute(name = "band")
        public int bandIndex; // 1-based

        @Override
        public int hashCode() {
            return sensorSource * 17 + bandIndex;
        }

        @Override
        public boolean equals(Object obj) {
            if (obj instanceof PhysicalElementKey)
                return sensorSource == ((PhysicalElementKey) obj).sensorSource
                        && bandIndex == ((PhysicalElementKey) obj).bandIndex;
            else
                return false;
        }
    }

    public static class PhysicalElementValue {
        public PhysicalElementValue() {
        }

        public PhysicalElementValue(String name, String units) {
            this.name = name;
            this.units = units;
        }

        @XmlAttribute
        public String name;

        @XmlAttribute
        public String units;
    }

    @XmlAccessorType(XmlAccessType.NONE)
    public static abstract class AbstractLookup<K, V> {
        public abstract Map<K, V> getMap();
    }

    @XmlRootElement
    @XmlAccessorType(XmlAccessType.FIELD)
    public static class PhysicalElements extends
            AbstractLookup<PhysicalElementKey, PhysicalElementValue> {
        public HashMap<PhysicalElementKey, PhysicalElementValue> map = new HashMap<>();

        @Override
        public Map<PhysicalElementKey, PhysicalElementValue> getMap() {
            return map;
        }
    }

    @XmlRootElement
    @XmlAccessorType(XmlAccessType.FIELD)
    public static class CreatingEntities extends
            AbstractLookup<Integer, String> {
        public HashMap<Integer, String> map = new HashMap<>();

        @Override
        public Map<Integer, String> getMap() {
            return map;
        }
    }

    @XmlRootElement
    @XmlAccessorType(XmlAccessType.FIELD)
    private static class AreaNames extends AbstractLookup<Integer, String> {
        public HashMap<Integer, String> map = new HashMap<>();

        @Override
        public Map<Integer, String> getMap() {
            return map;
        }
    }

    private PhysicalElements physicalElementLookup;

    private CreatingEntities creatingEntityLookup;

    private AreaNames areaNameLookup;

    public McidasSatelliteLookups() {
        try {
            init();
        } catch (Exception e) {
            theHandler.error("Failed to initialize lookups", e);
        }
    }

    private void init() throws Exception {
        JAXBContext context = JAXBContext.newInstance(PhysicalElements.class,
                CreatingEntities.class, AreaNames.class);
        Unmarshaller u = context.createUnmarshaller();
        physicalElementLookup = load(new PhysicalElements(),
                "physicalElements.xml", u);
        creatingEntityLookup = load(new CreatingEntities(),
                "creatingEntities.xml", u);
        areaNameLookup = load(new AreaNames(), "areaNames.xml", u);
    }

    private static <T extends AbstractLookup<K, V>, K, V> T load(
            T combinedLookup, String fileName, Unmarshaller u) {
        IPathManager pm = PathManagerFactory.getPathManager();

        List<LocalizationContext> contexts = Arrays.asList(pm
                .getLocalSearchHierarchy(LocalizationType.COMMON_STATIC));
        Collections.reverse(contexts);
        for (LocalizationContext ctx : contexts) {
            File f = pm.getFile(ctx, LOOKUP_DIR + IPathManager.SEPARATOR
                    + fileName);
            if (f != null && f.isFile()) {
                try {
                    @SuppressWarnings("unchecked")
                    T lookup = (T) u.unmarshal(f);
                    if (!combinedLookup.getClass().isAssignableFrom(
                            lookup.getClass())) {
                        throw new Exception(String.format(
                                "file contains %s' expected %s",
                                lookup.getClass(), combinedLookup.getClass()));
                    }
                    combinedLookup.getMap().putAll(lookup.getMap());
                } catch (Exception e) {
                    theHandler.error(
                            String.format("%s: %s", f, e.getMessage()), e);
                }
            }
        }
        return combinedLookup;
    }

    public PhysicalElementValue getPhysicalElement(int sensorSource,
            int bandIndex) {
        return physicalElementLookup.map.get(new PhysicalElementKey(
                sensorSource, bandIndex));
    }

    public String getCreatingEntity(int sensorSource) {
        return creatingEntityLookup.map.get(sensorSource);
    }

    public String getAreaName(int areaNumber) {
        return areaNameLookup.map.get(areaNumber);
    }

    private static McidasSatelliteLookups instance;

    public static synchronized McidasSatelliteLookups getInstance() {
        if (instance == null)
            instance = new McidasSatelliteLookups();
        return instance;
    }

    public static synchronized void reload() {
        instance = new McidasSatelliteLookups();
    }
}
