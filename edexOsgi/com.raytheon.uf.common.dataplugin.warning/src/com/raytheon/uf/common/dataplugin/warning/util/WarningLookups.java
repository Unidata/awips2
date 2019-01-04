package com.raytheon.uf.common.dataplugin.warning.util;

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
 * Class for looking up phensig info for warnings/advisories.
 * 
 * 
 * <pre>
 * 
 * SOFTWARE HISTORY
 * 
 * Date          Ticket#  Engineer  Description
 * ------------- -------- --------- --------------------------------------------
 * Dec 19, 2018           mjames    Initial creation
 * 
 * </pre>
 * 
 * @author mjames
 */
public class WarningLookups {
    private static final IUFStatusHandler theHandler = UFStatus.getHandler(WarningLookups.class);

    private static final String LOOKUP_DIR = "warngen";

    public static class PhensigValue {
        public PhensigValue() {}

        public PhensigValue(String name, String color) {
            this.name = name;
            this.color = color;
        }

        @XmlAttribute
        public String name;

        @XmlAttribute
        public String color;
    }

    @XmlAccessorType(XmlAccessType.NONE)
    public static abstract class AbstractLookup<K, V> {
        public abstract Map<K, V> getMap();
    }

    @XmlRootElement
    @XmlAccessorType(XmlAccessType.FIELD)
    public static class PhensigColors extends
            AbstractLookup<String, PhensigValue> {
        public HashMap<String, PhensigValue> map = new HashMap<>();

        @Override
        public Map<String, PhensigValue> getMap() {
            return map;
        }
    }

    private PhensigColors phensigColorLookup;

    public WarningLookups() {
        try {
            init();
        } catch (Exception e) {
            theHandler.error("Failed to initialize lookups", e);
        }
    }

    private void init() throws Exception {
        JAXBContext context = JAXBContext.newInstance(PhensigColors.class);
        Unmarshaller u = context.createUnmarshaller();
        phensigColorLookup = load(new PhensigColors(),  "phensigColors.xml", u);
    }

    private static <T extends AbstractLookup<K, V>, K, V> T load(
            T combinedLookup, String fileName, Unmarshaller u) {
    	
        IPathManager pm = PathManagerFactory.getPathManager();

        List<LocalizationContext> contexts = Arrays.asList(pm
                .getLocalSearchHierarchy(LocalizationType.COMMON_STATIC));
        Collections.reverse(contexts);
        
        for (LocalizationContext ctx : contexts) {
            File f = pm.getFile(ctx, LOOKUP_DIR + IPathManager.SEPARATOR + fileName);
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

    public PhensigValue getPhensig(String phensigCode) {
        return phensigColorLookup.map.get(phensigCode);
    }

    private static WarningLookups instance;

    public static synchronized WarningLookups getInstance() {
        if (instance == null)
            instance = new WarningLookups();
        return instance;
    }

    public static synchronized void reload() {
        instance = new WarningLookups();
    }
}
