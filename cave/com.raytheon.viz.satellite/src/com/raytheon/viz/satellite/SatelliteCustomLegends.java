package com.raytheon.viz.satellite;

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
import javax.xml.bind.annotation.XmlRootElement;

import com.raytheon.uf.common.localization.IPathManager;
import com.raytheon.uf.common.localization.LocalizationContext;
import com.raytheon.uf.common.localization.LocalizationContext.LocalizationType;
import com.raytheon.uf.common.localization.PathManagerFactory;
import com.raytheon.uf.common.status.IUFStatusHandler;
import com.raytheon.uf.common.status.UFStatus;

/**
 * This code unmarshals an XML file to modify the default product legends for imagery handled
 * through the raytheon viz satellite resources.
 * 
 * This is similar to code that Tom Kretz previously developed for the edex regionalsat plug-in.
 * 
 * The default customLegends.xml file contains the following:
 * <pre>
 * 
 * <customLegends>
 *	<map>
 *		<entry><key>GOES-13(N) Imager Visible</key><value>GOES-13 Visible Imagery</value></entry>
 *		<entry><key>GOES-14(O) Imager Visible</key><value>GOES-14 Visible Imagery</value></entry>
 *		<entry><key>GOES-15(P) Imager Visible</key><value>GOES-15 Visible Imagery</value></entry>
 *		<entry><key>Imager Visible</key><value>Visible Imagery</value></entry>
 *	</map>
 * </customLegends>
 * 
 * </pre>
 * 
 * <pre>
 * 
 * SOFTWARE HISTORY
 *                   
 * date          Ticket#     Engineer    Description
 * -----------  ----------  ----------- --------------------------
 * 4/21/2014    DCS 14960   jgerth      Initial Creation
 * 
 * </pre>
 * 
 * @author jgerth
 * @version 1.0
 */

public class SatelliteCustomLegends {
    private static final IUFStatusHandler theHandler = UFStatus
            .getHandler(SatelliteCustomLegends.class);
    private static final String LOOKUP_DIR = "satellite";

    @XmlAccessorType(XmlAccessType.NONE)
    public static abstract class AbstractLookup<K,V> {
        public abstract Map<K, V> getMap();
    }

    @XmlRootElement
    @XmlAccessorType(XmlAccessType.FIELD)
    private static class CustomLegends extends AbstractLookup<String, String> {
        public HashMap<String, String> map = new HashMap<String, String>();

        @Override
        public Map<String, String> getMap() {
            return map;
        }
    }

    private CustomLegends customLegendsLookup;

    public SatelliteCustomLegends() {
        try {
            init();
        } catch (Exception e) {
            theHandler.error("Failed to initialize lookups", e);
        }
    }

    // initialize the lookup tables from the configuration files
    private void init() throws Exception {
        JAXBContext context = JAXBContext.newInstance(CustomLegends.class);
        Unmarshaller u = context.createUnmarshaller();
        customLegendsLookup = load(new CustomLegends(), "customLegends.xml", u);
    }

    private static <T extends AbstractLookup<K,V>, K, V> T load(T combinedLookup, String fileName, Unmarshaller u) throws Exception {
        IPathManager pm = PathManagerFactory.getPathManager();

        List<LocalizationContext> contexts = Arrays.asList(pm.getLocalSearchHierarchy(LocalizationType.CAVE_STATIC));
        Collections.reverse(contexts);
        for (LocalizationContext ctx : contexts) {
            File f = pm.getFile(ctx, LOOKUP_DIR + File.separator + fileName);
            if (f != null && f.isFile()) {
                try {
                    @SuppressWarnings("unchecked")
                    T lookup = (T) u.unmarshal(f);
                    if (!combinedLookup.getClass().isAssignableFrom(lookup.getClass())) {
                        throw new Exception(String.format("file contains %s, expected %s",
                                lookup.getClass(), combinedLookup.getClass()));
                    }
                    combinedLookup.getMap().putAll(lookup.getMap());
                } catch (Exception e) {
                    theHandler.error(String.format("%s: %s", f, e.getMessage()), e);
                }
            }
        }
        return combinedLookup;
    }

    /**
     * @param cl
     * @return
     */
    public String getCustomLegend(String cl) {
        return customLegendsLookup.map.get(cl);
    }

    private static SatelliteCustomLegends instance;

    public static synchronized SatelliteCustomLegends getInstance() {
        if (instance == null)
            instance = new SatelliteCustomLegends();
        return instance;
    }

    public static synchronized void reload() {
        instance = new SatelliteCustomLegends();
    }
}
