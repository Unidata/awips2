/**
 * 
 * gov.noaa.nws.ost.edex.plugin.alaskasat.util.RegionalSatLookups
 * 
 * 12-01-11
 * 
 * This code has been developed by the NWS/OST/SEC for use in the AWIPS2 system.
 *
 **/

package gov.noaa.nws.ost.edex.plugin.regionalsat.util;

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
 * The RegionalSatLookups class is responsible for making the lookup parameters configurable.
 * Using map objects to contain the parameters removes the dependency on database schema changes,
 * since these parameters are only used during ingesting and storing the data. Looking up values
 * for PhysicalElements, CreatingEntities and Source parameters are supported.
 * 
 * This class is based on the com.raytheon.uf.edex.plugin.satellite.mcidas.McidasSatelliteLookups
 * class
 * 
 * The xml configuration files are in the utility/edex_static/base/satellite/regionalsat directory.
 * The configuration file names are creatingEntities.xml, physicalelements.xml and source.xml.
 * 
 * The creatingEntities file contains the following:
 * <pre>
 * <creatingEntities>
 *	<map>
 *		<entry><key>DMSP</key><value>DMSP</value></entry>	
 *		<entry><key>FY1C</key><value>FY1C</value></entry>
 * 		<entry><key>FY3C</key><value>FY3C</value></entry>
 *		<entry><key>GVAR</key><value>GVAR</value></entry>	
 *		<entry><key>HRPT</key><value>HRPT</value></entry>
 *		<entry><key>MTSAT</key><value>MTSAT</value></entry>	
 *		<entry><key>GOESR-PG</key><value>GOESR-PG</value></entry>
 *		<entry><key>Blended2</key><value>Blended2</value></entry>
 *	</map>
 * </creatingEntities>
 * </pre>
 * 
 * The physicalElements files contains the folloowing:
 * <pre>
 * <physicalElements>
 *	<map>
 *		<entry><key channel="0.58 - 0.68 micron VISL" satName="HRPT"/><value name="Imager Visible" /></entry>
 *		<entry><key channel="3 Channel Differencing" satName="HRPT"/><value name="Imager 3 Channel Diff" /></entry>
 *		<entry><key channel="3.55-3.93 micron IRUL" satName="HRPT"/><value name="Imager 3.9 micron IR" units="IRPixel" /></entry>
 *		<entry><key channel="10.5 - 11.5 micron IRUL" satName="HRPT"/><value name="Imager 11 micron IR" units="IRPixel" /></entry>
 *		<entry><key channel="Channel 4-5 IR" satName="HRPT"/><value name="Imager Channel 4-5 IR" /></entry>
 *  	<entry><key channel="0.7 micron visible" satName="MTSAT"/><value name="Imager Visible" /></entry>
 *		<entry><key channel="10 micron IR" satName="MTSAT"/><value name="Imager 11 micron IR" units="IRPixel"/></entry>
 *		<entry><key channel="Channel 4-5 IR" satName="MTSAT"/><value name="Imager Channel 4-5 IR" /></entry>
 *		<entry><key channel="0.40 - 1.10 micron VISL" satName="DMSP"/><value name="Imager Visible" /></entry>
 *		<entry><key channel="10.0 - 12.0 micron IRUL" satName="DMSP"/><value name="Imager 11 micron IR" units="IRPixel"/></entry>
 *		<entry><key channel="0.55-0.75 micron VISL" satName="GVAR"/><value name="Imager Visible" /></entry>
 *		<entry><key channel="3.80-4.00 micron IRUL" satName="GVAR"/><value name="Imager 3.9 micron IR" units="IRPixel"/></entry>
 *		<entry><key channel="6.50-7.00 micron IRUL" satName="GVAR"/><value name="Imager 6.7-6.5 micron IR (WV)" /></entry>
 *		<entry><key channel="10.20-11.20 micron IRUL" satName="GVAR"/><value name="Imager 11 micron IR" units="IRPixel"/></entry>
 *		<entry><key channel="0.58 - 0.68 micron VISL" satName="FY1C"/><value name="Imager Visible" /></entry>
 *		<entry><key channel="10.5 - 11.5 micron IRUL" satName="FY1C"/><value name="Imager 11 micron IR" units="IRPixel" /></entry>
 *		<entry><key channel="3 Channel Differencing" satName="FY1C"/><value name="Imager 3 Channel Diff" /></entry>
 *		<entry><key channel="Channel 4-5 IR" satName="FY1C"/><value name="Imager Channel 4-5 IR" /></entry>
 *		<entry><key channel="0.58 - 0.68 micron VISL" satName="FY3C"/><value name="Imager Visible" /></entry>
 *		<entry><key channel="10.5 - 11.5 micron IRUL" satName="FY3C"/><value name="Imager 11 micron IR" units="IRPixel" /></entry>
 *		<entry><key channel="3 Channel Differencing" satName="FY3C"/><value name="Imager 3 Channel Diff" /></entry>
 *		<entry><key channel="Channel 4-5 IR" satName="FY3C"/><value name="Imager Channel 4-5 IR" /></entry>
 *		<entry><key channel="AIRS" satName="GOESR-PG"/><value name="Imager Channel 4-5 IR" /></entry>
 *		<entry><key channel="ABI" satName="GOESR-PG"/><value name="Imager 6.7-6.5 micron IR (WV)" /></entry>
 *		<entry><key channel="AVHRR" satName="GOESR-PG"/><value name="Imager Visible" /></entry>
 *		<entry><key channel="MIMTPW" satName="Blended2"/><value name="Imager Based Derived Precipitable Water (PW)" /></entry>
 *	</map>
 * </physicalElements>
 * </pre>
 * 
 * The source files contains the following:
 * <pre>
 * <source>
 *	<map>
 *		<entry><key>Source</key><value>ARH</value></entry>
 *	</map>
 * </source>
 * </pre>
 * 
 * <pre>
 * 
 * SOFTWARE HISTORY
 *                   
 * date          Ticket#     Engineer    Description
 * -----------  ----------  ----------- --------------------------
 * 7/15/11                      tk    	Initial Creation                          
 * 
 * </pre>
 * 
 * @author tk
 * @version 1.0
 */

public class RegionalSatLookups {
    private static final IUFStatusHandler theHandler = UFStatus
        .getHandler(RegionalSatLookups.class);
    private static final String LOOKUP_DIR = "satellite/regionalsat";
    
    public static class PhysicalElementKey {
        public PhysicalElementKey() {
        }
        
        public PhysicalElementKey(String name, String channel) {
            this.satName = name;
            this.channel = channel;
        }
        
        @XmlAttribute(name="satName") public String satName;
        @XmlAttribute(name="channel") public String channel;
        
        @Override
        public int hashCode() {
            return satName.hashCode() + channel.hashCode();
        }
        @Override
        public boolean equals(Object obj) {
            if (obj instanceof PhysicalElementKey)
                return satName.equals(((PhysicalElementKey) obj).satName) &&
                channel.equals(((PhysicalElementKey) obj).channel);
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

        @XmlAttribute public String name;
        @XmlAttribute public String units;
    }
    
    @XmlAccessorType(XmlAccessType.NONE)
    public static abstract class AbstractLookup<K,V> {        
        public abstract Map<K, V> getMap();
    }
    
    @XmlRootElement
    @XmlAccessorType(XmlAccessType.FIELD)
    public static class PhysicalElements extends AbstractLookup<PhysicalElementKey, PhysicalElementValue> {
        public HashMap<PhysicalElementKey, PhysicalElementValue> map = 
            new HashMap<RegionalSatLookups.PhysicalElementKey, PhysicalElementValue>();

        @Override
        public Map<PhysicalElementKey, PhysicalElementValue> getMap() {
            return map;
        }
    }
    
    @XmlRootElement
    @XmlAccessorType(XmlAccessType.FIELD)
    public static class CreatingEntities extends AbstractLookup<String, String> {
        public HashMap<String, String> map = new HashMap<String, String>();

        @Override
        public Map<String, String> getMap() {
            return map;
        }
    }

    @XmlRootElement
    @XmlAccessorType(XmlAccessType.FIELD)
    private static class Source extends AbstractLookup<String, String> {
        public HashMap<String, String> map = new HashMap<String, String>();

        @Override
        public Map<String, String> getMap() {
            return map;
        }
    }
    
    private PhysicalElements physicalElementLookup;
    private CreatingEntities creatingEntityLookup;
    private Source sourceLookup;

    public RegionalSatLookups() {
        try {
            init();
        } catch (Exception e) {
            theHandler.error("Failed to initialize lookups", e);
        }
    }

    // initialize the lookup tables from the configuration files
    private void init() throws Exception {
        JAXBContext context = JAXBContext.newInstance(PhysicalElements.class,
                CreatingEntities.class, Source.class);
        Unmarshaller u = context.createUnmarshaller();
        physicalElementLookup = load(new PhysicalElements(), "physicalElements.xml", u);
        creatingEntityLookup = load(new CreatingEntities(), "creatingEntities.xml", u);
        sourceLookup = load(new Source(), "source.xml", u);
    }
    
    private static <T extends AbstractLookup<K,V>, K, V> T load(T combinedLookup, String fileName, Unmarshaller u) throws Exception {
        IPathManager pm = PathManagerFactory.getPathManager();
        
        List<LocalizationContext> contexts = Arrays.asList(pm.getLocalSearchHierarchy(LocalizationType.EDEX_STATIC));
        Collections.reverse(contexts);
        for (LocalizationContext ctx : contexts) {
            File f = pm.getFile(ctx, LOOKUP_DIR + File.separator + fileName);
            if (f != null && f.isFile()) {
                try {
                    @SuppressWarnings("unchecked")
                    T lookup = (T) u.unmarshal(f);
                    if (! combinedLookup.getClass().isAssignableFrom(lookup.getClass())) {
                        throw new Exception(String.format("file contains %s' expected %s",
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
     * Returns a PhysicalElementValue object for the given
     * parameters
     * 
     * @param String name - satellite name
     * 
     * @param String channel
     *  
     * @return A PhysicalElementValue object with the given values
     * @throws Exception
     *             If errors occur during generation of the coverage object
     */
    public PhysicalElementValue getPhysicalElement(String name, String channel) {
        return physicalElementLookup.map.get(
                new PhysicalElementKey(name, channel));
    }
    
    /**
     * Returns a String object for the given parameter
     * 
     * @param String name - satellite name
     * 
     * @return A String object with the given values
     * @throws Exception
     *             If errors occur during generation of the coverage object
     */
    public String getCreatingEntity(String name) {
        return creatingEntityLookup.map.get(name);
    }
    
    /**
     * Returns a String object for the given parameter
     * 
     * @param String name - source name
     * 
     * @return A String object with the given values
     * @throws Exception
     *             If errors occur during generation of the coverage object
     */
    public String getSource(String source) {
        return sourceLookup.map.get(source);
    }

    private static RegionalSatLookups instance;
    
    public static synchronized RegionalSatLookups getInstance() {
        if (instance == null)
            instance = new RegionalSatLookups();
        return instance;
    }
    
    public static synchronized void reload() {
        instance = new RegionalSatLookups();
    }
}
