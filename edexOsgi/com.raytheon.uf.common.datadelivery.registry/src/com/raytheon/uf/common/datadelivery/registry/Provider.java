package com.raytheon.uf.common.datadelivery.registry;

import static com.google.common.base.Preconditions.checkNotNull;

import java.util.List;
import java.util.concurrent.TimeUnit;
import java.util.regex.Matcher;
import java.util.regex.Pattern;

import javax.persistence.Transient;
import javax.xml.bind.annotation.XmlAccessType;
import javax.xml.bind.annotation.XmlAccessorType;
import javax.xml.bind.annotation.XmlAttribute;
import javax.xml.bind.annotation.XmlElement;
import javax.xml.bind.annotation.XmlElements;
import javax.xml.bind.annotation.XmlEnum;
import javax.xml.bind.annotation.XmlEnumValue;
import javax.xml.bind.annotation.XmlRootElement;

import com.raytheon.uf.common.registry.annotations.RegistryObject;
import com.raytheon.uf.common.registry.annotations.SlotAttribute;
import com.raytheon.uf.common.serialization.ISerializableObject;
import com.raytheon.uf.common.serialization.annotations.DynamicSerialize;
import com.raytheon.uf.common.serialization.annotations.DynamicSerializeElement;

/**
 * 
 * Represents a service interface in ebRIM. Matches interface as defined in WSDL
 * 2.
 * 
 * 
 * <p>
 * Java class for ServiceInterfaceType complex type.
 * 
 * <p>
 * The following schema fragment specifies the expected content contained within
 * this class.
 * 
 * <pre>
 * &lt;complexType name="ServiceInterfaceType">
 *   &lt;complexContent>
 *     &lt;extension base="{urn:oasis:names:tc:ebxml-regrep:xsd:rim:4.0}RegistryObjectType">
 *     &lt;/extension>
 *   &lt;/complexContent>
 * &lt;/complexType>
 * </pre>
 * 
 * 
 */
@XmlRootElement(name = "provider")
@XmlAccessorType(XmlAccessType.NONE)
@DynamicSerialize
@RegistryObject({ "name" })
public class Provider implements ISerializableObject {

    /**
     * Enumeration of provider types, we know so far
     * 
     * <pre>
     * 
     * SOFTWARE HISTORY
     * 
     * Date         Ticket#    Engineer    Description
     * ------------ ---------- ----------- --------------------------
     * Feb 16, 2012            dhladky     Initial creation
     * Nov 19, 2012 1166       djohnson     Clean up JAXB representation of registry objects.
     * 
     * </pre>
     * 
     * @author dhladky
     * @version 1.0
     */
    @XmlEnum
    public enum ProviderType {
        @XmlEnumValue(ProviderType.GRID_STRING_VALUE)
        GRID(ProviderType.GRID_STRING_VALUE, "grid"), @XmlEnumValue(ProviderType.POINT_STRING_VALUE)
        POINT(ProviderType.POINT_STRING_VALUE, "point");

        private static final String GRID_STRING_VALUE = "Grid";

        private static final String POINT_STRING_VALUE = "point_data";

        private final String providerType;

        private final String plugin;

        private ProviderType(String name, String plugin) {
            providerType = name;
            this.plugin = plugin;
        }

        @Override
        public String toString() {
            return providerType;
        }

        /**
         * Return the name of the plugin responsible for the type of data.
         * 
         * @return the plugin name
         */
        public String getPlugin() {
            return plugin;
        }
    }

    /**
     * Service Type
     * 
     * <pre>
     * 
     * SOFTWARE HISTORY
     * 
     * Date         Ticket#    Engineer    Description
     * ------------ ---------- ----------- --------------------------
     * Feb 16, 2012            dhladky     Initial creation
     * Aug 16, 2012 1022       djohnson    Add bytesPerParameterRequest.
     * 
     * </pre>
     * 
     * @author dhladky
     * @version 1.0
     */
    public enum ServiceType {

        // TODO: Only OPENDAP has the correct amounts
        OPENDAP(5000, BYTES_IN_FLOAT), WCS(5000, BYTES_IN_FLOAT), WFS(5000,
                BYTES_IN_FLOAT), WMS(5000, BYTES_IN_FLOAT), WXXM(5000,
                BYTES_IN_FLOAT);

        private final long requestOverheadInBytes;

        private final long bytesPerParameterRequest;

        private ServiceType(long requestOverheadInBytes,
                long bytesPerParameterRequest) {
            this.requestOverheadInBytes = requestOverheadInBytes;
            this.bytesPerParameterRequest = bytesPerParameterRequest;
        }

        public long getRequestBytesPerParameterPerLevel(long numberOfPoints) {
            return bytesPerParameterRequest * numberOfPoints
                    + requestOverheadInBytes;
        }
    }

    private static final Integer BYTES_IN_FLOAT = Float.SIZE / Byte.SIZE;

    private static final Pattern POSTED_FILE_DELAY_PATTERN = Pattern
            .compile("\\s*(\\d+)\\s+([^\\s]+)\\s*");

    @XmlAttribute(name = "name", required = true)
    @DynamicSerializeElement
    @SlotAttribute
    private String name;

    @XmlElement(name = "connection", type = Connection.class, required = true)
    @DynamicSerializeElement
    private Connection connection;

    @XmlElements({ @XmlElement(name = "providerType", type = ProviderType.class, required = true) })
    @DynamicSerializeElement
    private List<ProviderType> providerType;

    @XmlAttribute(name = "serviceType", required = true)
    @DynamicSerializeElement
    @SlotAttribute
    private ServiceType serviceType;

    // NOTE: The @XmlElement is on the getter because JAXB must call the setter
    // for this value
    @DynamicSerializeElement
    private String postedFileDelay;

    @Transient
    private int postedFileDelayValue = 0;

    @Transient
    private TimeUnit postedFileDelayUnits = TimeUnit.HOURS;

    /**
     * The amount of time that should elapse between HTTP requests while
     * crawling the provider. Specified in milliseconds. Defaults to 500.
     */
    @XmlElement(name = "timeBetweenCrawlRequests", required = false)
    @DynamicSerializeElement
    private int timeBetweenCrawlRequests = 500;

    /**
     * The regular expression pattern searched for during a metadata purge run,
     * if the provider returned page contains the pattern the metadata will be
     * purged.
     * 
     * @see http://docs.oracle.com/javase/tutorial/essential/regex/
     */
    @XmlElement(name = "errorResponsePattern", required = true)
    @DynamicSerializeElement
    private String errorResponsePattern;

    @XmlElements({ @XmlElement(name = "projection", type = Projection.class) })
    @DynamicSerializeElement
    private List<Projection> projection;;

    public Provider() {

    };

    // TODO: Need to add a bunch of things here!

    public Connection getConnection() {
        return connection;
    }

    /**
     * @return errorResponsePattern
     */
    public String getErrorResponsePattern() {
        return errorResponsePattern;
    }

    public String getName() {
        return name;
    }

    /**
     * @return the postedFileDelay
     */
    @XmlElement(name = "postedFileDelay")
    public String getPostedFileDelay() {
        return postedFileDelay;
    }

    /**
     * Return the {@link TimeUnit} for the posted file delay.
     * 
     * @return the {@link TimeUnit}
     */
    public TimeUnit getPostedFileDelayUnits() {
        return postedFileDelayUnits;
    }

    /**
     * Return the value of the posted file delay.
     * 
     * @return the value
     */
    public int getPostedFileDelayValue() {
        return postedFileDelayValue;
    }

    public List<Projection> getProjection() {
        return projection;
    }

    public Projection getProjectionByName(String inproj) {
        for (Projection proj : getProjection()) {
            if (proj.getName().equals(inproj)) {
                return proj;
            }
        }

        return null;
    }

    public List<ProviderType> getProviderType() {
        return providerType;
    }

    public ServiceType getServiceType() {
        return serviceType;
    }

    /**
     * Return the time that should elapse between crawl requests.
     * 
     * @return the timeBetweenCrawlRequests
     */
    public int getTimeBetweenCrawlRequests() {
        return timeBetweenCrawlRequests;
    }

    public String getUrl() {
        return getConnection().getUrl();
    }

    public void setConnection(Connection connection) {
        this.connection = connection;
    }

    /**
     * @param errorResponsePattern
     *            the errorResponsePattern to set
     */
    public void setErrorResponsePattern(String errorResponsePattern) {
        this.errorResponsePattern = errorResponsePattern;
    }

    public void setName(String name) {
        this.name = name;
    }

    /**
     * Specifies the time delay to keep checking previous data along with the
     * current day.
     * 
     * <pre>
     * It consists of two parts:
     *   1) a numeric integer argument with a range between 0 and 2^31 - 1 (Integer.MAX_VALUE).
     *   2) the corresponding time units to apply to the postedFileDelay, any of the enumerated constants of TimeUnit
     *      [NANOSECONDS, MICROSECONDS, MILLISECONDS, SECONDS, MINUTES, HOURS, DAYS]
     * Defaults to 0 HOURS.
     *  
     * Examples: "3 HOURS", "1 DAYS", "30 MINUTES"
     * </pre>
     * 
     * @param postedFileDelay
     *            the postedFileDelay to set
     * @throws NullPointerException
     *             if passed a null value
     * @throws IllegalArgumentException
     *             if the string value cannot be parsed into a value and/or
     *             units
     */
    public void setPostedFileDelay(String postedFileDelay) {
        checkNotNull(postedFileDelay, "postedFileDelay cannot be null!");

        this.postedFileDelay = postedFileDelay;

        Matcher matcher = POSTED_FILE_DELAY_PATTERN.matcher(postedFileDelay);
        if (matcher.matches()) {
            postedFileDelayValue = Integer.parseInt(matcher.group(1));
            String units = matcher.group(2);
            postedFileDelayUnits = TimeUnit.valueOf(units);

            if (postedFileDelayUnits == null) {
                throw new IllegalArgumentException(units
                        + " cannot be parsed into a valid units instance!");
            }
        } else {
            throw new IllegalArgumentException(postedFileDelay
                    + " cannot be parsed into a valid value and units!");
        }
    }

    public void setProjection(List<Projection> projection) {
        this.projection = projection;
    }

    public void setProviderType(List<ProviderType> providerType) {
        this.providerType = providerType;
    }

    public void setServiceType(ServiceType serviceType) {
        this.serviceType = serviceType;
    }

    /**
     * Set the time that should elapse between crawl requests.
     * 
     * @param timeBetweenCrawlRequests
     *            the timeBetweenCrawlRequests to set
     */
    public void setTimeBetweenCrawlRequests(int timeBetweenCrawlRequests) {
        this.timeBetweenCrawlRequests = timeBetweenCrawlRequests;
    }

}
