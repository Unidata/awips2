package com.raytheon.uf.common.datadelivery.registry;

import static com.google.common.base.Preconditions.checkNotNull;

import java.util.List;

import javax.xml.bind.annotation.XmlAccessType;
import javax.xml.bind.annotation.XmlAccessorType;
import javax.xml.bind.annotation.XmlAttribute;
import javax.xml.bind.annotation.XmlElement;
import javax.xml.bind.annotation.XmlElements;
import javax.xml.bind.annotation.XmlRootElement;

import com.raytheon.uf.common.registry.annotations.RegistryObject;
import com.raytheon.uf.common.registry.annotations.RegistryObjectVersion;
import com.raytheon.uf.common.registry.annotations.SlotAttribute;
import com.raytheon.uf.common.serialization.annotations.DynamicSerialize;
import com.raytheon.uf.common.serialization.annotations.DynamicSerializeElement;
import com.raytheon.uf.common.time.domain.Durations;
import com.raytheon.uf.common.time.domain.api.IDuration;

/**
 * 
 * 
 * Provider Object
 * 
 * <pre>
 * 
 * SOFTWARE HISTORY
 * 
 * Date         Ticket#    Engineer    Description
 * ------------ ---------- ----------- --------------------------
 * Feb 16, 2012            dhladky     Initial creation
 * jun 11, 2013 2101       dhladky     Updated for username/password DPA exchanges
 * Dec 08, 2013 2584       dhladky     Version update
 * 
 * </pre>
 * 
 * @author dhladky
 * @version 1.0
 */

@XmlRootElement(name = "provider")
@XmlAccessorType(XmlAccessType.NONE)
@DynamicSerialize
@RegistryObject({ "name" })
@RegistryObjectVersion(value = 1.0f)
public class Provider {

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
     * Sept 10, 2013 2352      dhladky     Changed default size for point overhead
     * Nov 20,  2013 2554      dhladky     Changed WFS again, added gzipping compensation
     * 
     * </pre>
     * 
     * @author dhladky
     * @version 1.0
     */
    public enum ServiceType {

        // TODO: Only OPENDAP and WFS have the correct amounts
        OPENDAP(5000, BYTES_IN_FLOAT), WCS(5000, BYTES_IN_FLOAT), WFS(355862,
                OneByOneBox), WMS(5000, BYTES_IN_FLOAT), WXXM(5000,
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

        /**
         * Takes in a WFS request and gives you a nominal byte size based on the
         * size in lat/lon of the bounding box and the interval in multiples of
         * 5 min intervals.
         * 
         * timeSpan ~ minutes latSpan ~ span in degrees of lat for bounding box
         * lonSpan ~ span in degrees of lon for bounding box
         * 
         * @param latSpan
         * @param lonSpan
         * @param timeSpan
         * @return
         */
        public long getRequestBytesPerLatLonBoxAndTime(double latSpan,
                double lonSpan, int timeSpan) {
            // increments are in 5 minutes so 5/5 = 1
            // 30 min increment would be 30/5 = 6 etc.
            return (long) (latSpan * lonSpan * (timeSpan/5) * requestOverheadInBytes);
        }
    }
    
    private static final Integer BYTES_IN_FLOAT = Float.SIZE / Byte.SIZE;

    /** a one degree by one degree box **/
    private static final Integer OneByOneBox = 1;

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

    @XmlElement
    @DynamicSerializeElement
    private IDuration postedFileDelay = Durations.ZERO;

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
    private List<Projection> projection;

    public Provider() {

    }

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
    public IDuration getPostedFileDelay() {
        return postedFileDelay;
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
    public void setPostedFileDelay(IDuration postedFileDelay) {
        checkNotNull(postedFileDelay, "postedFileDelay cannot be null!");

        this.postedFileDelay = postedFileDelay;
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

    /**
     * @param dataSetType
     * @return
     */
    public ProviderType getProviderType(DataType dataSetType) {
        for (ProviderType providerType : getProviderType()) {
            if (providerType.getDataType().equals(dataSetType)) {
                return providerType;
            }
        }
        return null;
    }

}
