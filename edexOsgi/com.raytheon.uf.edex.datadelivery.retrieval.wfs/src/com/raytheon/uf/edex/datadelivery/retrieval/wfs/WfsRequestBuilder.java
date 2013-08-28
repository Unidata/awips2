package com.raytheon.uf.edex.datadelivery.retrieval.wfs;

import java.text.SimpleDateFormat;
import java.util.Date;
import java.util.TimeZone;

import org.geotools.geometry.jts.ReferencedEnvelope;

import com.raytheon.uf.common.datadelivery.registry.Coverage;
import com.raytheon.uf.common.datadelivery.registry.EnvelopeUtils;
import com.raytheon.uf.common.datadelivery.registry.Provider.ServiceType;
import com.raytheon.uf.common.datadelivery.registry.Time;
import com.raytheon.uf.common.datadelivery.retrieval.util.HarvesterServiceManager;
import com.raytheon.uf.common.datadelivery.retrieval.xml.RetrievalAttribute;
import com.raytheon.uf.common.datadelivery.retrieval.xml.ServiceConfig;
import com.raytheon.uf.common.status.IUFStatusHandler;
import com.raytheon.uf.common.status.UFStatus;
import com.raytheon.uf.edex.datadelivery.retrieval.request.RequestBuilder;
import com.vividsolutions.jts.geom.Coordinate;

/**
 * 
 * WFS Request Builder.
 * 
 * <pre>
 * 
 * SOFTWARE HISTORY
 * 
 * Date         Ticket#    Engineer    Description
 * ------------ ---------- ----------- --------------------------
 * May 12, 2013 753        dhladky      created.
 * May 31, 2013 2038       djohnson     Move to correct repo.
 * Jun 11, 2013 1763       dhladky      Made operational.
 * Jun 18, 2013 2120       dhladky      Added times and max feature processing
 * Aug 07, 2013 2097       dhladky      Revamped WFS to use POST (still 1.1.0 WFS)
 * 
 * </pre>
 * 
 * @author dhladky
 * @version 1.0
 */

public class WfsRequestBuilder extends RequestBuilder {

    private static final IUFStatusHandler statusHandler = UFStatus
            .getHandler(WfsRequestBuilder.class);
    
    protected static ServiceConfig wfsServiceConfig;
    
    public static final String SEPERATOR = getServiceConfig().getConstantValue("COMMA");
    
    public static final String SLASH = getServiceConfig().getConstantValue("FORWARD_SLASH");
    
    public static final String BBOX = getServiceConfig().getConstantValue("BBOX_HEADER");
    
    public static final String SRS = getServiceConfig().getConstantValue("SRSNAME_HEADER");
    
    public static final String CRS = getServiceConfig().getConstantValue("DEFAULT_CRS");
    
    public static final String TIME = getServiceConfig().getConstantValue("TIME_HEADER");
    
    public static final String EQUALS = getServiceConfig().getConstantValue("EQUALS");
    
    public static final String BLANK = getServiceConfig().getConstantValue("BLANK");
    
    public static final String MAX = getServiceConfig().getConstantValue("MAX");
    
    public static final String VERSION = getServiceConfig().getConstantValue("VERSION");
    
    public static final String AMPERSAND = "&";
    
    private final String wfsURL;
    
    private String typeName = null;
        
    public WfsRequestBuilder(WfsRetrievalAdapter adapter,
            RetrievalAttribute attXML) {
        super(attXML);
        // Create XML doc
        this.typeName = attXML.getPlugin();
        StringBuilder buffer = new StringBuilder(256);
        buffer.append(createHeader());
        buffer.append("<ogc:Filter>\n");
        
        if (attXML.getCoverage() != null && attXML.getTime() != null) {
            buffer.append("<ogc:And>\n");
        }
        
        buffer.append(processTime(attXML.getTime()));
        buffer.append(processCoverage());
  
        if (attXML.getCoverage() != null && attXML.getTime() != null) {
            buffer.append("</ogc:And>\n");
        }
        
        buffer.append("</ogc:Filter>\n");
        buffer.append(createFooter());

        this.wfsURL = buffer.toString().trim();
    }

    private static ThreadLocal<SimpleDateFormat> ogcDateFormat = new ThreadLocal<SimpleDateFormat>() {

        @Override
        protected SimpleDateFormat initialValue() {

            SimpleDateFormat sdf = new SimpleDateFormat(getServiceConfig()
                    .getDateConfig().getFormats().get(0));
            sdf.setTimeZone(TimeZone.getTimeZone("GMT"));
            return sdf;
        }
    };
    
    /**
     * Creates the WFS XML Query header
     * @return
     */
    private String createHeader() {

        StringBuilder sb = new StringBuilder(256);
        sb.append("<?xml version=\"1.0\" ?>\n");
        sb.append("<wfs:GetFeature service=\"WFS\"\n");
        sb.append("version=\"").append(VERSION).append("\"\n");
        sb.append("outputFormat=\"application/gml+xml; version=3.1\"\n");
        sb.append("xmlns:").append(typeName).append("=\"http://").append(typeName).append(".edex.uf.raytheon.com\"\n");
        sb.append("xmlns:wfs=\"http://www.opengis.net/wfs\"\n");
        sb.append("xmlns:gml=\"http://www.opengis.net/gml\"\n");
        sb.append("xmlns:ogc=\"http://www.opengis.net/ogc\"\n");
        sb.append("xmlns:xsi=\"http://www.w3.org/2001/XMLSchema-instance\"\n");
        sb.append("xsi:schemaLocation=\"http://www.opengis.net/wfs../wfs/").append(VERSION).append("/WFS.xsd\">\n");
        sb.append("<wfs:Query typeName=\"").append(typeName).append(":").append(typeName).append("\" maxFeatures=\"").append(MAX).append("\">\n");
        
        return sb.toString();
    }
    
    /**
     * Creates the WFS XML Query Footer
     * @return
     */
    private String createFooter() {
        String footer = "</wfs:Query>\n</wfs:GetFeature>\n";
        return footer;
    }
  
    @Override
    public String processTime(Time inTime) {

        try {
            if (inTime.getStartDate() != null) {

                Date sDate = inTime.getStartDate();
                Date eDate = inTime.getEndDate();
                String endDateString = ogcDateFormat.get().format(eDate);
                String startDateString = ogcDateFormat.get().format(sDate);
                
                StringBuilder sb = new StringBuilder(256);
                sb.append("<ogc:PropertyIsGreaterThan>\n");
                sb.append("<ogc:PropertyName>").append(typeName).append(":timeObs</ogc:PropertyName>\n");
                sb.append("<ogc:Literal>").append(startDateString).append("</ogc:Literal>\n");
                sb.append("</ogc:PropertyIsGreaterThan>\n");
                
                if (endDateString != null) {
                    sb.append("<ogc:PropertyIsLessThan>\n");
                    sb.append("<ogc:PropertyName>").append(typeName).append(":timeObs</ogc:PropertyName>\n");
                    sb.append("<ogc:Literal>").append(endDateString).append("</ogc:Literal>\n");
                    sb.append("</ogc:PropertyIsLessThan>\n");
                }

                return sb.toString();
            }
        } catch (Exception e) {
            statusHandler.error("Couldn't parse Time object.", e);
        }

        return BLANK;
    }

    @Override
    public String processCoverage() {

        Coverage coverage = getRetrievalAttribute().getCoverage();

        if (coverage != null) {
            try {
                StringBuilder sb = new StringBuilder(256);
                ReferencedEnvelope re = coverage.getRequestEnvelope();
                Coordinate ll = EnvelopeUtils.getLowerLeftLatLon(re);
                Coordinate ur = EnvelopeUtils.getUpperRightLatLon(re);
                // manage the box
                double lowerLon = ll.x;
                double lowerLat = ll.y;
                double upperLon = ur.x;
                double upperLat = ur.y;

                sb.append("<ogc:Within>\n");
                sb.append("<ogc:PropertyName>location/location</ogc:PropertyName>\n");
                sb.append("<gml:Envelope srsName=\"").append(CRS)
                        .append("\">\n");
                sb.append("<gml:lowerCorner>");
                sb.append(lowerLon);
                sb.append(" ");
                sb.append(lowerLat);
                sb.append("</gml:lowerCorner>\n");
                sb.append("<gml:upperCorner>");
                sb.append(upperLon);
                sb.append(" ");
                sb.append(upperLat);
                sb.append("</gml:upperCorner>\n");
                sb.append("</gml:Envelope>\n");
                sb.append("</ogc:Within>\n");

                return sb.toString();

            } catch (Exception e) {
                statusHandler.error("Couldn't parse Coverage object.", e);
            }
        }

        return BLANK;
    }
    
    @Override
    public String getRequest() {
        return wfsURL;
    }

    @Override
    public RetrievalAttribute getAttribute() {
        return getRetrievalAttribute();
    }
    
    /**
     * Get the instance of the service config
     * @return
     */
    private static ServiceConfig getServiceConfig() {
        
        if (wfsServiceConfig == null) {
            wfsServiceConfig = HarvesterServiceManager.getInstance()
            .getServiceConfig(ServiceType.WFS);
        }
        
        return wfsServiceConfig;
    }

}
