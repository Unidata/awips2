package com.raytheon.uf.common.comm;

import java.text.SimpleDateFormat;
import java.util.Date;
import java.util.TimeZone;

import org.geotools.geometry.jts.ReferencedEnvelope;

import com.raytheon.uf.common.datadelivery.registry.Coverage;
import com.raytheon.uf.common.datadelivery.registry.EnvelopeUtils;
import com.raytheon.uf.common.datadelivery.registry.Time;
import com.raytheon.uf.common.datadelivery.retrieval.xml.RetrievalAttribute;
import com.raytheon.uf.common.status.IUFStatusHandler;
import com.raytheon.uf.common.status.UFStatus;
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
 * Jun 18, 2013 2120       dhladky      Initial Release
 * Oct 10, 2013 1797       bgonzale     Refactored registry Time objects.
 * 
 * </pre>
 * 
 * @author dhladky
 * @version 1.0
 */

public class MockWfsRequestBuilder  {

    private static final IUFStatusHandler statusHandler = UFStatus
            .getHandler(MockWfsRequestBuilder.class);
   
    public static final String crs = "crs:84";
    
    private final String wfsURL;
    
    private final String version = "1.1.0";
    
    private String typeName = null;
    
    public MockWfsRequestBuilder(RetrievalAttribute ra) {

        this.typeName = ra.getPlugin();
        StringBuilder buffer = new StringBuilder();
                
        buffer.append(createHeader(version, typeName));
        buffer.append("<ogc:Filter>\n");
        
        if (ra.getCoverage() != null && ra.getTime() != null) {
            buffer.append("<ogc:And>\n");
        }
        
        buffer.append(processTime(ra.getTime(), typeName));
        buffer.append(processCoverage(ra.getCoverage()));
  
        if (ra.getCoverage() != null && ra.getTime() != null) {
            buffer.append("</ogc:And>\n");
        }
        
        buffer.append("</ogc:Filter>\n");
        buffer.append(createFooter());
                
        this.wfsURL = buffer.toString().trim();
        System.out.println(wfsURL);
    }

    private static SimpleDateFormat getDateFormat() {

        SimpleDateFormat sdf = new SimpleDateFormat("yyyy-MM-dd'T'HH:mm:ss.SSS");
        sdf.setTimeZone(TimeZone.getTimeZone("GMT"));
        return sdf;

    }
   
    public String createHeader(String version, String typeName) {
        int maxfeatures = 5000;
        StringBuilder sb = new StringBuilder();
        sb.append("<?xml version=\"1.0\" ?>\n");
        sb.append("<wfs:GetFeature service=\"WFS\"\n");
        sb.append("version=\"").append(version).append("\"\n");
        sb.append("outputFormat=\"application/gml+xml; version=3.1\"\n");
        sb.append("xmlns:").append(typeName).append("=\"http://").append(typeName).append(".edex.uf.raytheon.com\"\n");
        sb.append("xmlns:wfs=\"http://www.opengis.net/wfs\"\n");
        sb.append("xmlns:gml=\"http://www.opengis.net/gml\"\n");
        sb.append("xmlns:ogc=\"http://www.opengis.net/ogc\"\n");
        sb.append("xmlns:xsi=\"http://www.w3.org/2001/XMLSchema-instance\"\n");
        sb.append("xsi:schemaLocation=\"http://www.opengis.net/wfs../wfs/").append(version).append("/WFS.xsd\">\n");
        sb.append("<wfs:Query typeName=\"").append(typeName).append(":").append(typeName).append("\" maxFeatures=\""+maxfeatures+"\">\n");
        
        return sb.toString();
    }
    
    public String createFooter() {
        StringBuilder sb = new StringBuilder();
        sb.append("</wfs:Query>\n");
        sb.append("</wfs:GetFeature>\n");
        return sb.toString();
    }
 
    public String processCoverage(Coverage coverage) {

        ReferencedEnvelope re = coverage.getRequestEnvelope();
        Coordinate ll = EnvelopeUtils.getLowerLeftLatLon(re);
        Coordinate ur = EnvelopeUtils.getUpperRightLatLon(re);
        // manage the box
        double lowerLon = ll.x;
        double lowerLat = ll.y;
        double upperLon = ur.x;
        double upperLat = ur.y;

        StringBuilder sb = new StringBuilder();
        sb.append("<ogc:Within>\n");
        sb.append("<ogc:PropertyName>location/location</ogc:PropertyName>\n");
        sb.append("<gml:Envelope srsName=\"").append(crs).append("\">\n");
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
    }
    
    public String processTime(Time inTime, String typeName) {

        try {
            if (inTime.getStart() != null) {

                Date sDate = inTime.getRequestStart();
                Date eDate = inTime.getRequestEnd();
                String endDateString = getDateFormat().format(eDate);
                String startDateString = getDateFormat().format(sDate);
                
                StringBuilder sb = new StringBuilder();
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
            statusHandler.error("Couldn't parse Time object." + e);
        }

        // no times, return blank
        return "";

    }
    
    public String getRequest() {
        return wfsURL;
    }
 
}
