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
    
    public static final String separator = getServiceConfig().getConstantValue("COMMA");
    
    public static final String slash = getServiceConfig().getConstantValue("FORWARD_SLASH");
    
    public static final String bbox = getServiceConfig().getConstantValue("BBOX_HEADER");
    
    public static final String srs = getServiceConfig().getConstantValue("SRSNAME_HEADER");
    
    public static final String crs = getServiceConfig().getConstantValue("DEFAULT_CRS");
    
    public static final String time = getServiceConfig().getConstantValue("TIME_HEADER");
    
    public static final String equals = getServiceConfig().getConstantValue("EQUALS");
    
    public static final String blank = getServiceConfig().getConstantValue("BLANK");
    
    public static final String ampersand = "&";
    
    private final String wfsURL;
    
    WfsRequestBuilder(WfsRetrievalAdapter adapter,
            RetrievalAttribute attXML) {
        super(attXML);
        // Create URL
        // this works in this order
        StringBuilder buffer = new StringBuilder();
        // apply the base WFS service URL
        buffer.append(adapter.getProviderRetrievalXMl().getConnection()
                .getUrl());
        // process the coverage bounding box
        buffer.append(processCoverage());
        // process the time range you are trying to retrieve
        buffer.append(processTime(getRetrievalAttribute().getTime()));
        
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

    @Override
    public String processTime(Time inTime) {

        try {
            if (inTime.getEndDate() != null && inTime.getStartDate() != null) {
            
                
                Date sDate = inTime.getStartDate();
                Date eDate = inTime.getEndDate();
                String endDateString = ogcDateFormat.get().format(eDate);
                String startDateString = ogcDateFormat.get().format(sDate);
                StringBuilder sb = new StringBuilder();
                sb.append(ampersand);
                sb.append(time);
                sb.append(equals);
                sb.append(startDateString);
                sb.append(slash);

                if (!endDateString.equals(startDateString)) {
                    sb.append(endDateString);
                }

                return sb.toString();
            }
        } catch (Exception e) {
            statusHandler.error("Couldn't parse Time object." + e);
        }

        // no times, return blank
        return blank;

    }

    @Override
    public String processCoverage() {

        StringBuilder sb = new StringBuilder();
        Coverage coverage = getRetrievalAttribute().getCoverage();

        if (coverage != null) {

            ReferencedEnvelope re = coverage.getRequestEnvelope();
            Coordinate ll = EnvelopeUtils.getLowerLeftLatLon(re);
            Coordinate ur = EnvelopeUtils.getUpperRightLatLon(re);
            // manage the box
            double lowerLon = ll.x;
            double lowerLat = ll.y;
            double upperLon = ur.x;
            double upperLat = ur.y;

            sb.append(ampersand);
            sb.append(srs);
            sb.append(equals);
            sb.append(crs);
            //TODO Revisit this when we have a better idea of how to switch them
            //sb.append(coverage.getEnvelope().getCoordinateReferenceSystem()
            //        .getName());
            sb.append(ampersand);
            sb.append(bbox);
            sb.append(equals);
            sb.append(lowerLon);
            sb.append(separator);
            sb.append(lowerLat);
            sb.append(separator);
            sb.append(upperLon);
            sb.append(separator);
            sb.append(upperLat);
            
            return sb.toString();
        }

        return blank;
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
