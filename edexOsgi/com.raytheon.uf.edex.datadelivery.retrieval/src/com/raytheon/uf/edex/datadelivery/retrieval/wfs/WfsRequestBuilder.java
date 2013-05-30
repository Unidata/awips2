package com.raytheon.uf.edex.datadelivery.retrieval.wfs;

import java.text.SimpleDateFormat;
import java.util.Date;

import org.geotools.geometry.jts.ReferencedEnvelope;

import com.raytheon.uf.common.datadelivery.registry.Coverage;
import com.raytheon.uf.common.datadelivery.registry.Provider.ServiceType;
import com.raytheon.uf.common.datadelivery.registry.Time;
import com.raytheon.uf.common.datadelivery.retrieval.util.HarvesterServiceManager;
import com.raytheon.uf.common.datadelivery.retrieval.xml.RetrievalAttribute;
import com.raytheon.uf.common.datadelivery.retrieval.xml.ServiceConfig;
import com.raytheon.uf.common.status.IUFStatusHandler;
import com.raytheon.uf.common.status.UFStatus;
import com.raytheon.uf.edex.datadelivery.retrieval.request.RequestBuilder;

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
 * May 12, 2013 753        dhladky     created.
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
        buffer.append("?");
        // apply the feature you are trying to retrieve
        buffer.append(getRetrievalAttribute().getParameter().getProviderName());
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
            return sdf;
        }
    };

    @Override
    public String processTime(Time time) {

        try {
            if (time.getEndDate() != null && time.getStartDate() != null) {
                Date sDate = time.getStartDate();
                Date eDate = time.getEndDate();
                String endDateString = ogcDateFormat.get().format(eDate);
                String startDateString = ogcDateFormat.get().format(sDate);
                StringBuilder sb = new StringBuilder();
                sb.append("&TIME=");
                sb.append(startDateString);
                sb.append("/");

                if (!endDateString.equals(startDateString)) {
                    sb.append(endDateString);
                }

                return sb.toString();
            }
        } catch (Exception e) {
            statusHandler.error("Couldn't parse Time object." + e);
        }

        // no times, return blank
        return getServiceConfig().getConstantValue("BLANK");
    }

    @Override
    public String processCoverage() {

        // &srsName=crs:84&bbox=-100.0,41.0,-98.0,43.0
        StringBuilder sb = new StringBuilder();
        Coverage coverage = getRetrievalAttribute().getCoverage();

        if (coverage != null) {
            ReferencedEnvelope re = coverage.getRequestEnvelope();
            // manage the box
            double lowerLon = re.getMinX();
            double lowerLat = re.getMinY();
            double upperLon = re.getMaxX();
            double upperLat = re.getMaxY();

            sb.append("&srsName=");
            sb.append(coverage.getEnvelope().getCoordinateReferenceSystem()
                    .getName());
            sb.append("&bbox=");
            sb.append(lowerLon);
            sb.append(separator);
            sb.append(lowerLat);
            sb.append(separator);
            sb.append(upperLon);
            sb.append(separator);
            sb.append(upperLat);
            
            return sb.toString();
        }

        return getServiceConfig().getConstantValue("BLANK");
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
