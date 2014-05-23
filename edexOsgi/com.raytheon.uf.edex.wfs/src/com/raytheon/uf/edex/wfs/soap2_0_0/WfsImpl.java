/**
 * Copyright 09/24/12 Raytheon Company.
 *
 * Unlimited Rights
 * This software was developed pursuant to Contract Number 
 * DTFAWA-10-D-00028 with the US Government. The US Government’s rights 
 * in and to this copyrighted software are as specified in DFARS
 * 252.227-7014 which was made part of the above contract. 
 */
package com.raytheon.uf.edex.wfs.soap2_0_0;

import javax.annotation.Resource;
import javax.xml.ws.WebServiceContext;

import net.opengis.wfs.v_2_0_0.CreateStoredQueryResponseType;
import net.opengis.wfs.v_2_0_0.CreateStoredQueryType;
import net.opengis.wfs.v_2_0_0.DescribeFeatureTypeType;
import net.opengis.wfs.v_2_0_0.DescribeStoredQueriesResponseType;
import net.opengis.wfs.v_2_0_0.DescribeStoredQueriesType;
import net.opengis.wfs.v_2_0_0.DropStoredQuery;
import net.opengis.wfs.v_2_0_0.ExecutionStatusType;
import net.opengis.wfs.v_2_0_0.FeatureCollectionType;
import net.opengis.wfs.v_2_0_0.GetCapabilitiesType;
import net.opengis.wfs.v_2_0_0.GetFeatureType;
import net.opengis.wfs.v_2_0_0.GetFeatureWithLockType;
import net.opengis.wfs.v_2_0_0.GetPropertyValueType;
import net.opengis.wfs.v_2_0_0.ListStoredQueriesResponseType;
import net.opengis.wfs.v_2_0_0.ListStoredQueriesType;
import net.opengis.wfs.v_2_0_0.LockFeatureResponseType;
import net.opengis.wfs.v_2_0_0.LockFeatureType;
import net.opengis.wfs.v_2_0_0.TransactionResponseType;
import net.opengis.wfs.v_2_0_0.TransactionType;
import net.opengis.wfs.v_2_0_0.ValueCollectionType;
import net.opengis.wfs.v_2_0_0.WFSCapabilitiesType;

import com.raytheon.uf.common.status.IUFStatusHandler;
import com.raytheon.uf.common.status.UFStatus;
import com.raytheon.uf.edex.ogc.common.http.EndpointInfo;
import com.raytheon.uf.edex.ogc.common.soap.AbstractOwsService;
import com.raytheon.uf.edex.ogc.common.soap.ServiceExceptionReport;
import com.raytheon.uf.edex.ogc.common.stats.IStatsRecorder;
import com.raytheon.uf.edex.ogc.common.stats.OperationType;
import com.raytheon.uf.edex.ogc.common.stats.ServiceType;
import com.raytheon.uf.edex.ogc.common.stats.StatsRecorderFinder;
import com.raytheon.uf.edex.soap.RequestLogController;
import com.raytheon.uf.edex.wfs.WfsException;
import com.raytheon.uf.edex.wfs.WfsException.Code;
import com.raytheon.uf.edex.wfs.request.DescFeatureTypeReq;
import com.raytheon.uf.edex.wfs.request.DescQueryReq;
import com.raytheon.uf.edex.wfs.request.GetCapReq;
import com.raytheon.uf.edex.wfs.request.GetFeatureReq;
import com.raytheon.uf.edex.wfs.soap2_0_0.util.DescribeFeatureTypeResponseType;
import com.raytheon.uf.edex.wfs.v2_0_0.Wfs2_0_0Provider;

/**
 * SOAP endpoint implementation for WFS 2.0
 * 
 * <pre>
 * 
 * SOFTWARE HISTORY
 * 
 * Date         Ticket#    Engineer    Description
 * ------------ ---------- ----------- --------------------------
 * Oct 9, 2012            bclement     Initial creation
 * 
 * </pre>
 * 
 * @author bclement
 * @version 1.0
 */

public class WfsImpl extends AbstractOwsService implements Wfs {

    protected Wfs2_0_0Provider provider;

    private static final String VERSION = "2.0.0";
    
    protected IUFStatusHandler log = UFStatus.getHandler(this.getClass());

    @Resource
    protected WebServiceContext context;

    public WfsImpl(Wfs2_0_0Provider provider) {
        this.provider = provider;
    }

    /*
     * (non-Javadoc)
     * 
     * @see
     * com.raytheon.uf.edex.wfs.soap2_0_0.Wfs#wfsGetCapabilities(net.opengis
     * .wfs.v_2_0_0.GetCapabilitiesType)
     */
    @Override
    public WFSCapabilitiesType wfsGetCapabilities(GetCapabilitiesType body)
            throws ServiceExceptionReport {
        long start = System.nanoTime();
        boolean success = true;
        EndpointInfo info = getInfo();
        try {
            return provider.getCapabilities(new GetCapReq(body), info);
        } catch (WfsException e) {
            success = false;
            throw getReport(e);
        } finally {
        	long duration = System.nanoTime() - start;
            IStatsRecorder statRecorder = StatsRecorderFinder.find();
            statRecorder.recordRequest(System.currentTimeMillis(),
            		duration, ServiceType.WFS, OperationType.QUERY, success);

            logRequestInfo(duration, success, info);
        }
    }

    private ServiceExceptionReport getReport(WfsException e) {
        return getReport(e.getCode().toString(), e.getMessage(), VERSION);
    }

    /*
     * (non-Javadoc)
     * 
     * @see
     * com.raytheon.uf.edex.wfs.soap2_0_0.Wfs#wfsDescribeFeatureType(net.opengis
     * .wfs.v_2_0_0.DescribeFeatureTypeType)
     */
    @Override
    public DescribeFeatureTypeResponseType wfsDescribeFeatureType(
            DescribeFeatureTypeType body) throws ServiceExceptionReport {
        long start = System.nanoTime();
        boolean success = true;
        EndpointInfo info = getInfo();
        try {
            return provider.describeFeatureType(
                    new DescFeatureTypeReq(body),
                    info);
        } catch (WfsException e) {
            success = false;
            throw getReport(e);
        } finally {
        	long duration = System.nanoTime() - start;
            IStatsRecorder statRecorder = StatsRecorderFinder.find();
            statRecorder.recordRequest(System.currentTimeMillis(),
            		duration, ServiceType.WFS, OperationType.QUERY, success);

            logRequestInfo(duration, success, info);
        }
    }

    /*
     * (non-Javadoc)
     * 
     * @see
     * com.raytheon.uf.edex.wfs.soap2_0_0.Wfs#wfsGetPropertyValue(net.opengis
     * .wfs.v_2_0_0.GetPropertyValueType)
     */
    @Override
    public ValueCollectionType wfsGetPropertyValue(GetPropertyValueType body)
            throws ServiceExceptionReport {
        long start = System.nanoTime();
        boolean success = true;
        EndpointInfo info = getInfo();
        try {
            return provider.getPropertyValues(body, info);
        } catch (WfsException e) {
            success = false;
            throw getReport(e);
        } finally {
        	long duration = System.nanoTime() - start;
            IStatsRecorder statRecorder = StatsRecorderFinder.find();
            statRecorder.recordRequest(System.currentTimeMillis(),
            		duration, ServiceType.WFS, OperationType.QUERY, success);

            logRequestInfo(duration, success, info);
        }
    }

    /*
     * (non-Javadoc)
     * 
     * @see
     * com.raytheon.uf.edex.wfs.soap2_0_0.Wfs#wfsGetFeature(net.opengis.wfs.
     * v_2_0_0.GetFeatureType)
     */
    @Override
    public FeatureCollectionType wfsGetFeature(GetFeatureType body)
            throws ServiceExceptionReport {
        long start = System.nanoTime();
        boolean success = true;
        EndpointInfo info = getInfo();
        try {
            return provider.getFeatureGML(new GetFeatureReq(body, provider),
                    info);
        } catch (WfsException e) {
            success = false;
            throw getReport(e);
        } finally {
        	long duration = System.nanoTime() - start;
            IStatsRecorder statRecorder = StatsRecorderFinder.find();
            statRecorder.recordRequest(System.currentTimeMillis(),
            		duration, ServiceType.WFS, OperationType.QUERY, success);

            logRequestInfo(duration, success, info);
        }
    }

    /*
     * (non-Javadoc)
     * 
     * @see
     * com.raytheon.uf.edex.wfs.soap2_0_0.Wfs#wfsGetFeatureWithLock(net.opengis
     * .wfs.v_2_0_0.GetFeatureWithLockType)
     */
    @Override
    public FeatureCollectionType wfsGetFeatureWithLock(
            GetFeatureWithLockType body) throws ServiceExceptionReport {
        throw getReport(new WfsException(Code.OperationNotSupported));
    }

    /*
     * (non-Javadoc)
     * 
     * @see
     * com.raytheon.uf.edex.wfs.soap2_0_0.Wfs#wfsLockFeature(net.opengis.wfs
     * .v_2_0_0.LockFeatureType)
     */
    @Override
    public LockFeatureResponseType wfsLockFeature(LockFeatureType body)
            throws ServiceExceptionReport {
        throw getReport(new WfsException(Code.OperationNotSupported));
    }

    /*
     * (non-Javadoc)
     * 
     * @see
     * com.raytheon.uf.edex.wfs.soap2_0_0.Wfs#wfsTransaction(net.opengis.wfs
     * .v_2_0_0.TransactionType)
     */
    @Override
    public TransactionResponseType wfsTransaction(TransactionType body)
            throws ServiceExceptionReport {
        throw getReport(new WfsException(Code.OperationNotSupported));
    }

    /*
     * (non-Javadoc)
     * 
     * @see
     * com.raytheon.uf.edex.wfs.soap2_0_0.Wfs#wfsListStoredQueries(net.opengis
     * .wfs.v_2_0_0.ListStoredQueriesType)
     */
    @Override
    public ListStoredQueriesResponseType wfsListStoredQueries(
            ListStoredQueriesType body) throws ServiceExceptionReport {
        long start = System.nanoTime();
        boolean success = true;
        EndpointInfo info = getInfo();
        try {
            return provider.listQueries(info);
        } catch (WfsException e) {
            success = false;
            throw getReport(e);
        } finally {
        	long duration = System.nanoTime() - start;
            IStatsRecorder statRecorder = StatsRecorderFinder.find();
            statRecorder.recordRequest(System.currentTimeMillis(),
            		duration, ServiceType.WFS, OperationType.QUERY, success);

            logRequestInfo(duration, success, info);
        }
    }

    /*
     * (non-Javadoc)
     * 
     * @see
     * com.raytheon.uf.edex.wfs.soap2_0_0.Wfs#wfsDescribeStoredQueries(net.opengis
     * .wfs.v_2_0_0.DescribeStoredQueriesType)
     */
    @Override
    public DescribeStoredQueriesResponseType wfsDescribeStoredQueries(
            DescribeStoredQueriesType body) throws ServiceExceptionReport {
        long start = System.nanoTime();
        boolean success = true;
        EndpointInfo info = getInfo();
        try {
            return provider.describeQueries(new DescQueryReq(body), info);
        } catch (WfsException e) {
            success = false;
            throw getReport(e);
        } finally {
        	long duration = System.nanoTime() - start;
            IStatsRecorder statRecorder = StatsRecorderFinder.find();
            statRecorder.recordRequest(System.currentTimeMillis(),
            		duration, ServiceType.WFS, OperationType.QUERY, success);

            logRequestInfo(duration, success, info);
        }
    }

    /*
     * (non-Javadoc)
     * 
     * @see
     * com.raytheon.uf.edex.wfs.soap2_0_0.Wfs#wfsCreateStoredQuery(net.opengis
     * .wfs.v_2_0_0.CreateStoredQueryType)
     */
    @Override
    public CreateStoredQueryResponseType wfsCreateStoredQuery(
            CreateStoredQueryType body) throws ServiceExceptionReport {
        throw getReport(new WfsException(Code.OperationNotSupported));
    }

    /*
     * (non-Javadoc)
     * 
     * @see
     * com.raytheon.uf.edex.wfs.soap2_0_0.Wfs#wfsDropStoredQuery(net.opengis
     * .wfs.v_2_0_0.DropStoredQuery)
     */
    @Override
    public ExecutionStatusType wfsDropStoredQuery(DropStoredQuery body)
            throws ServiceExceptionReport {
        throw getReport(new WfsException(Code.OperationNotSupported));
    }

    /*
     * (non-Javadoc)
     * 
     * @see com.raytheon.uf.edex.ogc.common.soap.AbstractOwsService#getContext()
     */
    @Override
    protected WebServiceContext getContext() {
        return context;
    }
    
    private void logRequestInfo(long durationNanos, boolean success,
			EndpointInfo endInfo) {
		if (endInfo != null &&
				RequestLogController.getInstance().shouldLogRequestsInfo() &&
				log.isPriorityEnabled(RequestLogController.getInstance().getRequestLogLevel())) {
			String requestLog = "";
			if(success){
				requestLog += "Successfully processed ";
			} else {
				requestLog += "Failed to process ";
			}
			requestLog += "request from " + endInfo.getHost() + ".  ";
			requestLog += "Duration of " + (durationNanos/1000000000.0) + "s.";
			log.handle(RequestLogController.getInstance().getRequestLogLevel(), 
					requestLog);
		}
	}

}
