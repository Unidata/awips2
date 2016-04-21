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

//import gov.faa.nawx.v_1_5_0.ObjectFactory;

import javax.jws.WebMethod;
import javax.jws.WebParam;
import javax.jws.WebResult;
import javax.jws.WebService;
import javax.jws.soap.SOAPBinding;
import javax.xml.bind.annotation.XmlSeeAlso;
import javax.xml.ws.BindingType;

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

import org.apache.cxf.annotations.GZIP;
import org.apache.cxf.interceptor.InInterceptors;

import com.raytheon.uf.edex.ogc.common.soap.ServiceExceptionReport;
import com.raytheon.uf.edex.wfs.soap2_0_0.util.DescribeFeatureTypeResponseType;


/**
 * SOAP endpoint interface for WFS 2.0
 * 
 * <pre>
 * 
 * SOFTWARE HISTORY
 * 
 * Date         Ticket#    Engineer    Description
 * ------------ ---------- ----------- --------------------------
 * Oct 9, 2012            bclement     Initial creation
 * 10/22/2013   2472        dhladky     removed FAA dependencies, fixed path for CXFlogger.
 * 
 * </pre>
 * 
 * @author bclement
 * @version 1.0
 */
@GZIP
@InInterceptors(interceptors = "com.raytheon.uf.edex.soap.CXFLogger")
@WebService(name = "wfs", targetNamespace = "http://www.opengis.net/wfs/requests/2.0")
@SOAPBinding(parameterStyle = SOAPBinding.ParameterStyle.BARE)
@BindingType(javax.xml.ws.soap.SOAPBinding.SOAP12HTTP_BINDING)
@XmlSeeAlso({ org.w3.xmlschema.ObjectFactory.class,
        net.opengis.wfs.v_2_0_0.ObjectFactory.class,
        net.opengis.filter.v_2_0_0.ObjectFactory.class,
        net.opengis.gml.v_3_2_1.ObjectFactory.class,
        com.eurocontrol.avwx.v_1_1_1.ObjectFactory.class,
        com.eurocontrol.wx.v_1_1_1.ObjectFactory.class,
        net.opengis.sensorml.v_1_0_1_gml32.ObjectFactory.class})
public interface Wfs {

    /**
     * 
     * @param body
     * @return returns net.opengis.wfs._2.WFSCapabilitiesType
     * @throws ServiceExceptionReport
     */
    @WebMethod(operationName = "wfs.getCapabilities", action = "http://www.opengis.net/wfs/requests#GetCapabilities")
    @WebResult(name = "WFS_Capabilities", targetNamespace = "http://www.opengis.net/wfs/2.0", partName = "Body")
    public WFSCapabilitiesType wfsGetCapabilities(
            @WebParam(name = "GetCapabilities", targetNamespace = "http://www.opengis.net/wfs/2.0", partName = "Body") GetCapabilitiesType body)
            throws ServiceExceptionReport;

    /**
     * 
     * @param body
     * @return returns java.lang.String
     * @throws ServiceExceptionReport
     */
    @WebMethod(operationName = "wfs.describeFeatureType", action = "http://www.opengis.net/wfs/requests#DescribeFeatureType")
    @WebResult(name = "DescribeFeatureTypeResponse", targetNamespace = "http://www.opengis.net/wfs-util/2.0", partName = "Body")
    public DescribeFeatureTypeResponseType wfsDescribeFeatureType(
            @WebParam(name = "DescribeFeatureType", targetNamespace = "http://www.opengis.net/wfs/2.0", partName = "Body") DescribeFeatureTypeType body)
            throws ServiceExceptionReport;

    /**
     * 
     * @param body
     * @return returns net.opengis.wfs._2.ValueCollectionType
     * @throws ServiceExceptionReport
     */
    @WebMethod(operationName = "wfs.getPropertyValue", action = "http://www.opengis.net/wfs/requests#GetPropertyValue")
    @WebResult(name = "ValueCollection", targetNamespace = "http://www.opengis.net/wfs/2.0", partName = "Body")
    public ValueCollectionType wfsGetPropertyValue(
            @WebParam(name = "GetPropertyValue", targetNamespace = "http://www.opengis.net/wfs/2.0", partName = "Body") GetPropertyValueType body)
            throws ServiceExceptionReport;

    /**
     * 
     * @param body
     * @return returns net.opengis.wfs._2.FeatureCollectionType
     * @throws ServiceExceptionReport
     */
    @WebMethod(operationName = "wfs.getFeature", action = "http://www.opengis.net/wfs/requests#GetFeature")
    @WebResult(name = "FeatureCollection", targetNamespace = "http://www.opengis.net/wfs/2.0", partName = "Body")
    public FeatureCollectionType wfsGetFeature(
            @WebParam(name = "GetFeature", targetNamespace = "http://www.opengis.net/wfs/2.0", partName = "Body") GetFeatureType body)
            throws ServiceExceptionReport;

    /**
     * 
     * @param body
     * @return returns net.opengis.wfs._2.FeatureCollectionType
     * @throws ServiceExceptionReport
     */
    @WebMethod(operationName = "wfs.getFeatureWithLock", action = "http://www.opengis.net/wfs/requests#GetFeatureWithLock")
    @WebResult(name = "FeatureCollection", targetNamespace = "http://www.opengis.net/wfs/2.0", partName = "Body")
    public FeatureCollectionType wfsGetFeatureWithLock(
            @WebParam(name = "GetFeatureWithLock", targetNamespace = "http://www.opengis.net/wfs/2.0", partName = "Body") GetFeatureWithLockType body)
            throws ServiceExceptionReport;

    /**
     * 
     * @param body
     * @return returns net.opengis.wfs._2.LockFeatureResponseType
     * @throws ServiceExceptionReport
     */
    @WebMethod(operationName = "wfs.lockFeature", action = "http://www.opengis.net/wfs/requests#LockFeature")
    @WebResult(name = "LockFeatureResponse", targetNamespace = "http://www.opengis.net/wfs/2.0", partName = "Body")
    public LockFeatureResponseType wfsLockFeature(
            @WebParam(name = "LockFeature", targetNamespace = "http://www.opengis.net/wfs/2.0", partName = "Body") LockFeatureType body)
            throws ServiceExceptionReport;

    /**
     * 
     * @param body
     * @return returns net.opengis.wfs._2.TransactionResponseType
     * @throws ServiceExceptionReport
     */
    @WebMethod(operationName = "wfs.transaction", action = "http://www.opengis.net/wfs/requests#Transaction")
    @WebResult(name = "TransactionResponse", targetNamespace = "http://www.opengis.net/wfs/2.0", partName = "Body")
    public TransactionResponseType wfsTransaction(
            @WebParam(name = "Transaction", targetNamespace = "http://www.opengis.net/wfs/2.0", partName = "Body") TransactionType body)
            throws ServiceExceptionReport;

    /**
     * 
     * @param body
     * @return returns net.opengis.wfs._2.ListStoredQueriesResponseType
     * @throws ServiceExceptionReport
     */
    @WebMethod(operationName = "wfs.listStoredQueries", action = "http://www.opengis.net/wfs/requests#ListStoredQueries")
    @WebResult(name = "ListStoredQueriesResponse", targetNamespace = "http://www.opengis.net/wfs/2.0", partName = "Body")
    public ListStoredQueriesResponseType wfsListStoredQueries(
            @WebParam(name = "ListStoredQueries", targetNamespace = "http://www.opengis.net/wfs/2.0", partName = "Body") ListStoredQueriesType body)
            throws ServiceExceptionReport;

    /**
     * 
     * @param body
     * @return returns net.opengis.wfs._2.DescribeStoredQueriesResponseType
     * @throws ServiceExceptionReport
     */
    @WebMethod(operationName = "wfs.describeStoredQueries", action = "http://www.opengis.net/wfs/requests#DescribeStoredQueries")
    @WebResult(name = "DescribeStoredQueriesResponse", targetNamespace = "http://www.opengis.net/wfs/2.0", partName = "Body")
    public DescribeStoredQueriesResponseType wfsDescribeStoredQueries(
            @WebParam(name = "DescribeStoredQueries", targetNamespace = "http://www.opengis.net/wfs/2.0", partName = "Body") DescribeStoredQueriesType body)
            throws ServiceExceptionReport;

    /**
     * 
     * @param body
     * @return returns net.opengis.wfs._2.CreateStoredQueryResponseType
     * @throws ServiceExceptionReport
     */
    @WebMethod(operationName = "wfs.createStoredQuery", action = "http://www.opengis.net/wfs/requests#CreateStoredQuery")
    @WebResult(name = "CreateStoredQueryResponse", targetNamespace = "http://www.opengis.net/wfs/2.0", partName = "Body")
    public CreateStoredQueryResponseType wfsCreateStoredQuery(
            @WebParam(name = "CreateStoredQuery", targetNamespace = "http://www.opengis.net/wfs/2.0", partName = "Body") CreateStoredQueryType body)
            throws ServiceExceptionReport;

    /**
     * 
     * @param body
     * @return returns net.opengis.wfs._2.ExecutionStatusType
     * @throws ServiceExceptionReport
     */
    @WebMethod(operationName = "wfs.dropStoredQuery", action = "http://www.opengis.net/wfs/requests#DropStoredQuery")
    @WebResult(name = "DropStoredQueryResponse", targetNamespace = "http://www.opengis.net/wfs/2.0", partName = "Body")
    public ExecutionStatusType wfsDropStoredQuery(
            @WebParam(name = "DropStoredQuery", targetNamespace = "http://www.opengis.net/wfs/2.0", partName = "Body") DropStoredQuery body)
            throws ServiceExceptionReport;

}
