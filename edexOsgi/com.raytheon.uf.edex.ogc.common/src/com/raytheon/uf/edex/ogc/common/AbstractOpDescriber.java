/**
 * Copyright 09/24/12 Raytheon Company.
 *
 * Unlimited Rights
 * This software was developed pursuant to Contract Number 
 * DTFAWA-10-D-00028 with the US Government. The US Government’s rights 
 * in and to this copyrighted software are as specified in DFARS
 * 252.227-7014 which was made part of the above contract. 
 */
package com.raytheon.uf.edex.ogc.common;

import java.util.ArrayList;
import java.util.Arrays;
import java.util.Collection;
import java.util.LinkedList;
import java.util.List;

import javax.xml.bind.JAXBElement;

import net.opengis.ows.v_1_1_0.AllowedValues;
import net.opengis.ows.v_1_1_0.DCP;
import net.opengis.ows.v_1_1_0.DomainType;
import net.opengis.ows.v_1_1_0.HTTP;
import net.opengis.ows.v_1_1_0.ObjectFactory;
import net.opengis.ows.v_1_1_0.Operation;
import net.opengis.ows.v_1_1_0.OperationsMetadata;
import net.opengis.ows.v_1_1_0.RequestMethodType;
import net.opengis.ows.v_1_1_0.ValueType;

/**
 * Abstract base for OGC operation descriptions. Provides common utility methods
 * for populating OGC capabilities documents.
 * 
 * <pre>
 * 
 * SOFTWARE HISTORY
 * 
 * Date         Ticket#    Engineer    Description
 * ------------ ---------- ----------- --------------------------
 * May 21, 2013            bclement     Initial creation
 * 
 * </pre>
 * 
 * @author bclement
 * @version 1.0
 */
public abstract class AbstractOpDescriber<T> {

    protected ObjectFactory owsFactory = new ObjectFactory();

    /**
     * Get OWS operations metadata from service info
     * 
     * @param serviceinfo
     * @return
     */
    public OperationsMetadata getOpData(OgcServiceInfo<T> serviceinfo) {
        OperationsMetadata rval = new OperationsMetadata();
        List<Operation> operations = new LinkedList<Operation>();
        for (OgcOperationInfo<T> op : serviceinfo.getOperations()) {
            Operation to = new Operation();
            to.setName(op.getType().toString());
            to.setDCP(getDcpList(op));
            to.setParameter(getOpParams(op));
            operations.add(to);
        }
        rval.setOperation(operations);
        List<DomainType> params = getParams(serviceinfo);
        if (params != null && !params.isEmpty()) {
            rval.setParameter(params);
        }
        List<DomainType> constraints = getConstraints(serviceinfo);
        if (constraints != null && !constraints.isEmpty()) {
            rval.setConstraint(constraints);
        }
        Object extendedCapabilities = getExtendedCapabilities(serviceinfo);
        if (extendedCapabilities != null) {
            rval.setExtendedCapabilities(extendedCapabilities);
        }
        return rval;
    }

    /**
     * @param serviceinfo
     * @return
     */
    protected Object getExtendedCapabilities(OgcServiceInfo<T> serviceinfo) {
        return null;
    }

    /**
     * @param serviceinfo
     * @return
     */
    protected List<DomainType> getConstraints(OgcServiceInfo<T> serviceinfo) {
        return new ArrayList<DomainType>(0);
    }

    /**
     * Get global parameters
     * 
     * @param serviceinfo
     * @return
     */
    protected abstract List<DomainType> getParams(OgcServiceInfo<T> serviceinfo);

    /**
     * Get HTTP endpoint information for operation
     * 
     * @param op
     * @return
     */
    protected List<DCP> getDcpList(OgcOperationInfo<T> op) {
        List<DCP> rval = new LinkedList<DCP>();
        DCP dcp = new DCP();
        HTTP http = new HTTP();
        List<JAXBElement<RequestMethodType>> value = new LinkedList<JAXBElement<RequestMethodType>>();
        if (op.hasHttpGet()) {
            RequestMethodType req = getRequestType(op.getHttpGetRes(), "KVP");
            value.add(owsFactory.createHTTPGet(req));
        }
        if (op.hasHttpPost()) {
            RequestMethodType req = getRequestType(op.getHttpPostRes(),
                    op.getPostEncoding());
            value.add(owsFactory.createHTTPPost(req));
        }
        http.setGetOrPost(value);
        dcp.setHTTP(http);
        rval.add(dcp);
        return rval;
    }

    /**
     * Create request method object
     * 
     * @param value
     * @param postEncoding
     * @return
     */
    protected RequestMethodType getRequestType(String value, String postEncoding) {
        RequestMethodType rval = owsFactory.createRequestMethodType();
        rval.setHref(value);
        rval.setConstraint(Arrays.asList(getAsDomainType("PostEncoding",
                Arrays.asList(postEncoding))));
        return rval;
    }

    /**
     * Get parameters for operation
     * 
     * @param op
     * @return
     */
    protected abstract List<DomainType> getOpParams(OgcOperationInfo<T> op);

    /**
     * Get keyed values as domain object
     * 
     * @param name
     * @param values
     * @return
     */
    protected DomainType getAsDomainType(String name, String... values) {
        return getAsDomainType(name, Arrays.asList(values));
    }

    /**
     * Get keyed values as domain object
     * 
     * @param name
     * @param values
     * @return
     */
    protected DomainType getAsDomainType(String name, Collection<String> values) {
        DomainType rval = new DomainType();
        rval.setName(name);

        AllowedValues avs = new AllowedValues();

        List<Object> toVals = new LinkedList<Object>();
        for (String val : values) {
            ValueType vt = new ValueType();
            vt.setValue(val);
            toVals.add(vt);
        }

        avs.setValueOrRange(toVals);
        rval.setAllowedValues(avs);

        return rval;
    }
}
