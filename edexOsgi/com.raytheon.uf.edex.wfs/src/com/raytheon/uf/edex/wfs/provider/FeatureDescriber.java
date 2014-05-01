/**
 * Copyright 09/24/12 Raytheon Company.
 *
 * Unlimited Rights
 * This software was developed pursuant to Contract Number 
 * DTFAWA-10-D-00028 with the US Government. The US Government’s rights 
 * in and to this copyrighted software are as specified in DFARS
 * 252.227-7014 which was made part of the above contract. 
 */
package com.raytheon.uf.edex.wfs.provider;

import java.util.ArrayList;
import java.util.List;

import com.raytheon.uf.common.status.IUFStatusHandler;
import com.raytheon.uf.common.status.UFStatus;
import com.raytheon.uf.edex.ogc.common.OgcOperationInfo;
import com.raytheon.uf.edex.ogc.common.OgcServiceInfo;
import com.raytheon.uf.edex.wfs.IWfsProvider;
import com.raytheon.uf.edex.wfs.IWfsProvider.WfsOpType;
import com.raytheon.uf.edex.wfs.WfsException;
import com.raytheon.uf.edex.wfs.WfsFeatureType;
import com.raytheon.uf.edex.wfs.reg.IWfsSource;
import com.raytheon.uf.edex.wfs.reg.WfsRegistryImpl;
import com.raytheon.uf.edex.wfs.request.DescFeatureTypeReq;
import com.raytheon.uf.edex.wfs.request.QualifiedName;

/**
 * Generates feature type descriptions for describe feature type operation
 * 
 * <pre>
 * 
 * SOFTWARE HISTORY
 * 
 * Date         Ticket#    Engineer    Description
 * ------------ ---------- ----------- --------------------------
 * Oct 25, 2012            bclement     Initial creation
 * 
 * </pre>
 * 
 * @author bclement
 * @version 1.0
 */

public class FeatureDescriber {

    protected WfsRegistryImpl registry;

    protected IWfsProvider provider;

	protected IUFStatusHandler log = UFStatus.getHandler(this.getClass());

    /**
     * 
     */
    public FeatureDescriber(WfsRegistryImpl registry, IWfsProvider provider) {
        this.registry = registry;
        this.provider = provider;
    }

    public String describe(DescFeatureTypeReq request,
            OgcServiceInfo<WfsOpType> serviceInfo) throws WfsException {
        List<QualifiedName> typenames = request.getTypenames();
        String xml;
        if (typenames == null || typenames.size() == 0) {
            xml = getAllSchemas(serviceInfo);
        } else if (typenames.size() == 1) {
            xml = getOneSchema(typenames.get(0));
        } else {
            xml = getMergedSchemas(typenames, serviceInfo);
        }
        return xml;
    }

    /**
     * @param roles
     * @param username
     * @param typenames
     * @return
     * @throws WfsException
     */
    protected String getMergedSchemas(List<QualifiedName> typenames,
            OgcServiceInfo<WfsOpType> serviceInfo) throws WfsException {
        int count = 0;
        boolean success = false;
        StringBuilder rval = new StringBuilder("<?xml version=\"1.0\" ?>\n");
        rval.append("<schema xmlns=\"http://www.w3.org/2001/XMLSchema\">\n");
        String base = getBaseDescUrl(serviceInfo)
                + "&request=describefeaturetype";
        // TODO better encoding
        base = base.replaceAll("&", "&amp;");
        for (QualifiedName name : typenames) {
            if (registry.getSource(name) == null) {
                continue;
            }
            success = true;
            if (name.getPrefix() == null) {
                name.setPrefix("ns" + count++);
            }
            rval.append("<import namespace=\"");
            rval.append(name.getNamespace());
            rval.append("\" schemaLocation=\"");
            rval.append(base);
            rval.append("&amp;namespace=xmlns(");
            rval.append(name.getPrefix());
            rval.append("=");
            rval.append(name.getNamespace());
            rval.append(")&amp;typeName=");
            rval.append(name.getPrefix());
            rval.append(":");
            rval.append(name.getName());
            rval.append("\"/>\n");
        }
        rval.append("</schema>");
        return success ? rval.toString() : null;
    }

    protected String getBaseDescUrl(OgcServiceInfo<WfsOpType> serviceInfo) {
        for (OgcOperationInfo<WfsOpType> opinfo : serviceInfo.getOperations()) {
            if (opinfo.getType().equals(WfsOpType.DescribeFeatureType)) {
                return "http://" + opinfo.getHttpBaseHostname()
                        + provider.getHttpServiceLocation();
            }
        }
        log.error("Unable to construct describe feature URL");
        return "http://localhost:8085/wfs?service=wfs";
    }

    /**
     * @param roles
     * @param username
     * @param qualifiedName
     * @return
     * @throws WfsException
     */
    protected String getOneSchema(QualifiedName feature) throws WfsException {
        IWfsSource source = registry.getSource(feature);
        return source == null ? null : source.describeFeatureType(feature);
    }

    /**
     * @param roles
     * @param username
     * @return
     * @throws WfsException
     */
    protected String getAllSchemas(OgcServiceInfo<WfsOpType> serviceInfo)
            throws WfsException {
        return getMergedSchemas(getFeatureNames(), serviceInfo);
    }

    public List<QualifiedName> getFeatureNames() {
        List<WfsFeatureType> features = registry.getFeatures();
        List<QualifiedName> rval = new ArrayList<QualifiedName>(features.size());
        for (WfsFeatureType f : features) {
            rval.add(f.getName());
        }
        return rval;
    }

}
