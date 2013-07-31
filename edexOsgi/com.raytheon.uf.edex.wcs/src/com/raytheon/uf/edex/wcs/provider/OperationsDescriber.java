/*
 * The following software products were developed by Raytheon:
 *
 * ADE (AWIPS Development Environment) software
 * CAVE (Common AWIPS Visualization Environment) software
 * EDEX (Environmental Data Exchange) software
 * uFrame™ (Universal Framework) software
 *
 * Copyright (c) 2010 Raytheon Co.
 * All rights reserved. This program and the accompanying materials
 * are made available under the terms of the Eclipse Public License v1.0
 * which accompanies this distribution, and is available at
 * http://www.eclipse.org/org/documents/epl-v10.php
 *
 *
 * Contractor Name: Raytheon Company
 * Contractor Address:
 * 6825 Pine Street, Suite 340
 * Mail Stop B8
 * Omaha, NE 68106
 * 402.291.0100
 *
 *
 * SOFTWARE HISTORY
 *
 * Date         Ticket#    Engineer    Description
 * ------------ ---------- ----------- --------------------------
 * May 9, 2011            bclement     Initial creation
 *
 */
package com.raytheon.uf.edex.wcs.provider;

import java.util.ArrayList;
import java.util.Arrays;
import java.util.LinkedList;
import java.util.List;

import net.opengis.ows.v_1_1_0.DomainType;
import net.opengis.ows.v_1_1_0.ObjectFactory;

import com.raytheon.uf.edex.ogc.common.AbstractOpDescriber;
import com.raytheon.uf.edex.ogc.common.OgcOperationInfo;
import com.raytheon.uf.edex.ogc.common.OgcServiceInfo;
import com.raytheon.uf.edex.wcs.provider.OgcWcsProvider.WcsOpType;

/**
 * 
 * @author bclement
 * @version 1.0
 */
public class OperationsDescriber extends AbstractOpDescriber<WcsOpType> {

    protected ObjectFactory owsFactory = new ObjectFactory();

    /*
     * (non-Javadoc)
     * 
     * @see
     * com.raytheon.uf.edex.ogc.common.AbstractOpDescriber#getParams(com.raytheon
     * .uf.edex.ogc.common.OgcServiceInfo)
     */
    @Override
    protected List<DomainType> getParams(OgcServiceInfo<WcsOpType> serviceinfo) {
        List<DomainType> rval = new LinkedList<DomainType>();
        // TODO: this info should be passed in from somewhere
        DomainType parameter = new DomainType();
        parameter.setName("srsName");
        List<String> value = new LinkedList<String>();
        value.add("EPSG:4326");
        rval.add(parameter);
        return rval;
    }

    /*
     * (non-Javadoc)
     * 
     * @see
     * com.raytheon.uf.edex.ogc.common.AbstractOpDescriber#getOpParams(com.raytheon
     * .uf.edex.ogc.common.OgcOperationInfo)
     */
    @Override
    protected List<DomainType> getOpParams(OgcOperationInfo<WcsOpType> op) {

        List<DomainType> opParamList = new ArrayList<DomainType>();

        switch (op.getType()) {
        case GetCapabilities:
            opParamList = Arrays.asList(
                    getAsDomainType("AcceptVersions", op.getVersions()),
                    getAsDomainType("AcceptFormats", op.getFormats()),
                    getAsDomainType("service", op.getServices()));
            break;
        case DescribeCoverage:
            opParamList = Arrays.asList(
                    getAsDomainType("AcceptVersions", op.getVersions()),
                    getAsDomainType("AcceptFormats", op.getFormats()),
                    getAsDomainType("service", op.getServices()));// ,
            // getAsDomainType("Identifier", getLayerIdentifierList()));
            break;
        case GetCoverage:
            opParamList = Arrays
                    .asList(getAsDomainType("AcceptVersions", op.getVersions()),
                            getAsDomainType("AcceptFormats", op.getFormats()),
                            getAsDomainType("service", op.getServices()),
                            // getAsDomainType("Identifier",
                            // getLayerIdentifierList()),
                            getAsDomainType("InterpolationType",
                                    getInterpolationType()),
                            getAsDomainType("store",
                                    Arrays.asList("true", "false")));
            break;
        default:

        }

        return opParamList;
    }

    /**
     * @return default interpolation types
     */
    protected List<String> getInterpolationType() {
        List<String> interpolationType = new ArrayList<String>();

        interpolationType.add("nearest");
        interpolationType.add("linear");

        return interpolationType;
    }

    /**
     * @return default formats
     */
    protected List<String> getFormat() {
        return new ArrayList<String>();
    }

}
