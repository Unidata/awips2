/**
 * Copyright 09/24/12 Raytheon Company.
 *
 * Unlimited Rights
 * This software was developed pursuant to Contract Number 
 * DTFAWA-10-D-00028 with the US Government. The US Government’s rights 
 * in and to this copyrighted software are as specified in DFARS
 * 252.227-7014 which was made part of the above contract. 
 */
package com.raytheon.uf.edex.wfs.v2_0_0;

import java.util.ArrayList;
import java.util.Arrays;
import java.util.List;

import net.opengis.ows.v_1_1_0.DomainType;

import com.raytheon.uf.edex.ogc.common.AbstractOpDescriber;
import com.raytheon.uf.edex.ogc.common.OgcOperationInfo;
import com.raytheon.uf.edex.ogc.common.OgcServiceInfo;
import com.raytheon.uf.edex.wfs.IWfsProvider.WfsOpType;

/**
 * Produces operations metadata for WFS 2.0 capabilities document
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
public class WfsOperationsDescriber extends AbstractOpDescriber<WfsOpType> {

    private final List<DomainType> constraints = new ArrayList<DomainType>(14);

    public WfsOperationsDescriber() {
        constraints.add(getAsDomainType("ImplementsBasicWFS", "TRUE"));
        constraints.add(getAsDomainType("ImplementsTransactionalWFS", "FALSE"));
        constraints.add(getAsDomainType("ImplementsLockingWFS", "FALSE"));
        constraints.add(getAsDomainType("KVPEncoding", "TRUE"));
        constraints.add(getAsDomainType("XMLEncoding", "TRUE"));
        constraints.add(getAsDomainType("SOAPEncoding", "TRUE"));
        constraints.add(getAsDomainType("ImplementsInheritance", "FALSE"));
        constraints.add(getAsDomainType("ImplementsRemoteResolve", "FALSE"));
        constraints.add(getAsDomainType("ImplementsResultPaging", "FALSE"));
        constraints.add(getAsDomainType("ImplementsStandardJoins", "FALSE"));
        constraints.add(getAsDomainType("ImplementsSpatialJoins", "FALSE"));
        constraints.add(getAsDomainType("ImplementsTemporalJoins", "FALSE"));
        constraints
                .add(getAsDomainType("ImplementsFeatureVersioning", "FALSE"));
        constraints.add(getAsDomainType("ManageStoredQueries", "FALSE"));
    }

    /* (non-Javadoc)
     * @see com.raytheon.uf.edex.ogc.common.AbstractOpDescriber#getParams(com.raytheon.uf.edex.ogc.common.OgcServiceInfo)
     */
    @Override
    protected List<DomainType> getParams(OgcServiceInfo<WfsOpType> serviceinfo) {
        // TODO this info should be passed in from somewhere
        return Arrays.asList(getAsDomainType("srsName",
                Arrays.asList("EPSG:4326")));
    }

    /* (non-Javadoc)
     * @see com.raytheon.uf.edex.ogc.common.AbstractOpDescriber#getOpParams(com.raytheon.uf.edex.ogc.common.OgcOperationInfo)
     */
    @Override
    protected List<DomainType> getOpParams(OgcOperationInfo<WfsOpType> op) {
        return Arrays.asList(
                getAsDomainType("AcceptVersions", op.getVersions()),
                getAsDomainType("AcceptFormats", op.getFormats()));
    }

    /*
     * (non-Javadoc)
     * 
     * @see
     * com.raytheon.uf.edex.ogc.common.AbstractOpDescriber#getConstraints(com
     * .raytheon.uf.edex.ogc.common.OgcServiceInfo)
     */
    @Override
    protected List<DomainType> getConstraints(
            OgcServiceInfo<WfsOpType> serviceinfo) {
        return constraints;
    }

}
