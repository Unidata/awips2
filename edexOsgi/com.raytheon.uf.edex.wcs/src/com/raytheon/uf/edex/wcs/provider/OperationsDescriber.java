/**
 * This software was developed and / or modified by Raytheon Company,
 * pursuant to Contract DG133W-05-CQ-1067 with the US Government.
 * 
 * U.S. EXPORT CONTROLLED TECHNICAL DATA
 * This software product contains export-restricted data whose
 * export/transfer/disclosure is restricted by U.S. law. Dissemination
 * to non-U.S. persons whether in the United States or abroad requires
 * an export license or other authorization.
 * 
 * Contractor Name:        Raytheon Company
 * Contractor Address:     6825 Pine Street, Suite 340
 *                         Mail Stop B8
 *                         Omaha, NE 68106
 *                         402.291.0100
 * 
 * See the AWIPS II Master Rights File ("Master Rights File.pdf") for
 * further licensing information.
 **/
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
		// TODO: this info should be passed in from somewhere
		List<String> value = new LinkedList<String>();
		value.add("EPSG:4326");
		return Arrays.asList(getAsDomainType("srsName", value));
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
