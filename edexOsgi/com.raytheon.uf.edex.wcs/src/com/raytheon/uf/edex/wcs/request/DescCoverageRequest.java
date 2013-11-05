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
package com.raytheon.uf.edex.wcs.request;

import java.util.Collection;
import java.util.Iterator;
import java.util.List;

import net.opengis.wcs.v_1_1_2.DescribeCoverage;

public class DescCoverageRequest extends WcsRequest{

	protected String outputformat = "text/xml; subtype=gml/3.1.1";
	
    protected String[] externalIds;

    protected String[] internalIds;
	
	public DescCoverageRequest() {
		super(Type.DescribeCoverage);
	}

    public DescCoverageRequest(DescribeCoverage req) {
        super(Type.DescribeCoverage);
        this.request = req;
        List<String> ids = req.getIdentifier();
        this.externalIds = new String[ids.size()];
        this.internalIds = new String[ids.size()];
        setIdentifiers(req.getIdentifier());
    }

	public String getOutputformat() {
		return outputformat;
	}

	public void setOutputformat(String outputformat) {
		this.outputformat = outputformat;
	}

    /**
     * @return the externalIds
     */
    public String[] getExternalIds() {
        return externalIds;
    }

    /**
     * @param externalIds
     *            the externalIds to set
     */
    public void setExternalIds(String[] externalIds) {
        this.externalIds = externalIds;
    }

    /**
     * @return the internalIds
     */
    public String[] getInternalIds() {
        return internalIds;
    }

    /**
     * @param internalIds
     *            the internalIds to set
     */
    public void setInternalIds(String[] internalIds) {
        this.internalIds = internalIds;
    }

    /**
     * @param array
     */
    public void setIdentifiers(Collection<String> ids) {
        Iterator<String> iter = ids.iterator();
        for (int i = 0; iter.hasNext(); ++i) {
            externalIds[i] = iter.next();
            internalIds[i] = externalToInternal(externalIds[i]);
        }
    }

}
