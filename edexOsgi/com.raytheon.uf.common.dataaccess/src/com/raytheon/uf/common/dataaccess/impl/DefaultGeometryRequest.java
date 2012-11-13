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
package com.raytheon.uf.common.dataaccess.impl;

import com.raytheon.uf.common.dataaccess.geom.IGeometryRequest;
import com.vividsolutions.jts.geom.Envelope;

/**
 * A default IGeometryRequest that can be used for most IGeometryRequests.
 * 
 * <pre>
 * 
 * SOFTWARE HISTORY
 * 
 * Date         Ticket#    Engineer    Description
 * ------------ ---------- ----------- --------------------------
 * Nov 5, 2012            njensen     Initial creation
 * 
 * </pre>
 * 
 * @author njensen
 * @version 1.0
 */

public class DefaultGeometryRequest extends AbstractDataRequest implements
        IGeometryRequest {

    protected Envelope envelope;

    protected String[] locationNames;

    @Override
    public void setEnvelope(Envelope env) {
        this.envelope = env;
    }

    @Override
    public Envelope getEnvelope() {
        return envelope;
    }

    @Override
    public void setLocationNames(String... locationNames) {
        this.locationNames = locationNames;

    }

    @Override
    public String[] getLocationNames() {
        return locationNames;
    }

}
