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
package com.raytheon.uf.viz.d2d.nsharp.tool;

import gov.noaa.nws.ncep.ui.nsharp.display.map.AbstractNsharpMapResource;
import gov.noaa.nws.ncep.ui.nsharp.display.map.AbstractNsharpMapResourceData;

import com.raytheon.uf.viz.core.drawables.IDescriptor;
import com.raytheon.uf.viz.core.exception.VizException;
import com.raytheon.uf.viz.core.rsc.LoadProperties;

/**
 *
 * The class for D2D NSHARP Map Resource Data
 *
 * <pre>
*
* SOFTWARE HISTORY
*
* Date          Ticket#  Engineer  Description
* ------------- -------- --------- -----------------
* Mar 23, 2020  73172    smanoj   Initial creation
 *
 * </pre>
 *
 * @author smanoj
 */

public class D2DNsharpMapResourceData extends AbstractNsharpMapResourceData {

    public D2DNsharpMapResourceData() {
        super();
    }

    @Override
    public AbstractNsharpMapResource construct(LoadProperties loadProperties,
            IDescriptor descriptor) throws VizException {
        return new D2DNsharpMapResource(this, loadProperties);
    }

    @Override
    public boolean equals(Object obj) {
        if (obj == null || !(obj instanceof D2DNsharpMapResourceData)) {
            return false;
        }
        D2DNsharpMapResourceData rdata = (D2DNsharpMapResourceData) obj;
        if (this.markerState.equals(rdata.getMarkerState())
                && this.markerType.equals(rdata.getMarkerType())
                && this.markerSize.equals(rdata.getMarkerSize())
                && this.markerWidth.equals(rdata.getMarkerWidth())
                && this.markerTextSize.equals(rdata.getMarkerTextSize())
                && this.stnMarkerType.equals(rdata.getStnMarkerType())) {
            return true;
        } else {
            return false;
        }
    }

}