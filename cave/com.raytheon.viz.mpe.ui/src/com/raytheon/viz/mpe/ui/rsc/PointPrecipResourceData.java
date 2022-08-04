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
package com.raytheon.viz.mpe.ui.rsc;

import javax.xml.bind.annotation.XmlAccessType;
import javax.xml.bind.annotation.XmlAccessorType;
import javax.xml.bind.annotation.XmlRootElement;

import com.raytheon.uf.viz.core.drawables.IDescriptor;
import com.raytheon.uf.viz.core.exception.VizException;
import com.raytheon.uf.viz.core.rsc.LoadProperties;
import com.raytheon.viz.hydrocommon.whfslib.colorthreshold.ColorLookupParameters;
import com.raytheon.viz.mpe.ui.MPEDisplayManager;

/**
 * TODO Add Description
 * 
 * <pre>
 * 
 * SOFTWARE HISTORY
 * Date         Ticket#    Engineer    Description
 * ------------ ---------- ----------- --------------------------
 * Feb 27, 2009            randerso     Initial creation
 * 
 * </pre>
 * 
 * @author randerso
 * @version 1.0
 */
@XmlAccessorType(XmlAccessType.NONE)
@XmlRootElement(name = "pointPrecipResourceData")
public class PointPrecipResourceData extends
		PointResourceData<PointPrecipPlotResource> {


	public PointPrecipResourceData() {
	}

	public PointPrecipResourceData(MPEDisplayManager mpeDisplayManager,
			String name, ColorLookupParameters lookupParameters) {
		super(mpeDisplayManager, name, lookupParameters);
	}
    /*
     * (non-Javadoc)
     * 
     * @see
     * com.raytheon.uf.viz.core.rsc.AbstractResourceData#construct(com.raytheon
     * .uf.viz.core.comm.LoadProperties,
     * com.raytheon.uf.viz.core.drawables.IDescriptor)
     */
    @Override
    public PointPrecipPlotResource construct(LoadProperties loadProperties,
            IDescriptor descriptor) throws VizException {
		return new PointPrecipPlotResource(this, loadProperties);
    }

    /*
     * (non-Javadoc)
     * 
     * @see
     * com.raytheon.uf.viz.core.rsc.AbstractResourceData#update(java.lang.Object
     * )
     */
    @Override
    public void update(Object updateData) {

    }

    /*
     * (non-Javadoc)
     * 
     * @see java.lang.Object#hashCode()
     */
    @Override
    public int hashCode() {
        final int prime = 31;
        int result = 1;
		result = prime
				* result
				+ ((mpeDisplayManager == null) ? 0 : mpeDisplayManager
						.hashCode());
        return result;
    }

    /*
     * (non-Javadoc)
     * 
     * @see java.lang.Object#equals(java.lang.Object)
     */
    @Override
    public boolean equals(Object obj) {
        if (this == obj) {
            return true;
        }
        if (getClass() != obj.getClass()) {
            return false;
        }
        return true;
    }


}
