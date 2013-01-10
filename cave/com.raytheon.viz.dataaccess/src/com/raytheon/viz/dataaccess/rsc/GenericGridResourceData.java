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
package com.raytheon.viz.dataaccess.rsc;

import javax.xml.bind.annotation.XmlAccessType;
import javax.xml.bind.annotation.XmlAccessorType;
import javax.xml.bind.annotation.XmlElement;

import com.raytheon.uf.viz.core.drawables.IDescriptor;
import com.raytheon.uf.viz.core.exception.VizException;
import com.raytheon.uf.viz.core.rsc.AbstractResourceData;
import com.raytheon.uf.viz.core.rsc.AbstractVizResource;
import com.raytheon.uf.viz.core.rsc.LoadProperties;
import com.raytheon.uf.common.dataaccess.impl.DefaultGridRequest;
import com.raytheon.uf.common.dataaccess.DataAccessLayer;
import com.raytheon.uf.common.time.DataTime;
import com.raytheon.uf.common.dataaccess.grid.IGridData;

/**
 * Uses the Data Access Framework to execute a default grid data request.
 * 
 * <pre>
 * 
 * SOFTWARE HISTORY
 * 
 * Date         Ticket#    Engineer    Description
 * ------------ ---------- ----------- --------------------------
 * Jan 8, 2013            bkowal     Initial creation
 * 
 * </pre>
 * 
 * @author bkowal
 * @version 1.0
 */

@XmlAccessorType(XmlAccessType.NONE)
public class GenericGridResourceData extends AbstractResourceData {
    @XmlElement
    private DefaultGridRequest defaultGridRequest;

    private IGridData gridData = null;

    /*
     * (non-Javadoc)
     * 
     * @see
     * com.raytheon.uf.viz.core.rsc.AbstractResourceData#construct(com.raytheon
     * .uf.viz.core.rsc.LoadProperties,
     * com.raytheon.uf.viz.core.drawables.IDescriptor)
     */
    @Override
    public AbstractVizResource<?, ?> construct(LoadProperties loadProperties,
            IDescriptor descriptor) throws VizException {
        /*
         * Use the Data Access Framework to retrieve the data.
         * 
         * TODO: move to a parent class?
         */

        // First, get the available data times based on the request
        DataTime[] dataTimes = DataAccessLayer
                .getAvailableTimes(this.defaultGridRequest);
        if (dataTimes.length > 0) {
            this.getData(dataTimes[0]);
        }

        return new GenericGridResource(this, loadProperties);
    }

    private void getData(DataTime dataTime) {
        // Now, retrieve the data using the latest data time
        IGridData[] data = DataAccessLayer.getData(this.defaultGridRequest,
                dataTime);
        if (data.length > 0) {
            this.gridData = data[0];
        }
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
        /* Do Nothing. */
    }

    /*
     * (non-Javadoc)
     * 
     * @see
     * com.raytheon.uf.viz.core.rsc.AbstractResourceData#equals(java.lang.Object
     * )
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

    /**
     * Returns the grid data that was retrieved
     * 
     * @return the grid data
     */
    public IGridData getGridData() {
        return gridData;
    }
}