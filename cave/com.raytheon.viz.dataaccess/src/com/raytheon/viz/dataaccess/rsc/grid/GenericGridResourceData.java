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
package com.raytheon.viz.dataaccess.rsc.grid;

import javax.xml.bind.annotation.XmlAccessType;
import javax.xml.bind.annotation.XmlAccessorType;
import javax.xml.bind.annotation.XmlElement;

import com.raytheon.uf.viz.core.drawables.IDescriptor;
import com.raytheon.uf.viz.core.exception.VizException;
import com.raytheon.uf.viz.core.rsc.AbstractVizResource;
import com.raytheon.uf.viz.core.rsc.LoadProperties;
import com.raytheon.uf.common.dataaccess.impl.DefaultGridRequest;
import com.raytheon.uf.common.dataaccess.grid.IGridData;
import com.raytheon.viz.dataaccess.rsc.AbstractDataAccessResourceData;

/**
 * Uses the Data Access Framework to execute a default grid data request.
 * 
 * <pre>
 * 
 * SOFTWARE HISTORY
 * 
 * Date         Ticket#    Engineer    Description
 * ------------ ---------- ----------- --------------------------
 * Jan 8, 2013            bkowal       Initial creation
 * Jan 31, 2013 #1555     bkowal       Refactor
 * 
 * </pre>
 * 
 * @author bkowal
 * @version 1.0
 */

@XmlAccessorType(XmlAccessType.NONE)
public class GenericGridResourceData extends
        AbstractDataAccessResourceData<DefaultGridRequest, IGridData> {
    @XmlElement
    private DefaultGridRequest request;

    /*
     * (non-Javadoc)
     * 
     * @see com.raytheon.viz.dataaccess.rsc.AbstractDataAccessResourceData#
     * constructResource(com.raytheon.uf.viz.core.rsc.LoadProperties,
     * com.raytheon.uf.viz.core.drawables.IDescriptor)
     */
    @Override
    protected AbstractVizResource<?, ?> constructResource(
            LoadProperties loadProperties, IDescriptor descriptor)
            throws VizException {
        return new GenericGridResource(this, loadProperties);
    }

    /*
     * (non-Javadoc)
     * 
     * @see
     * com.raytheon.viz.dataaccess.rsc.AbstractDataAccessResourceData#getRequest
     * ()
     */
    @Override
    protected DefaultGridRequest getRequest() {
        return this.request;
    }
}