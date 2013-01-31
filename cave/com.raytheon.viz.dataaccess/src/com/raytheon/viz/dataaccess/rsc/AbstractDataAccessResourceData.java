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

import com.raytheon.uf.common.dataaccess.DataAccessLayer;
import com.raytheon.uf.common.dataaccess.IData;
import com.raytheon.uf.common.dataaccess.IDataRequest;
import com.raytheon.uf.common.dataaccess.exception.TimeAgnosticDataException;
import com.raytheon.uf.common.time.DataTime;
import com.raytheon.uf.viz.core.drawables.IDescriptor;
import com.raytheon.uf.viz.core.exception.NoDataAvailableException;
import com.raytheon.uf.viz.core.exception.VizException;
import com.raytheon.uf.viz.core.rsc.AbstractResourceData;
import com.raytheon.uf.viz.core.rsc.AbstractVizResource;
import com.raytheon.uf.viz.core.rsc.LoadProperties;

/**
 * Abstracts the retrieval of data using the Data Access Framework.
 * 
 * <pre>
 * 
 * SOFTWARE HISTORY
 * 
 * Date         Ticket#    Engineer    Description
 * ------------ ---------- ----------- --------------------------
 * Jan 31, 2013            bkowal     Initial creation
 * 
 * </pre>
 * 
 * @author bkowal
 * @version 1.0
 */

public abstract class AbstractDataAccessResourceData<T extends IDataRequest<X>, X extends IData>
        extends AbstractResourceData {

    private X[] data;

    /**
     * Constructor
     */
    public AbstractDataAccessResourceData() {
        this.data = null;
    }

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
        this.retrieveData(this.getRequest());
        return this.constructResource(loadProperties, descriptor);
    }

    /**
     * Retrieves and stores the data associated using the specified request.
     * 
     * @param request
     *            the request
     * @param clazz
     *            the class that is requesting the data; will always be a
     *            subclass of AbstractDataAccessResourceData
     */
    private void retrieveData(T request) throws NoDataAvailableException {
        this.data = null;
        boolean timeAgnostic = false;

        DataTime[] dataTimes = null;
        try {
            dataTimes = DataAccessLayer.getAvailableTimes(request);
        } catch (TimeAgnosticDataException e1) {
            timeAgnostic = true;
        }

        if (timeAgnostic) {
            /*
             * If we were a legitimate resource, we would want to cache time
             * agnostic data that was retrieved. The cache could be a simple Map
             * or even the cache provided by GOOG in the guava library.
             */
            this.data = DataAccessLayer.getData(request);
        } else {
            if (dataTimes == null || dataTimes.length <= 0) {
                throw new NoDataAvailableException(this.getClass());
            }

            /* Just use the latest time since this is a sample / test resource */
            this.data = DataAccessLayer.getData(request, dataTimes[0]);
        }

        if (this.data == null || this.data.length <= 0) {
            throw new NoDataAvailableException(this.getClass());
        }
    }

    /**
     * Retrieves the data access framework request associated with the resource data
     * 
     * @return the data access framework request to execute
     */
    protected abstract T getRequest();

    /**
     * Constructs the resource
     * 
     * @param loadProperties
     * @param descriptor
     * @return the resource
     * @throws VizException
     */
    protected abstract AbstractVizResource<?, ?> constructResource(
            LoadProperties loadProperties, IDescriptor descriptor) throws VizException;

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
     * Returns all of the data that was retrieved
     * 
     * @return all of the data that was retrieved
     */
    public X[] getData() {
        return data;
    }

    /**
     * Returns the first data element that has been retrieved
     * 
     * @return the first data element that has been retrieved
     */
    public X getFirstDataElement() {
        return (this.data == null) ? null : this.data[0];
    }
}