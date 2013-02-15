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

import java.util.HashMap;
import java.util.Map;

import javax.xml.bind.annotation.XmlElement;

import com.raytheon.uf.common.dataaccess.DataAccessLayer;
import com.raytheon.uf.common.dataaccess.IData;
import com.raytheon.uf.common.dataaccess.IDataRequest;
import com.raytheon.uf.common.dataaccess.exception.TimeAgnosticDataException;
import com.raytheon.uf.common.dataaccess.impl.DefaultDataRequest;
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
 * Feb 14, 2013 1614       bsteffen    Refactor data access framework to use
 *                                     single request.
 * 
 * </pre>
 * 
 * @author bkowal
 * @version 1.0
 */

public abstract class AbstractDataAccessResourceData<X extends IData> extends
        AbstractResourceData {

    @XmlElement
    private DefaultDataRequest request;

    private DataTime[] dataTimes;

    private Map<DataTime, X[]> data;

    /**
     * Constructor
     */
    public AbstractDataAccessResourceData() {
        this.data = new HashMap<DataTime, X[]>();
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
        this.populateTimes(request);
        return this.constructResource(loadProperties, descriptor);
    }

    /**
     * Retrieves and stores the dataTimes associated using the specified
     * request.
     * 
     * @param request
     *            the request
     */
    protected void populateTimes(IDataRequest request)
            throws NoDataAvailableException {
        dataTimes = null;
        try {
            dataTimes = DataAccessLayer.getAvailableTimes(request);
            if (dataTimes == null || dataTimes.length <= 0) {
                throw new NoDataAvailableException(this.getClass());
            }
        } catch (TimeAgnosticDataException e1) {
            // Make sure that time agnostic has data before continuing.
            dataTimes = null;
            X[] data = retreiveData(request);
            if (data == null || data.length == 0) {
                throw new NoDataAvailableException(this.getClass());
            }
        }

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
    protected abstract X[] retreiveData(IDataRequest request, DataTime... times);

    /**
     * Constructs the resource
     * 
     * @param loadProperties
     * @param descriptor
     * @return the resource
     * @throws VizException
     */
    protected abstract AbstractVizResource<?, ?> constructResource(
            LoadProperties loadProperties, IDescriptor descriptor)
            throws VizException;

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
    public X[] getData(DataTime time) {
        if (data.containsKey(time)) {
            return data.get(time);
        } else if (time != null) {
            X[] data = retreiveData(request, time);
            this.data.put(time, data);
            return data;
        } else if (dataTimes == null) {
            X[] data = retreiveData(request);
            this.data.put(time, data);
            return data;
        } else {
            return null;
        }
    }

    /**
     * get the dataTimes
     * 
     * @return all available times for this data or null if time agnostic.
     */
    public DataTime[] getDataTimes() {
        return dataTimes;
    }

    /**
     * Returns the first data element that has been retrieved
     * 
     * @return the first data element that has been retrieved
     */
    public X getFirstDataElement(DataTime time) {
        X[] data = getData(time);
        if (data == null) {
            return null;
        } else {
            return data[0];
        }
    }
}