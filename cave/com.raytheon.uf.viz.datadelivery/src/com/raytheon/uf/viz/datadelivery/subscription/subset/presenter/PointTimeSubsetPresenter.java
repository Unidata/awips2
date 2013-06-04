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
package com.raytheon.uf.viz.datadelivery.subscription.subset.presenter;

import com.raytheon.uf.common.datadelivery.registry.DataSet;
import com.raytheon.uf.common.datadelivery.registry.PointDataSet;
import com.raytheon.uf.common.datadelivery.registry.PointDataSetMetaData;
import com.raytheon.uf.common.datadelivery.registry.ebxml.PointDataSetMetaDataQuery;
import com.raytheon.uf.viz.datadelivery.subscription.subset.xml.PointTimeXML;

/**
 * Point timing tab Presenter.
 * 
 * <pre>
 * 
 * SOFTWARE HISTORY
 * 
 * Date         Ticket#    Engineer    Description
 * ------------ ---------- ----------- --------------------------
 * May 29, 2013   223      mpduff      Initial creation.
 * 
 * </pre>
 * 
 * @author mpduff
 * @version 1.0
 */

public class PointTimeSubsetPresenter
        extends
        DataTimingSubsetPresenter<PointDataSet, PointDataSetMetaData, IPointDataTimingSubsetView, PointTimeXML, PointDataSetMetaDataQuery> {

    /**
     * Constructor.
     * 
     * @param dataSet
     *            the PointDataSet
     * @param view
     *            The View
     */
    public PointTimeSubsetPresenter(PointDataSet dataSet,
            IPointDataTimingSubsetView view) {
        super(dataSet, view);
    }

    /**
     * {@inheritDoc}
     */
    @Override
    public boolean isValid() {
        // This is a combo box. Something is always selected. Just return true.
        return true;
    }

    /**
     * {@inheritDoc}
     */
    @Override
    public PointTimeXML getSaveInfo() {
        PointTimeXML ptx = new PointTimeXML();
        ptx.setDataRetrievalInterval(view.getDataRetrievalInterval());
        return ptx;
    }

    /**
     * {@inheritDoc}
     */
    @Override
    public void populate(PointTimeXML time, DataSet dataSet) {
        view.setDataRetrievalInterval(time.getDataRetrievalInterval());
    }

    /**
     * {@inheritDoc}
     */
    @Override
    public void updateSettings(PointTimeXML time) {
        view.setDataRetrievalInterval(time.getDataRetrievalInterval());
    }
}
