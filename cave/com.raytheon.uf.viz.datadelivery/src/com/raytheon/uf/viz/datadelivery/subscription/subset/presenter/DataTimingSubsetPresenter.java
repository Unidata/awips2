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

import com.google.common.annotations.VisibleForTesting;
import com.raytheon.uf.common.datadelivery.registry.DataSet;
import com.raytheon.uf.common.datadelivery.registry.DataSetMetaData;
import com.raytheon.uf.common.datadelivery.registry.ebxml.DataSetMetaDataFilterableQuery;
import com.raytheon.uf.viz.datadelivery.subscription.subset.xml.TimeXML;

/**
 * The presenter class that handles logic for {@link IDataTimingSubsetView}
 * classes. The model-view-presenter pattern (MVP) allows all of the logic and
 * important state to be gui implementation independent, which greatly assists
 * testing. This class is thread-safe.
 * 
 * @see http://aspiringcraftsman.com/tag/model-view-presenter
 * 
 *      <pre>
 * 
 * SOFTWARE HISTORY
 * 
 * Date         Ticket#    Engineer    Description
 * ------------ ---------- ----------- --------------------------
 * Aug 15, 2012 0743       djohnson     Initial creation
 * Aug 29, 2012 0223       mpduff       Changed as result of renamed objects.
 * Sep 24, 2012  1209      djohnson     Sub-classes provide isValid() implementations.
 * 
 * </pre>
 * 
 * @author djohnson
 * @version 1.0
 */
public abstract class DataTimingSubsetPresenter<DATASET extends DataSet, DATASETMETADATA extends DataSetMetaData, VIEW extends IDataTimingSubsetView, TIMEXML extends TimeXML, QUERY extends DataSetMetaDataFilterableQuery<DATASETMETADATA>> {
    @VisibleForTesting
    final String POPUP_TITLE = "Notice";

    @VisibleForTesting
    final String NO_DATA_FOR_DATE_AND_CYCLE = "No data is available for the specified date and cycle combination.";

    @VisibleForTesting
    final String MORE_THAN_ONE_CYCLE_SELECTED = "Adhoc queries can only be requested for a single cycle.";

    @VisibleForTesting
    final String SUBSCRIPTIONS_ONLY_USE_LATEST_DATE = "Subscriptions always use the latest date, only adhoc queries support specific date requests."
            + "  Overriding options to use the latest date.";

    protected final VIEW view;

    protected DATASET dataSet;

    /**
     * @param dataSet
     * @param dataTimingSubsetTab
     */
    public DataTimingSubsetPresenter(DATASET dataSet, VIEW view) {
        this.dataSet = dataSet;
        this.view = view;
    }

    /**
     * Initialize the presenter.
     */
    public void init() {
        view.init();
    }

    /**
     * @return
     */
    public boolean isDirty() {
        return view.isDirty();
    }

    /**
     * Check whether the view is valid.
     *
     * @return true if the view is valid
     */
    public abstract boolean isValid();

    /**
     * Set boolean to whether or not date or cycle selections have changed.
     *
     * @param dateCycleDirty
     *            time/cycle have changed
     */
    public void setDirty(boolean b) {
        view.setDirty(b);
    }

    /**
     * Get the save information for this data timing presenter.
     *
     * @return the save information
     */
    public abstract TIMEXML getSaveInfo();

    /**
     * Restore this data timing presenter from the save information.
     *
     * @param time
     *            the saved time information
     */
    public abstract void populate(TIMEXML time, DataSet dataSet);
}
