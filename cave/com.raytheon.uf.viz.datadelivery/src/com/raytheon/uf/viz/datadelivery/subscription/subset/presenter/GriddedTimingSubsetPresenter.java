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

import java.util.ArrayList;
import java.util.List;
import java.util.SortedSet;
import java.util.TreeSet;

import com.raytheon.uf.common.datadelivery.registry.DataSet;
import com.raytheon.uf.common.datadelivery.registry.GriddedDataSet;
import com.raytheon.uf.common.datadelivery.registry.GriddedDataSetMetaData;
import com.raytheon.uf.common.datadelivery.registry.ebxml.GriddedDataSetMetaDataQuery;
import com.raytheon.uf.common.util.CollectionUtil;
import com.raytheon.uf.viz.datadelivery.subscription.subset.xml.SpecificDateTimeXML;

/**
 * {@link DataTimingSubsetPresenter} for Gridded data.
 * 
 * <pre>
 * 
 * SOFTWARE HISTORY
 * 
 * Date         Ticket#    Engineer    Description
 * ------------ ---------- ----------- --------------------------
 * Aug 22, 2012 0743       djohnson     Initial creation
 * Aug 29, 2012 0223       mpduff       Removed cycles.
 * Sep 24, 2012 1209       djohnson     Move isValid() in from view.
 * Jan 10, 2013 1444       mpduff       Add updateSettings method.
 * 
 * </pre>
 * 
 * @author djohnson
 * @version 1.0
 */

public class GriddedTimingSubsetPresenter
        extends
        DataTimingSubsetPresenter<GriddedDataSet, GriddedDataSetMetaData, IGriddedDataTimingSubsetView, SpecificDateTimeXML, GriddedDataSetMetaDataQuery> {

    /**
     * Constructor.
     * 
     * @param dataSet
     * @param view
     */
    public GriddedTimingSubsetPresenter(GriddedDataSet dataSet,
            IGriddedDataTimingSubsetView view) {
        super(dataSet, view);
    }

    @Override
    public void init() {
        super.init();

        SortedSet<Integer> forecastHours = new TreeSet<Integer>(
                dataSet.getForecastHours());

        List<String> forecastHoursAsString = new ArrayList<String>();
        for (Integer integer : forecastHours) {
            forecastHoursAsString.add(Integer.toString(integer));
        }

        view.setAvailableForecastHours(forecastHoursAsString);
    }

    /**
     * 
     * {@inheritDoc}
     */
    @Override
    public SpecificDateTimeXML getSaveInfo() {

        SpecificDateTimeXML time = new SpecificDateTimeXML();

        String[] fcstHrs = view.getSelectedFcstHours();
        for (String hr : fcstHrs) {
            time.addHour(hr);
        }

        return time;
    }

    /**
     * Get the selected forecast hours
     * 
     * @return the selected forecast hours
     */
    public String[] getSelectedFcstHours() {
        return view.getSelectedFcstHours();
    }

    @Override
    public void populate(SpecificDateTimeXML time, DataSet dataSet) {
        view.setSelectedForecastHours(time.getFcstHours());
    }

    /**
     * Check whether the selections are valid.
     * 
     * @return true if tab is valid
     */
    @Override
    public boolean isValid() {
        String[] forecastHours = view.getSelectedFcstHours();

        return !CollectionUtil.isNullOrEmpty(forecastHours);
    }

    /**
     * {@inheritDoc}
     */
    @Override
    public void updateSettings(SpecificDateTimeXML time) {
        view.updateSelectedForecastHours(time.getFcstHours());
    }
}
