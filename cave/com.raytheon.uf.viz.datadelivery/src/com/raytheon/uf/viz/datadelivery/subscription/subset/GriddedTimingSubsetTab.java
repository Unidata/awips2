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
package com.raytheon.uf.viz.datadelivery.subscription.subset;

import java.util.ArrayList;
import java.util.Arrays;
import java.util.Collections;
import java.util.List;

import org.eclipse.swt.SWT;
import org.eclipse.swt.layout.GridData;
import org.eclipse.swt.layout.GridLayout;
import org.eclipse.swt.widgets.Composite;
import org.eclipse.swt.widgets.Group;
import org.eclipse.swt.widgets.Shell;

import com.raytheon.uf.common.util.CollectionUtil;
import com.raytheon.uf.viz.datadelivery.subscription.subset.presenter.IDataTimingSubsetView;
import com.raytheon.uf.viz.datadelivery.subscription.subset.presenter.IGriddedDataTimingSubsetView;
import com.raytheon.viz.ui.widgets.duallist.DualList;
import com.raytheon.viz.ui.widgets.duallist.DualListConfig;

/**
 * {@link IDataTimingSubsetView} implementation for Gridded data.
 * 
 * <pre>
 * 
 * SOFTWARE HISTORY
 * 
 * Date         Ticket#    Engineer    Description
 * ------------ ---------- ----------- --------------------------
 * Aug 22, 2012 0743       djohnson     Initial creation
 * Aug 29, 2012 0223       mpduff       Removed cycle times.
 * Sep 07, 2012 0684       mpduff       Clear fcstHour selection before setting new selection.
 * Sep 24, 2012 1209       djohnson     Display text when there are no available cycles, move validation to presenter.
 * Jan 10, 2013 1444       mpduff       Add updateSelectedForecastHours method.
 * 
 * </pre>
 * 
 * @author djohnson
 * @version 1.0
 */

public class GriddedTimingSubsetTab extends DataTimingSubsetTab implements
        IGriddedDataTimingSubsetView {

    /** Forecast dual list */
    private DualList fcstDualList;

    /** Forecast dual configuration */
    private final DualListConfig forecastHoursDualConfig = new DualListConfig();

    private Group forecastGrp;

    /**
     * Constructor.
     * 
     * @param parentComp
     * @param dataSet
     * @param callback
     * @param shell
     */
    public GriddedTimingSubsetTab(Composite parentComp, IDataSize callback,
            Shell shell) {
        super(parentComp, callback, shell);
    }

    /**
     * {@inheritDoc}
     */
    @Override
    public void init() {
        super.init();

        GridLayout gl = new GridLayout(1, false);
        GridData gd = new GridData(SWT.FILL, SWT.DEFAULT, true, false);

        gl.horizontalSpacing = 0;
        gl.verticalSpacing = 0;
        gl.marginWidth = 0;
        gl.marginHeight = 0;

        forecastGrp = new Group(parentComp, SWT.NONE);
        forecastGrp.setText(" Forecast Hours ");
        forecastGrp.setLayout(gl);
        forecastGrp.setLayoutData(gd);

        forecastHoursDualConfig.setAvailableListLabel("Available Hours:");
        forecastHoursDualConfig.setSelectedListLabel("Selected Hours:");
        forecastHoursDualConfig.setListHeight(125);
        forecastHoursDualConfig.setListWidth(175);
        forecastHoursDualConfig.setShowUpDownBtns(false);
        forecastHoursDualConfig.setNumericData(true);

        fcstDualList = new DualList(forecastGrp, SWT.NONE,
                forecastHoursDualConfig, this);
    }

    /**
     * 
     * {@inheritDoc}
     */
    @Override
    public String[] getSelectedFcstHours() {
        return this.fcstDualList.getSelectedListItems();
    }

    /**
     * {@inheritDoc}
     */
    @Override
    public void setSelectedForecastHours(List<String> fcstHours) {
        fcstDualList.clearSelection();

        if (!CollectionUtil.isNullOrEmpty(fcstHours)) {
            String[] selHours = fcstHours.toArray(new String[fcstHours.size()]);
            fcstDualList.selectItems(selHours);
        }
    }

    /**
     * {@inheritDoc}
     */
    @Override
    public void setAvailableForecastHours(List<String> forecastHours) {
        fcstDualList.setFullList(new ArrayList<String>(forecastHours));
    }

    /**
     * {@inheritDoc}
     */
    @Override
    public void updateSelectedForecastHours(List<String> fcstHours) {
        List<String> selectedHrs = new ArrayList<String>();
        String[] selectedItems = fcstDualList.getSelectedSelection();

        // Add the saved hours
        for (String fcstHr : fcstDualList.getAvailableListItems()) {
            if (fcstHours.contains(fcstHr)) {
                selectedHrs.add(fcstHr);
            }
        }

        // Add in the previously selected hours
        selectedHrs.addAll(Arrays.asList(selectedItems));

        // Sort the hours
        List<Integer> intList = new ArrayList<Integer>();
        for (String hr : selectedHrs) {
            intList.add(Integer.parseInt(hr));
        }

        Collections.sort(intList);

        selectedHrs.clear();
        for (int i : intList) {
            selectedHrs.add(String.valueOf(i));
        }
        fcstDualList.selectItems(selectedHrs.toArray(new String[selectedHrs
                .size()]));
    }
}
