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

import static org.mockito.Matchers.anyListOf;
import static org.mockito.Mockito.mock;
import static org.mockito.Mockito.verify;

import org.junit.Before;
import org.junit.Test;

import com.google.common.collect.Sets;
import com.raytheon.uf.common.datadelivery.registry.GriddedDataSet;
import com.raytheon.uf.common.datadelivery.registry.OpenDapGriddedDataSet;

/**
 * Test {@link GriddedTimingSubsetPresenter}.
 *
 * <pre>
 *
 * SOFTWARE HISTORY
 *
 * Date         Ticket#    Engineer    Description
 * ------------ ---------- ----------- --------------------------
 * Aug 15, 2012 0743       djohnson     Initial creation
 *
 * </pre>
 *
 * @author djohnson
 * @version 1.0
 */
public class GriddedTimingSubsetPresenterTest {
    private final IGriddedDataTimingSubsetView view = mock(IGriddedDataTimingSubsetView.class);

    private final GriddedDataSet dataSet = new OpenDapGriddedDataSet();
    {
        dataSet.setDataSetName("name");
        dataSet.setProviderName("provider");
        dataSet.setForecastHours(Sets.newHashSet(0, 3, 6));
        dataSet.setCycles(Sets.newHashSet(0, 6, 12));
    }

    private final GriddedTimingSubsetPresenter presenter = new GriddedTimingSubsetPresenter(
            dataSet, view);

    @Before
    public void setUp() {
        presenter.init();
    }


    @Test
    public void initSetsForecastHours() {
        verify(view).setAvailableForecastHours(anyListOf(String.class));
    }
}
