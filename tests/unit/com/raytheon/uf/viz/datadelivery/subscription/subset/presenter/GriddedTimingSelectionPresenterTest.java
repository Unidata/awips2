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

import static org.mockito.Mockito.mock;
import static org.mockito.Mockito.verify;
import static org.mockito.Mockito.when;

import java.util.ArrayList;
import java.util.List;

import org.junit.Before;
import org.junit.Test;

import com.google.common.collect.Sets;
import com.raytheon.uf.common.datadelivery.registry.GriddedDataSet;
import com.raytheon.uf.common.datadelivery.registry.OpenDapGriddedDataSet;

/**
 * Test {@link GriddedTimingSelectionPresenter}.
 * 
 * <pre>
 * 
 * SOFTWARE HISTORY
 * 
 * Date         Ticket#    Engineer    Description
 * ------------ ---------- ----------- --------------------------
 * Sep 12, 2012            mpduff     Initial creation
 * 
 * </pre>
 * 
 * @author mpduff
 * @version 1.0
 */

public class GriddedTimingSelectionPresenterTest {
    private final IGriddedTimingSelectionDlgView view = mock(IGriddedTimingSelectionDlgView.class);
    private GriddedTimingSelectionPresenter presenter;

    private final GriddedDataSet dataSet = new OpenDapGriddedDataSet();
    {
        dataSet.setDataSetName("name");
        dataSet.setProviderName("provider");
        dataSet.setForecastHours(Sets.newHashSet(0, 3, 6));
        dataSet.setCycles(Sets.newHashSet(0, 6, 12));
    }

    @Before
    public void setUp() {
        List<String> cycles = new ArrayList<String>();
        cycles.add("20120501 - 00Z");
        presenter = new GriddedTimingSelectionPresenter(view, dataSet, cycles);
    }

    @Test
    public void initSetsLatestDataCheckBox() {
        presenter.init();
        verify(view).setLatestDataCheckBox(presenter.latestDataChkConf);
    }

    @Test
    public void initSetsDateCycleList() {
        presenter.init();
        verify(view).setDateCycleList(presenter.dateCycleListConf);
    }

    @Test
    public void initSetsOkButton() {
        presenter.init();
        verify(view).setOkButton(presenter.okBtnConf);
    }

    @Test
    public void testOpenCallsViewOpen() {
        presenter.open();
        verify(view).openDlg();
    }

    @Test
    public void uncheckLatestDateEnablesSpecificDateList() {
        when(view.isLatestDataEnabled()).thenReturn(false);

        presenter.latestDataChkConf.getOnCheckedChangeAction().run();

        verify(view).setDateCycleListEnabled();
    }

    @Test
    public void checkLatestDateEnablesSpecificDateList() {
        when(view.isLatestDataEnabled()).thenReturn(true);

        presenter.latestDataChkConf.getOnCheckedChangeAction().run();

        verify(view).setDateCycleListEnabled();
    }
}
