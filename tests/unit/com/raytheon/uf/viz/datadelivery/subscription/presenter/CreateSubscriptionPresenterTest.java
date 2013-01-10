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
package com.raytheon.uf.viz.datadelivery.subscription.presenter;

import static com.raytheon.uf.common.util.Matchers.yyyyMmDdMatches;
import static org.junit.Assert.assertTrue;
import static org.mockito.Matchers.any;
import static org.mockito.Matchers.anyString;
import static org.mockito.Matchers.argThat;
import static org.mockito.Matchers.same;
import static org.mockito.Mockito.mock;
import static org.mockito.Mockito.never;
import static org.mockito.Mockito.times;
import static org.mockito.Mockito.verify;
import static org.mockito.Mockito.when;

import java.text.ParseException;
import java.util.Calendar;
import java.util.Date;

import org.junit.After;
import org.junit.Before;
import org.junit.Test;

import com.google.common.collect.Sets;
import com.raytheon.uf.common.datadelivery.registry.GriddedDataSet;
import com.raytheon.uf.common.datadelivery.registry.OpenDapGriddedDataSet;
import com.raytheon.uf.common.datadelivery.registry.Subscription;
import com.raytheon.uf.common.datadelivery.registry.SubscriptionBuilder;
import com.raytheon.uf.common.datadelivery.registry.SubscriptionFixture;
import com.raytheon.uf.common.registry.handler.RegistryHandlerException;
import com.raytheon.uf.common.time.util.TimeUtil;
import com.raytheon.uf.common.time.util.TimeUtilTest;
import com.raytheon.uf.viz.core.SameThreadGuiTaskExecutor;
import com.raytheon.uf.viz.datadelivery.subscription.ISubscriptionService;
import com.raytheon.uf.viz.datadelivery.subscription.ISubscriptionService.ISubscriptionServiceResult;
import com.raytheon.uf.viz.datadelivery.subscription.SubscriptionService.IForceApplyPromptDisplayText;
import com.raytheon.uf.viz.datadelivery.subscription.view.ICreateSubscriptionDlgView;

/**
 * Test {@link CreateSubscriptionDlgPresenter}.
 * 
 * <pre>
 * 
 * SOFTWARE HISTORY
 * 
 * Date         Ticket#    Engineer    Description
 * ------------ ---------- ----------- --------------------------
 * Aug 24, 2012            mpduff       Initial creation
 * Oct 17, 2012 0726       djohnson     Remove unused registry code.
 * Nov 09, 2012 1286       djohnson     Add test for storing subscription.
 * Nov 20, 2012 1286       djohnson     {@link ISubscriptionService} now takes the view.
 * Jan 02, 2012 1345       djohnson     Remove obsolete test.
 * Jan 04, 2013 1453       djohnson     Add tests for setting the active period.
 * 
 * </pre>
 * 
 * @author mpduff
 * @version 1.0
 */

public class CreateSubscriptionPresenterTest {

    private final ICreateSubscriptionDlgView view = mock(ICreateSubscriptionDlgView.class);

    private final GriddedDataSet dataSet = new OpenDapGriddedDataSet();
    {
        dataSet.setDataSetName("name");
        dataSet.setProviderName("provider");
        dataSet.setForecastHours(Sets.newHashSet(0, 3, 6));
        dataSet.setCycles(Sets.newHashSet(0, 6, 12));
    }

    private final CreateSubscriptionDlgPresenter presenter = new CreateSubscriptionDlgPresenter(
            view, dataSet, true, new SameThreadGuiTaskExecutor());

    private final ISubscriptionService service = mock(ISubscriptionService.class);

    @Before
    public void setUp() {
        CreateSubscriptionDlgPresenter.subscriptionService = service;
        presenter.init();
    }

    @After
    public void tearDown() {
        TimeUtilTest.resumeTime();
    }

    @Test
    public void testOpenCallsViewOpen() throws Exception {
        presenter.open();
        verify(view).openDlg();
    }

    @Test
    public void testInitDeliverComboIsSet() {
        verify(view).setDeliveryOptionsComboConf(presenter.DELIVERY_COMBO_CONF);
    }

    @Test
    public void verifyDeliveryComboBoxConfiguration() {
        verify(view).setDeliveryOptions(presenter.DELIVERY_OPTIONS);
    }

    @Test
    public void testInitOkBtnIsSet() {
        verify(view).setOkConf(presenter.OK_CONF);
    }

    @Test
    public void verifyOkButtonConfiguration() {
        verify(view).setOkConf(presenter.OK_CONF);
    }

    @Test
    public void verifyDurationValidationNoExpriationCheck() {
        when(view.isNoExpirationDate()).thenReturn(true);
        assertTrue(presenter.durationValidChk());
        verify(view).isNoExpirationDate();
    }

    @Test
    public void verifyDurationValidationDates() {
        when(view.getStartText()).thenReturn("12/01/1999 01");
        when(view.getExpirationText()).thenReturn("12/31/1999 01");
        assertTrue(presenter.durationValidChk());
        verify(view).isNoExpirationDate();
        verify(view, times(2)).getStartText();
        verify(view, times(2)).getExpirationText();
    }

    @Test
    public void verifyActivePeriodValidationAlwaysActive() {
        when(view.isAlwaysActive()).thenReturn(true);
        assertTrue(presenter.activePeriodValidChk());
    }

    @Test
    public void verifyActivePeriodValidationDates() {
        when(view.isAlwaysActive()).thenReturn(false);
        when(view.getActiveStartText()).thenReturn("12/01");
        when(view.getActiveEndText()).thenReturn("12/31");
        assertTrue(presenter.activePeriodValidChk());
    }

    @Test
    public void verifySubscriptionIsSubmittedToSubscriptionService()
            throws RegistryHandlerException {
        Subscription subscription = SubscriptionFixture.INSTANCE.get();

        final ISubscriptionServiceResult result = mock(ISubscriptionServiceResult.class);
        when(
                service.store(same(subscription),
                        any(IForceApplyPromptDisplayText.class))).thenReturn(
                result);

        presenter.storeSubscription(subscription, "username");
        verify(service).store(same(subscription),
                any(IForceApplyPromptDisplayText.class));
    }

    @Test
    public void verifyIfMessageIsNotReturnedFromSubscriptionServiceThenItIsNotDisplayed()
            throws RegistryHandlerException {
        Subscription subscription = SubscriptionFixture.INSTANCE.get();

        final ISubscriptionServiceResult result = mock(ISubscriptionServiceResult.class);
        when(result.hasMessageToDisplay()).thenReturn(false);

        when(
                service.store(same(subscription),
                        any(IForceApplyPromptDisplayText.class))).thenReturn(
                result);
        presenter.storeSubscription(subscription, "username");
        verify(view, never()).displayPopup(anyString(), anyString());
    }

    @Test
    public void testActivePeriodAssumesTheCurrentYearWhenDateIsToday()
            throws ParseException {
        TimeUtilTest.freezeTime();

        Calendar cal = TimeUtil.newGmtCalendar();
        cal.add(Calendar.YEAR, -1);
        Date oneYearAgo = cal.getTime();

        Subscription subscription = new SubscriptionBuilder()
                .withActivePeriodStart(oneYearAgo)
                .withActivePeriodEnd(oneYearAgo).build();

        CreateSubscriptionDlgPresenter presenter = new CreateSubscriptionDlgPresenter(
                view, dataSet, true, new SameThreadGuiTaskExecutor());
        presenter.setSubscriptionData(subscription);
        presenter.init();

        verify(view).setActiveStartDate(
                argThat(yyyyMmDdMatches(TimeUtil.newDate())));
    }

    @Test
    public void testActivePeriodStartAssumesTheNextYearWhenStartAndEndDateIsBeforeToday()
            throws ParseException {
        Calendar cal = TimeUtil.newGmtCalendar();
        cal.set(Calendar.MONTH, Calendar.JANUARY);
        cal.set(Calendar.DAY_OF_MONTH, 4);
        final Date yesterday = cal.getTime();

        // Freeze time at Jan. 5
        TimeUtilTest.freezeTime(yesterday.getTime() + TimeUtil.MILLIS_PER_DAY);

        Subscription subscription = new SubscriptionBuilder()
                .withActivePeriodStart(yesterday)
                .withActivePeriodEnd(yesterday).build();

        CreateSubscriptionDlgPresenter presenter = new CreateSubscriptionDlgPresenter(
                view, dataSet, true, new SameThreadGuiTaskExecutor());
        presenter.setSubscriptionData(subscription);
        presenter.init();

        Calendar januaryFourthAYearLater = TimeUtil.newGmtCalendar();
        januaryFourthAYearLater.setTime(yesterday);
        januaryFourthAYearLater.add(Calendar.YEAR, 1);

        verify(view).setActiveStartDate(
                argThat(yyyyMmDdMatches(januaryFourthAYearLater.getTime())));

    }

    @Test
    public void testActivePeriodStartAssumesTheCurrentYearWhenInActiveWindow()
            throws ParseException {
        Calendar cal = TimeUtil.newGmtCalendar();
        cal.set(Calendar.MONTH, Calendar.JANUARY);
        cal.set(Calendar.DAY_OF_MONTH, 4);
        final Date yesterday = cal.getTime();

        // Freeze time at Jan. 5
        cal.add(Calendar.DAY_OF_MONTH, 1);
        TimeUtilTest.freezeTime(cal.getTimeInMillis());

        cal.add(Calendar.DAY_OF_MONTH, 3);
        Date threeDaysFromNow = cal.getTime();

        Subscription subscription = new SubscriptionBuilder()
                .withActivePeriodStart(yesterday)
                .withActivePeriodEnd(threeDaysFromNow).build();

        CreateSubscriptionDlgPresenter presenter = new CreateSubscriptionDlgPresenter(
                view, dataSet, true, new SameThreadGuiTaskExecutor());
        presenter.setSubscriptionData(subscription);
        presenter.init();

        verify(view).setActiveStartDate(argThat(yyyyMmDdMatches(yesterday)));
    }

    @Test
    public void testActivePeriodEndAssumesStartYearWhenDateIsToday()
            throws ParseException {

        TimeUtilTest.freezeTime();

        Calendar cal = TimeUtil.newGmtCalendar();
        cal.add(Calendar.YEAR, -1);
        Date oneYearAgo = cal.getTime();

        Calendar cal2 = TimeUtil.newGmtCalendar();
        cal2.setTime(oneYearAgo);
        cal2.add(Calendar.DAY_OF_YEAR, 3);
        Date oneYearAgoPlusThreeDays = cal2.getTime();

        Subscription subscription = new SubscriptionBuilder()
                .withActivePeriodStart(oneYearAgo)
                .withActivePeriodEnd(oneYearAgoPlusThreeDays).build();

        CreateSubscriptionDlgPresenter presenter = new CreateSubscriptionDlgPresenter(
                view, dataSet, true, new SameThreadGuiTaskExecutor());
        presenter.setSubscriptionData(subscription);
        presenter.init();

        Calendar expectedCalendar = TimeUtil.newGmtCalendar();
        expectedCalendar.setTime(oneYearAgoPlusThreeDays);
        expectedCalendar.add(Calendar.YEAR, 1);

        verify(view).setActiveEndDate(
                argThat(yyyyMmDdMatches(expectedCalendar.getTime())));
    }

    @Test
    public void testActivePeriodEndCrossingYearBoundary() throws ParseException {

        Calendar cal = TimeUtil.newGmtCalendar();
        cal.set(Calendar.MONTH, Calendar.DECEMBER);
        cal.set(Calendar.DAY_OF_MONTH, 30);
        cal.set(Calendar.YEAR, 1970);
        final Date decemberThirtieth = cal.getTime();

        Calendar cal2 = TimeUtil.newGmtCalendar();
        cal2.setTime(decemberThirtieth);
        cal2.set(Calendar.MONTH, Calendar.JANUARY);
        cal2.set(Calendar.DAY_OF_MONTH, 4);
        cal2.set(Calendar.YEAR, 1970);
        Date januaryFourth = cal2.getTime();

        Subscription subscription = new SubscriptionBuilder()
                .withActivePeriodStart(decemberThirtieth)
                .withActivePeriodEnd(januaryFourth).build();

        // freeze time at december thirtieth of the current year
        Calendar decemberTenth = TimeUtil.newGmtCalendar();
        decemberTenth.set(Calendar.MONTH, Calendar.DECEMBER);
        decemberTenth.set(Calendar.DAY_OF_MONTH, 10);
        decemberTenth.set(Calendar.YEAR, 2000);
        TimeUtilTest.freezeTime(decemberTenth.getTimeInMillis());

        CreateSubscriptionDlgPresenter presenter = new CreateSubscriptionDlgPresenter(
                view, dataSet, true, new SameThreadGuiTaskExecutor());
        presenter.setSubscriptionData(subscription);
        presenter.init();

        Calendar expectedStartCalendar = TimeUtil.newGmtCalendar();
        expectedStartCalendar.setTime(decemberTenth.getTime());
        expectedStartCalendar.set(Calendar.DAY_OF_MONTH, 30);

        Calendar expectedEndCalendar = TimeUtil.newGmtCalendar();
        expectedEndCalendar.setTime(decemberTenth.getTime());
        expectedEndCalendar.add(Calendar.YEAR, 1);
        expectedEndCalendar.set(Calendar.MONTH, Calendar.JANUARY);
        expectedEndCalendar.set(Calendar.DAY_OF_MONTH, 4);

        verify(view).setActiveEndDate(
                argThat(yyyyMmDdMatches(expectedEndCalendar.getTime())));
    }

    @Test
    public void verifySubscriptionSetToView() {
        verify(view).setSubscription(presenter.getSubscription());
    }

    @Test
    public void verifyCycleTimesSetToView() {
        verify(view).setCycleTimes(presenter.cycleTimes);
    }

}
