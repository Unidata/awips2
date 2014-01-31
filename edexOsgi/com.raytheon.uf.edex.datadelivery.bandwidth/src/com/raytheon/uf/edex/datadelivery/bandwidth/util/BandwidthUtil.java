package com.raytheon.uf.edex.datadelivery.bandwidth.util;

import java.util.Calendar;
import java.util.Date;

import com.raytheon.uf.common.datadelivery.registry.DataSetMetaData;
import com.raytheon.uf.common.datadelivery.registry.DataType;
import com.raytheon.uf.common.datadelivery.registry.Subscription;
import com.raytheon.uf.common.datadelivery.registry.handlers.DataDeliveryHandlers;
import com.raytheon.uf.common.registry.handler.RegistryHandlerException;
import com.raytheon.uf.common.serialization.SerializationException;
import com.raytheon.uf.common.time.util.TimeUtil;
import com.raytheon.uf.edex.datadelivery.bandwidth.dao.BandwidthDataSetUpdate;
import com.raytheon.uf.edex.datadelivery.bandwidth.dao.BandwidthSubscription;

/**
 * Bandwidth Manager utility methods.
 * 
 * <pre>
 * 
 * SOFTWARE HISTORY
 * Date         Ticket#    Engineer    Description
 * ------------ ---------- ----------- --------------------------
 * Jul 26, 2012 726        jspinks     Initial creation
 * Oct 10, 2012 0726       djohnson    Add bandwidthManagementEnabled, some more utility methods,
 *                                     use availability delay to determine which starting hours to schedule.
 * Nov 09, 2012 1286       djohnson    Separate DAO utility methods from general utility.
 * Dec 11, 2012 1403       djohnson    No longer valid to run without bandwidth management.
 * Feb 14, 2013 1595       djohnson    Use subscription rescheduling strategy.
 * Jun 13, 2013 2095       djohnson    Point subscriptions don't check for dataset updates on aggregation.
 * Jun 25, 2013 2106       djohnson    CheapClone was cheap in ease, not performance.
 * Jul 11, 2013 2106       djohnson    Use SubscriptionPriority enum.
 * Oct 30, 2013 2448       dhladky     Moved methods to TimeUtil.
 * Dec 20, 2013  2636      mpduff      Changed dataset delay to offset.
 * Jan 08, 2014 2615       bgonzale    Moved Calendar min and max methods to TimeUtil.
 * 
 * </pre>
 * 
 * @version 1.0
 */
public class BandwidthUtil {

    public static final long BYTES_PER_KILOBYTE = 1024;

    public static final long DEFAULT_IDENTIFIER = -1L;

    public static final int[] MONTHS_OF_YEAR = { Calendar.JANUARY,
            Calendar.FEBRUARY, Calendar.MARCH, Calendar.APRIL, Calendar.MAY,
            Calendar.JUNE, Calendar.JULY, Calendar.AUGUST, Calendar.SEPTEMBER,
            Calendar.OCTOBER, Calendar.NOVEMBER, Calendar.DECEMBER };

    // Create an 'instance' Object so that implementations of some
    // algorithms can be injected with Spring..
    private static final BandwidthUtil instance = new BandwidthUtil();

    private final AveragingAvailablityCalculator dataSetAvailabilityCalculator;

    private ISubscriptionLatencyCalculator subscriptionLatencyCalculator;

    private ISubscriptionRescheduleStrategy subscriptionRescheduleStrategy;

    private BandwidthUtil() {
        dataSetAvailabilityCalculator = new AveragingAvailablityCalculator(
                DataDeliveryHandlers.getDataSetMetaDataHandler());
    };

    public static int getSubscriptionLatency(Subscription<?, ?> subscription) {
        return instance.subscriptionLatencyCalculator.getLatency(subscription);
    }

    /**
     * Seconds and milliseconds on a Calendar are not used in bandwidth
     * management and can alter some of the time arithmetic that is used
     * throughout the code. Zero out the seconds and milliseconds values as a
     * convenience
     * 
     * @return
     */
    public static Calendar now() {
        Calendar now = TimeUtil.newCalendar();
        now.set(Calendar.SECOND, 0);
        now.set(Calendar.MILLISECOND, 0);
        return now;
    }

    /**
     * Calculate the number of minutes of offset between a Subscriptions base
     * reference time and the time the data should be available.
     * 
     * @param subscription
     *            The Subscription Object to obtain the availability for.
     * @param referenceTime
     *            Data reference time
     * @return The offset in minutes.
     * @throws RegistryHandlerException
     */
    public static int getDataSetAvailablityOffset(
            Subscription<?, ?> subscription, Calendar referenceTime)
            throws RegistryHandlerException {
        return instance.dataSetAvailabilityCalculator
                .getDataSetAvailablityOffset(subscription, referenceTime);
    }

    /**
     * @param subscriptionLatencyCalculator
     *            the subscriptionLatencyCalculator to set
     */
    public void setSubscriptionLatencyCalculator(
            ISubscriptionLatencyCalculator subscriptionLatencyCalculator) {
        this.subscriptionLatencyCalculator = subscriptionLatencyCalculator;
    }

    /**
     * @param subscriptionRescheduleStrategy
     *            the subscriptionRescheduleStrategy to set
     */
    public void setSubscriptionRescheduleStrategy(
            ISubscriptionRescheduleStrategy subscriptionRescheduleStrategy) {
        this.subscriptionRescheduleStrategy = subscriptionRescheduleStrategy;
    }

    /**
     * @return the instance
     */
    public static BandwidthUtil getInstance() {
        return instance;
    }

    /**
     * Format a Calendar Object into a standard String format.
     * 
     * @param calendar
     * 
     * @return The standard String format of the provided Calendar.
     */
    public static String format(Calendar calendar) {
        return String.format("%1$tY%1$tm%1$td %1$tH:%1$tM:%1$tS", calendar);
    }

    public static String format(Date date) {
        return String.format("%1$tY%1$tm%1$td %1$tH:%1$tM:%1$tS", date);
    }

    public static int minuteOfDay(Calendar calendar) {
        return calendar.get(Calendar.HOUR_OF_DAY) * 60
                + calendar.get(Calendar.MINUTE);
    }

    /**
     * Create a new {@link BandwidthSubscription} Object based on the
     * {@link Subscription} and {@link Calendar} Objects provided.
     * 
     * @param subscription
     *            the subscription
     * @param baseReferenceTime
     *            the base reference time
     * @return the {@link BandwidthSubscription}
     * @throws SerializationException
     *             on error serializing the subscription
     */
    public static BandwidthSubscription getSubscriptionDaoForSubscription(
            Subscription<?, ?> subscription, Calendar baseReferenceTime) {
        BandwidthSubscription dao = new BandwidthSubscription();

        dao.setDataSetName(subscription.getDataSetName());
        dao.setProvider(subscription.getProvider());
        dao.setOwner(subscription.getOwner());
        dao.setName(subscription.getName());
        dao.setEstimatedSize(subscription.getDataSetSize());
        dao.setRoute(subscription.getRoute());
        dao.setBaseReferenceTime(baseReferenceTime);
        dao.setCycle(baseReferenceTime.get(Calendar.HOUR_OF_DAY));
        dao.setPriority(subscription.getPriority());
        dao.setRegistryId(subscription.getId());
        dao.setCheckForDataSetUpdate(subscription.getDataSetType() != DataType.POINT);
        return dao;
    }

    /**
     * Create a new {@link BandwidthDataSetUpdate} Object based on the
     * {@link DataSetMetaData} Object provided.
     * 
     * @param dataSetMetaData
     *            the metadata
     * @return the dao
     */
    public static BandwidthDataSetUpdate newDataSetMetaDataDao(
            DataSetMetaData<?> dataSetMetaData) {
        BandwidthDataSetUpdate dao = new BandwidthDataSetUpdate();
        // Set the fields we need to have..
        dao.setDataSetName(dataSetMetaData.getDataSetName());
        dao.setProviderName(dataSetMetaData.getProviderName());
        dao.setUpdateTime(BandwidthUtil.now());
        dao.setDataSetBaseTime(TimeUtil.newCalendar(dataSetMetaData.getDate()));
        dao.setUrl(dataSetMetaData.getUrl());

        return dao;
    }

    /**
     * Convert the number of kilobytes per second to bytes for the number of
     * specified minutes.
     * 
     * @param kilobytesPerSecond
     *            the kilobytes per second
     * @param numberOfMinutes
     *            the number of minutes
     * @return the bytes per specified number of minutes
     */
    public static long convertKilobytesPerSecondToBytesPerSpecifiedMinutes(
            int kilobytesPerSecond, int numberOfMinutes) {
        return kilobytesPerSecond * BandwidthUtil.BYTES_PER_KILOBYTE
                * numberOfMinutes * TimeUtil.SECONDS_PER_MINUTE;
    }

    /**
     * Convert bytes to kilobytes.
     * 
     * @param bytes
     *            the bytes
     * @return the kilobytes
     */
    public static long convertBytesToKilobytes(long bytes) {
        return bytes / BandwidthUtil.BYTES_PER_KILOBYTE;
    }

    /**
     * Check whether a subscription should be rescheduled on an update.
     * 
     * @param subscription
     *            the subscription
     * @param old
     *            the old version
     * @return true if the subscription should be rescheduled
     */
    public static boolean subscriptionRequiresReschedule(
            Subscription<?, ?> subscription, Subscription<?, ?> old) {
        return instance.subscriptionRescheduleStrategy
                .subscriptionRequiresReschedule(subscription, old);
    }

    /**
     * Sets up the activePeriod Start/End to plan Start/End calendar
     */
    public static Calendar planToPeriodCompareCalendar(Calendar planCalendar,
            Calendar activePeriod) {

        Calendar cal = TimeUtil.newCalendar(planCalendar);
        cal.set(Calendar.MONTH, activePeriod.get(Calendar.MONTH));
        cal.set(Calendar.DAY_OF_MONTH, activePeriod.get(Calendar.DAY_OF_MONTH));

        return cal;
    }
}
