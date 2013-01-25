package com.raytheon.uf.edex.datadelivery.bandwidth.util;

import java.io.Serializable;
import java.util.Calendar;
import java.util.Date;

import com.raytheon.uf.common.datadelivery.registry.DataSetMetaData;
import com.raytheon.uf.common.datadelivery.registry.Subscription;
import com.raytheon.uf.common.serialization.SerializationException;
import com.raytheon.uf.common.serialization.SerializationUtil;
import com.raytheon.uf.common.time.util.TimeUtil;
import com.raytheon.uf.edex.datadelivery.bandwidth.dao.DataSetMetaDataDao;
import com.raytheon.uf.edex.datadelivery.bandwidth.dao.SubscriptionDao;

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

    // An implementation of the data set availability delay calculator
    // injected with Spring.
    private IDataSetAvailablityCalculator dataSetAvailabilityCalculator;

    private ISubscriptionLatencyCalculator subscriptionLatencyCalculator;

    private BandwidthUtil() {
    };

    public static int getSubscriptionLatency(Subscription subscription) {
        return instance.subscriptionLatencyCalculator.getLatency(subscription);
    }

    public static Calendar min(Date lhs, Calendar rhs) {
        return min(copy(lhs), rhs);
    }

    public static Calendar max(Date lhs, Calendar rhs) {
        return max(copy(lhs), rhs);
    }

    public static Calendar max(Calendar lhs, Calendar rhs) {
        Calendar calendar = null;
        if (lhs != null && rhs != null) {
            if (lhs.equals(rhs)) {
                return lhs;
            } else {
                return lhs.after(rhs) ? lhs : rhs;
            }
        }
        return calendar;
    }

    public static Calendar min(Calendar lhs, Calendar rhs) {
        Calendar calendar = null;
        if (lhs != null && rhs != null) {
            if (lhs.equals(rhs)) {
                return lhs;
            } else {
                return lhs.before(rhs) ? lhs : rhs;
            }
        }
        return calendar;
    }

    public static Calendar copy(final Date date) {
        Calendar t = null;
        if (date != null) {
            t = TimeUtil.newCalendar();
            t.setTime(date);
        }
        return t;
    }

    public static Calendar copy(final Calendar calendar) {
        Calendar t = null;
        if (calendar != null) {
            t = TimeUtil.newCalendar();
            t.setTimeInMillis(calendar.getTimeInMillis());
        }
        return t;
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
     * Calculate the number of minutes of delay between a Subscriptions base
     * reference time and the time the data should be available.
     * 
     * @param subscription
     *            The Subscription Object to obtain the availability for.
     * 
     * @return The delay in minutes.
     */
    public static int getDataSetAvailablityDelay(Subscription subscription) {
        return instance.dataSetAvailabilityCalculator
                .getDataSetAvailablityDelay(subscription);
    }

    public void setDataSetAvailabilityCalculator(
            IDataSetAvailablityCalculator dataSetAvailabilityCalculator) {
        this.dataSetAvailabilityCalculator = dataSetAvailabilityCalculator;
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
     * Create a new {@link SubscriptionDao} Object based on the
     * {@link Subscription} and {@link Calendar} Objects provided.
     * 
     * @param subscription
     *            the subscription
     * @param baseReferenceTime
     *            the base reference time
     * @return the {@link SubscriptionDao}
     * @throws SerializationException
     *             on error serializing the subscription
     */
    public static SubscriptionDao getSubscriptionDaoForSubscription(
            Subscription subscription, Calendar baseReferenceTime)
            throws SerializationException {
        SubscriptionDao dao = new SubscriptionDao();

        dao.setDataSetName(subscription.getDataSetName());
        dao.setProvider(subscription.getProvider());
        dao.setOwner(subscription.getOwner());
        dao.setName(subscription.getName());
        dao.setEstimatedSize(subscription.getDataSetSize());
        dao.setSubscription(subscription);
        dao.setRoute(subscription.getRoute());
        dao.setBaseReferenceTime(baseReferenceTime);
        // TODO: This is grid specific and only works for gridded times.
        // will have to revisit when other data type are introduced.
        // perhaps minute of the day?
        dao.setCycle(baseReferenceTime.get(Calendar.HOUR_OF_DAY));
        dao.setPriority(subscription.getPriority());
        dao.setRegistryId(subscription.getId());
        return dao;
    }

    /**
     * Create a new {@link DataSetMetaDataDao} Object based on the
     * {@link DataSetMetaData} Object provided.
     * 
     * @param dataSetMetaData
     *            the metadata
     * @return the dao
     */
    public static DataSetMetaDataDao newDataSetMetaDataDao(
            DataSetMetaData dataSetMetaData) {
        DataSetMetaDataDao dao = new DataSetMetaDataDao();
        // Set the fields we need to have..
        dao.setDataSetName(dataSetMetaData.getDataSetName());
        dao.setProviderName(dataSetMetaData.getProviderName());
        dao.setUpdateTime(BandwidthUtil.now());
        dao.setDataSetBaseTime(BandwidthUtil.copy(dataSetMetaData.getDate()));
        dao.setUrl(dataSetMetaData.getUrl());

        return dao;
    }

    /**
     * Creates a cheap clone by dynamically serializing (via thrift) the object
     * and then deserializing it.
     * 
     * @param clazz
     *            the class type
     * @param t
     *            the object
     * @return the cloned object
     * @throws SerializationException
     *             on error serializing
     */
    public static <T extends Serializable> T cheapClone(Class<T> clazz, T t)
            throws SerializationException {
        return SerializationUtil.transformFromThrift(clazz,
                SerializationUtil.transformToThrift(t));
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
}
