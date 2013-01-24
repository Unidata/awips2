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
package com.raytheon.uf.common.stats.data;

import java.math.BigDecimal;
import java.text.DecimalFormat;
import java.text.SimpleDateFormat;
import java.util.Date;
import java.util.TimeZone;

import javax.xml.bind.annotation.XmlAccessType;
import javax.xml.bind.annotation.XmlAccessorType;
import javax.xml.bind.annotation.XmlRootElement;

import com.raytheon.uf.common.serialization.annotations.DynamicSerialize;
import com.raytheon.uf.common.serialization.annotations.DynamicSerializeElement;
import com.raytheon.uf.common.stats.util.DataViewUtils;

/**
 * Class holding an x,y data point and other associated information for the
 * point.
 * 
 * <pre>
 * 
 * SOFTWARE HISTORY
 * 
 * Date         Ticket#    Engineer    Description
 * ------------ ---------- ----------- --------------------------
 * Sep 7, 2012            mpduff     Initial creation
 * 
 * </pre>
 * 
 * @author mpduff
 * @version 1.0
 */

@XmlRootElement
@XmlAccessorType(XmlAccessType.NONE)
@DynamicSerialize
public class DataPoint implements Comparable<DataPoint> {
    /** Date Format object */
    private final ThreadLocal<SimpleDateFormat> sdf = new ThreadLocal<SimpleDateFormat>() {
        @Override
        protected SimpleDateFormat initialValue() {
            SimpleDateFormat sTemp = new SimpleDateFormat("MM/dd/yyyy HH:mm");
            sTemp.setTimeZone(TimeZone.getTimeZone("GMT"));
            return sTemp;
        }
    };

    /** Decimal Format object */
    private final ThreadLocal<DecimalFormat> decFormat = new ThreadLocal<DecimalFormat>() {
        @Override
        protected DecimalFormat initialValue() {
            DecimalFormat format = new DecimalFormat("########.#");
            return format;
        }
    };

    /**
     * X value - millis
     */
    @DynamicSerializeElement
    private long x;

    /**
     * Text display for the sampling of this point
     */
    @DynamicSerializeElement
    protected String sampleText;

    /** Min value */
    @DynamicSerializeElement
    private BigDecimal min = new BigDecimal(Integer.MAX_VALUE);

    /** Max value */
    @DynamicSerializeElement
    private BigDecimal max = new BigDecimal(Integer.MIN_VALUE);

    /** Count */
    @DynamicSerializeElement
    private BigDecimal count = new BigDecimal(0);

    /** Sum */
    @DynamicSerializeElement
    private BigDecimal sum = new BigDecimal(0);

    /** Constructor */
    public DataPoint() {
        sum = sum.setScale(1, BigDecimal.ROUND_HALF_UP);
        min = min.setScale(1, BigDecimal.ROUND_HALF_UP);
        max = max.setScale(1, BigDecimal.ROUND_HALF_UP);
        count = count.setScale(1, BigDecimal.ROUND_HALF_UP);
    }

    /**
     * @param sampleText
     *            the sampleText to set
     */
    public void setSampleText(String sampleText) {
        this.sampleText = sampleText;
    }

    /**
     * Get the sample text for this point object
     * 
     * @return the sample text string
     */
    public String getSampleText(String view) {
        SimpleDateFormat dateFormat = sdf.get();
        DecimalFormat decimalFormat = decFormat.get();

        return dateFormat.format(new Date(x)) + "Z, "
                + decimalFormat.format(getValue(view));
    }

    /**
     * @param x
     *            the x to set
     */
    public void setX(long x) {
        this.x = x;
    }

    /**
     * @return the x
     */
    public long getX() {
        return x;
    }

    /**
     * {@inheritDoc}
     */
    @Override
    public int compareTo(DataPoint dp) {
        if (this.x == dp.x) {
            return 0;
        } else if (this.x < dp.x) {
            return -1;
        } else if (this.x > dp.x) {
            return 1;
        }

        return 0;
    }

    /**
     * @return the min
     */
    public double getMin() {
        return min.doubleValue();
    }

    /**
     * Set the min value if it is less than the current min.
     * 
     * @param min
     *            the min to set
     */
    public void setMin(double min) {
        if (this.min.doubleValue() > min) {
            BigDecimal m = new BigDecimal(min);
            m = m.setScale(1, BigDecimal.ROUND_HALF_UP);
            this.min = m;
        }
    }

    /**
     * @return the max
     */
    public double getMax() {
        return max.doubleValue();
    }

    /**
     * Set the max value if it is greater than the current max.
     * 
     * @param max
     *            the max to set
     */
    public void setMax(double max) {
        if (this.max.doubleValue() < max) {
            this.max = new BigDecimal(max);
            this.max = this.max.setScale(1, BigDecimal.ROUND_HALF_UP);
        }
    }

    /**
     * @return the count
     */
    public double getCount() {
        return count.doubleValue();
    }

    /**
     * @param count
     *            the count to set
     */
    public void setCount(double count) {
        this.count = new BigDecimal(count);
        this.count = this.count.setScale(1, BigDecimal.ROUND_HALF_UP);
    }

    /**
     * 
     * @param count
     *            the count to add
     */
    public void addToCount(double count) {
        BigDecimal cnt = new BigDecimal(count);
        cnt = cnt.setScale(1, BigDecimal.ROUND_HALF_UP);
        this.count = this.count.add(cnt);
    }

    /**
     * @return the sum
     */
    public double getSum() {
        return sum.doubleValue();
    }

    /**
     * @param sum
     *            the sum to set
     */
    public void setSum(double sum) {
        BigDecimal s = new BigDecimal(sum);
        s = s.setScale(1, BigDecimal.ROUND_HALF_UP);
        this.sum = this.sum.add(s);
    }

    /**
     * @return the avg
     */
    public double getAvg() {
        if (count.doubleValue() > 0) {
            return sum.divide(count, BigDecimal.ROUND_HALF_UP).doubleValue();
        }

        return 0;
    }

    /**
     * Get the value for the provided data view type.
     * 
     * @param view
     *            the view type
     */
    public double getValue(String view) {
        if (view.equals(DataViewUtils.DataView.AVG.getView())) {
            return getAvg();
        } else if (view.equals(DataViewUtils.DataView.MIN.getView())) {
            return getMin();
        } else if (view.equals(DataViewUtils.DataView.MAX.getView())) {
            return getMax();
        } else if (view.equals(DataViewUtils.DataView.SUM.getView())) {
            return getSum();
        } else {
            return getCount();
        }
    }
}
