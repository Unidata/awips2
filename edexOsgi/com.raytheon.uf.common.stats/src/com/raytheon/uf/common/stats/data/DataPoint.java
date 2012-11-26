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

import java.text.DecimalFormat;
import java.text.SimpleDateFormat;
import java.util.Date;
import java.util.TimeZone;

import javax.xml.bind.annotation.XmlAccessType;
import javax.xml.bind.annotation.XmlAccessorType;
import javax.xml.bind.annotation.XmlRootElement;

import com.raytheon.uf.common.serialization.annotations.DynamicSerialize;
import com.raytheon.uf.common.serialization.annotations.DynamicSerializeElement;

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
     * Y value
     */
    @DynamicSerializeElement
    protected double y = 0.0;

    /**
     * Text display for the sampling of this point
     */
    @DynamicSerializeElement
    protected String sampleText;

    /** Min value */
    @DynamicSerializeElement
    private double min = Integer.MAX_VALUE;

    /** Max value */
    @DynamicSerializeElement
    private double max = Integer.MIN_VALUE;

    /** Count */
    @DynamicSerializeElement
    private double count = 0;

    /** Sum */
    @DynamicSerializeElement
    private double sum;

    /** Constructor */
    public DataPoint() {

    }

    /**
     * @return the y
     */
    public double getY() {
        return this.getAvg();
    }

    /**
     * @param y
     *            the y to set
     */
    public void setY(double y) {
        this.y += y;
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
    public String getSampleText() {
        SimpleDateFormat dateFormat = sdf.get();
        DecimalFormat decimalFormat = decFormat.get();

        return dateFormat.format(new Date(x)) + "Z, "
                + decimalFormat.format(getAvg());
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
        return min;
    }

    /**
     * Set the min value if it is less than the current min.
     *
     * @param min
     *            the min to set
     */
    public void setMin(double min) {
        if (this.min > min) {
            this.min = min;
        }
    }

    /**
     * @return the max
     */
    public double getMax() {
        return max;
    }

    /**
     * Set the max value if it is greater than the current max.
     *
     * @param max
     *            the max to set
     */
    public void setMax(double max) {
        if (this.max < max) {
            this.max = max;
        }
    }

    /**
     * @return the count
     */
    public double getCount() {
        return count;
    }

    /**
     * @param count
     *            the count to set
     */
    public void setCount(double count) {
        this.count = count;
    }

    /**
     *
     * @param count
     *            the count to add
     */
    public void addToCount(double count) {
        this.count += count;
    }

    /**
     * @return the sum
     */
    public double getSum() {
        return sum;
    }

    /**
     * @param sum
     *            the sum to set
     */
    public void setSum(double sum) {
        this.sum += sum;
    }

    /**
     * @return the avg
     */
    public double getAvg() {
        if (count > 0) {
            return this.sum / count;
        }

        return 0;
    }
}
