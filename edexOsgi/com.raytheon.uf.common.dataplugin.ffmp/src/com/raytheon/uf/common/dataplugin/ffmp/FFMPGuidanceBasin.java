package com.raytheon.uf.common.dataplugin.ffmp;

import java.util.ArrayList;
import java.util.Comparator;
import java.util.Date;
import java.util.HashMap;
import java.util.TreeMap;

import javax.persistence.Transient;
import javax.xml.bind.annotation.XmlAccessType;
import javax.xml.bind.annotation.XmlAccessorType;
import javax.xml.bind.annotation.XmlElement;
import javax.xml.bind.annotation.XmlRootElement;

import com.raytheon.uf.common.monitor.config.FFFGDataMgr;
import com.raytheon.uf.common.serialization.ISerializableObject;
import com.raytheon.uf.common.serialization.annotations.DynamicSerialize;
import com.raytheon.uf.common.serialization.annotations.DynamicSerializeElement;

/**
 * FFMP basin/aggregated guidance value holder
 * 
 * <pre>
 * 
 * SOFTWARE HISTORY
 * 
 * Date         Ticket#     Engineer    Description
 * ------------ ----------  ----------- --------------------------
 * 08/22/10      3437       D. Hladky   Initial release
 * 
 * </pre>
 * 
 * @author dhladky
 * @version 1
 */
@XmlRootElement
@XmlAccessorType(XmlAccessType.NONE)
@DynamicSerialize
public class FFMPGuidanceBasin extends FFMPBasin implements ISerializableObject {
    public FFMPGuidanceBasin() {

    }

    @DynamicSerializeElement
    @XmlElement
    protected TreeMap<Date, HashMap<String, Float>> guidValues;

    @Transient
    protected long countyFips = 0l;

    /**
     * Adds a source/value pair
     * 
     * @param source
     * @param value
     */
    public void setValue(String sourceName, Date date, Float dvalue) {
        if (guidValues.containsKey(date)) {
            guidValues.get(date).put(sourceName, dvalue);
        } else {
            HashMap<String, Float> guids = new HashMap<String, Float>();
            guids.put(sourceName, dvalue);
            guidValues.put(date, guids);
        }
    }

    /**
     * get the Hash for data
     * 
     * @return
     */
    public TreeMap<Date, HashMap<String, Float>> getGuidValues() {
        return guidValues;
    }

    /**
     * set the Hash for data
     * 
     * @return
     */
    public void setGuidValues(TreeMap<Date, HashMap<String, Float>> guidValues) {
        this.guidValues = guidValues;
    }

    /**
     * used by stand alone guidance displays
     * 
     * @param date
     * @param sourceName
     * @return
     */
    public Float getValue(Date date, String sourceName) {
        if (guidValues.containsKey(date)) {
            if (guidValues.get(date).containsKey(sourceName)) {
                return guidValues.get(date).get(sourceName);
            } else {
                return Float.NaN;
            }
        } else {
            return Float.NaN;
        }
    }

    /**
     * purge out old entries
     * 
     * @param date
     */
    public void purgeData(Date date) {
        if (guidValues != null) {
            synchronized (guidValues) {
                ArrayList<Date> removes = new ArrayList<Date>();
                for (Date mdate : guidValues.keySet()) {
                    if (mdate.before(date)) {
                        removes.add(mdate);
                    }
                }

                for (Date rdate : removes) {
                    guidValues.remove(rdate);
                }
            }
        }
    }

    /**
     * Check for a source, only recent date
     * 
     * @param sourceName
     * @return
     */
    public boolean containsKey(String sourceName) {
        if (guidValues.firstEntry() != null) {
            return guidValues.firstEntry().getValue().containsKey(sourceName);
        } else {
            return false;
        }
    }

    /**
     * check for a source with the date
     * 
     * @param date
     * @param sourceName
     * @return
     */
    public boolean containsKey(Date date, String sourceName) {
        if (guidValues.containsKey(date)) {
            HashMap<String, Float> guids = guidValues.get(date);
            if (guids.containsKey(sourceName)) {
                return true;
            } else {
                return false;
            }
        } else {
            return false;
        }
    }

    /**
     * Gets the most recent value with interpolation delay
     * 
     * @param sourceName
     * @return
     */
    public Float getValue(String sourceName, long expiration) {
        Float val = Float.NaN;
        Date date = getMostRecent(sourceName, expiration);
        // System.out.println("Most recent date: " + date);

        if (date != null) {
            val = guidValues.get(date).get(sourceName);
            // System.out.println("Value: " + val);
            if (val == null) {
                val = Float.NaN;
            }
        }

        return val;
    }

    /**
     * Gets a Value for a FFG source
     * 
     * @param date
     * @return
     */
    public Float getValue(String sourceName,
            FFMPGuidanceInterpolation interpolation, long expiration) {
        Float dvalue = Float.NaN;

        dvalue = getValue(sourceName, expiration);

        FFFGDataMgr dman = FFFGDataMgr.getInstance();
        if (dman.isExpired() == false) {
            dvalue = dman.adjustValue(dvalue, sourceName, this.pfaf,
                    this.countyFips);
        }

        return dvalue;
    }

    /**
     * Gets a Value for a FFG source stand alone
     * 
     * @param sourceName
     * @param date
     * @return
     */
    public Float getValue(String sourceName, Date date,
            FFMPGuidanceInterpolation interpolation, long expiration) {

        Float dvalue = Float.NaN;
        Float value = Float.NaN;
        Date closestDate = getClosest(sourceName, date, expiration);

        if (closestDate != null) {
            value = getValue(closestDate, sourceName);
        }

        if (!value.isNaN()) {
            FFFGDataMgr dman = FFFGDataMgr.getInstance();
            if (dman.isExpired() == false) {

                dvalue = dman.adjustValue(value, sourceName, this.pfaf,
                        this.countyFips);
            } else {
                dvalue = value;
            }
        }

        return dvalue;

    }

    /**
     * Get Youngest Key
     * 
     * @param sourceName
     * @return
     */
    public Date getMostRecent(String sourceName, long expiration) {

        Date rdate = null;

        if (guidValues != null && guidValues.size() > 0) {

            Date markerDate = guidValues.firstKey();

            for (Date checkDate : guidValues.keySet()) {

                if (guidValues.get(checkDate).containsKey(sourceName)) {
                    float val = guidValues.get(checkDate).get(sourceName);
                    if (val != FFMPUtils.MISSING) {

                        long time1 = markerDate.getTime();
                        long time2 = checkDate.getTime();

                        if ((time1 - time2) < expiration) {
                            rdate = checkDate;
                        }
                        break;
                    }
                }
            }
        }

        return rdate;
    }

    /**
     * Get Closest Key
     * 
     * @param sourceName
     * @param Date
     * @param expiration
     * @return
     */
    public Date getClosest(String sourceName, Date date, long expiration) {

        Date rdate = null;

        if (guidValues != null && guidValues.size() > 0) {

            if (guidValues.containsKey(date)) {
                if (guidValues.get(date).containsKey(sourceName)) {

                    float val = guidValues.get(date).get(sourceName);

                    if (val != FFMPUtils.MISSING) {
                        rdate = date;
                    }
                }
            }

            if (rdate == null) {

                long time1 = date.getTime();

                for (Date checkDate : guidValues.descendingKeySet()) {
                    if (guidValues.get(checkDate).containsKey(sourceName)) {

                        float val2 = guidValues.get(checkDate).get(sourceName);

                        if (val2 != FFMPUtils.MISSING) {

                            long time2 = checkDate.getTime();
                            // as long as it is +- expiration from orig date,
                            // golden
                            if (date.after(checkDate)) {
                                if ((time1 - time2) < expiration) {
                                    rdate = checkDate;
                                    break;
                                }
                            } else {
                                if ((time2 - time1) < expiration) {
                                    rdate = checkDate;
                                    break;
                                }
                            }
                        }
                    }
                }
            }
        }

        return rdate;
    }

    /**
     * Interpolate between guidance sources
     * 
     * @param source1
     * @param source2
     * @param ratioOffset
     * @return
     */
    public Float getInterpolatedValue(String source1, String source2,
            double ratioOffset, FFMPGuidanceInterpolation interpolation,
            long expiration) {

        float value1 = 0.0f;
        float value2 = 0.0f;

        // interpolate from zero to first guidance
        if (source1.equals(source2)) {
            if ((ratioOffset == Double.NaN) || (ratioOffset == 0.0)) {
                return Float.NaN;
            }
            Float value = getValue(source2, null, expiration);
            if (value.isNaN()) {
                // interpolate for this source
                value2 = interpolation.interpolateSourcePoint(source2, this);
            } else {
                value2 = value.floatValue();
            }

            // straight from awips1 code ( FFMPdataUtils.C )
            // We have an extrapolation to zero (the low side).
            // The formula below yields:
            // coeff = 0.62 for 0.25 time frame (endpoints.second)
            // coeff = 0.75 for 0.50 time frame (endpoints.second)
            // coeff = 0.88 for 0.75 time frame (endpoints.second)
            // coeff = 0.95 for 0.90 time frame (endpoints.second)
            // float mid, frac;
            // mid = endpoints.second / 2.0;
            // frac = 1.0 - ( ( duration - mid ) / mid );
            // coeff = ( duration / endpoints.second ) + (0.25 * frac);

            if ((interpolation.getHour(source1) == 0)
                    || (source1.equals(source2) && (interpolation
                            .getHour(source2) == 1))) {
                Double ratio = new Double(ratioOffset);
                if (ratio.equals(.25)) {
                    return (float) (.62 * value2);
                } else if (ratio.equals(.5)) {
                    return (float) (.75 * value2);
                } else if (ratio.equals(.75)) {
                    return (float) (.88 * value2);
                } else if (ratio.equals(.9)) {
                    return (float) (.95 * value2);
                }
            }

            // otherwise interpolate linearly I guess

        } else {
            // check if values at the source do not exist
            Float value = getValue(source1, interpolation, expiration);
            if (value.isNaN()) {
                // interpolate a value for this source
                value1 = interpolation.interpolateSourcePoint(source1, this);
            } else {
                value1 = value.floatValue();
            }
            value = getValue(source2, interpolation, expiration);
            if (value.isNaN()) {
                // interpolate for this source
                value2 = interpolation.interpolateSourcePoint(source2, this);
            } else {
                value2 = value.floatValue();
            }

        }

        if ((value1 == Float.NaN) || (value2 == Float.NaN)) {
            return Float.NaN;
        }

        return (float) (value1 + ((value2 - value1) * ratioOffset));
    }

    /**
     * useful constructor
     */
    public FFMPGuidanceBasin(Long pfaf, boolean aggregated) {
        setPfaf(pfaf);
        setAggregated(aggregated);

        guidValues = new TreeMap<Date, HashMap<String, Float>>(
                new Comparator<Date>() {
                    @Override
                    public int compare(Date o1, Date o2) {
                        // Null checks?
                        return (o2.before(o1) ? -1 : (o1.equals(o2) ? 0 : 1));
                    }

                });

    }

    /**
     * @return the countyFips
     */
    public long getCountyFips() {
        return countyFips;
    }

    /**
     * @param countyFips
     *            the countyFips to set
     */
    public void setCountyFips(long countyFips) {
        this.countyFips = countyFips;
    }

    @Override
    public String toString() {

        StringBuffer buff = new StringBuffer();
        buff.append("PFAF ID: " + pfaf + "\n");
        buff.append("Aggregated : " + aggregated + "\n");
        for (Date date : guidValues.keySet()) {
            for (String source : guidValues.get(date).keySet())
                buff.append("Source: " + source + " Value : "
                        + guidValues.get(source) + "\n");
        }
        return buff.toString();
    }

}
