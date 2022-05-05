package com.raytheon.uf.common.dataplugin.ffmp;

import java.util.ArrayList;
import java.util.Collections;
import java.util.Date;
import java.util.HashMap;
import java.util.List;
import java.util.Map;
import java.util.Map.Entry;
import java.util.NavigableMap;
import java.util.TreeMap;

import javax.persistence.Transient;

import com.raytheon.uf.common.dataplugin.ffmp.collections.BasinMapFactory;
import com.raytheon.uf.common.monitor.config.FFFGDataMgr;
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
 * 01/17/13      1478        D. Hladky  Removed un-needed XML attributes
 * Aug 08, 2015 4722        dhladky     Dynamic serialize imp not needed.
 * Oct 26, 2015  5056       dhladky     Simplified Guidance interpolator.
 * Jun 21, 2016  5704       dhladky     Updated getClosest() logic to include 0.0f checks.
 * Aug 31, 2016  5704       mduff       Changed iteration of TreeMap to use navigableKeySet.
 * Aug 22, 2018  6720       njensen     Cleanup
 * 
 * </pre>
 * 
 * @author dhladky
 */
@DynamicSerialize
public class FFMPGuidanceBasin extends FFMPBasin {

    public FFMPGuidanceBasin() {

    }

    /**
     * A map of timestamp to a map of source name to basin value. The outer map
     * is sorted in reverse order, with the newest times at the front.
     * 
     * An FFMPGuidanceBasin differs from an FFMPBasin in that the
     * FFMPGuidanceBasin holds the data for all sources of a particular source
     * family.
     */
    @DynamicSerializeElement
    protected NavigableMap<Date, Map<String, Float>> guidValues;

    @Transient
    protected long countyFips = 0l;

    /**
     * Adds a source/value pair
     * 
     * @param sourceName
     * @param date
     * @param dvalue
     */
    public void setValue(String sourceName, Date date, Float dvalue) {
        if (guidValues.containsKey(date)) {
            guidValues.get(date).put(sourceName, dvalue);
        } else {
            Map<String, Float> guids = new HashMap<>();
            guids.put(sourceName, dvalue);
            guidValues.put(date, guids);
        }
    }

    public NavigableMap<Date, Map<String, Float>> getGuidValues() {
        return guidValues;
    }

    public void setGuidValues(
            NavigableMap<Date, Map<String, Float>> guidValues) {
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
    @Override
    public void purgeData(Date date) {
        if (guidValues != null) {
            synchronized (guidValues) {
                List<Date> removes = new ArrayList<>();
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
            Map<String, Float> guids = guidValues.get(date);
            return guids.containsKey(sourceName);
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

        if (date != null) {
            val = guidValues.get(date).get(sourceName);
            if (val == null) {
                val = Float.NaN;
            }
        }

        return val;
    }

    /**
     * Gets a Value for a FFG source
     * 
     * @param sourceName
     * @param expiration
     * @return
     */
    public Float getValue(String sourceName,
            FFMPGuidanceInterpolation interpolation, long expiration) {
        Float dvalue = Float.NaN;

        dvalue = getValue(sourceName, expiration);

        FFFGDataMgr dman = FFFGDataMgr.getInstance();
        if (!dman.isExpired()) {
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
            if (!dman.isExpired()) {
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

            for (Entry<Date, Map<String, Float>> entry : guidValues
                    .entrySet()) {
                Date checkDate = entry.getKey();
                if (entry.getValue().containsKey(sourceName)) {
                    float val = entry.getValue().get(sourceName);
                    if (val != FFMPUtils.MISSING && val != 0.0f) {
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
     * @param date
     * @param expiration
     * @return
     */
    public Date getClosest(String sourceName, Date date, long expiration) {
        Date rdate = null;

        if (guidValues != null && guidValues.size() > 0) {
            if (guidValues.containsKey(date)) {
                if (guidValues.get(date).containsKey(sourceName)) {
                    float val = guidValues.get(date).get(sourceName);

                    if (val != FFMPUtils.MISSING && val != 0.0f) {
                        rdate = date;
                    }
                }
            }

            if (rdate == null) {
                long time1 = date.getTime();

                for (Date checkDate : guidValues.navigableKeySet()) {
                    if (guidValues.get(checkDate).containsKey(sourceName)) {
                        float val2 = guidValues.get(checkDate).get(sourceName);

                        if (val2 != FFMPUtils.MISSING && val2 != 0.0f) {
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
     * @param interpolation
     * @param expiration
     * @return
     */
    public Float getInterpolatedValue(FFMPGuidanceInterpolation interpolation,
            long expiration) {
        float value1 = 0.0f;
        float value2 = 0.0f;

        String source1 = interpolation.getSource1();
        String source2 = interpolation.getSource2();
        double ratioOffset = interpolation.getInterpolationOffset();
        // interpolate from zero to first guidance
        if (source1.equals(source2)) {
            if (Double.isNaN(ratioOffset) || ratioOffset == 0.0) {
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
                    || (source1.equals(source2)
                            && (interpolation.getHour(source2) == 1))) {
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

        if (Float.isNaN(value1) || Float.isNaN(value2)) {
            return Float.NaN;
        }

        return (float) (value1 + ((value2 - value1) * ratioOffset));
    }

    /**
     * Constructor used in producing a new GuidanceBasin
     * 
     * @param pfaf
     * @param aggregated
     */
    public FFMPGuidanceBasin(Long pfaf, boolean aggregated) {
        setPfaf(pfaf);
        setAggregated(aggregated);

        guidValues = new TreeMap<>(Collections.reverseOrder());
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
        StringBuilder sb = new StringBuilder();
        sb.append("PFAF ID: ").append(pfaf).append("\n");
        sb.append("Aggregated: ").append(aggregated).append("\n");
        for (Entry<Date, Map<String, Float>> entry : guidValues.entrySet()) {
            Map<String, Float> innerMap = entry.getValue();
            for (Entry<String, Float> innerEntry : innerMap.entrySet()) {
                String source = innerEntry.getKey();
                sb.append("Source: ").append(source).append(" Value : ")
                        .append(innerEntry.getValue()).append("\n");
            }
        }
        return sb.toString();
    }

    @Override
    public void deserialize(long[] times, BasinMapFactory<Date> mapFactory) {
        // does nothing here, this class is serialized as is.
    }

    /**
     * populates the serialized array
     */
    @Override
    public void serialize() {
        // does nothing here, this class is serialized as is.
    }
}
