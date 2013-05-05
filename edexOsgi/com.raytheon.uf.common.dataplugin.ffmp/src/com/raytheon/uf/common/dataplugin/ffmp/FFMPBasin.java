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
package com.raytheon.uf.common.dataplugin.ffmp;

import java.util.ArrayList;
import java.util.Collections;
import java.util.Date;
import java.util.Map.Entry;
import java.util.NavigableMap;
import java.util.TreeMap;

import javax.persistence.Transient;

import com.raytheon.uf.common.serialization.ISerializableObject;
import com.raytheon.uf.common.serialization.annotations.DynamicSerialize;
import com.raytheon.uf.common.serialization.annotations.DynamicSerializeElement;
import com.raytheon.uf.common.time.util.ImmutableDate;

/**
 * FFMP basin/aggregated value holder
 * 
 * <pre>
 * 
 * SOFTWARE HISTORY
 * 
 * Date         Ticket#     Engineer    Description
 * ------------ ----------  ----------- --------------------------
 * 06/22/09      2152       D. Hladky   Initial release
 * 01/27/13      1478       D. Hladky   Added support for writing aggregate record cache
 * Apr 22, 2013 1912        bsteffen    optimized the creation of NavigableMaps
 *                                      from aggregate records and delayed
 *                                      TreeMap creation to the tertiary loader.
 * Apr 26, 2013 1954        bsteffen    Minor code cleanup throughout FFMP.
 * 
 * </pre>
 * 
 * @author dhladky
 * @version 1
 */
@DynamicSerialize
public class FFMPBasin implements ISerializableObject, Cloneable {

    /** pfafstetter id(key) in GIS **/
    @DynamicSerializeElement
    protected Long pfaf;

    /** boolean aggregator **/
    @DynamicSerializeElement
    protected boolean aggregated = false;

    /** object used in calculations 
     *  not serialized!
     **/
    @Transient
    protected NavigableMap<Date, Float> values;
    
    /** object used for serialization **/
    @DynamicSerializeElement
    public float[] serializedValues;

    public void setSerializedValues(float[] serializedValues) {
        this.serializedValues = serializedValues;
    }

    /**
     * Get the float array of serialized values
     * @return
     */
    public float[] getSerializedValues() {
        return serializedValues;
    }

    /**
     * @return the pfaf_id
     */
    public Long getPfaf() {
        return pfaf;
    }

    /**
     * @param basin_id
     *            the basin_id to set
     */
    public void setPfaf(Long pfaf) {
        this.pfaf = pfaf;
    }

    /**
     * Aggregation marker
     * 
     * @return
     */
    public boolean getAggregated() {
        return aggregated;
    }

    /**
     * required setter
     * 
     * @param aggregated
     */
    public void setAggregated(boolean aggregated) {
        this.aggregated = aggregated;
    }
    
    public boolean contains(Date date) {
        return values.containsKey(date);
    }

    /**
     * Gets a Value for a date
     * 
     * @param date
     * @return
     */
    public Float getValue(Date date) {
        Float dvalue = null;

        if (date != null) {
            dvalue = values.get(date);
        }

        if (dvalue == null) {
            dvalue = 0.0f;
        }

        return dvalue;
    }

    /**
     * Gets the most recent
     * 
     * @return
     */
    public Float getValue() {
        Float value = 0.0f;
        Entry<Date, Float> entry = values.firstEntry();
        if (entry != null) {
            value = entry.getValue();
        }

        return value;
    }

    /**
     * Gets the accumulated value from this time back.
     * 
     * @param afterDate
     * @param beforeDate
     * @return
     */
    public float getAccumValue(Date afterDate, Date beforeDate,
            long expirationTime, boolean rate) {
        float dvalue = 0.0f;
        Date prevDate = null;
      
        // map ordered newest first, so grab from newest date to oldest date
        if (afterDate.before(beforeDate) && (values.size() > 0)) {

            synchronized (values) {

                float factor = 0.0f;

                for (Entry<Date, Float> entry : values.subMap(beforeDate,
                        false, afterDate, false).entrySet()) {
                    Date tdate = entry.getKey();
                    float val = entry.getValue();

                    if (!rate) {

                        if (prevDate == null) {
                            prevDate = beforeDate;
                        }

						if (val > 0.0f) {
							
							if ((prevDate.getTime() - tdate.getTime()) > expirationTime) {
								// handle the gap and accumulate the book ends
								// of it
								factor = ((prevDate.getTime() - (prevDate
										.getTime() - expirationTime)) / (1000.0f * 60.0f * 60.0f));

							} else {
								factor = ((prevDate.getTime() - tdate
										.getTime()) / (1000.0f * 60.0f * 60.0f));
							}
							// do absolute values so it dosen't matter which way
							// you traverse the list
							val = val * Math.abs(factor);
							
						}
                    }
  
                    dvalue += val;
                    prevDate = tdate;
                }
            }
        }

        return dvalue;
    }

    /**
     * Gets the value within a time window
     * 
     * @param afterDate
     * @param beforeDate
     * @return
     */
    public Float getValue(Date afterDate, Date beforeDate) {
        Float val = 0.0f;
        synchronized (values) {
            Date checkDate = values.ceilingKey(afterDate);
            if ((checkDate != null) && checkDate.before(beforeDate)) {
                val = values.get(checkDate);
            }
        }

        return val;
    }
    
    /**
     * Used for mosaic sources where the times come in irregularly (QPF)
     * @param date
     * @param buffer
     * @return
     */
    public Float getAverageValue(Date date, long buffer) {
    	Date afterDate = new Date(date.getTime()-(buffer/2));
    	Date beforeDate = new Date(date.getTime()+(buffer/2));
    	//System.out.println("AfterDate: "+afterDate+ " BeforeDate: "+beforeDate);
    	return getAverageValue(afterDate, beforeDate);
    }
    
    /**
     * Gets the average value within a time window, used for mosaic
     * 
     * @param afterDate
     * @param beforeDate
     * @return
     */
	public Float getAverageValue(Date afterDate, Date beforeDate) {
		Float val = 0.0f;
		int i = 0;

		synchronized (values) {

			for (Date date : values.keySet()) {
				if (date.before(beforeDate) && date.after(afterDate)) {
					float val1 = values.get(date);
					if (val1 > 0.0) {
						val += val1;
						i++;
					}
				}
			}

			if (i != 0) {
				val = val / i;
			}
		}

		return val;
	}
	
	/**
     * Gets the average value within a time window, used for mosaic
     * 
     * @param afterDate
     * @param beforeDate
     * @return
     */
	public Float getMaxValue(Date afterDate, Date beforeDate) {
		Float val = 0.0f;

		synchronized (values) {

			for (Date date : values.keySet()) {
				if (date.before(beforeDate) && date.after(afterDate)) {
					if (val > values.get(date)) {
						val = values.get(date);
					}
				}
			}
		}

		return val;
	}


    /**
     * Adds a date/value pair
     * 
     * @param date
     * @param value
     */
    public void setValue(Date date, Float dvalue) {
        synchronized (values) {
            if (!(values instanceof TreeMap) && !values.containsKey(dvalue)) {
                // ArrayBackedMap may have been used if this basin was
                // deserialized. It is much faster to do inserts on a TreeMap so
                // convert now.
                values = new TreeMap<Date, Float>(values);
            }
            values.put(date, dvalue);
        }
    }

    /**
     * get the TreeMap for data
     * 
     * @return
     */
    public NavigableMap<Date, Float> getValues() {
        return values;
    }

    /**
     * Sets the linked Hash for archived data
     * 
     * @param values
     */
    public void setValues(TreeMap<Date, Float> values) {
        this.values = values;
    }

    /**
     * No arg serial constructor
     */
    public FFMPBasin() {

        values = new TreeMap<Date, Float>(Collections.reverseOrder());
    }

    /**
     * Useful constructor
     * @param pfaf
     * @param aggregated
     */
    public FFMPBasin(Long pfaf, boolean aggregated) {
        setPfaf(pfaf);
        setAggregated(aggregated);
        values = new TreeMap<Date, Float>(Collections.reverseOrder());
    }
    
    /**
     * Populates the map from the serialized values
     * 
     * @param times
     */
    public void deserialize(long[] times) {
        // safe to avoid Array Index Exceptions / shouldn't happen but.....

        if (serializedValues != null
                && (times.length == serializedValues.length)) {
            NavigableMap<Date, Float> fastMap = new ArrayBackedMap(times,
                    serializedValues);
            values = fastMap.descendingMap();

            // values = new TreeMap<Date, Float>(fastMap.descendingMap());
        }
        serializedValues = null;
    }
    
    /**
     * populates the serialized array
     */
    public void serialize() {
        
        serializedValues = new float[values.size()];
        int i = 0;
        
        for (Date date: values.descendingKeySet()) {
            serializedValues[i] = values.get(date);
            i++;
        }
        //System.out.println("wrote :"+i+" pfaf : "+pfaf);
    }

    /**
     * purge out old entries
     * 
     * @param date
     */
    public void purgeData(Date date) {
        if (values != null) {
            synchronized (values) {
                ArrayList<Date> removes = new ArrayList<Date>();
                for (Date mdate : values.keySet()) {
                	if (mdate.before(date)) {
                		removes.add(mdate);
                	}
                }

                for (Date rdate : removes) {
                    values.remove(rdate);
                }
            }
        }
    }

    @Override
    public String toString() {

        StringBuffer buff = new StringBuffer();
        buff.append("PFAF ID: " + pfaf + "\n");
        buff.append("Aggregated : " + aggregated + "\n");
        for (Date date : values.keySet()) {
            buff.append("Value : " + values.get(date) + "\n");
        }
        return buff.toString();
    }

    @Override
    protected Object clone() throws CloneNotSupportedException {
        FFMPBasin basin = new FFMPBasin(pfaf, aggregated);
        basin.setValues(new TreeMap<Date, Float>(values));
        return basin;
    }

}
