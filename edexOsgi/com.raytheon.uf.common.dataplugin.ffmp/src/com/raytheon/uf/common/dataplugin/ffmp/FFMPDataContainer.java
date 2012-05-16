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

import java.io.File;
import java.io.IOException;
import java.util.ArrayList;
import java.util.Date;
import java.util.HashMap;
import java.util.Map.Entry;
import java.util.Set;

import com.raytheon.uf.common.monitor.config.FFMPSourceConfigurationManager.SOURCE_TYPE;
import com.raytheon.uf.common.monitor.xml.SourceXML;
import com.raytheon.uf.common.serialization.DynamicSerializationManager;
import com.raytheon.uf.common.serialization.DynamicSerializationManager.SerializationType;
import com.raytheon.uf.common.serialization.SerializationException;
import com.raytheon.uf.common.util.FileUtil;

/**
 * FFTI Data Container
 * 
 * <pre>
 * 
 * SOFTWARE HISTORY
 * 
 * Date         Ticket#     Engineer    Description
 * ------------ ----------  ----------- --------------------------
 * 03/31/11     5489     D. Hladky   Initial release
 * 
 * </pre>
 * 
 * @author dhladky
 * @version 1
 */

public class FFMPDataContainer {

    private HashMap<String, FFMPBasinData> basinDataMap = new HashMap<String, FFMPBasinData>();

    private String sourceName = null;

    public FFMPDataContainer() {
        // public unused constructor
    }

    public FFMPDataContainer(String sourceName) {
        this.sourceName = sourceName;
        basinDataMap.put("ALL", new FFMPBasinData("ALL"));
        // System.out.println("Creating source: " + sourceName);
    }

    public FFMPDataContainer(String sourceName, Set<String> hucs) {
        // System.out.println("Creating source with hucs: " + sourceName);
        this.sourceName = sourceName;
        for (String huc : hucs) {
            basinDataMap.put(huc, new FFMPBasinData(huc));
        }
    }

    public Set<String> getKeys() {
        return basinDataMap.keySet();
    }

    /**
     * Get the one you are looking for
     * 
     * @return
     */
    public FFMPBasinData getBasinData(String huc) {
        return basinDataMap.get(huc);
    }

    /**
     * maybe this will work
     * 
     * @param basins
     * @param hucName
     */
    public void setBasinBuddyData(FFMPBasinData basins, String hucName) {
        // long time = System.currentTimeMillis();
        for (Entry<Long, FFMPBasin> entry : basins.getBasins().entrySet()) {
            FFMPBasin basin = getBasinData(hucName).get(entry.getKey());
            if (basin != null) {
                if (basin instanceof FFMPGuidanceBasin) {
                    FFMPGuidanceBasin gbasin = (FFMPGuidanceBasin) basin;
                    gbasin.getGuidValues().putAll(
                            ((FFMPGuidanceBasin) entry.getValue())
                                    .getGuidValues());
                } else {
                    basin.getValues().putAll(entry.getValue().getValues());
                }
            } else {
                getBasinData(hucName).put(entry.getKey(), entry.getValue());
            }
        }
        // long time2 = System.currentTimeMillis();
        // System.out.println("time to load HUC: " + (time2 - time) + " ms");
    }

    /**
     * Add a brand new one for new source, or initialization
     * 
     * @param basins
     */
    public void setBasinData(String huc, FFMPBasinData fftiData) {
        basinDataMap.put(huc, fftiData);
    }

    /**
     * Adds to the cache
     * 
     * @param date
     * @param newBasinData
     */
    public void addFFMPEntry(Date date, SourceXML source,
            FFMPBasinData newBasinData, String huc, String siteKey) {

        boolean guid = false;

        if (source.getSourceType().equals(SOURCE_TYPE.GUIDANCE.getSourceType())) {
            guid = true;
        }

        FFMPBasinData currBasinData = getBasinData(huc);

        for (Long key : newBasinData.getBasins().keySet()) {

            if (guid) {

                FFMPGuidanceBasin basin = null;

                if (currBasinData.get(key) instanceof FFMPGuidanceBasin) {
                    basin = (FFMPGuidanceBasin) currBasinData.get(key);
                }

                if (basin == null) {

                    FFMPBasin newbasin = newBasinData.get(key);
                    basin = new FFMPGuidanceBasin(key, newbasin.getAggregated());

                    if (newbasin instanceof FFMPGuidanceBasin) {

                        basin.setValue(
                                source.getSourceName(),
                                date,
                                ((FFMPGuidanceBasin) newbasin).getValue(
                                        source.getSourceName(),
                                        source.getExpirationMinutes(siteKey) * 60 * 1000));
                    } else {
                        basin.setValue(source.getSourceName(), date,
                                newBasinData.get(key).getValue());
                    }

                    currBasinData.put(key, basin);

                } else {

                    FFMPBasin newbasin = newBasinData.get(key);

                    if (newbasin instanceof FFMPGuidanceBasin) {

                        if (basin.getValue(date, source.getSourceName()) != null
                                && (basin
                                        .getValue(date, source.getSourceName()) >= 0.0f && !basin
                                        .getValue(date, source.getSourceName())
                                        .isNaN()) && source.isMosaic()) {
					
								if (((FFMPGuidanceBasin) newbasin).getValue(
										date, source.getSourceName()) >= 0.0f
										&& !((FFMPGuidanceBasin) newbasin)
												.getValue(date,
														source.getSourceName())
												.isNaN()) {

									float val = (float) (basin.getValue(date,
											source.getSourceName()) + ((FFMPGuidanceBasin) newbasin)
											.getValue(
													source.getSourceName(),
													source.getExpirationMinutes(siteKey) * 60 * 1000) / 2.0);

									basin.setValue(source.getSourceName(),
											date, val);
								}
				
                        } else {

                            if (!basin
                                    .containsKey(date, source.getSourceName())
                                    && newbasin != null) {
                                basin.setValue(source.getSourceName(), date,
                                        ((FFMPGuidanceBasin) newbasin)
                                                .getValue(date,
                                                        source.getSourceName()));
                            }
                        }

                    } else {
                        // meaning, it's a brand new file, we don't cast
                        // those out

						if (newbasin.getValue(date) != null
								&& newbasin.getValue(date) >= 0.0f
								&& !newbasin.getValue(date).isNaN()
								&& ((FFMPGuidanceBasin) basin).getValue(date,
										source.getSourceName()) >= 0.0f
								&& !((FFMPGuidanceBasin) basin).getValue(date,
										source.getSourceName()).isNaN()
								&& source.isMosaic()) {

							float val = (float) ((basin.getValue(date,
									source.getSourceName()) + newbasin
									.getValue()) / 2);

							basin.setValue(source.getSourceName(), date, val);

						} else {

							basin.setValue(source.getSourceName(), date,
									newbasin.getValue(date));
						}
					}
				}

            } else {

                FFMPBasin basin = currBasinData.get(key);
                FFMPBasin newbasin = newBasinData.get(key);
                Float val = 0.0f;

                if (basin == null) {

                    basin = new FFMPBasin(key, newbasin.getAggregated());
                    val = newbasin.getValue(date);

                    if (val.isNaN()) {
                        val = 0.0f;
                    }

                    basin.setValue(date, val);
                    currBasinData.put(key, basin);

                } else {

					if (basin.getValue(date) != null
							&& !basin.getValue(date).isNaN()
							&& basin.getValue(date) != 0.0 && source.isMosaic()) {
						if (newbasin.getValue(date) != null
								&& !newbasin.getValue(date).isNaN()
								&& newbasin.getValue(date) != 0.0) {

							val = (float) ((basin.getValue(date) + newbasin
									.getValue(date)) / 2);
						}

					} else {
						val = newbasin.getValue(date);

						if (val.isNaN()) {
							val = 0.0f;
                        }
                    }

                    basin.setValue(date, val);
                }
            }
        }
    }

    public void setSourceName(String sourceName) {
        this.sourceName = sourceName;
    }

    public String getSourceName() {
        return sourceName;
    }

    /**
     * check for the oldest key
     * 
     * @return
     */
    public Date getOldest() {
        try {
            for (Entry<Long, FFMPBasin> entry : getBasinData("ALL").getBasins()
                    .entrySet()) {
                FFMPBasin basin = entry.getValue();
                if (basin instanceof FFMPGuidanceBasin) {
                    ((FFMPGuidanceBasin) basin).getGuidValues().firstKey();
                } else {
                    return basin.getValues().firstKey();
                }
            }
        } catch (Exception e) {
            // no such element
        }
        return null;
    }

    /**
     * Gets the list of ordered time keys
     * 
     * @param barrierTime
     * @return
     */
    public ArrayList<Date> getOrderedTimes(Date barrierTime) {
        ArrayList<Date> orderedTimes = new ArrayList<Date>();
        try {
            for (Entry<Long, FFMPBasin> entry : getBasinData("ALL").getBasins()
                    .entrySet()) {
                FFMPBasin basin = entry.getValue();
                for (Date time : basin.getValues().descendingKeySet()) {
                    if (time.after(barrierTime)) {
                        orderedTimes.add(time);
                    }
                }
                return orderedTimes;
            }
        } catch (Exception e) {
            // no such element
        }

        return null;
    }

    /**
     * check for the newest key
     * 
     * @return
     */
    public Date getNewest() {
        try {
            for (Entry<Long, FFMPBasin> entry : getBasinData("ALL").getBasins()
                    .entrySet()) {
                FFMPBasin basin = entry.getValue();
                if (basin instanceof FFMPGuidanceBasin) {
                    ((FFMPGuidanceBasin) basin).getGuidValues().lastKey();
                } else {
                    return basin.getValues().lastKey();
                }
            }
        } catch (Exception e) {
            e.printStackTrace();
        }

        return null;
    }

    /**
     * check for the key
     * 
     * @param date
     * @return
     */
    public boolean containsKey(Date date) {
        boolean contains = false;
        if (getBasinData("ALL") != null) {
            for (Entry<Long, FFMPBasin> entry : getBasinData("ALL").getBasins()
                    .entrySet()) {
                FFMPBasin basin = entry.getValue();
                contains = basin.getValues().containsKey(date);
                if (contains == true) {
                    return true;
                }
            }
        }
        return false;
    }

    /**
     * check for the key
     * 
     * @param date
     * @return
     */
    public boolean containsKey(String sourceName) {
        boolean contains = false;
        for (Entry<Long, FFMPBasin> entry : getBasinData("ALL").getBasins()
                .entrySet()) {
            FFMPBasin basin = entry.getValue();
            if (basin instanceof FFMPGuidanceBasin) {
                contains = ((FFMPGuidanceBasin) basin).containsKey(sourceName);
                if (contains == true) {
                    // System.out.println("Contains Key: " + sourceName);
                    return true;
                }
            }
        }
        // System.out.println("No Key: " + sourceName);
        return false;
    }

    /**
     * check for size
     * 
     * @param date
     * @return
     */
    public int size() {
        for (Entry<Long, FFMPBasin> entry : getBasinData("ALL").getBasins()
                .entrySet()) {
            FFMPBasin basin = entry.getValue();
            if (basin instanceof FFMPGuidanceBasin) {
                return ((FFMPGuidanceBasin) basin).getGuidValues().size();
            } else {
                return basin.getValues().size();
            }
        }
        return 0;
    }

    /**
     * Get the maximum value in the monitored area.
     * 
     * @param pfafs
     * @param backDate
     * @param currDate
     * @param expirationTime
     * @param rate
     * @return
     */
    public double getMaxValue(ArrayList<Long> pfafs, Date backDate,
            Date currDate, long expirationTime, boolean rate) {

        // System.out.println("BackDate: " + backDate);
        // System.out.println("CurrDate: " + currDate);
        // System.out.println("expirationTime: " + (expirationTime / 1000) /
        // 3600);

        return getBasinData("ALL").getAccumMaxValue(pfafs, backDate, currDate,
                expirationTime, rate);
    }

    /*
     * clean up old junk
     */
    public void purge(Date backDate) {
        for (String huc : basinDataMap.keySet()) {
            getBasinData(huc).purgeData(backDate);
        }
    }

    /**
     * Write out the loader buddy files
     * 
     * @param fileName
     */
    public void writeDataContainer(String fileName, String path, String wfo) {

        if (fileName != null) {
            try {
                synchronized (basinDataMap) {
                    for (String huc : basinDataMap.keySet()) {
                        byte[] bdata = DynamicSerializationManager.getManager(
                                SerializationType.Thrift).serialize(
                                getBasinData(huc));
                        File file = new File(path + wfo + "/" + fileName + "-"
                                + huc + ".bin");
                        FileUtil.bytes2File(bdata, file);
                    }
                }
            } catch (SerializationException e) {
                e.printStackTrace();
            } catch (IOException e) {
                e.printStackTrace();
            }
        }
    }
}
