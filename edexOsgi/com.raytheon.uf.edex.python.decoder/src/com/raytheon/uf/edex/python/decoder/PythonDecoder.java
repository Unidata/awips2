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
package com.raytheon.uf.edex.python.decoder;

import java.io.File;
import java.util.ArrayList;
import java.util.Calendar;
import java.util.HashMap;
import java.util.List;
import java.util.Map;
import java.util.Map.Entry;
import java.util.TimeZone;

import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

import com.raytheon.edex.exception.DecoderException;
import com.raytheon.uf.common.dataplugin.PluginDataObject;
import com.raytheon.uf.common.python.PythonScript;
import com.raytheon.uf.common.time.DataTime;
import com.vividsolutions.jts.geom.Coordinate;
import com.vividsolutions.jts.geom.Geometry;
import com.vividsolutions.jts.geom.GeometryFactory;

import jep.JepException;
import net.sf.cglib.beans.BeanMap;

/**
 * Generic decoder for decoding in python
 *
 * <pre>
 * SOFTWARE HISTORY
 * Date          Ticket#  Engineer    Description
 * ------------- -------- ----------- --------------------------
 * Sep 22, 2008           njensen     Initial creation
 * Nov 24, 2008           chammack    Camel Refactor
 * Aug 30, 2013  2298     rjpeter     Make getPluginName abstract
 * Oct 03, 2013  2402     bsteffen    Make PythonDecoder more extendable.
 * Nov 21, 2016  5959     njensen     Cleanup
 * Nov 08, 2017  6509     dgilling    Better support for RiverPro products,
 *                                    improve error handling.
 *
 * </pre>
 *
 * @author njensen
 */

public class PythonDecoder {

    protected static final Logger logger = LoggerFactory
            .getLogger(PythonDecoder.class);

    private static final String MISSING_LAT_LON = "Missing";

    private static Map<Long, PythonScript> cachedInterpreters = new HashMap<>();

    private String recordClassname;

    private Class<?> recordClass;

    private String pluginName;

    private String moduleName;

    private String pluginFQN;

    /**
     * Whether or not to cache the PythonScript (Python interpreters)
     */
    private boolean cache = false;

    public PythonDecoder() {
    }

    public PluginDataObject[] decode(File file) throws Exception {
        Map<String, Object> argMap = new HashMap<>(4);
        argMap.put("filePath", file.getPath());
        return decode(argMap);
    }

    public PluginDataObject[] decode(Map<String, Object> args) throws Exception {

        List<PluginDataObject> decodedObjects = new ArrayList<>(0);

        PythonScript py = null;
        long id = Thread.currentThread().getId();
        try {
            if (!cache || (cachedInterpreters.get(id) == null)) {
                py = PythonDecoderFactory.makePythonDecoder(pluginFQN,
                        moduleName);
            } else {
                py = cachedInterpreters.get(id);
            }
            args.put("moduleName", moduleName);
            List<?> result = (List<?>) py.execute("decode", args);

            decodedObjects = asPluginDataObjects(result);
        } catch (JepException e) {
            throw new DecoderException(e.getMessage(), e);
        } catch (ClassNotFoundException e) {
            throw new DecoderException("Unable to find record class"
                    + recordClass, e);
        } catch (Exception e) {
            throw e;
        } finally {
            if (cache) {
                cachedInterpreters.put(id, py);
            } else {
                py.dispose();
            }
        }

        return decodedObjects.toArray(new PluginDataObject[decodedObjects
                .size()]);
    }

    /**
     * Convert decoder result to a list of PluginDataObjects.
     *
     * @param result
     *            A list of PluginDataObjects, or a List of Maps
     * @return The input list converted to a list of PluginDataObjects
     * @throws Exception
     */
    @SuppressWarnings("unchecked")
    public List<PluginDataObject> asPluginDataObjects(List<?> result)
            throws Exception {
        List<PluginDataObject> decodedObjects = null;
        if ((result == null) || result.isEmpty()) {
            decodedObjects = new ArrayList<>(0);
        } else {
            if (result.get(0) instanceof Map) {
                decodedObjects = new ArrayList<>(result.size());
                if (this.recordClass == null) {
                    this.recordClass = Class.forName(this.recordClassname);
                }

                List<Map<String, Object>> resultList = (List<Map<String, Object>>) result;
                PluginDataObject record = (PluginDataObject) recordClass
                        .newInstance();
                BeanMap bm = BeanMap.create(record);
                for (Map<String, Object> map : resultList) {
                    record = (PluginDataObject) recordClass.newInstance();
                    bm.setBean(record);
                    try {
                        for (Entry<String, Object> entry : map.entrySet()) {
                            bm.put(entry.getKey(), transformValue(
                                    entry.getKey(), entry.getValue(), bm));
                        }
                        decodedObjects.add((PluginDataObject) bm.getBean());
                    } catch (Exception e) {
                        throw new DecoderException(e.getMessage(), e);
                    }
                }
            } else {
                decodedObjects = (List<PluginDataObject>) result;
            }
        }
        return decodedObjects;
    }

    private Object transformValue(String key, Object val, BeanMap bm) {
        Class<?> type = bm.getPropertyType(key);
        if (type != null) {
            if (type.equals(Calendar.class) && (val instanceof Long)) {
                Calendar cal = Calendar.getInstance(TimeZone
                        .getTimeZone("Zulu"));
                cal.setTimeInMillis((Long) val);
                val = cal;
            } else if (type.equals(DataTime.class) && (val instanceof Long)) {
                Calendar cal = Calendar.getInstance(TimeZone
                        .getTimeZone("Zulu"));
                cal.setTimeInMillis((Long) val);
                val = new DataTime(cal);
            } else if (type.equals(Geometry.class) && (val instanceof String)) {
                val = buildGeometry((String) val);
            }
        }

        return val;
    }

    /**
     * This method creates a Geometry object for storage in the database which
     * defines the polygon represented by a warning.
     *
     * @param tempPoly
     * @return
     */
    private Geometry buildGeometry(String tempPoly) {
        tempPoly = tempPoly.trim();

        /*
         * Certain RiverPro-issued products do not contain a valid polygon in
         * its LAT...LON line. They use the word "Missing" instead, so we'll
         * just give the record a null Geometry to match.
         */
        if (MISSING_LAT_LON.equalsIgnoreCase(tempPoly)) {
            return null;
        }

        String[] coords = tempPoly.split("[\\r\\n ]+");
        if ((coords.length % 2) != 0) {
            logger.error("Polygon string [" + tempPoly
                    + "] must contain an even number of coordinates.");
            return null;
        } else if (coords.length < 6) {
            logger.error("Polygon string [" + tempPoly
                    + "] must have at least 3 coordinate pairs to make a complete polygon.");
            return null;
        }

        List<Coordinate> latLonPairs = new ArrayList<>((coords.length / 2) + 1);
        try {
            for (int i = 0; i < coords.length; i += 2) {
                double lat = Double.valueOf(coords[i]) / 100;
                double lon = Double.valueOf(coords[i + 1]) / -100;
                latLonPairs.add(new Coordinate(lon, lat));
            }
        } catch (NumberFormatException e) {
            logger.error("Could not parse polygon string from decoder ["
                    + tempPoly + "]", e);
            return null;
        }

        if (!latLonPairs.get(0)
                .equals2D(latLonPairs.get(latLonPairs.size() - 1))) {
            latLonPairs.add(latLonPairs.get(0));
        }
        try {
            return new GeometryFactory()
                    .createPolygon(latLonPairs.toArray(new Coordinate[0]));
        } catch (IllegalArgumentException e) {
            logger.error("Could not build valid polygon from string ["
                    + tempPoly + "].", e);
        }

        return null;
    }

    public String getPluginName() {
        return pluginName;
    }

    public void setPluginName(String pluginName) {
        this.pluginName = pluginName;
    }

    public String getModuleName() {
        return moduleName;
    }

    public void setModuleName(String moduleName) {
        this.moduleName = moduleName;
    }

    public String getPluginFQN() {
        return pluginFQN;
    }

    public void setPluginFQN(String pluginFQN) {
        this.pluginFQN = pluginFQN;
    }

    public String getRecordClassname() {
        return recordClassname;
    }

    public void setRecordClassname(String recordClassname) {
        this.recordClassname = recordClassname;
    }

    public boolean isCache() {
        return cache;
    }

    public void setCache(boolean cache) {
        this.cache = cache;
    }

}
