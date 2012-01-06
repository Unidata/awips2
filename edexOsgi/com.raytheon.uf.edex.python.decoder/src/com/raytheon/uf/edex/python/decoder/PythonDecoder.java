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
import java.util.Arrays;
import java.util.Calendar;
import java.util.HashMap;
import java.util.List;
import java.util.Map;
import java.util.TimeZone;

import jep.JepException;
import net.sf.cglib.beans.BeanMap;

import com.raytheon.edex.exception.DecoderException;
import com.raytheon.edex.plugin.AbstractDecoder;
import com.raytheon.uf.common.dataplugin.PluginDataObject;
import com.raytheon.uf.common.python.PythonScript;
import com.raytheon.uf.common.time.DataTime;
import com.vividsolutions.jts.geom.Geometry;
import com.vividsolutions.jts.io.ParseException;
import com.vividsolutions.jts.io.WKTReader;

/**
 * Generic decoder for decoding in python
 * 
 * <pre>
 * SOFTWARE HISTORY
 * Date         Ticket#    Engineer    Description
 * ------------ ---------- ----------- --------------------------
 * Sep 22, 2008            njensen     Initial creation
 * Nov 24, 2008            chammack    Camel Refactor
 * </pre>
 * 
 * @author njensen
 * @version 1.0
 */

public class PythonDecoder extends AbstractDecoder {

    private static Map<Long, PythonScript> cachedInterpreters = new HashMap<Long, PythonScript>();

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

        List<PluginDataObject> decodedObjects = new ArrayList<PluginDataObject>(0);

        PythonScript py = null;
        long id = Thread.currentThread().getId();
        try {
            if (!cache || cachedInterpreters.get(id) == null) {
                py = PythonDecoderFactory.makePythonDecoder(pluginFQN,
                        moduleName);
            } else {
                py = cachedInterpreters.get(id);
            }
            HashMap<String, Object> argMap = new HashMap<String, Object>();
            argMap.put("moduleName", moduleName);
            argMap.put("fileToDecode", file.getPath());
            List<?> result = (List<?>) py.execute("decode", argMap);

            decodedObjects = asPluginDataObjects(result);
        } catch (JepException e) {
            throw new DecoderException(e.getMessage());
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

        for (PluginDataObject pdo : decodedObjects) {
            pdo.constructDataURI();
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
        if (result == null || result.isEmpty()) {
            decodedObjects = new ArrayList<PluginDataObject>(0);
        } else {
            if (result.get(0) instanceof Map) {
                decodedObjects = new ArrayList<PluginDataObject>(result.size());
                if (this.recordClass == null) {
                    this.recordClass = Class.forName(this.recordClassname);
                }

                ArrayList<HashMap<String, Object>> resultList = (ArrayList<HashMap<String, Object>>) result;
                PluginDataObject record = (PluginDataObject) recordClass
                        .newInstance();
                record.setPluginName(pluginName);
                BeanMap bm = BeanMap.create(record);
                for (HashMap<String, Object> map : resultList) {
                    record = (PluginDataObject) recordClass.newInstance();
                    record.setPluginName(pluginName);
                    bm.setBean(record);
                    try {
                        for (String key : map.keySet()) {
                            bm.put(key, transformValue(key, map.get(key), bm));
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
            if (type.equals(Calendar.class) && val instanceof Long) {
                Calendar cal = Calendar.getInstance(TimeZone
                        .getTimeZone("Zulu"));
                cal.setTimeInMillis((Long) val);
                val = cal;
            } else if (type.equals(DataTime.class) && val instanceof Long) {
                Calendar cal = Calendar.getInstance(TimeZone
                        .getTimeZone("Zulu"));
                cal.setTimeInMillis((Long) val);
                val = new DataTime(cal);
            } else if (type.equals(Geometry.class) && val instanceof String) {
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
        StringBuffer buf = new StringBuffer();
        StringBuffer tempbuf = new StringBuffer();
        StringBuffer firstpt = new StringBuffer();
        Geometry geo = null;
        try {
            List<String> coords = Arrays.asList(tempPoly.split("[\\r\\n ]+"));

            buf.append("POLYGON((");

            int counter = 0;
            for (String coord : coords) {
                counter++;
                if ((counter % 2) == 0) {
                    buf.append(Double.parseDouble(coord) / -100 + " "
                            + tempbuf.toString() + ", ");
                    if (counter == 2) {
                        firstpt.append(Double.parseDouble(coord) / -100 + " "
                                + tempbuf.toString() + ", ");
                    }
                    tempbuf.delete(0, tempbuf.length());
                } else {
                    tempbuf.append(Double.parseDouble(coord) / 100);
                }
            }
            if (!buf.toString().endsWith(firstpt.toString())) {
                buf.append(firstpt.toString());
            }
            buf.replace(buf.length() - 2, buf.length(), "))");

            geo = new WKTReader().read(buf.toString());
            // geo = PGgeometry.geomFromString(buf.toString());
        } catch (ParseException e) {
            logger.error("Could not build geometry", e);
        }

        return geo;
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
