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

package com.raytheon.edex.plugin.ldadmanual.dao;

import java.io.File;
import java.io.FileInputStream;
import java.io.FileNotFoundException;
import java.io.StringReader;
import java.lang.reflect.Field;
import java.lang.reflect.InvocationTargetException;
import java.lang.reflect.Method;
import java.lang.reflect.Modifier;
import java.text.ParseException;
import java.text.SimpleDateFormat;
import java.util.ArrayList;
import java.util.Calendar;
import java.util.Date;
import java.util.List;
import java.util.Properties;
import java.util.TimeZone;

import javax.measure.unit.Unit;
import javax.measure.unit.UnitFormat;
import javax.xml.bind.JAXBContext;
import javax.xml.bind.JAXBException;
import javax.xml.bind.Unmarshaller;

import com.raytheon.edex.exception.DecoderException;
import com.raytheon.edex.plugin.AbstractDecoder;
import com.raytheon.edex.plugin.IBinaryDecoder;
import com.raytheon.edex.plugin.ldad.common.DecodedData;
import com.raytheon.edex.plugin.ldad.common.LdadField;
import com.raytheon.uf.common.dataplugin.PluginDataObject;
import com.raytheon.uf.common.dataplugin.PluginException;
import com.raytheon.uf.common.localization.IPathManager;
import com.raytheon.uf.common.localization.LocalizationContext;
import com.raytheon.uf.common.localization.PathManagerFactory;
import com.raytheon.uf.common.pointdata.spatial.SurfaceObsLocation;
import com.raytheon.uf.common.time.DataTime;

/**
 * Decoder implementation for ldadmanual plugin.
 * 
 * <pre>
 * 
 * SOFTWARE HISTORY
 *                     
 * ate          Ticket#     Engineer    Description
 * -----------  ----------  ----------- --------------------------
 * 9/30/09                   vkorolev    Initial creation
 * </pre>
 * 
 * @author vkorolev
 * @version 1
 */

public class ManualDecoder<E> extends AbstractDecoder implements IBinaryDecoder {

    private final String PLUGIN_NAME;

    private String traceId = null;

    public SimpleDateFormat sdf = new SimpleDateFormat("yy/MM/dd HH:mm:ss");

    public File confile;

    public Properties configFile = new Properties();

    public ManualDecoder(String pluginName) throws DecoderException {
        PLUGIN_NAME = pluginName;
    }

    public void setTraceId(String id) {
        traceId = id;
    }

    /*
     * (non-Javadoc)
     * 
     * @see com.raytheon.edex.plugin.IBinaryDecoder#decode(byte[])
     */
    @Override
    public PluginDataObject[] decode(byte[] data) throws DecoderException {
        List<PluginDataObject> retVal = new ArrayList<PluginDataObject>();
        if (data != null) {
            IPathManager pathMgr = PathManagerFactory.getPathManager();
            LocalizationContext ctx = pathMgr.getContext(
                    LocalizationContext.LocalizationType.EDEX_STATIC,
                    LocalizationContext.LocalizationLevel.BASE);
            try {
                confile = pathMgr.getFile(ctx, "ldad/ldadUnitsMap.txt");
                FileInputStream fis = new FileInputStream(confile);
                try {
                    configFile.load(fis);
                } finally {
                    fis.close();
                }
            } catch (FileNotFoundException e) {
                logger.error(traceId
                        + "Configuration Units Map file (ldadUnitsMap.txt) not found.");
            } catch (Exception e) {
                logger.error(traceId + e);
            }
            try {
                JAXBContext context = JAXBContext
                        .newInstance(DecodedData.class);
                Unmarshaller u = context.createUnmarshaller();
                StringReader reader = new StringReader(new String(data));
                DecodedData dd = (DecodedData) u.unmarshal(reader);

                // Storage type separator
                if (dd.storageType.compareTo("manual") == 0) {
                    // Header
                    logger.info("\n***********" + "fileName= " + dd.fileName
                            + "**********\n" + "storageType= " + dd.storageType
                            + "\n" + "type= " + dd.type + "\n" + "root= "
                            + dd.root + "\n" + "source= " + dd.source + "\n"
                            + "provider= " + dd.provider + "\n"
                            + "missingValue= " + dd.missingValue + "\n"
                            + "reportTime= " + dd.reportTime + "\n"
                            + "--------------------------------------------"
                            + "\n");
                    String missval = dd.missingValue;
                    // Number of records
                    int recnum = dd.fields.get(0).values.size();
                    if (recnum == 0) {
                        logger.info(traceId + " - No data in the file.");
                        retVal.clear();
                        return retVal.toArray(new PluginDataObject[0]);
                    }
                    // Loop through records
                    badRecordStartAgain: for (int i = 0; i < recnum; i++) {
                        ManualLdadRecord record = new ManualLdadRecord();
                        SurfaceObsLocation location = new SurfaceObsLocation();
                        record.setProviderId(dd.provider);
                        record.setPluginName(PLUGIN_NAME);
                        // record.setReportType???(dd.type));
                        // record.set???(dd.reportTime);
                        // Loop through fields
                        for (LdadField v : dd.fields) {
                            String nn = v.variableName;
                            String unit = v.units;
                            String vv = v.values.get(i);

                            // value filtering
                            if (vv.equals(missval)) {
                                if (nn.equals("observationTime")) {
                                    logger.error(traceId
                                            + " - No Observation time was found.");
                                    continue badRecordStartAgain;
                                } else {
                                    continue;
                                }
                            }
                            if (nn.equals("_lat")) {
                                double val = Double.parseDouble(vv);
                                location.setLatitude(val);
                                continue;
                            }
                            if (nn.equals("_lon")) {
                                double val = Double.parseDouble(vv);
                                location.setLongitude(val);
                                continue;
                            }
                            if (nn.equals("_elev")) {
                                // elevation in meter - integer in location
                                double dv = Double.parseDouble(vv);
                                int val = (int) dv;
                                location.setElevation(val);
                                continue;
                            }
                            if (nn.equals("providerId")) {
                                location.setStationId(vv);
                                continue;
                            }
                            if (nn.equals("_tz")) {
                                sdf.setTimeZone(TimeZone.getTimeZone(vv));
                                continue;
                            }

                            // Construct MesonetLdadRecord
                            this.setProperty(nn, record, vv, unit);
                        }
                        // DataTime = Observation time
                        Calendar ot = record.getObservationTime();
                        DataTime dt = new DataTime(ot);
                        record.setDataTime(dt);
                        record.setLocation(location);
                        record.constructDataURI();
                        retVal.add(record);
                        // logger.info("-------------------------------------------------------");
                    }

                } else {
                    retVal.clear();
                }

            } catch (JAXBException e) {
                logger.error(traceId + " - Unable to unmarshall xml:", e);
            } catch (SecurityException e) {
                logger.error(traceId + " - SecurityException:" + e);
            } catch (IllegalArgumentException e) {
                logger.error(traceId + " - IllegalArgumentException:" + e);
            } catch (PluginException e) {
                logger.error(traceId + " - PluginException:" + e);
            }
        }
        return retVal.toArray(new PluginDataObject[retVal.size()]);
    }

    // Set values using Java Reflect API
    /**
     * @param name
     * @param obj
     * @param value
     * @return
     * @throws SecurityException
     * @throws NoSuchMethodException
     * @throws IllegalArgumentException
     * @throws IllegalAccessException
     * @throws InvocationTargetException
     * @throws ParseException
     * @throws ClassNotFoundException
     * @throws Throwable
     */

    @SuppressWarnings("unchecked")
    public void setProperty(String name, Object obj, String value, String vunit) {

        String prop = Character.toUpperCase(name.charAt(0)) + name.substring(1);
        String mname = "set" + prop;
        Object val = null;
        try {
            Class cls = obj.getClass();
            Field fld = cls.getDeclaredField(name);
            Class<?> clazz = fld.getType();
            // Type filter
            if (clazz == String.class) {
                val = value.trim();
            } else if (clazz == Calendar.class) {
                Date ot = sdf.parse(value);
                Calendar cal = Calendar.getInstance();
                cal.setTime(ot);
                val = cal;
                // only numbers
            } else {
                Double tval = Double.parseDouble(value);
                if (configFile.containsKey(vunit)) {
                    Unit<?> inUnit = (Unit<?>) UnitFormat.getUCUMInstance()
                            .parseObject(configFile.getProperty(vunit));
                    Unit<?> outUnit = (Unit<?>) UnitFormat.getUCUMInstance()
                            .parseObject(configFile.getProperty(name));
                    tval = inUnit.getConverterTo(outUnit).convert(
                            (tval).doubleValue());
                }
                if (clazz == Integer.class) {
                    val = tval.intValue();
                } else if (clazz == Short.class) {
                    val = tval.shortValue();
                } else if (clazz == Float.class) {
                    val = tval.floatValue();
                } else {
                    val = tval;
                }
            }
            Class<?> types = clazz;
            Method method = obj.getClass().getMethod(mname, types);
            method.invoke(obj, val);

        } catch (RuntimeException e) {
            logger.error(traceId + " - RuntimeException:", e);
        } catch (NoSuchMethodException e) {
            logger.error(traceId + " - NoSuchMethodException:", e);
        } catch (NoSuchFieldException e) {
            // logger.error(traceId + " - NoSuchFieldException = "+
            // e.getMessage());
        } catch (IllegalAccessException e) {
            logger.error(traceId + " - IllegalAccessException:", e);
        } catch (InvocationTargetException e) {
            logger.error(traceId + " - InvocationTargetException:", e);
        } catch (ParseException e) {
            logger.error(traceId + " - ParseException:", e);
        }
        return;
    }

    // List of Fields in record
    @SuppressWarnings("unchecked")
    public static void main(String args[]) {
        ManualLdadRecord record = new ManualLdadRecord();
        try {
            Class cls = record.getClass();

            Field fieldlist[] = cls.getDeclaredFields();
            for (int i = 0; i < fieldlist.length; i++) {
                Field fld = fieldlist[i];
                System.out.println("name = " + fld.getName());
                // System.out.println("decl class = " +
                // fld.getDeclaringClass());
                System.out.println("type  = " + fld.getType());
                int mod = fld.getModifiers();
                System.out.println("modifiers = " + Modifier.toString(mod));
                System.out.println("-----");
            }
        } catch (Throwable e) {
            System.err.println(e);
        }
    }
}
