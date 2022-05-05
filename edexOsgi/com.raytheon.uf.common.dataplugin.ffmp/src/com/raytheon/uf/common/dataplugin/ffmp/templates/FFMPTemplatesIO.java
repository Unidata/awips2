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
package com.raytheon.uf.common.dataplugin.ffmp.templates;

import java.io.IOException;
import java.io.InputStream;
import java.util.ArrayList;
import java.util.HashMap;
import java.util.LinkedHashMap;
import java.util.List;
import java.util.Map;
import java.util.Map.Entry;
import java.util.zip.GZIPInputStream;
import java.util.zip.GZIPOutputStream;

import com.raytheon.uf.common.dataplugin.ffmp.FFMPBasinMetaData;
import com.raytheon.uf.common.dataplugin.ffmp.FFMPRecord;
import com.raytheon.uf.common.dataplugin.ffmp.FFMPVirtualGageBasinMetaData;
import com.raytheon.uf.common.localization.ILocalizationFile;
import com.raytheon.uf.common.localization.IPathManager;
import com.raytheon.uf.common.localization.LocalizationContext;
import com.raytheon.uf.common.localization.LocalizationContext.LocalizationLevel;
import com.raytheon.uf.common.localization.LocalizationContext.LocalizationType;
import com.raytheon.uf.common.localization.PathManagerFactory;
import com.raytheon.uf.common.localization.SaveableOutputStream;
import com.raytheon.uf.common.localization.exception.LocalizationException;
import com.raytheon.uf.common.serialization.SerializationException;
import com.raytheon.uf.common.serialization.SerializationUtil;
import com.raytheon.uf.common.status.IUFStatusHandler;
import com.raytheon.uf.common.status.UFStatus;

/**
 * Handles the reading and writing of FFMP Template files. Initial code
 * extracted from FFMPTemplates.java. Only the class FFMPTemplates should use
 * the methods here.
 * 
 * <pre>
 *
 * SOFTWARE HISTORY
 *
 * Date         Ticket#    Engineer    Description
 * ------------ ---------- ----------- --------------------------
 * Jun 26, 2018 6641       njensen     Extracted from FFMPTemplates
 *
 * </pre>
 *
 * @author njensen
 */

public class FFMPTemplatesIO {

    private static final IUFStatusHandler statusHandler = UFStatus
            .getHandler(FFMPTemplatesIO.class);

    private static final String TEMPLATE_FILE_LOC = "ffmp"
            + IPathManager.SEPARATOR + "templates" + IPathManager.SEPARATOR;

    /**
     * Gets the FFMPBasinData template file/object
     *
     * @param dataKey
     * @param huc
     * @param cwa
     * @return
     */
    @SuppressWarnings("unchecked")
    public static LinkedHashMap<Long, ?> readTemplateFile(String dataKey,
            String huc, String cwa) {
        LinkedHashMap<Long, ?> map = null;
        long[] list = readDomainList(dataKey, huc, cwa);

        if (huc.equals(FFMPRecord.ALL)) {
            map = new LinkedHashMap<>(list.length, 1.0f);
            Map<Long, FFMPBasinMetaData> protoMap = (Map<Long, FFMPBasinMetaData>) readDomainMap(
                    dataKey, huc, cwa);
            for (long l : list) {
                ((Map<Long, FFMPBasinMetaData>) map).put(l, protoMap.get(l));
            }
        } else {
            map = fromPrimitive(
                    (Map<Long, long[]>) readDomainMap(dataKey, huc, cwa), list);
        }

        return map;
    }

    /**
     * Writes out the byte array with dynamic serialize thrift
     *
     * @param dataKey
     * @param huc
     * @param cwa
     * @param map
     */
    @SuppressWarnings("unchecked")
    public static void writeTemplateFile(String dataKey, String huc, String cwa,
            LinkedHashMap<Long, ?> map) {
        int x = 0;
        long[] list = new long[map.keySet().size()];
        for (long l : map.keySet()) {
            list[x] = l;
            x++;
        }

        try {
            IPathManager pathManager = PathManagerFactory.getPathManager();
            LocalizationContext lc = pathManager.getContext(
                    LocalizationType.COMMON_STATIC,
                    LocalizationLevel.CONFIGURED);

            ILocalizationFile lflist = pathManager.getLocalizationFile(lc,
                    getAbsoluteFileName(dataKey, huc, cwa, "list"));
            ILocalizationFile lfmap = pathManager.getLocalizationFile(lc,
                    getAbsoluteFileName(dataKey, huc, cwa, "map"));

            try (SaveableOutputStream listSos = lflist.openOutputStream();
                    GZIPOutputStream listGos = new GZIPOutputStream(listSos);
                    SaveableOutputStream mapSos = lfmap.openOutputStream();
                    GZIPOutputStream mapGos = new GZIPOutputStream(mapSos)) {

                listGos.write(SerializationUtil.transformToThrift(list));
                listGos.finish();
                listGos.flush();

                if (huc.equals(FFMPRecord.ALL)) {
                    mapGos.write(SerializationUtil.transformToThrift(map));
                } else {
                    mapGos.write(
                            SerializationUtil.transformToThrift(toPrimitive(
                                    (LinkedHashMap<Long, List<Long>>) map)));
                }
                mapGos.finish();
                mapGos.flush();

                listSos.save();
                mapSos.save();
            }

            list = null;

        } catch (Exception e) {
            statusHandler.error("Error writing template: cwa: " + cwa
                    + " dataKey:" + dataKey + " huc: " + huc, e);
        }
    }

    /**
     * Gets the completed filename
     * 
     * @param dataKey
     * @param huc
     * @param cwa
     * @param appendage
     *
     * @return
     */
    public static String getAbsoluteFileName(String dataKey, String huc,
            String cwa, String appendage) {
        String filename = null;
        if (appendage != null) {
            filename = TEMPLATE_FILE_LOC + huc + "-" + cwa + "-" + dataKey + "-"
                    + appendage + ".bin";
        } else {
            filename = TEMPLATE_FILE_LOC + huc + "-" + cwa + "-" + dataKey
                    + ".bin";
        }

        return filename;
    }

    /**
     * Gets the list for all of the pfafs
     *
     * @param huc
     * @param cwa
     * @param dataKey
     * @return
     */
    private static long[] readDomainList(String dataKey, String huc,
            String cwa) {
        long[] list = null;

        IPathManager pathManager = PathManagerFactory.getPathManager();
        LocalizationContext lc = pathManager.getContext(
                LocalizationType.COMMON_STATIC, LocalizationLevel.CONFIGURED);
        ILocalizationFile f = pathManager.getLocalizationFile(lc,
                getAbsoluteFileName(dataKey, huc, cwa, "list"));

        try (InputStream is = f.openInputStream();
                GZIPInputStream gis = new GZIPInputStream(is)) {
            list = SerializationUtil.transformFromThrift(long[].class, gis);
        } catch (SerializationException | IOException
                | LocalizationException e) {
            statusHandler
                    .error("Exception reading domain list: Read Domain. cwa: "
                            + cwa + " dataKey: " + dataKey + " huc: " + huc, e);
        }

        return list;
    }

    /**
     * Reads the actual domain map
     *
     * @param dataKey
     * @param huc
     * @param cwa
     * @return
     */
    @SuppressWarnings("unchecked")
    private static Map<Long, ?> readDomainMap(String dataKey, String huc,
            String cwa) {
        Map<Long, ?> map = null;

        IPathManager pathManager = PathManagerFactory.getPathManager();
        LocalizationContext lc = pathManager.getContext(
                LocalizationType.COMMON_STATIC, LocalizationLevel.CONFIGURED);
        ILocalizationFile f = pathManager.getLocalizationFile(lc,
                getAbsoluteFileName(dataKey, huc, cwa, "map"));

        try (InputStream is = f.openInputStream();
                GZIPInputStream gis = new GZIPInputStream(is)) {
            map = SerializationUtil.transformFromThrift(HashMap.class, gis);
        } catch (SerializationException | IOException
                | LocalizationException e) {
            statusHandler.error("Exception reading domain map. Domain Map: "
                    + dataKey + " cwa:" + cwa + " huc: " + huc, e);

        }

        return map;
    }

    /**
     * Gets the FFMPBasinData template file/object
     *
     * @param dataKey
     * @param cwa
     * @return
     */
    public static LinkedHashMap<String, FFMPVirtualGageBasinMetaData> readVGBFile(
            String dataKey, String cwa) {
        Map<String, FFMPVirtualGageBasinMetaData> protoMap = readVGBDomainMap(
                dataKey, cwa);
        String[] list = readVGBDomainList(dataKey, cwa);
        LinkedHashMap<String, FFMPVirtualGageBasinMetaData> map = new LinkedHashMap<>(
                list.length, 1.0f);

        // construct ordered map
        for (String lid : list) {
            map.put(lid, protoMap.get(lid));
        }

        return map;
    }

    /**
     * Writes out the byte array with dynamicserialize thrift
     *
     * @param map
     * @param dataKey
     * @param cwa
     */
    public static void writeVGBFile(
            Map<String, FFMPVirtualGageBasinMetaData> map, String dataKey,
            String cwa) {
        String[] list = new String[map.keySet().size()];

        int i = 0;
        for (String lid : map.keySet()) {
            list[i] = lid;
            i++;
        }

        try {
            IPathManager pathManager = PathManagerFactory.getPathManager();
            LocalizationContext lc = pathManager.getContext(
                    LocalizationType.COMMON_STATIC,
                    LocalizationLevel.CONFIGURED);
            ILocalizationFile lfmap = pathManager.getLocalizationFile(lc,
                    getAbsoluteFileName(dataKey, "VIRTUAL", cwa, "map"));
            ILocalizationFile lflist = pathManager.getLocalizationFile(lc,
                    getAbsoluteFileName(dataKey, "VIRTUAL", cwa, "list"));

            try (SaveableOutputStream listSos = lflist.openOutputStream();
                    GZIPOutputStream listGos = new GZIPOutputStream(listSos);
                    SaveableOutputStream mapSos = lfmap.openOutputStream();
                    GZIPOutputStream mapGos = new GZIPOutputStream(mapSos)) {

                listGos.write(SerializationUtil.transformToThrift(list));
                listGos.finish();
                listGos.flush();
                listSos.save();

                mapGos.write(SerializationUtil.transformToThrift(map));
                mapGos.finish();
                mapGos.flush();
                mapSos.save();
            }

            list = null;
        } catch (Exception se) {
            statusHandler.error(
                    "Error writing VGB: cwa: " + cwa + " dataKey: " + dataKey,
                    se);
        }
    }

    /**
     * Reads the actual VGB domain map
     *
     * @param dataKey
     * @param cwa
     * @return
     */
    @SuppressWarnings("unchecked")
    private static Map<String, FFMPVirtualGageBasinMetaData> readVGBDomainMap(
            String dataKey, String cwa) {
        Map<String, FFMPVirtualGageBasinMetaData> map = null;

        IPathManager pathManager = PathManagerFactory.getPathManager();
        LocalizationContext lc = pathManager.getContext(
                LocalizationType.COMMON_STATIC, LocalizationLevel.CONFIGURED);
        ILocalizationFile f = pathManager.getLocalizationFile(lc,
                getAbsoluteFileName(dataKey, FFMPRecord.VIRTUAL, cwa, "map"));

        try (InputStream is = f.openInputStream();
                GZIPInputStream gis = new GZIPInputStream(is)) {
            map = SerializationUtil.transformFromThrift(HashMap.class, gis);
        } catch (SerializationException | IOException
                | LocalizationException e) {
            statusHandler
                    .error("Exception reading VHB Domain map. Virtual Basins: "
                            + dataKey + " cwa: " + cwa, e);
        }

        return map;
    }

    /**
     * Reads the actual VGB domain list
     *
     * @param dataKey
     * @param cwa
     * @return
     */
    private static String[] readVGBDomainList(String dataKey, String cwa) {
        String[] list = null;

        IPathManager pathManager = PathManagerFactory.getPathManager();
        LocalizationContext lc = pathManager.getContext(
                LocalizationType.COMMON_STATIC, LocalizationLevel.CONFIGURED);
        ILocalizationFile f = pathManager.getLocalizationFile(lc,
                getAbsoluteFileName(dataKey, FFMPRecord.VIRTUAL, cwa, "list"));

        try (InputStream is = f.openInputStream();
                GZIPInputStream gis = new GZIPInputStream(is)) {
            list = SerializationUtil.transformFromThrift(String[].class, gis);
        } catch (SerializationException | IOException
                | LocalizationException e) {
            statusHandler.error(
                    "Exception reading VGB Domain List. Read Virtual Domain: cwa: "
                            + cwa + " dataKey: " + dataKey,
                    e);
        }

        return list;
    }

    /**
     * compress to primitive
     *
     * @param list
     * @return
     */
    private static Map<Long, long[]> toPrimitive(Map<Long, List<Long>> map) {
        Map<Long, long[]> primList = new HashMap<>();
        for (Entry<Long, List<Long>> entry : map.entrySet()) {
            List<Long> val = entry.getValue();
            long[] longs = new long[val.size()];
            for (int i = 0; i < val.size(); i++) {
                longs[i] = val.get(i).longValue();
            }
            primList.put(entry.getKey(), longs);
        }
        return primList;
    }

    /**
     * back to LinkedHash
     *
     * @param longs
     * @return
     */
    private static LinkedHashMap<Long, List<Long>> fromPrimitive(
            Map<Long, long[]> longs, long[] list) {
        // reconstructs the LinkedHash in order
        LinkedHashMap<Long, List<Long>> map = new LinkedHashMap<>();
        for (Long l : list) {
            List<Long> longa = new ArrayList<>();
            for (int i = 0; i < longs.get(l).length; i++) {
                longa.add(longs.get(l)[i]);
            }
            map.put(l, longa);
        }
        return map;
    }

}
