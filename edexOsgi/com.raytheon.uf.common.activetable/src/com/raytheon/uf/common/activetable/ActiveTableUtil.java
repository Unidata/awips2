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
package com.raytheon.uf.common.activetable;

import java.util.ArrayList;
import java.util.Date;
import java.util.HashMap;
import java.util.List;
import java.util.Map;
import java.util.concurrent.ExecutionException;

import com.raytheon.uf.common.status.IUFStatusHandler;
import com.raytheon.uf.common.status.UFStatus;

/**
 * Utility module for the ActiveTable plugin.
 *
 * <pre>
 *
 * SOFTWARE HISTORY
 *
 * Date         Ticket#    Engineer    Description
 * ------------ ---------- ----------- --------------------------
 * Mar 4, 2013             dgilling    Initial creation
 * Apr 28, 2015  #4027     randerso    Expunged Calendar from ActiveTableRecord
 * Dec 09, 2015  #5197     reblum      Fixed copy/paste error in convertToDict().
 * Jun 27, 2016  #5707     nabowle     Removed geometry from ActiveTableRecord.
 *
 * </pre>
 *
 * @author dgilling
 */

public class ActiveTableUtil {

    private static final IUFStatusHandler statusHandler = UFStatus
            .getHandler(ActiveTableUtil.class);

    private ActiveTableUtil() {
        // don't allow this class to be directly instantiated, only provides
        // static utility methods.
        throw new AssertionError();
    }

    /**
     * Convert the active table to a list of Map<String, ?>s. Doing it directly
     * in Java eliminates the need for Python paths, handling JepExceptions, and
     * at least one Python/Java conversion of the active table.
     *
     * @param records
     *            A list of ActiveTableRecords to convert to
     *            Map<String,Object>s.
     * @return records, converted to a list of Maps.
     */
    public static List<Map<String, Object>> convertToDict(
            List<ActiveTableRecord> records, String site) {

        List<Map<String, Object>> dicts = new ArrayList<>(
                records.size());
        for (ActiveTableRecord atr : records) {
            Map<String, Object> template = new HashMap<>();
            template.put("vtecstr", atr.getVtecstr());
            template.put("etn", Integer.valueOf(atr.getEtn()));
            template.put("sig", atr.getSig());
            template.put("phen", atr.getPhen());
            if (atr.getSegText() != null) {
                template.put("segText", atr.getSegText());
            }
            if (atr.getOverviewText() != null) {
                template.put("overviewText", atr.getOverviewText());
                template.put("hdln", atr.getOverviewText());
            }
            template.put("phensig", atr.getPhensig());
            template.put("act", atr.getAct());
            template.put("seg", atr.getSeg());
            template.put("startTime", atr.getStartTime().getTime() / 1000);
            template.put("endTime", atr.getEndTime().getTime() / 1000);
            template.put("ufn", atr.isUfn());
            template.put("officeid", atr.getOfficeid());
            template.put("purgeTime", atr.getPurgeTime().getTime() / 1000);
            template.put("issueTime", atr.getIssueTime().getTime() / 1000);
            template.put("state", "Decoded");
            template.put("xxxid", atr.getXxxid());

            template.put("pil",
                    remapPil(site, atr.getPhen(), atr.getSig(), atr.getPil()));
            template.put("productClass", atr.getProductClass());

            template.put("id", atr.getUgcZone());

            template.put("rawMessage", atr.getRawmessage());
            template.put("countyheader", atr.getCountyheader());
            Date floodBegin = atr.getFloodBegin();
            if (floodBegin != null) {
                long floodBeginMillis = floodBegin.getTime();
                if (floodBeginMillis != 0) {
                    template.put("floodBegin", floodBeginMillis / 1000);
                }
            }
            template.put("wmoid", atr.getWmoid());

            // Warngen fields
            Date floodCrest = atr.getFloodCrest();
            if (floodCrest != null) {
                long floodCrestMillis = floodCrest.getTime();
                if (floodCrestMillis != 0) {
                    template.put("floodCrest", floodCrestMillis / 1000);
                }
            }
            Date floodEnd = atr.getFloodEnd();
            if (floodEnd != null) {
                long floodEndMillis = floodEnd.getTime();
                if (floodEndMillis != 0) {
                    template.put("floodEnd", floodEndMillis / 1000);
                }
            }
            String floodStatus = atr.getFloodRecordStatus();
            if ((floodStatus != null) && !"".equals(floodStatus.trim())) {
                template.put("floodrecordstatus", floodStatus);
            }
            String floodSeverity = atr.getFloodSeverity();
            if ((floodSeverity != null) && !"".equals(floodSeverity.trim())) {
                template.put("floodseverity", floodSeverity);
            }

            String immediateCause = atr.getImmediateCause();
            if ((immediateCause != null) && !"".equals(immediateCause.trim())) {
                template.put("immediateCause", immediateCause);
            }

            String loc = atr.getLoc();
            if ((loc != null) && !"".equals(loc.trim())) {
                template.put("loc", loc);
            }

            String locationId = atr.getLocationID();
            if ((locationId != null) && !"".equals(locationId.trim())) {
                template.put("locationId", locationId);
            }

            Integer motdir = atr.getMotdir();
            if (motdir != null) {
                template.put("motdir", motdir);
            }

            Integer motspd = atr.getMotspd();
            if (motspd != null) {
                template.put("motspd", motspd);
            }

            dicts.add(template);
        }
        return dicts;
    }

    /**
     * Some events are issued in one PIL and cancelled or extended in another.
     * This finds the PIL needed.
     *
     * @param siteID
     *            The site from which
     * @param phen
     *            The phenomenon code to look for
     * @param sig
     *            The significance code to look for
     * @param dft
     *            The PIL to use if the phensig is not remapped
     * @return The PIL after remapping.
     */
    @SuppressWarnings("unchecked")
    private static String remapPil(String siteId, String phen, String sig,
            String dft) {
        try {
            Map<Object, String> MappedPils = (Map<Object, String>) VTECPartners
                    .getInstance(siteId).getattr("VTEC_MAPPED_PILS");
            List<String> key = new ArrayList<>(3);
            key.add(phen);
            key.add(sig);
            key.add(dft);
            return MappedPils.getOrDefault(key, dft);
        } catch (ExecutionException e) {
            statusHandler.warn(
                    "Failed to retrieve VTECPartners for site [" + siteId + "]",
                    e);
        }

        return dft;
    }
}
