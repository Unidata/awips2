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
package com.raytheon.uf.edex.plugin.acarssounding.tools;

import java.io.File;
import java.util.ArrayList;
import java.util.HashSet;
import java.util.List;
import java.util.Set;

import org.slf4j.Logger;

import com.raytheon.uf.common.dataplugin.acars.ACARSRecord;

/**
 * TODO Add Description
 * 
 * <pre>
 * 
 * SOFTWARE HISTORY
 * 
 * Date         Ticket#    Engineer    Description
 * ------------ ---------- ----------- --------------------------
 * Nov 29, 2010            jkorman     Initial creation
 * Aug 18, 2014 3530       bclement    removed dead code
 * Dec 10, 2015 5166       kbisanz     Update logging to use SLF4J
 * 
 * </pre>
 * 
 * @author jkorman
 * @version 1.0
 */

public class IntermediateData {

    private final ACARSAircraftInfo acftInfo;

    private List<ACARSRecord> recordList = null;

    private List<String> acarsDataURIs = null;

    /**
     * Construct an ACARS IntermediateData object with a given tail number.
     * 
     * @param tailNumber
     *            The aircraft tail number identifer associated with this data.
     */
    public IntermediateData(ACARSAircraftInfo acftInfo) {
        this.acftInfo = acftInfo;
    }

    /**
     * Construct an ACARS IntermediateData object with a given tail number.
     * 
     * @param recordList
     * @param acarsDataURIs
     * 
     * @param tailNumber
     *            The aircraft tail number identifer associated with this data.
     */
    public IntermediateData(ACARSAircraftInfo acftInfo,
            List<ACARSRecord> recordList, List<String> acarsDataURIs) {
        this(acftInfo);
        setAcarsData(recordList, acarsDataURIs);
    }

    /**
     * @return the acftInfo
     */
    public ACARSAircraftInfo getAcftInfo() {
        return acftInfo;
    }

    /**
     * 
     * @return
     */
    public String getTailNumber() {
        return (acftInfo != null) ? acftInfo.getTailNumber() : null;
    }

    /**
     * 
     * @return
     */
    public String getTailNumberFilePath() {
        return (acftInfo != null) ? acftInfo.getFilePath() : null;
    }

    /**
     * 
     * @return
     */
    public List<ACARSRecord> getRecordList() {
        return recordList;
    }

    /**
     * @return the acarsDataURIs
     */
    public List<String> getAcarsDataURIs() {
        return acarsDataURIs;
    }

    /**
     * Take a list of ACARSRecords and select only those records that have a
     * corresponding entry in the dataURI list.
     * 
     * @param recordList
     * @param acarsDataURIs
     */
    public void setAcarsData(List<ACARSRecord> recList, List<String> dataURIs) {

        Set<String> uris = new HashSet<String>();
        for (String s : dataURIs) {
            if (s.length() > 21) {
                uris.add(s.substring(21));
            }
        }
        this.recordList = new ArrayList<ACARSRecord>();
        for (ACARSRecord rec : recList) {
            if (uris.contains(rec.getDataURI())) {
                recordList.add(rec);
            }
        }
        acarsDataURIs = dataURIs;
    }

    /**
     * Remove any ACARSRecords and corresponding datauri that were used to
     * create a sounding.
     */
    public final void reconcile(Logger logger) {

        if ((recordList != null) && (acarsDataURIs != null)) {
            List<Integer> deletions = new ArrayList<Integer>();
            for (int i = 0; i < recordList.size(); i++) {
                ACARSRecord r = recordList.get(i);
                if (r.isUsedInSounding()) {
                    int pos = findDataUri(r.getDataURI(), acarsDataURIs);
                    if (pos >= 0) {
                        deletions.add(pos);
                        if (logger.isDebugEnabled()) {
                            logger.debug("removing dataURI " + r.getDataURI());
                        }
                    }
                }
            }

            for (ACARSRecord r : recordList) {
                if (r.isUsedInSounding()) {

                }
            } // for
              // Now run the list of deletions
            for (Integer n : deletions) {
                acarsDataURIs.set(n, null);
            }
            writeACARSDataUris(logger);
        }
    }

    /**
     * 
     * @param uri
     * @param dataURIs
     * @return Returns the index where the uri was found. Returns -1 if the uri
     *         is not found.
     */
    private static int findDataUri(String uri, List<String> dataURIs) {
        int pos = -1;

        if (uri != null) {
            pos = search(uri, dataURIs);
            if (pos < 0) {
                // We may have modified the dataURI so need additional
                // checks to make sure. Zero out the seconds and try
                // again.
                uri = uri.substring(0, 24) + "00" + uri.substring(26);
                // and try again
                pos = search(uri, dataURIs);
            }
        }

        return pos;
    }

    private static int search(String uri, List<String> uris) {
        int pos = -1;
        if ((uris != null) && (uri != null)) {
            for (int i = 0; i < uris.size(); i++) {
                if (uri.equals(uris.get(i).substring(21))) {
                    pos = i;
                    break;
                }
            }
        }
        return pos;
    }

    /**
     * 
     */
    private final void writeACARSDataUris(Logger logger) {
        if ((acftInfo != null) && (acftInfo.getFilePath() != null)) {
            if (acarsDataURIs != null) {
                File out = new File(acftInfo.getFilePath());

                boolean writeURIs = false;
                for (String s : acarsDataURIs) {
                    if (s != null) {
                        writeURIs = true;
                        break;
                    }
                }
                if (writeURIs) {
                    ACARSSoundingTools.writeAircraftData(out, acarsDataURIs,
                            logger);
                } else {
                    if (out.exists()) {
                        if (!out.delete()) {
                            logger.error("Could not delete empty file "
                                    + out.getName());
                        }
                    }
                }
            }
        }
    }

}
