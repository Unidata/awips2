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
package com.raytheon.uf.edex.plugin.mpe.dpa;

import java.nio.BufferUnderflowException;
import java.nio.ByteBuffer;
import java.sql.Timestamp;

import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

import com.raytheon.uf.common.dataplugin.shef.tables.Dparadar;
import com.raytheon.uf.common.dataplugin.shef.tables.DparadarId;
import com.raytheon.uf.common.dataplugin.shef.tables.Radarloc;
import com.raytheon.uf.common.mpe.constants.DPAConstants;
import com.raytheon.uf.edex.plugin.mpe.SequenceFinder;
import com.raytheon.uf.edex.plugin.mpe.dao.impl.RadarlocDao;
import com.raytheon.uf.edex.plugin.mpe.dpa.dao.DpaRadarDao;

/**
 * Collect supplemental parameters for entry to DPARadar table. Ported and
 * modified from { getsuppl.c, wrtodb_suppl.c }
 * 
 * <pre>
 * 
 * SOFTWARE HISTORY
 * 
 * Date         Ticket#    Engineer    Description
 * ------------ ---------- ----------- --------------------------
 * Jun 01, 2016 4622       jschmid     Initial creation
 * Sep 28, 2016 4622       skorolev    Added DPAConstants. 
 * Nov 02, 2016 4622       skorolev    Corrected writeToDbSupplementalParameters().
 *                                     Removed unnecessary condition.
 * Dec 15, 2016 4622       bkowal      Updates to handle the case when the Radar Location
 *                                     may not be found.
 * 
 * </pre>
 * 
 * @author jschmid
 */

public class SupplementalParameters {

    private final Logger logger = LoggerFactory.getLogger(getClass());

    private float areared = (float) DPAConstants.MISSING_VALUE;

    private float biscanr = (float) DPAConstants.MISSING_VALUE;

    private short nisolbin = DPAConstants.MISSING_VALUE_INT;

    private short noutint = DPAConstants.MISSING_VALUE_INT;

    private short noutrep = DPAConstants.MISSING_VALUE_INT;

    private short nbadscan = DPAConstants.MISSING_VALUE_INT;

    private short nhourout = DPAConstants.MISSING_VALUE_INT;

    private int blockedBins = DPAConstants.MISSING_VALUE_INT;

    private int clutterBins = DPAConstants.MISSING_VALUE_INT;

    private int smoothedBins = DPAConstants.MISSING_VALUE_INT;

    private float binsFilled = (float) DPAConstants.MISSING_VALUE;

    private float elevAngle = (float) DPAConstants.MISSING_VALUE;

    private float rainArea = (float) DPAConstants.MISSING_VALUE;

    private short volcovpat = DPAConstants.MISSING_VALUE_INT;

    private short opermode = DPAConstants.MISSING_VALUE_INT;

    private String missper = new String("F");

    private int pcipFlag = 4;

    /**
     * Populate this object with supplemental parameters for inclusion in
     * DPARadar table. The beginning of the supplemental data is found by
     * searching for for the header "SUPL(nn)" where nn = 27.
     * 
     * pcipflg set to 0 if the file indicates no precipitation detected, 1 if
     * file indicates a bad rate scan. 2 if the file indicates not enough data
     * in hour. 3 if the file indicates a disk error. 4 if the file indicates
     * precipitation.
     * 
     * NOTES This routine assumes that all supplemental data except the missing
     * period flag is stored in 80 byte records, with the last 8 bytes
     * containing the numerical value.
     * 
     * @param productBytes
     *            The ByteBuffer used to parse the DPA record so far.
     * @throws DecodeDPAException
     *             If unable to obtain all parameters.
     */
    public SupplementalParameters(ByteBuffer productBytes,
            SequenceFinder productSeq) throws DecodeDPAException {

        // Advance buffer to Supplemental Parameters header
        byte[] headerBytes = { 0x53, 0x55, 0x50, 0x4C, 0x28 }; // "SUPL(";

        try {
            productSeq.advanceToSequence(productBytes, headerBytes);
        } catch (BufferUnderflowException e) {
            // Exception takes the place of setting: (pcipFlag = 99)
            throw new DecodeDPAException(
                    "Unable to locate supplemental header bytes: 'SUPL('. Cannot read supplemental parameters.");
        }

        byte[] parameterCountBytes = new byte[2];
        productBytes.get(parameterCountBytes);
        String parameterCountStr = (new String(parameterCountBytes)).trim();
        int suplParamCount;
        try {
            suplParamCount = Integer.parseInt(parameterCountStr);
        } catch (Exception e) {
            throw new DecodeDPAException(
                    "Unable to read number of supplemental parameters in from 'SUPL(XX).  Could not parse expected integer: "
                            + parameterCountStr);
        }

        // Advance past character ')'
        productBytes.get();

        /*
         * If numparm = 1, then check for messages. Else read past rate scans,
         * hourly accum end date, hourly accum end time.
         */
        if (1 == suplParamCount) {

            /*
             * Search for one of the four possible strings that indicate there
             * is no supplemental data. The strings are in the last two records
             * of the DPA file. If one is found, then output variables will
             * remain as initialized, pcipflg will be set, and control returns.
             */
            int whileCount = 0;
            while ((whileCount < 160) && (productBytes.hasRemaining())) {

                // Read one char, check for shared first letter
                byte firstLetterByte = productBytes.get();
                char firstLetter = (char) firstLetterByte;
                whileCount += 1;
                if ('N' == firstLetter) {

                    // Check if the rest of the string matches one of the four.
                    byte[] restOfStrBytes = new byte[48];
                    productBytes.mark();
                    productBytes.get(restOfStrBytes);
                    String wholeString = ('N' + new String(restOfStrBytes));

                    // Check for "NO PRECIPITATION IN PREVIOUS HOUR"
                    if (wholeString.indexOf("NO PRECIP") >= 0) {
                        pcipFlag = 0;
                        return;
                    }

                    // Check for
                    // "NO HOURLY ACCUMULATION BECAUSE RATE SCAN FLAGGED BAD"
                    else if (wholeString.indexOf("FLAGGED") >= 0) {
                        pcipFlag = 1;
                        return;
                    }

                    // Check for
                    // "NO HOURLY ACCUMULATION BECAUSE NOT ENOUGH DATA IN HOUR"
                    else if (wholeString.indexOf("NOT ENOUGH") >= 0) {
                        pcipFlag = 2;
                        return;
                    }

                    // Check for
                    // "NO SUPPLEMENTAL DATA AVAILABLE DUE TO DISK ERROR"
                    else if (wholeString.indexOf("DISK ERROR") >= 0) {
                        pcipFlag = 3;
                        return;
                    } else {
                        productBytes.reset();
                    }
                }

                pcipFlag = 0;
            }

            logger.warn("No supplemental data message found");
            pcipFlag = 0;
            return;
        } else { // Parameter count != 1
            byte[] advance31Bytes = new byte[31];
            byte[] advance72Bytes = new byte[72];

            /*
             * Advance buffer to "TOTAL" from string
             * "TOTAL NO. OF BLOCKAGE BINS REJECTED"
             */
            byte[] totalSeq = { 0x54, 0x4F, 0x54, 0x41, 0x4C }; // "TOTAL"
            try {
                productSeq.advanceToSequence(productBytes, totalSeq);
            } catch (BufferUnderflowException e) {
                throw new DecodeDPAException(
                        "Unable to locate byte sequence: 'TOTAL'. Cannot read supplemental parameters.");
            }

            try { // Get Supplemental Data Fields:

                // Number of blockage bins
                productBytes.get(advance31Bytes);
                byte[] blocked_binsBytes = new byte[8];
                productBytes.get(blocked_binsBytes);
                String blocked_binsStr = new String(blocked_binsBytes).trim();
                blockedBins = Integer.parseInt(blocked_binsStr);

                // Number of clutter bins rejected is in the next 80 byte record
                productBytes.get(advance72Bytes);
                byte[] clutter_binsBytes = new byte[8];
                productBytes.get(clutter_binsBytes);
                String clutter_binsStr = new String(clutter_binsBytes).trim();
                clutterBins = Integer.parseInt(clutter_binsStr);

                // Number of bins smoothed is in the next 80 byte record
                productBytes.get(advance72Bytes);
                byte[] smoothed_binsBytes = new byte[8];
                productBytes.get(smoothed_binsBytes);
                String smoothed_binsStr = new String(smoothed_binsBytes).trim();
                smoothedBins = Integer.parseInt(smoothed_binsStr);

                // Percent of hybrid scan bins filled is in next 80 byte record
                productBytes.get(advance72Bytes);
                byte[] bins_filledBytes = new byte[8];
                productBytes.get(bins_filledBytes);
                String bins_filledStr = new String(bins_filledBytes).trim();
                binsFilled = Float.parseFloat(bins_filledStr);

                // Highest elevation angle is in the next 80 byte record
                productBytes.get(advance72Bytes);
                byte[] elev_angleBytes = new byte[8];
                productBytes.get(elev_angleBytes);
                String elev_angleStr = new String(elev_angleBytes).trim();
                elevAngle = Float.parseFloat(elev_angleStr);

                // Total hybrid scan rain area is in the next 80 byte record
                productBytes.get(advance72Bytes);
                byte[] rain_areaBytes = new byte[8];
                productBytes.get(rain_areaBytes);
                String rain_areaStr = new String(rain_areaBytes).trim();
                rainArea = Float.parseFloat(rain_areaStr);

                // Number of bad scans in hour is in the next 80 byte record
                productBytes.get(advance72Bytes);
                byte[] nbadscanBytes = new byte[8];
                productBytes.get(nbadscanBytes);
                String nbadscanStr = new String(nbadscanBytes).trim();
                nbadscan = Short.parseShort(nbadscanStr);

            } catch (Exception e) {
                throw new DecodeDPAException(
                        "Unable to parse numeric Supplemental Parameters field: ",
                        e);
            }

            /*
             * Skip bias estimate and bias error variance records Skip bias
             * estimate, effective # gr pairs, mem span records Search for and
             * read 'CURRENT VOLUME COVERAGE PATTERN'
             */
            byte[] currenSeq = { 0x43, 0x55, 0x52, 0x52, 0x45 }; // "CURRE"
            try {
                productSeq.advanceToSequence(productBytes, currenSeq);
            } catch (BufferUnderflowException e) {
                throw new DecodeDPAException(
                        "Unable to locate byte sequence: 'CURRE' to find CURRENT VOLUME COVERAGE PATTERN. Cannot read supplemental parameters.");
            }

            // Move 30 bytes past end of 'CURRE', read volume coverage pattern
            productBytes.get(advance31Bytes);
            byte[] volcovpatBytes = new byte[8];
            productBytes.get(volcovpatBytes);
            String volcovpatStr = new String(volcovpatBytes).trim();
            volcovpat = (short) Integer.parseInt(volcovpatStr);

            // Current operational (weather) mode is in the next 80 byte record
            productBytes.get(advance72Bytes);
            byte[] opermodeBytes = new byte[8];
            productBytes.get(opermodeBytes);
            String opermodeStr = new String(opermodeBytes).trim();
            opermode = Short.parseShort(opermodeStr);

            /*
             * Missing period statement is in the next 80 byte record Find
             * pattern "NO MIS" from string "NO MISSING PERIODS IN CURRENT HOUR"
             * If found, then set missper = 'F' (else 'T', and pcipflg = 4)
             */
            missper = "T"; // default
            byte[] noMissSeq = { 0x4E, 0x4F, 0x20, 0x4D, 0x49, 0x53 }; // "NO
                                                                       // MIS"
            try {
                productSeq.advanceToSequence(productBytes, noMissSeq);
                missper = "F";
            } catch (BufferUnderflowException e) {
                throw new DecodeDPAException(
                        "Unable to locate byte sequence: 'NO MIS' to find CURRENT VOLUME COVERAGE PATTERN. Cannot read supplemental parameters.");
            }
        }
    }

    public float getAreared() {
        return areared;
    }

    public float getBiscanr() {
        return biscanr;
    }

    public short getNisolbin() {
        return nisolbin;
    }

    public short getNoutint() {
        return noutint;
    }

    public short getNoutrep() {
        return noutrep;
    }

    public short getNbadscan() {
        return nbadscan;
    }

    public short getNhourout() {
        return nhourout;
    }

    public int getBlockedBins() {
        return blockedBins;
    }

    public int getClutterBins() {
        return clutterBins;
    }

    public int getSmoothedBins() {
        return smoothedBins;
    }

    public float getBinsFilled() {
        return binsFilled;
    }

    public float getElevAngle() {
        return elevAngle;
    }

    public float getRainArea() {
        return rainArea;
    }

    public short getVolcovpat() {
        return volcovpat;
    }

    public short getOpermode() {
        return opermode;
    }

    public String getMissper() {
        return missper;
    }

    public int getPcipflg() {
        return pcipFlag;
    }

    /**
     * Write supplemental data and other data to DPARadar table, or log what
     * would have been written if 'writeToDB' not enabled.
     * 
     * @param radarId
     * @param minoff
     * @param maxvalh
     * @param maxvald
     * @param s1BiasValue
     * @param prGenStr
     * @param operationalWeatherMode
     * @param radarlocDao
     * @param dparadarDao
     * @param writeToDB
     * @param gridFilename
     */
    public void writeToDbSupplementalParameters(String radarId, short minoff,
            float maxvalh, float maxvald, float s1BiasValue, String prGenStr,
            int operationalWeatherMode, RadarlocDao radarlocDao,
            DpaRadarDao dparadarDao, boolean writeToDB, String gridFilename) {

        Timestamp timestamp = null;
        try {
            timestamp = Timestamp.valueOf(prGenStr);
        } catch (IllegalArgumentException e) {
            logger.warn(
                    "Could not create timestamp in writeSupplementalParameters, record not written to 'dparadar' table.");
            return;
        }

        java.util.Date tsDate = new java.util.Date(timestamp.getTime());
        DparadarId curProductId = new DparadarId(radarId, tsDate);
        Radarloc curProductLoc = radarlocDao.getRadarloc(radarId);
        if (curProductLoc == null) {
            logger.warn(
                    "Could not find the associated Radar Location for radar: "
                            + radarId
                            + ", record not written to 'dparadar' table.");
            return;
        }

        Dparadar newRecord = new Dparadar(curProductId, curProductLoc);
        newRecord.setAreared(getAreared());
        newRecord.setBiscanr(getBiscanr());
        newRecord.setNisolbin(getNisolbin());
        newRecord.setNoutint(getNoutint());
        newRecord.setNoutrep(getNoutrep());
        newRecord.setNbadscan(getNbadscan());
        newRecord.setNhourout(getNhourout());
        newRecord.setBlockBinsReject(getBlockedBins());
        newRecord.setClutterBinsRej(getClutterBins());
        newRecord.setBinsSmoothed(getSmoothedBins());
        newRecord.setScanBinsFilled(getBinsFilled());
        newRecord.setHighElevAngle(getElevAngle());
        newRecord.setVolcovpat(getVolcovpat());
        newRecord.setOpermode((short) operationalWeatherMode);
        newRecord.setMissper(getMissper());
        newRecord.setMinoff(minoff);
        newRecord.setMaxvalh(maxvalh);
        newRecord.setMaxvald(maxvald);
        newRecord.setS1BiasValue(s1BiasValue);
        newRecord.setProducttime(tsDate);
        newRecord.setSupplmess((short) getPcipflg());
        newRecord.setGridFilename(gridFilename);

        try {
            if (writeToDB) {
                dparadarDao.saveOrUpdate(newRecord);
            } else {
                logger.debug("DB-write disabled, reporting un-updated record "
                        + "(dparadar table): " + newRecord.toString());
            }
        } catch (IllegalArgumentException e) {
            logger.warn(
                    "Error in writeSupplementalParameters. Record not written to 'dparadar' table. ",
                    e);
        }
    }
}
