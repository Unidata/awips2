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

import com.raytheon.uf.common.dataplugin.shef.tables.Dpaadapt;
import com.raytheon.uf.common.dataplugin.shef.tables.DpaadaptId;
import com.raytheon.uf.common.mpe.constants.DPAConstants;
import com.raytheon.uf.edex.plugin.mpe.SequenceFinder;
import com.raytheon.uf.edex.plugin.mpe.dpa.dao.DpaAdaptDao;

/**
 * Get the correct number of adaptable parameters and populate Adaptable
 * Parameters. Ported and modified from: { getadapt.c, wrtodb_adapt.c }
 * 
 * <pre>
 * 
 * SOFTWARE HISTORY
 * 
 * Date         Ticket#    Engineer    Description
 * ------------ ---------- ----------- --------------------------
 * May 27, 2016 4622       jschmid     Initial creation
 * Sep 28, 2016 4622       skorolev    Added DPAConstants
 * Nov 02, 2016 4622       skorolev    Removed unnecessary condition.
 * 
 * </pre>
 * 
 * @author jschmid
 */

public class AdaptableParameters {

    private int numParameters = 0;

    private float[] adaptableFloats;

    private String adaptableBooleanStr;

    private final Logger logger = LoggerFactory.getLogger(getClass());

    /**
     * Get the correct number of adaptable parameters for the build number and
     * populate this object with adaptable parameters from raw DPA products.
     * 
     * The parameters are found by searching for the header of the form
     * "ADAP(nn)" where nn = 32 for Build 8.
     * 
     * ORPG Build 8 has dropped the following parameters: max storm speed
     * threshold max time difference minimum area time continuity test time
     * continuity parameter #1 time continuity parameter #2 max rate of echo
     * area change.
     * 
     * @param productBytes
     *            The ByteBuffer used to parse the DPA record so far.
     * @param productSeq
     *            Sequence Finder
     * @throws DecodeDPAException
     *             If unable to obtain all parameters.
     */
    public AdaptableParameters(ByteBuffer productBytes,
            SequenceFinder productSeq) throws DecodeDPAException {

        // Advance buffer to Adaptable Parameters header
        byte[] headerBytes = { 0x41, 0x44, 0x41, 0x50, 0x28 }; // "ADAP("
        try {
            productSeq.advanceToSequence(productBytes, headerBytes);
        } catch (BufferUnderflowException e) {
            throw new DecodeDPAException(
                    "Unable to locate ADAP header bytes: 'ADAP('. Cannot read adaptable parameters.");
        }

        // Determine number of parameters from build
        numParameters = 32; // Build 8 or compatible

        byte[] parameterCountBytes = new byte[2];
        productBytes.get(parameterCountBytes);
        String parameterCountStr = new String(parameterCountBytes);
        if (numParameters != Integer.parseInt(parameterCountStr)) {
            throw new DecodeDPAException(
                    "Number of parameters read from adaptable parameters header ("
                            + parameterCountStr
                            + ") did not match number implied by build number ("
                            + numParameters + ").");
        }

        // Advance past character ')'
        productBytes.get();

        // Read Float parms
        int numDoubleParms = (numParameters - 1);
        this.adaptableFloats = new float[numDoubleParms];
        byte[] fixedWidthTxt = new byte[8];
        String floatTxt = null;
        for (int n = 0; n < numDoubleParms; n++) {
            try {
                productBytes.get(fixedWidthTxt);
                floatTxt = new String(fixedWidthTxt);
                if ((floatTxt.trim()).length() > 0) {
                    adaptableFloats[n] = Float.parseFloat(floatTxt);
                } else {
                    adaptableFloats[n] = (float) DPAConstants.MISSING_VALUE; // Flag
                                                                             // invalid
                }
            } catch (NumberFormatException e) {
                throw new DecodeDPAException(
                        "Invalid value for double adaptable parameter (# " + n
                                + ").  Read: " + floatTxt);
            }
        }

        // Read boolean: 'adaptableBooleanStr'
        productBytes.get(fixedWidthTxt);
        String booleanTxt = (new String(fixedWidthTxt)).trim();
        if (!(booleanTxt.equals("T") || (booleanTxt.equals("F")))) {
            throw new DecodeDPAException(
                    "Invalid value for boolean adaptable parameter.  Expected 'T' or 'F', read: "
                            + booleanTxt);
        }
        this.adaptableBooleanStr = booleanTxt;
    }

    public int getNumParameters() {
        return numParameters;
    }

    public float[] getFloats() {
        return adaptableFloats;
    }

    public String getBooleanStr() {
        return adaptableBooleanStr;
    }

    /**
     * Write adaptable parameters to 'DpaAdapt' database table.
     * 
     * @param obsTimeStr
     * @param radarId
     * @param dpaAdaptDao
     * @param writeToDB
     */
    public void writeToDbAdaptableParameters(String obsTimeStr, String radarId,
            DpaAdaptDao dpaAdaptDao, boolean writeToDB) {

        Timestamp timestamp = null;
        try {
            timestamp = Timestamp.valueOf(obsTimeStr);
        } catch (IllegalArgumentException e) {
            logger.warn("Error in writeAdaptableParameters creating timestamp: "
                    + e.getMessage()
                    + " Record not written to 'dpaadapt' table.");
            return;
        }

        Dpaadapt dpaAdaptRecord = new Dpaadapt();
        java.util.Date tsDate = new java.util.Date(timestamp.getTime());
        DpaadaptId dpaRecordId = new DpaadaptId(radarId, tsDate);
        dpaAdaptRecord.setId(dpaRecordId);

        dpaAdaptRecord.setMltZrcoef(getFloats()[9]);
        dpaAdaptRecord.setPwrZrcoef(getFloats()[10]);
        dpaAdaptRecord.setMinZrefl(getFloats()[11]);
        dpaAdaptRecord.setMaxZrefl(getFloats()[12]);

        dpaAdaptRecord.setBeamWidth(getFloats()[0]);
        dpaAdaptRecord.setBlockageThresh(getFloats()[1]);
        dpaAdaptRecord.setClutterThresh(getFloats()[2]);
        dpaAdaptRecord.setWeightThresh(getFloats()[3]);
        dpaAdaptRecord.setHybridScanThresh(getFloats()[4]);
        dpaAdaptRecord.setLowReflectThresh(getFloats()[5]);
        dpaAdaptRecord.setDetectReflectThr(getFloats()[6]);
        dpaAdaptRecord.setDetectAreaThresh(getFloats()[7]);
        dpaAdaptRecord.setDetectTimeThresh(getFloats()[8]);
        dpaAdaptRecord.setExclusionZones(getFloats()[13]);

        dpaAdaptRecord.setMinReflth((float) DPAConstants.MISSING_VALUE);
        dpaAdaptRecord.setMaxReflth((float) DPAConstants.MISSING_VALUE);
        dpaAdaptRecord.setRefTltest((float) DPAConstants.MISSING_VALUE);
        dpaAdaptRecord.setRngTltin((float) DPAConstants.MISSING_VALUE);
        dpaAdaptRecord.setRngTltout((float) DPAConstants.MISSING_VALUE);
        dpaAdaptRecord.setMaxBirng((float) DPAConstants.MISSING_VALUE);
        dpaAdaptRecord.setMinEchoar((float) DPAConstants.MISSING_VALUE);
        dpaAdaptRecord.setMinAwrefl((float) DPAConstants.MISSING_VALUE);
        dpaAdaptRecord.setMaxPctred((float) DPAConstants.MISSING_VALUE);
        dpaAdaptRecord.setMinBirng((float) DPAConstants.MISSING_VALUE);

        dpaAdaptRecord.setMaxStmspd((float) DPAConstants.MISSING_VALUE);
        dpaAdaptRecord.setMaxTimdif((float) DPAConstants.MISSING_VALUE);
        dpaAdaptRecord.setMinArtcon((float) DPAConstants.MISSING_VALUE);
        dpaAdaptRecord.setTimP1cont((float) DPAConstants.MISSING_VALUE);
        dpaAdaptRecord.setTimP2cont((float) DPAConstants.MISSING_VALUE);
        dpaAdaptRecord.setMaxEcarch((float) DPAConstants.MISSING_VALUE);
        dpaAdaptRecord.setRngCutoff(getFloats()[14]);
        dpaAdaptRecord.setRngE1coef(getFloats()[15]);
        dpaAdaptRecord.setRngE2coef(getFloats()[16]);
        dpaAdaptRecord.setRngE3coef(getFloats()[17]);
        dpaAdaptRecord.setMinPrate(getFloats()[18]);
        dpaAdaptRecord.setMaxPrate(getFloats()[19]);

        dpaAdaptRecord.setTimRestrt(getFloats()[20]);
        dpaAdaptRecord.setMaxTimint(getFloats()[21]);
        dpaAdaptRecord.setMinTimprd(getFloats()[22]);
        dpaAdaptRecord.setThrHlyout(getFloats()[23]);
        dpaAdaptRecord.setEndTimgag(getFloats()[24]);
        dpaAdaptRecord.setMaxPrdval(getFloats()[25]);
        dpaAdaptRecord.setMaxHlyval(getFloats()[26]);
        dpaAdaptRecord.setTimBiest(getFloats()[27]);
        dpaAdaptRecord.setThrNosets(getFloats()[28]);
        dpaAdaptRecord.setResBias(getFloats()[29]);
        dpaAdaptRecord.setLongestLag(getFloats()[30]);
        dpaAdaptRecord.setBiasApplied(getBooleanStr());

        try {
            if (writeToDB) {
                dpaAdaptDao.saveOrUpdate(dpaAdaptRecord);
            } else {
                logger.debug("DB-write disabled, reporting un-updated record "
                        + "(dpaadapt table): " + dpaAdaptRecord.toString());
            }
        } catch (IllegalArgumentException e) {
            logger.warn(
                    "Error in writeToDbAdaptableParameters. Record not written to 'dparadar' table. ",
                    e);
        }
    }
}
