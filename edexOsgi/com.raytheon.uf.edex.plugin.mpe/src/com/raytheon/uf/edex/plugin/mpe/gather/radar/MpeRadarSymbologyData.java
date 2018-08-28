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
package com.raytheon.uf.edex.plugin.mpe.gather.radar;

import java.nio.ByteBuffer;
import java.nio.charset.StandardCharsets;
import java.util.ArrayList;
import java.util.Collections;
import java.util.List;
import java.util.regex.Matcher;
import java.util.regex.Pattern;

/**
 * Symbology data block of a Radar product.
 *
 * <pre>
 *
 * SOFTWARE HISTORY
 *
 * Date         Ticket#    Engineer    Description
 * ------------ ---------- ----------- --------------------------
 * Nov 22, 2016 5588      nabowle     Initial creation
 * Jan 10, 2016 6058      bkowal      Updated to allow extracting only the symbology
 *                                    from a radar data file.
 * Jul 19, 2018 5588      mapeters    Fix readParams() ignoring the last param
 *
 * </pre>
 *
 * @author nabowle
 */

public class MpeRadarSymbologyData {

    private static final float EMPTY_PARAM = -99.0F;

    private static final int PARAM_LEN = 8;

    private static final Pattern PSM_PATTERN = Pattern
            .compile("PSM \\(([ 0-9][0-9])\\)");

    private static final Pattern ADAP_PATTERN = Pattern
            .compile("ADAP\\(([ 0-9][0-9])\\)");

    private static final String PARAM_VALUE_TRUE = "T";

    private List<MpeRadarDataRecord> data = Collections.emptyList();

    private float[] adapParams;

    private float[] psmParams;

    private String biasApplied;

    protected MpeRadarSymbologyData() {
    }

    /**
     * @throws InvalidMpeRadarException
     *
     */
    public MpeRadarSymbologyData(ByteBuffer buf)
            throws InvalidMpeRadarException {
        super();
        readData(buf);
        readPsmParams(buf);
        readAdaptableParams(buf);
    }

    /**
     * Read the Graphic data and produces {@link MpeRadarDataRecord}s for every
     * "row" of data that is read.
     *
     * @param byteBuffer
     *            the buffer to read the data from
     * @throws InvalidMpeRadarException
     */
    protected void readData(final ByteBuffer buf)
            throws InvalidMpeRadarException {
        if (buf.getShort() != -1) {
            throw new InvalidMpeRadarException("Symbology block is malformed.");
        }
        buf.position(buf.position() + 2);
        int blockLength = buf.getInt();
        MpeRadarDecodeUtils.checkFileRemaining(buf, "Symbology data",
                blockLength - 8);
        buf.position(buf.position() + 4);
        int dataLayerLength = buf.getInt();
        if (buf.remaining() < dataLayerLength) {
            throw new InvalidMpeRadarException("Data is improperly formed.");
        }
        buf.position(buf.position() + 4);
        short numRangeBins = buf.getShort();
        buf.position(buf.position() + 6);
        int numRadials = buf.getShort();
        if (numRadials != MpeRadarDecodeConstants.MAX_AZIMUTH) {
            throw new InvalidMpeRadarException(
                    "Unexpected number of radials. Expected "
                            + MpeRadarDecodeConstants.MAX_AZIMUTH
                            + ", but read " + numRadials);
        }
        data = new ArrayList<>(numRadials);
        for (int i = 0; i < numRadials; i++) {
            MpeRadarDataRecord rec = new MpeRadarDataRecord(numRangeBins, buf);
            data.add(rec);
        }
    }

    /**
     * @param buf
     * @throws InvalidMpeRadarException
     */
    protected void readPsmParams(final ByteBuffer buf)
            throws InvalidMpeRadarException {
        int numParams = getParametersCount(buf, PSM_PATTERN, "PSM");
        this.psmParams = new float[numParams];
        readParams(buf, psmParams, "PSM");
    }

    /**
     * @param buf
     * @throws InvalidMpeRadarException
     */
    private void readAdaptableParams(final ByteBuffer buf)
            throws InvalidMpeRadarException {
        int numParams = getParametersCount(buf, ADAP_PATTERN, "adaptable");
        // the first N-1 parameters are floats expressed as 8 characters.
        this.adapParams = new float[numParams - 1];
        readParams(buf, this.adapParams, "Adaptable");
        this.biasApplied = readNextParamString(buf);
    }

    /**
     * @param buf
     * @param numParams
     * @throws InvalidMpeRadarException
     */
    private void readParams(final ByteBuffer buf, float[] paramArray,
            String parameterType) throws InvalidMpeRadarException {
        String paramStr;
        for (int i = 0; i < paramArray.length; i++) {
            paramStr = readNextParamString(buf);
            if (paramStr.isEmpty()) {
                paramArray[i] = EMPTY_PARAM;
            } else {
                try {
                    paramArray[i] = Float.parseFloat(paramStr);
                } catch (NumberFormatException e) {
                    throw new InvalidMpeRadarException(
                            parameterType + " parameter at index " + i
                                    + " is malformed. Value is " + paramStr);
                }
            }
        }
    }

    /**
     * Reads {@link #PARAM_LEN} bytes, interprets them as an ASCII String, and
     * returns the trimmed string.
     *
     * @return The next {@link #PARAM_LEN} bytes interpreted as an ASCII string.
     */
    private String readNextParamString(final ByteBuffer buf) {
        byte[] paramBytes = new byte[PARAM_LEN];
        buf.get(paramBytes);
        return new String(paramBytes, StandardCharsets.US_ASCII).trim();
    }

    /**
     * Finds the parameters sub-layer and returns the number of parameters. The
     * buffer will be positioned at the start of the first parameter.
     *
     * @param buf
     * @throws InvalidMpeRadarException
     */
    protected int getParametersCount(final ByteBuffer buf,
            Pattern searchPattern, String parameterType)
            throws InvalidMpeRadarException {
        char firstChar = searchPattern.toString().charAt(0);
        byte[] checkBytes = new byte[PARAM_LEN];
        buf.get(checkBytes);
        String checkStr = new String(checkBytes, StandardCharsets.US_ASCII);

        Matcher m;
        while (!(m = searchPattern.matcher(checkStr)).matches()
                && buf.remaining() >= PARAM_LEN) {
            int firstPos = checkStr.indexOf(firstChar, 1);
            if (firstPos > 0) {
                buf.position(buf.position() - PARAM_LEN + firstPos);
            }
            buf.get(checkBytes);
            checkStr = new String(checkBytes, StandardCharsets.US_ASCII);
        }
        if (!m.matches()) {
            throw new InvalidMpeRadarException(
                    "Unable to read the " + parameterType + " parameters.");
        }
        return Integer.parseInt(m.group(1).trim());
    }

    /**
     * @return the data
     */
    public List<MpeRadarDataRecord> getData() {
        return data;
    }

    /**
     * Get the precip status message parameters.
     *
     * @return the adapParams
     */
    public float[] getPsmParams() {
        return psmParams;
    }

    /**
     * Get the adaptable parameters.
     *
     * @return the adapParams
     */
    public float[] getAdapParams() {
        return adapParams;
    }

    /**
     * @return the biasApplied
     */
    public boolean isBiasApplied() {
        return PARAM_VALUE_TRUE.equals(biasApplied);
    }

    /**
     * @return the biasApplied
     */
    public String getBiasApplied() {
        return biasApplied;
    }
}
