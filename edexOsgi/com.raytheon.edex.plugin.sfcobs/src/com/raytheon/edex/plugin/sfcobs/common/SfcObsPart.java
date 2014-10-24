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
package com.raytheon.edex.plugin.sfcobs.common;

import java.util.HashMap;
import java.util.Map;

/**
 * The SfcObsPart class wraps the string value of parts parsed from a text
 * observation. In addition various predefined parts and convenience methods are
 * defined.
 * 
 * <pre>
 * 
 * SOFTWARE HISTORY
 * 
 * Date          Ticket#  Engineer    Description
 * ------------- -------- ----------- --------------------------
 * Sep 25, 2007  391      jkorman     Initial Coding.
 * Oct 29, 2013  2489     bsteffen    Add NAVTEX_END_PART
 * Sep 26, 2014  3629     mapeters    Removed static imports.
 * 
 * </pre>
 * 
 * @author jkorman
 * @version 1.0
 */
public class SfcObsPart {

    public static final SfcObsPart CR_PART = new SfcObsPart("CARRIAGECONTROL",
            false);

    public static final SfcObsPart RE_PART = new SfcObsPart("RPT_END", false);

    public static final SfcObsPart MS_PART = new SfcObsPart("MSG_START", false);

    public static final SfcObsPart ME_PART = new SfcObsPart("MSG_END", false);

    public static final SfcObsPart AAXX_PART = new SfcObsPart("AAXX",false);

    public static final SfcObsPart BBXX_PART = new SfcObsPart("BBXX", true);

    public static final SfcObsPart OOXX_PART = new SfcObsPart("OOXX",true);

    public static final SfcObsPart ZZYY_PART = new SfcObsPart("ZZYY",true);

    public static final SfcObsPart CMAN_PART = new SfcObsPart("CMAN",false);

    public static final SfcObsPart METAR_PART = new SfcObsPart("METAR",false);

    public static final SfcObsPart SPECI_PART = new SfcObsPart("SPECI",false);
    
    public static final SfcObsPart MAROB_PART = new SfcObsPart("MAROB",true);

    public static final SfcObsPart NAVTEX_END_PART = new SfcObsPart("NNNN",
            true);

    private static final Map<String, SfcObsPart> OBS_TYPE_MAP = new HashMap<String, SfcObsPart>();
    static {
        OBS_TYPE_MAP.put(AAXX_PART.partValue, AAXX_PART);
        OBS_TYPE_MAP.put(BBXX_PART.partValue, BBXX_PART);
        OBS_TYPE_MAP.put(OOXX_PART.partValue, OOXX_PART);
        OBS_TYPE_MAP.put(ZZYY_PART.partValue, ZZYY_PART);
        OBS_TYPE_MAP.put(CMAN_PART.partValue, CMAN_PART);
        OBS_TYPE_MAP.put(METAR_PART.partValue, METAR_PART);
        OBS_TYPE_MAP.put(SPECI_PART.partValue, SPECI_PART);
        OBS_TYPE_MAP.put(MAROB_PART.partValue, MAROB_PART);
    }

    private final int intHashCode;

    private final String partValue;
    
    private final boolean startNew;

    /**
     * Construct a new Observation part with a given value.
     * 
     * @param value
     *            The value to give this part.
     */
    public SfcObsPart(String value, boolean startNew) {
        partValue = value;
        this.startNew = startNew;
        intHashCode = 31 + ((partValue == null) ? 0 : partValue.hashCode());
    }

    /**
     * Calculate the hash code for this instance.
     * 
     * @return The calculated hash code.
     */
    @Override
    public int hashCode() {
        return intHashCode;
    }

    /**
     * Is this instance equal to another SynopticPart instance?
     * 
     * @return Is this instance equal to another SynopticPart instance?
     */
    @Override
    public boolean equals(Object obj) {
        if (this == obj) {
            return true;
        }
        if (obj == null) {
            return false;
        }
        if (getClass() != obj.getClass()) {
            return false;
        }
        final SfcObsPart other = (SfcObsPart) obj;
        if (partValue == null) {
            if (other.partValue != null) {
                return false;
            }
        } else if (!partValue.equals(other.partValue)) {
            return false;
        }
        return true;
    }

    /**
     * Add the string representation for this part into a Stringbuilder
     * instance.
     * 
     * @param builder
     *            The instance to receive the string. If passed a null value a
     *            new StringBuilder instance is created.
     * @return The StringBuilder instance that received the string.
     */
    public StringBuilder toString(StringBuilder builder) {
        if (builder == null) {
            builder = new StringBuilder();
        }
        builder.append(partValue);
        return builder;
    }

    /**
     * Get the string representation for this part.
     * 
     * @return The string representation for this part.
     */
    public String toString() {
        return toString(null).toString();
    }

    /**
     * Get the value for this part.
     * 
     * @return The part value.
     */
    public String getPartValue() {
        return partValue;
    }
    
    /**
     * Should a new sub-message be started?
     * @return Should a new sub-message be started?
     */
    public boolean isStartNew() {
        return startNew;
    }

    /**
     * Retrieve a predefined observation part by its name.
     * 
     * @param partName
     *            The name of the observation part.
     * @return The part instance or null if not found.
     */
    public static SfcObsPart partFactory(String partName) {
        return OBS_TYPE_MAP.get(partName);
    }

}
