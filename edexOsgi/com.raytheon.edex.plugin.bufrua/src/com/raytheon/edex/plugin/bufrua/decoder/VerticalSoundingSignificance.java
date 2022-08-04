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
package com.raytheon.edex.plugin.bufrua.decoder;

import java.util.EnumSet;

/**
 * Adapted from the WMO FM 94 BUFR code and flag tables. These are the values
 * for BUFR descriptor 0 08 001. Each bit in bufr value has a specific meaning.
 * The recommended use of this class is to use {@link #decode(int)} to map the
 * bufr value into a set of significances.
 * 
 * <pre>
 * 
 * SOFTWARE HISTORY
 * 
 * Date          Ticket#  Engineer  Description
 * ------------- -------- --------- -----------------
 * Jul 06, 2016  5736     bsteffen  Initial creation
 * 
 * </pre>
 * 
 * @author bsteffen
 */
public enum VerticalSoundingSignificance {

    SURFACE(1, "Surface"),

    STANDARD(2, "Standard level"),

    TROPOPAUSE(3, "Tropopause level"),

    MAX_WIND(4, "Maximum wind level"),

    SIG_TEMP(5, "Significant level, temperature and/or relative humidity"),

    SIG_WIND(6, "Significant level, wind"),

    /*
     * The documentation does not mention what the 7th bit means. Assigning it
     * to reserved ensures it will be decoded instead of ignored. It is never
     * expected that this will be set.
     */
    RESERVED(7, "<Reserved>");

    private final String description;

    private final int mask;

    private VerticalSoundingSignificance(int bitNumber, String description) {
        this.description = description;
        /*
         * The bitNumber is directly from the WMO tables which number the bits
         * starting at the MSB with bit 1. There are a total of 7 bits in this
         * field.
         */
        this.mask = 1 << (7 - bitNumber);
    }

    @Override
    public String toString() {
        return description;
    }

    public static EnumSet<VerticalSoundingSignificance> decode(int sig) {
        EnumSet<VerticalSoundingSignificance> result = EnumSet
                .noneOf(VerticalSoundingSignificance.class);
        for (VerticalSoundingSignificance test : values()) {
            if ((test.mask & sig) != 0) {
                result.add(test);
            }
        }
        return result;
    }
}
