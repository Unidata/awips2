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
 * for BUFR descriptor 0 08 042. Each bit in bufr value has a specific meaning.
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
public enum ExtendedVerticalSoundingSignificance {

    SURFACE(1, "Surface"),

    STANDARD(2, "Standard level"),

    TROPOPAUSE(3, "Tropopause level"),

    MAX_WIND(4, "Maximum wind level"),

    SIG_TEMP(5, "Significant temperature level"),

    SIG_HUM(6, "Significant humidity level"),

    SIG_WIND(7, "Significant wind level"),

    BEGIN_MISSING_TEMP(8, "Beginning of missing temperature data"),

    END_MISSING_TEMP(9, "End of missing temperature data"),

    BEGIN_MISSING_HUM(10, "Beginning of missing humidity data"),

    END_MISSING_HUM(11, "End of missing humidity data"),

    BEGIN_MISSING_WIND(12, "Beginning of missing wind data"),

    END_MISSING_WIND(13, "End of missing wind data"),

    TOP_WIND(14, "Top of wind sounding"),

    LOCAL(15, "Level determined by regional decision"),

    FREEZING(16, "Freezing level"),

    PRESSURE_HEIGHT(17,
            "Pressure level originally indicated by height as the vertical coordinate");

    private final String description;

    private final int mask;

    private ExtendedVerticalSoundingSignificance(int bitNumber, String description) {
        this.description = description;
        /*
         * The bitNumber is directly from the WMO tables which number the bits
         * starting at the MSB with bit 1. There are a total of 18 bits in this
         * field.
         */
        this.mask = 1 << (18 - bitNumber);
    }

    @Override
    public String toString() {
        return description;
    }

    public static EnumSet<ExtendedVerticalSoundingSignificance> decode(int sig) {
        EnumSet<ExtendedVerticalSoundingSignificance> result = EnumSet
                .noneOf(ExtendedVerticalSoundingSignificance.class);
        for (ExtendedVerticalSoundingSignificance test : values()) {
            if ((test.mask & sig) != 0) {
                result.add(test);
            }
        }
        return result;
    }
}
