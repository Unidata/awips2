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
package com.raytheon.viz.mpe.ui.actions;

/**
 * Basic POJO to keep track of legend override details that are generated as a
 * result of user interaction with the legend. This is only an initial
 * implementation created to replace the previously used static variables.
 * 
 * <pre>
 *
 * SOFTWARE HISTORY
 *
 * Date         Ticket#    Engineer    Description
 * ------------ ---------- ----------- --------------------------
 * Feb 27, 2017 6157       bkowal      Initial creation
 *
 * </pre>
 *
 * @author bkowal
 */

public class MPELegendOverride {

    public static enum OverrideType {
        UP, DOWN, OFF
    }

    private final OverrideType overrideType;

    private final Double overrideIndex;

    public MPELegendOverride() {
        this(OverrideType.OFF, null);
    }

    public MPELegendOverride(final OverrideType overrideType,
            final Double overrideIndex) {
        if (overrideType == null) {
            throw new IllegalArgumentException(
                    "Required argument 'overrideType' cannot be NULL.");
        }
        if (overrideIndex == null && overrideType != OverrideType.OFF) {
            throw new IllegalArgumentException(
                    "Required argument 'overrideIndex' cannot be NULL when overrides are not "
                            + OverrideType.OFF.name() + ".");
        }
        this.overrideType = overrideType;
        this.overrideIndex = overrideIndex;
    }

    public OverrideType getOverrideType() {
        return overrideType;
    }

    public Double getOverrideIndex() {
        return overrideIndex;
    }
}