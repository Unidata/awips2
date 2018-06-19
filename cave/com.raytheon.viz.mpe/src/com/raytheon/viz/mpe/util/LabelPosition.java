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
package com.raytheon.viz.mpe.util;

/**
 * TODO Add Description
 * 
 * <pre>
 * 
 * SOFTWARE HISTORY
 * 
 * Date         Ticket#    Engineer    Description
 * ------------ ---------- ----------- --------------------------
 * Jul 7, 2011            snaples     Initial creation
 * 
 * </pre>
 * 
 * @author snaples
 * @version 1.0
 */

public class LabelPosition {
    String stationID;

    String parameterCode;

    int xOffset, yOffset;

    public LabelPosition() {
        stationID = "";
        parameterCode = "";
        xOffset = -99;
        yOffset = -99;
    }

    public LabelPosition(String stationID, String parameterCode, int xOffset,
            int yOffset) {
        this.stationID = stationID;
        this.parameterCode = parameterCode;
        this.xOffset = xOffset;
        this.yOffset = yOffset;
    }

    @Override
    public String toString() {
        String output;

        output = stationID + ", " + parameterCode + ", " + xOffset + ", "
                + yOffset;
        return (output);
    }

} /* class LabelPosition */
