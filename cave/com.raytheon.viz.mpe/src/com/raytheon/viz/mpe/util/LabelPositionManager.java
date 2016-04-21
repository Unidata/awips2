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

import java.io.BufferedReader;
import java.io.FileReader;
import java.io.IOException;
import java.util.HashMap;
import java.util.Map;

import com.raytheon.viz.mpe.util.DailyQcUtils.Station;

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

public class LabelPositionManager {

    private Map<String, LabelPosition> labelPositionsMap = new HashMap<String, LabelPosition>();

    public void writeLabelPositions(
            Map<String, LabelPosition> labelPositionsMap, String labelPathName)
    // throws IOException
    {
        ;
    }

    public String getKey(LabelPosition position) {
        return (position.stationID + position.parameterCode);
    }

    public String getKey(Station station) {
        return (station.hb5 + station.parm);
    }

    public LabelPosition getLabelPosition(Station station) {
        return labelPositionsMap.get(getKey(station));
    }

    public void setLabelPosition(Station station) {
        LabelPosition myLabelPosition = new LabelPosition();
        myLabelPosition = labelPositionsMap.get(getKey(station));

        if (myLabelPosition != null) {
            station.xadd = myLabelPosition.xOffset;
            station.yadd = myLabelPosition.yOffset;
        } else {
            station.xadd = 0;
            station.yadd = 0;
        }

    } /* setLabelPosition */

    public void readLabelPositions(FileReader labelPositionFileReader)
            throws IOException {
        String inputLine; // one line of input
        BufferedReader inputReader = null;

        String[] commentTokens; // tokens broken into input and comments
        String[] labelPositionTokens; // tokens from input (label posn data)

        inputReader = new BufferedReader(labelPositionFileReader);

        inputLine = inputReader.readLine();

        while (inputLine != null) {
            labelPositionTokens = null;

            if (inputLine.charAt(0) != '#') // not a comment
            {
                // Strip away anything after a '!' (in-line comment)
                // character
                commentTokens = inputLine.split("!");
                labelPositionTokens = commentTokens[0].split("\\s+", 4);
            }

            if ((labelPositionTokens != null) && // If we got input and
                    (labelPositionTokens.length == 4) && // input is valid and
                    (inputLine.charAt(0) != '#')) // not a comment
            {
                LabelPosition position = new LabelPosition(
                        labelPositionTokens[0], labelPositionTokens[1],
                        Integer.parseInt(labelPositionTokens[2]),
                        Integer.parseInt(labelPositionTokens[3]));

                // If this isn't a default value, we'll store it
                if (position.xOffset != 0 || position.yOffset != 0) {
                    labelPositionsMap.put(getKey(position), position);
                }

            } /* if we got valid, non-commented input */

            inputLine = inputReader.readLine();

        } /* while (line != null) */

    } /* readLabelPositions */

} /* class LabelPositionManager */
