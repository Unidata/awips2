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
package com.raytheon.viz.hydrocommon.pdc.data;

import java.text.ParseException;
import java.util.HashMap;
import java.util.Map;

/**
 * Point Data Control Preset Data Object.
 * 
 * <pre>
 * 
 * SOFTWARE HISTORY
 * 
 * Date         Ticket#    Engineer    Description
 * ------------ ---------- ----------- --------------------------
 * Oct 23, 2008 1644       mpduff      Initial creation
 * May 03, 2016 5623       bkowal      Added {@link #toString()}.
 * Sep 21, 2018 7379       mduff       Support PDC Refactor.
 * 
 * </pre>
 * 
 * @author mpduff
 */

public class PointDataPreset {
    /** Preset ID */
    private String presetId = null;

    /** Preset description */
    private String description = null;

    /** Preset Ranking */
    private int presetRank;

    /** Preset String */
    private String presetString = null;

    /**
     * @return the presetId
     */
    public String getPresetId() {
        return presetId;
    }

    /**
     * @param presetId
     *            the presetId to set
     */
    public void setPresetId(String presetId) {
        this.presetId = presetId;
    }

    /**
     * @return the description
     */
    public String getDescription() {
        return description;
    }

    /**
     * @param description
     *            the description to set
     */
    public void setDescription(String description) {
        this.description = description;
    }

    /**
     * @return the presetRank
     */
    public int getPresetRank() {
        return presetRank;
    }

    /**
     * @param presetRank
     *            the presetRank to set
     */
    public void setPresetRank(int presetRank) {
        this.presetRank = presetRank;
    }

    /**
     * @return the presetString
     */
    public String getPresetString() {
        return presetString;
    }

    /**
     * @param presetString
     *            the presetString to set
     */
    public void setPresetString(String presetString) {
        this.presetString = presetString;
    }

    public Map<String, String[]> getParsedPresetData() throws ParseException {
        return tokenizePresetString(this.presetString);
    }

    private Map<String, String[]> tokenizePresetString(String presets)
            throws ParseException {
        if ((presets == null) || (presets.isEmpty())) {
            throw new ParseException("Empty preset String", 0);
        }

        Map<String, String[]> presetData = new HashMap<>();
        String[] parts = presets.split(";");

        for (String part : parts) {
            String[] pairs = part.split("=");
            String[] values = null;

            if (pairs.length > 1) {
                if (pairs[1].contains(",")) {
                    values = pairs[1].split(",");
                } else {
                    values = new String[] { pairs[1] };
                }

                presetData.put(pairs[0], values);
            }
        }

        return presetData;
    }

    @Override
    public String toString() {
        StringBuilder sb = new StringBuilder("PointDataPreset [");
        sb.append("presetId=").append(this.presetId);
        sb.append(", description=").append(this.description);
        sb.append(", presetRank=").append(this.presetRank);
        sb.append(", presetString=").append(this.presetString);
        sb.append("]");
        return sb.toString();
    }
}
