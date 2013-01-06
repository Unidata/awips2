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
package com.raytheon.edex.plugin.grib.decoderpostprocessors;

import java.util.HashMap;
import java.util.Map;

import com.raytheon.edex.plugin.grib.exception.GribException;
import com.raytheon.uf.common.dataplugin.grid.GridRecord;
import com.raytheon.uf.common.parameter.Parameter;

/**
 * TODO Add Description
 * 
 * <pre>
 * 
 * SOFTWARE HISTORY
 * 
 * Date         Ticket#    Engineer    Description
 * ------------ ---------- ----------- --------------------------
 * Sep 7, 2011            bphillip     Initial creation
 * 
 * </pre>
 * 
 * @author bphillip
 * @version 1.0
 */

public class TPCSurgeProbPostProcessor implements IDecoderPostProcessor {

    private static final String PROB_NAME = "Probabilistic Storm Surge probability > %1 feet";

    private static final Map<String, String> TPCSG_MAP = new HashMap<String, String>();

    static {
        TPCSG_MAP.put("2", PROB_NAME.replace("%1", "2"));
        TPCSG_MAP.put("3", PROB_NAME.replace("%1", "3"));
        TPCSG_MAP.put("4", PROB_NAME.replace("%1", "4"));
        TPCSG_MAP.put("5", PROB_NAME.replace("%1", "5"));
        TPCSG_MAP.put("6", PROB_NAME.replace("%1", "6"));
        TPCSG_MAP.put("7", PROB_NAME.replace("%1", "7"));
        TPCSG_MAP.put("8", PROB_NAME.replace("%1", "8"));
        TPCSG_MAP.put("9", PROB_NAME.replace("%1", "9"));
        TPCSG_MAP.put("10", PROB_NAME.replace("%1", "10"));
        TPCSG_MAP.put("11", PROB_NAME.replace("%1", "11"));
        TPCSG_MAP.put("12", PROB_NAME.replace("%1", "12"));
        TPCSG_MAP.put("13", PROB_NAME.replace("%1", "13"));
        TPCSG_MAP.put("14", PROB_NAME.replace("%1", "14"));
        TPCSG_MAP.put("15", PROB_NAME.replace("%1", "15"));
        TPCSG_MAP.put("16", PROB_NAME.replace("%1", "16"));
        TPCSG_MAP.put("17", PROB_NAME.replace("%1", "17"));
        TPCSG_MAP.put("18", PROB_NAME.replace("%1", "18"));
        TPCSG_MAP.put("19", PROB_NAME.replace("%1", "19"));
        TPCSG_MAP.put("20", PROB_NAME.replace("%1", "20"));
        TPCSG_MAP.put("21", PROB_NAME.replace("%1", "21"));
        TPCSG_MAP.put("22", PROB_NAME.replace("%1", "22"));
        TPCSG_MAP.put("23", PROB_NAME.replace("%1", "23"));
        TPCSG_MAP.put("24", PROB_NAME.replace("%1", "24"));
        TPCSG_MAP.put("25", PROB_NAME.replace("%1", "25"));
    }

    /*
     * (non-Javadoc)
     * 
     * @see
     * com.raytheon.edex.plugin.grib.decoderpostprocessors.IDecoderPostProcessor
     * #process(com.raytheon.uf.common.dataplugin.grib.GridRecord)
     */
    @Override
    public GridRecord[] process(GridRecord record) throws GribException {
        boolean modified = false;
        Parameter param = record.getParameter();
        if (param.getAbbreviation().startsWith("PSurge")) {
            String surge = param.getAbbreviation().toLowerCase()
                    .replace("psurge", "").replace("ft", "");
            param = new Parameter(param.getAbbreviation(),
                    TPCSG_MAP.get(surge), param.getUnitString());
            modified = true;
        } else if (param.getAbbreviation().equals("TPCSG_SLOSH")) {
            param = new Parameter(param.getAbbreviation(),
                    "Real Time Slosh MEOW", param.getUnitString());
            modified = true;
        } else if (param.getAbbreviation().startsWith("TPCSG-")) {
            String abbr = param.getAbbreviation().substring(0, 8)
                    .replace("-", "_");
            param = new Parameter(abbr, param.getName(), param.getUnitString());
            modified = true;
        } else if (param.getAbbreviation().endsWith("10Pct")) {
            String abbr = param.getAbbreviation().replace("10Pct", "");
            param = new Parameter(abbr, param.getName(), param.getUnitString());
            modified = true;
        }
        if (!modified) {
            return new GridRecord[] { record };
        }

        record.setParameter(param);
        record.getInfo().setId(null);
        record.setDataURI(null);
        try {
            record.constructDataURI();
        } catch (Exception e) {
            throw new GribException(
                    "Error creating new dataURI for TPCSurgeProb data!", e);
        }

        record.setOverwriteAllowed(true);
        return new GridRecord[] { record };
    }
}
