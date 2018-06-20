package com.raytheon.viz.radar.textcontributors;

import com.raytheon.uf.common.dataplugin.radar.RadarRecord;

/**
 *
 * Display "MRLE/SAILS/REPEAT" at upper text for repeat elev scans
 *
 * <pre>
 *
 * SOFTWARE HISTORY
 * Date         Ticket#    Engineer    Description
 * ------------ ---------- ----------- --------------------------
 * 05/02/2017   DCS20055   jdynina      Initial creation
 *
 * </pre>
 *
 * @author jdynina
 * @version 1.0
 */

public class RepeatElevTextContributor implements IRadarTextContributor {

    @Override
    public String contributeText(RadarRecord record) {
        String elevStr = "";

        if (record.getProductDependentValues() == null) {
            return "Loading";
        }

        if ((record.getVolumeCoveragePattern() != 80) &&
                (record.getVolumeCoveragePattern() != 90)) {  // RPG's
            short[] productDependent = record.getProductDependentValues();
            if (productDependent[6] == 1) {
                elevStr = "MRLE";
            } else if (productDependent[6] == 2) {
                elevStr = "SAILS";
            } else if (productDependent[6] == 0 &&   // Build versions prior to RPG Bld 18 - no MRLE, only SAILS
                    (record.getPrimaryElevationAngle() <= 0.5 &&
                    record.getElevationNumber() > 2)) {
                elevStr = "SAILS";
            }
        } else if (record.getVolumeCoveragePattern() == 80) {  // SPG VCP80
            if ((record.getPrimaryElevationAngle() <= 0.8 && record.getElevationNumber() > 2) ||
                    (record.getPrimaryElevationAngle() == 1 && record.getElevationNumber() > 3)) {  // account for SPG Bld 2
                elevStr = "REPEAT";
            }
        }

        return elevStr;
    }
}
