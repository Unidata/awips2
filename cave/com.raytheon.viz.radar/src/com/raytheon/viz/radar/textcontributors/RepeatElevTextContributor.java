package com.raytheon.viz.radar.textcontributors;

import java.util.ArrayList;
import java.util.Arrays;
import java.util.List;

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
 * 06/13/2018   DCS20774   jdynina      Account for supplemental elevation flag
 *                                      bit splitting in RPG Build 19 where
 *                                      only bits 0-1 are now used to distinguish
 *                                      SAILS/MRLE.
 * 06/24/2019   DCS21371   jdynina      Correctly handle minivolume scan labeling
 *                                      for VCP80.
 *
 * </pre>
 *
 * @author jdynina
 * @version 1.0
 */

public class RepeatElevTextContributor implements IRadarTextContributor {

    // repeat elevation numbers in TDWR Bld 2
    private final List<Integer> TDWR_REPEAT_ELEVATIONS = new ArrayList<>(
            Arrays.asList(6, 10, 13, 14, 15, 16, 17, 18, 19, 20, 21, 22, 23));

    @Override
    public String contributeText(RadarRecord record) {
        String elevStr = "";

        if (record.getProductDependentValues() == null) {
            return "Loading";
        }

        if ((record.getVolumeCoveragePattern() != 80) &&
                (record.getVolumeCoveragePattern() != 90)) {  // RPG's
            short[] productDependent = record.getProductDependentValues();
            if (productDependent[6] != 0) {  // RPG Build 19 - parse out MRLE/SAILS flag bits
                int supFlag = productDependent[6] & 0b11;
                if (supFlag == 1) {
                    elevStr = "MRLE";
                } else if (supFlag ==2) {
                    elevStr = "SAILS";
                }
            } else if (productDependent[6] == 0 &&   // Build versions prior to RPG Bld 18 - no MRLE, only SAILS
                    (record.getPrimaryElevationAngle() <= 0.5 &&
                    record.getElevationNumber() > 2)) {
                elevStr = "SAILS";
            }
        } else if (record.getVolumeCoveragePattern() == 80) {  // SPG VCP80
            if (TDWR_REPEAT_ELEVATIONS.contains(record.getElevationNumber())) {
                elevStr = "REPEAT";
            }
        }

        return elevStr;
    }
}
