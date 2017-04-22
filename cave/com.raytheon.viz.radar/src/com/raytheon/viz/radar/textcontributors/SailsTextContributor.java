package com.raytheon.viz.radar.textcontributors;

import com.raytheon.uf.common.dataplugin.radar.RadarRecord;

/**
 *
 * Display "SAILS" at upper text for SAILS products
 *
 * <pre>
 *
 * SOFTWARE HISTORY
 * Date         Ticket#    Engineer    Description
 * ------------ ---------- ----------- --------------------------
 * 07/08/2014   DR17356    zwang       Initial creation
 *
 * </pre>
 *
 * @author zwang
 * @version 1.0
 */

public class SailsTextContributor implements IRadarTextContributor {

    @Override
    public String contributeText(RadarRecord record) {
        String sailsStr = "";

        if (record.getPrimaryElevationAngle() <= 0.5 && record.getElevationNumber() > 2)
            sailsStr = "SAILS";

        return sailsStr;
    }
}
