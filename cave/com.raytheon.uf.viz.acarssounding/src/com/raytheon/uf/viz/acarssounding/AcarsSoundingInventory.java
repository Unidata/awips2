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
package com.raytheon.uf.viz.acarssounding;

import java.util.Arrays;
import java.util.List;

import com.raytheon.uf.viz.core.exception.VizException;
import com.raytheon.viz.pointdata.util.AbstractPointDataInventory;

/**
 * TODO Add Description
 * 
 * <pre>
 * 
 * SOFTWARE HISTORY
 * Date         Ticket#    Engineer    Description
 * ------------ ---------- ----------- --------------------------
 * Apr 13, 2010            bsteffen     Initial creation
 * 
 * </pre>
 * 
 * @author bsteffen
 * @version 1.0
 */

public class AcarsSoundingInventory extends AbstractPointDataInventory {

    protected static final String REFTIME = "refTime";

    protected static final String FORECASTHR = "forecastHr";

    protected static final String NUMLEVELS = "numLevels";

    protected static final String STATIONID = "stationId";

    protected static final String DATAURI = "dataURI";

    protected static final String TAILNUMBER = "tailNumber";

    protected static final String ALTITUDE = "altitude";

    protected static final String TEMPERATURE = "T";

    protected static final String RELHUM = "RH";

    protected static final String MIXRATIO = "mixRat";

    protected static final String WINDDIR = "WD";

    protected static final String WINDSPD = "wSp";

    protected static final String DEWPOINT = "DpT";

    protected static final String PRESSURE = "P";

    protected static final String[] BASE_PARAMS = { REFTIME, FORECASTHR,
            NUMLEVELS, STATIONID, DATAURI, TAILNUMBER, ALTITUDE, TEMPERATURE,
            RELHUM, MIXRATIO, WINDDIR, WINDSPD, DEWPOINT, PRESSURE };

    /**
     * @param plugins
     */
    public AcarsSoundingInventory() {
        super(Arrays.asList("acarssounding"));
    }

    protected List<String> getBaseParams(String pluginName, String type)
            throws VizException {
        return Arrays.asList(BASE_PARAMS);
    }

}
