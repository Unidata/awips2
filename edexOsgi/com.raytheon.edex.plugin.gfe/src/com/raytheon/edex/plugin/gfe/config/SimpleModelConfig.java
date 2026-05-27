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
package com.raytheon.edex.plugin.gfe.config;

import java.util.ArrayList;
import java.util.List;

/**
 * Simple version of the model configuration for use in python.
 * 
 * <pre>
 * SOFTWARE HISTORY
 * Date         Ticket#    Engineer    Description
 * ------------ ---------- ----------- --------------------------
 * 03/11/08     #1030      randerso    Initial port 
 * 
 * </pre>
 * 
 * @author randerso
 * @version 1.0
 */

public class SimpleModelConfig {
    public String siteID;

    public String format;

    public String type;

    public String modelName;

    public String projectionID;

    public boolean singleton;

    public boolean official;

    public int numVersions;

    public int gridPurgeAge;

    public List<SimpleGridParmConfig> grids;

    public SimpleModelConfig() {
        singleton = official = false;
        numVersions = gridPurgeAge = 0;
    }

    public SimpleModelConfig(String site, String format, String type,
            String name, String projID, boolean single, boolean official,
            int numVer, int purgeAge) {
        this.siteID = site;
        this.format = format;
        this.type = type;
        this.modelName = name;
        this.projectionID = projID;
        this.modelName = name;
        this.singleton = single;
        this.official = official;
        this.numVersions = numVer;
        this.gridPurgeAge = purgeAge;
        this.grids = new ArrayList<SimpleGridParmConfig>();
    }

}
