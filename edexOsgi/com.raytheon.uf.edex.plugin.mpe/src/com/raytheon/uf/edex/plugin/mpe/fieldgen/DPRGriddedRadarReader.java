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
package com.raytheon.uf.edex.plugin.mpe.fieldgen;

import java.nio.file.Path;

import com.raytheon.uf.common.dataplugin.shef.tables.DPRRadar;
import com.raytheon.uf.edex.plugin.mpe.dao.impl.DPRRadarDao;
import com.raytheon.uf.edex.plugin.mpe.fieldgen.hpe.HPERunConfiguration;

/**
 * Implementation of the Gridded Radar data reader for DPR radar.
 * 
 * <pre>
 *
 * SOFTWARE HISTORY
 *
 * Date         Ticket#    Engineer    Description
 * ------------ ---------- ----------- --------------------------
 * Sep 27, 2016 5631       bkowal      Initial creation
 *
 * </pre>
 *
 * @author bkowal
 */

public class DPRGriddedRadarReader
        extends AbstractGriddedRadarReader<DPRRadar> {

    public DPRGriddedRadarReader(Path decodedRadarPath,
            HPERunConfiguration runConfig) {
        super(new DPRRadarDao(), decodedRadarPath, runConfig);
    }
}