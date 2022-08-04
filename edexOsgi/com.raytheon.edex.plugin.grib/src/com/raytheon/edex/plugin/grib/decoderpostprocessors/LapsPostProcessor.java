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

import com.raytheon.edex.plugin.grib.exception.GribException;
import com.raytheon.uf.common.dataplugin.grid.GridRecord;
import com.raytheon.uf.common.dataplugin.level.Level;
import com.raytheon.uf.common.dataplugin.level.LevelFactory;
import com.raytheon.uf.common.parameter.Parameter;

/**
 * Adjusts the LAPS Grib data to have to correct level information
 *
 * <pre>
 *
 * SOFTWARE HISTORY
 *
 * Date          Ticket#  Engineer    Description
 * ------------- -------- ----------- --------------------------
 * Apr 07, 2011  6619     bphillip    Initial creation
 * Oct 15, 2013  2473     bsteffen    Remove deprecated method calls.
 * Sep 09, 2014  3356     njensen     Remove CommunicationException
 * Oct 07, 2015  3756     nabowle     Extends DecoderPostProcessor.
 *
 * </pre>
 *
 * @author bphillip
 * @version 1.0
 */
public class LapsPostProcessor extends DecoderPostProcessor {

    private static final String FHAG = "FHAG";

    private static final String SFC = "SFC";

    private static final String PMSL = "PMSL";

    private static final String MSLP = "MSLP";

    @Override
    public GridRecord[] process(GridRecord record) throws GribException {

        String levelName = record.getLevel().getMasterLevel().getName();
        boolean modelInfoModified = false;
        if (levelName.equals(FHAG)) {
            Level sfcLevel = LevelFactory.getInstance().getLevel(SFC, 0);
            record.setLevel(sfcLevel);
            modelInfoModified = true;
        }

        if (record.getParameter().getAbbreviation().equals(PMSL)) {
            Parameter param = new Parameter(MSLP, "Mean Sea Level Pressure",
                    record.getParameter().getUnit());
            record.setParameter(param);
            modelInfoModified = true;
        }

        if (modelInfoModified) {
            record.getInfo().setId(null);
            record.setDataURI(null);
        }

        record.setOverwriteAllowed(true);
        return new GridRecord[] { record };
    }
}
