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

import com.raytheon.edex.plugin.grib.util.GribModelCache;
import com.raytheon.uf.common.dataplugin.grib.GribModel;
import com.raytheon.uf.common.dataplugin.grib.GribRecord;
import com.raytheon.uf.common.dataplugin.grib.exception.GribException;
import com.raytheon.uf.common.dataplugin.level.Level;
import com.raytheon.uf.common.dataplugin.level.LevelFactory;
import com.raytheon.uf.edex.database.DataAccessLayerException;

/**
 * Adjusts the LAPS Grib data to have to correct level information
 * 
 * <pre>
 * 
 * SOFTWARE HISTORY
 * 
 * Date         Ticket#    Engineer    Description
 * ------------ ---------- ----------- --------------------------
 * 4/7/2011     #6619      bphillip     Initial creation
 * 
 * </pre>
 * 
 * @author bphillip
 * @version 1.0
 */
public class LapsPostProcessor implements IDecoderPostProcessor {

    private static final String FHAG = "FHAG";

    private static final String SFC = "SFC";

    private static final String PMSL = "PMSL";

    private static final String MSLP = "MSLP";

    @Override
    public GribRecord[] process(GribRecord record) throws GribException {

        String levelName = record.getModelInfo().getLevel().getMasterLevel()
                .getName();
        GribModel gribModel = record.getModelInfo();
        boolean modelInfoModified = false;
        if (levelName.equals(FHAG)) {
            Level sfcLevel = LevelFactory.getInstance().getLevel(SFC, 0);
            gribModel.setLevel(sfcLevel);
            modelInfoModified = true;
        }

        if (gribModel.getParameterAbbreviation().equals(PMSL)) {
            gribModel.setParameterAbbreviation(MSLP);
            modelInfoModified = true;
        }

        if (modelInfoModified) {
            try {
                gribModel.generateId();
                GribModel cachedModel = GribModelCache.getInstance().getModel(
                        gribModel);
                record.setModelInfo(cachedModel);
                record.setDataURI(null);
                try {
                    record.constructDataURI();
                } catch (Exception e) {
                    throw new GribException(
                            "Error creating new dataURI for LAPS data!", e);
                }
            } catch (DataAccessLayerException e) {
                throw new GribException("Error modifying LAPS levels!", e);
            }
        }

        record.setOverwriteAllowed(true);
        return new GribRecord[] { record };
    }
}
