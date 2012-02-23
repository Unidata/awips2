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

import com.raytheon.edex.plugin.grib.dao.GribModelDao;
import com.raytheon.edex.plugin.grib.util.GribModelCache;
import com.raytheon.uf.common.dataplugin.grib.GribModel;
import com.raytheon.uf.common.dataplugin.grib.GribRecord;
import com.raytheon.uf.common.dataplugin.grib.exception.GribException;
import com.raytheon.uf.common.dataplugin.persist.PersistableDataObject;
import com.raytheon.uf.edex.database.DataAccessLayerException;

/**
 * TODO Add Description
 * 
 * <pre>
 * 
 * SOFTWARE HISTORY
 * 
 * Date         Ticket#    Engineer    Description
 * ------------ ---------- ----------- --------------------------
 * Aug 18, 2011            bphillip     Initial creation
 * 
 * </pre>
 * 
 * @author bphillip
 * @version 1.0
 */

public class MSASPostProcessor implements IDecoderPostProcessor {

    private static final String TSLSA = "TSLSA";

    private static final String SFC = "SFC";

    private static final String PMSL = "PMSL";

    private static final String MSLP_ABBR = "MSLP";

    private static final String MSLP_DESC = "NWS mean sea level pressure";

    private static final String PT3_ABBR = "PT3";

    private static final String ALTI_ABBR = "Alti";

    private static final String ALTI_DESC = "Analysis of altimeter setting";

    /*
     * (non-Javadoc)
     * 
     * @see
     * com.raytheon.edex.plugin.grib.decoderpostprocessors.IDecoderPostProcessor
     * #process(com.raytheon.uf.common.dataplugin.grib.GribRecord)
     */
    @Override
    public GribRecord[] process(GribRecord record) throws GribException {
        boolean modelInfoModified = false;
        GribModel gribModel = record.getModelInfo();
        String currentAbbr = gribModel.getParameterAbbreviation();
        String levelName = gribModel.getLevel().getMasterLevel().getName();

        // Reassign PMSL at the SFC to Altimeter
        if (currentAbbr.equals(PMSL) && levelName.equals(SFC)) {
            gribModel.setParameterAbbreviation(ALTI_ABBR);
            gribModel.setParameterName(ALTI_DESC);
            modelInfoModified = true;
        }

        else if (currentAbbr.equals(PMSL)) {
            gribModel.setParameterAbbreviation(MSLP_ABBR);
            gribModel.setParameterName(MSLP_DESC);
            modelInfoModified = true;
        }

        // Reassigns 3-hr pressure tendency to abbreviation used by MSAS
        else if (currentAbbr.equals(TSLSA)) {
            gribModel.setParameterAbbreviation(PT3_ABBR);
            gribModel.setParameterUnit("mb*100");
            modelInfoModified = true;
        }

        if (modelInfoModified) {
            GribModelDao dao = new GribModelDao();
            PersistableDataObject obj = dao.queryById(gribModel.getId());
            if (obj != null) {
                try{
                    dao.delete(obj);
                }catch(Throwable e){
                    e.printStackTrace();
                }
            }
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
                            "Error creating new dataURI for MSAS data!", e);
                }
            } catch (DataAccessLayerException e) {
                throw new GribException("Error modifying MSAS levels!", e);
            }
        }

        record.setOverwriteAllowed(true);
        return new GribRecord[] { record };
    }
}
