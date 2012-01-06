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

import com.raytheon.edex.plugin.grib.dao.GribModelDao;
import com.raytheon.edex.plugin.grib.util.GribModelCache;
import com.raytheon.uf.common.dataplugin.grib.GribModel;
import com.raytheon.uf.common.dataplugin.grib.GribRecord;
import com.raytheon.uf.common.dataplugin.grib.exception.GribException;
import com.raytheon.uf.common.dataplugin.persist.PersistableDataObject;
import com.raytheon.uf.edex.database.DataAccessLayerException;

/**
 * 
 * Change the parameter name and units for all the SREF probability grids.
 * 
 * <pre>
 * 
 * SOFTWARE HISTORY
 * 
 * Date         Ticket#    Engineer    Description
 * ------------ ---------- ----------- --------------------------
 * Sep 19, 2011            bsteffen     Initial creation
 * 
 * </pre>
 * 
 * @author bsteffen
 * @version 1.0
 */
public class SREFProbPostProcessor implements IDecoderPostProcessor {

    private static final Map<String, String> abbrev2name = new HashMap<String, String>();

    static {
        // Add nearly identical entries for precip 3, 6, 12, and 24 hours.
        for (String p : new String[] { "3", "6", "12", "24" }) {
            abbrev2name.put("tp" + p + "c1", p + "-hr POP > 0.01 in");
            abbrev2name.put("tp" + p + "c2", p + "-hr POP > 0.05 in");
            abbrev2name.put("tp" + p + "c3", p + "-hr POP > 0.10 in");
            abbrev2name.put("tp" + p + "c4", p + "-hr POP > 0.25 in");
            abbrev2name.put("tp" + p + "c5", p + "-hr POP > 0.50 in");
            abbrev2name.put("tp" + p + "c6", p + "-hr POP > 1.00 in");
            abbrev2name.put("tp" + p + "c7", p + "-hr POP > 1.50 in");
            abbrev2name.put("tp" + p + "c8", p + "-hr POP > 2.00 in");
        }

        abbrev2name.put("CAPEc1", "Prob CAPE > 500 J/kg");
        abbrev2name.put("CAPEc2", "Prob CAPE > 1000 J/kg");
        abbrev2name.put("CAPEc3", "Prob CAPE > 2000 J/kg");
        abbrev2name.put("CAPEc4", "Prob CAPE > 2000 J/kg");
        abbrev2name.put("CAPEc5", "Prob CAPE > 4000 J/kg");

        abbrev2name.put("CFRZRc1", "Prob Categorical Freezing Rain = YES");
        abbrev2name.put("CICEPc1", "Prob Categorical Ice Pellets = YES");
        abbrev2name.put("CRAINc1", "Prob Categorical RAIN = YES");
        abbrev2name.put("CSNOWc1", "Prob Categorical SNOW = YES");

        abbrev2name.put("Cigc1", "Prob Ceiling Hgt < 500 ft");
        abbrev2name.put("Cigc2", "Prob Ceiling Hgt < 1000 ft");
        abbrev2name.put("Cigc3", "Prob Ceiling Hgt < 3000 ft");

        abbrev2name.put("PLIxc1", "Prob LI < 0");
        abbrev2name.put("PLIxc2", "Prob LI < -2");
        abbrev2name.put("PLIxc3", "Prob LI < -4");
        abbrev2name.put("PLIxc4", "Prob LI < -6");
        abbrev2name.put("PLIxc5", "Prob LI < -8");

        abbrev2name.put("SNOL12c1", "Prob 12-hr SNOW > 1 in");
        abbrev2name.put("SNOL12c2", "Prob 12-hr SNOW > 2 in");
        abbrev2name.put("SNOL12c3", "Prob 12-hr SNOW > 4 in");
        abbrev2name.put("SNOL12c4", "Prob 12-hr SNOW > 6 in");
        abbrev2name.put("SNOL12c5", "Prob 12-hr SNOW > 7.5 in");
        abbrev2name.put("SNOL12c6", "Prob 12-hr SNOW > 8 in");
        abbrev2name.put("SNOL12c7", "Prob 12-hr SNOW > 10 in");
        abbrev2name.put("SNOL12c8", "Prob 12-hr SNOW > 12 in");
        abbrev2name.put("SNOL12c9", "Prob 12-hr SNOW > 16 in");
        abbrev2name.put("SNOL12c10", "Prob 12-hr SNOW > 24 in");

        abbrev2name.put("Tc1", "Prob Temp < O C");

        abbrev2name.put("Visc1", "Prob Sfc Visibility < 1 mile");
        abbrev2name.put("Visc2", "Prob Sfc Visibility < 3 mile");

        abbrev2name.put("WSc1", "Prob SFC wind speed > 25 kt");
        abbrev2name.put("WSc2", "Prob SFC wind speed > 34 kt");
        abbrev2name.put("WSc3", "Prob SFC wind speed > 48 kt");
        abbrev2name.put("WSc4", "Prob SFC wind speed > 50 kt");

    }

    /*
     * (non-Javadoc)
     * 
     * @see
     * com.raytheon.edex.plugin.grib.decoderpostprocessors.IDecoderPostProcessor
     * #process(com.raytheon.uf.common.dataplugin.grib.GribRecord)
     */
    @Override
    public GribRecord[] process(GribRecord record) throws GribException {
        GribModel gribModel = record.getModelInfo();
        String abbrev = gribModel.getParameterAbbreviation();
        String name = abbrev2name.get(abbrev);
        if (name != null) {
            gribModel.setParameterUnit("%");
            gribModel.setParameterName(name);
        } else {
            return new GribRecord[] { record };
        }
        GribModelDao dao = new GribModelDao();
        PersistableDataObject obj = dao.queryById(gribModel.getId());
        if (obj != null) {
            try {
                dao.delete(obj);
            } catch (Throwable e) {

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
                        "Error creating new dataURI for SREF data!", e);
            }
        } catch (DataAccessLayerException e) {
            throw new GribException("Error getting cached data for SREF!", e);
        }

        record.setOverwriteAllowed(true);
        return new GribRecord[] { record };
    }
}
