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
package com.raytheon.viz.lightning;

import java.util.ArrayList;
import java.util.Arrays;
import java.util.Collection;
import java.util.Collections;
import java.util.HashMap;
import java.util.Iterator;
import java.util.LinkedHashMap;
import java.util.List;
import java.util.Map;
import java.util.Set;

import com.raytheon.uf.common.datalisting.DataListing;
import com.raytheon.uf.common.datalisting.impl.DefaultDataListing;
import com.raytheon.uf.common.dataplugin.binlightning.BinLightningRecord;
import com.raytheon.uf.common.dataplugin.binlightning.LightningConstants;
import com.raytheon.uf.common.time.util.TimeUtil;

/**
 * {@link DataListing} which can determine available lightning data and format
 * the data attributes nicely.
 *
 * <pre>
 *
 * SOFTWARE HISTORY
 *
 * Date         Ticket#    Engineer    Description
 * ------------ ---------- ----------- --------------------------
 * Oct 26, 2017 6402       mapeters    Initial creation
 *
 * </pre>
 *
 * @author mapeters
 */

public class LightningDataListing extends DefaultDataListing {

    protected static final String START_TIME = "startTime";

    protected static final String TYPE = "type";

    protected static final String POSITIVE = "Positive";

    protected static final String NEGATIVE = "Negative";

    protected static final String POSITIVE_NEGATIVE = "Positive/Negative";

    protected static final String CLOUD_FLASH = "Cloud Flash";

    protected static final String PULSE = "Pulse";

    private static final String ENTLN = "ENTLN";

    // Match the Obs->Lightning menu text
    private static final String ENTLN_DISPLAY_NAME = "ENI Total Lightning";

    private static final String GLMFL = "GLMfl";

    private static final String GLMFL_DISPLAY_NAME = "GLM Flash";

    private static final String GLMEV = "GLMev";

    private static final String GLMEV_DISPLAY_NAME = "GLM Event";

    private static final String GLMGR = "GLMgr";

    private static final String GLMGR_DISPLAY_NAME = "GLM Group";

    private static final int[] OFFSETS = new int[] { 3600, 900, 300, 60 };

    private static final String[] TYPES = new String[] { POSITIVE, NEGATIVE,
            POSITIVE_NEGATIVE, CLOUD_FLASH, PULSE };

    public LightningDataListing(List<String> keySet) {
        super(BinLightningRecord.PLUGIN_NAME, keySet);
    }

    public LightningDataListing(Set<String> keySet) {
        super(BinLightningRecord.PLUGIN_NAME, keySet);
    }

    @Override
    public Collection<String> getValues(String key, Map<String, String> keyVals)
            throws Exception {
        switch (key) {
        case LightningConstants.SOURCE:
            /*
             * None of the other keys are database fields, so don't pass them to
             * super
             */
            Collection<String> sources = super.getValues(key,
                    Collections.emptyMap());
            String type = keyVals.get(TYPE);
            if (type != null) {
                // Filter out sources that don't support the selected type
                Iterator<String> sourcesItr = sources.iterator();
                while (sourcesItr.hasNext()) {
                    if (!sourceSupportsType(sourcesItr.next(), type)) {
                        sourcesItr.remove();
                    }
                }
            }
            return sources;
        case START_TIME:
            // Unaffected by other keys' values
            List<String> times = new ArrayList<>();
            for (int offset : OFFSETS) {
                times.add(Integer.toString(offset));
            }
            return times;
        case TYPE:
            List<String> types = Arrays.asList(TYPES);
            String source = keyVals.get(LightningConstants.SOURCE);
            if (source != null) {
                // Filter out types that aren't supported by the selected source
                types = new ArrayList<>(types);
                Iterator<String> typesItr = types.iterator();
                while (typesItr.hasNext()) {
                    if (!sourceSupportsType(source, typesItr.next())) {
                        typesItr.remove();
                    }
                }
            }
            return types;
        default:
            throw new IllegalArgumentException("Invalid data key for "
                    + BinLightningRecord.PLUGIN_NAME + ": " + key);
        }
    }

    private static boolean sourceSupportsType(String source, String type) {
        if (CLOUD_FLASH.equals(type) || PULSE.equals(type)) {
            /*
             * Other sources may support these too, but for now we will match
             * how the Obs->Lightning menu works
             */
            return ENTLN.equals(source);
        }
        return true;
    }

    @Override
    protected Map<String, String> getFormattedValues(String key,
            Collection<String> values) {
        switch (key) {
        case LightningConstants.SOURCE:
            Map<String, String> sources = new HashMap<>();
            for (String source : values) {
                sources.put(source, getFormattedSource(source));
            }
            return sortByValue(sources);
        case START_TIME:
            // Maintain insertion order
            Map<String, String> times = new LinkedHashMap<>();
            for (String value : values) {
                int offset = Integer.valueOf(value);
                times.put(value, offset / TimeUtil.SECONDS_PER_MINUTE + " min");
            }
            return times;
        case TYPE:
            // Maintain insertion order
            Map<String, String> types = new LinkedHashMap<>();
            for (String value : values) {
                types.put(value, value);
            }
            return types;
        default:
            throw new IllegalArgumentException("Invalid data key for "
                    + BinLightningRecord.PLUGIN_NAME + ": " + key);
        }
    }

    private static String getFormattedSource(String source) {
        switch (source) {
        case ENTLN:
            return ENTLN_DISPLAY_NAME;
        case GLMFL:
            return GLMFL_DISPLAY_NAME;
        case GLMEV:
            return GLMEV_DISPLAY_NAME;
        case GLMGR:
            return GLMGR_DISPLAY_NAME;
        default:
            return source;
        }
    }

    private static Map<String, String> sortByValue(Map<String, String> map) {
        List<Map.Entry<String, String>> list = new ArrayList<>(map.entrySet());
        Collections.sort(list, Map.Entry.comparingByValue());

        Map<String, String> rval = new LinkedHashMap<>();
        for (Map.Entry<String, String> entry : list) {
            rval.put(entry.getKey(), entry.getValue());
        }

        return rval;
    }
}
