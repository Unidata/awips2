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
package com.raytheon.viz.volumebrowser.datacatalog;

import java.util.Arrays;
import java.util.Collection;
import java.util.Collections;
import java.util.HashMap;
import java.util.HashSet;
import java.util.List;
import java.util.Set;

import com.raytheon.uf.common.dataquery.requests.RequestConstraint;
import com.raytheon.viz.volumebrowser.vbui.SelectedData;
import com.raytheon.viz.volumebrowser.vbui.VBMenuBarItemsMgr.ViewMenu;

public class DmdDataCatalog extends AbstractDataCatalog {

    private static final String[] fieldPlanePairs = { "LowLyr:ShrMag",
            "MaxShear:ShrMag", "LowLyr:wDiv", "MidLyr:wDiv", "LowLyr:g2gsh",
            "LowLyr:RRV", "MaxWind:RRV", "LowLyr:diam", "Layer:sRank",
            "LowLyr:GH", "MaxWind:GH", "MaxShear:GH", "HiLyr:GH", "LowLyr:P",
            "MaxWind:P", "MaxShear:P", "HiLyr:P", POINT_LINE_KEY + ":ShrMag",
            POINT_LINE_KEY + ":wDiv", POINT_LINE_KEY + ":g2gsh",
            POINT_LINE_KEY + ":RRV", POINT_LINE_KEY + ":diam",
            POINT_LINE_KEY + ":GH", POINT_LINE_KEY + ":P" };

    @Override
    protected void addProductParameters(IDataCatalogEntry catalogEntry,
            HashMap<String, RequestConstraint> productParameters) {
        productParameters.put("pluginName", new RequestConstraint("radar"));
        productParameters.put("productCode", new RequestConstraint("149"));
    }

    @Override
    protected String[] getPlugins(ViewMenu setting) {
        return new String[] { "radar" };
    }

    @Override
    public List<String> getSupportedSources() {
        return Arrays.asList("radar149");
    }

    @Override
    public IDataCatalogEntry getCatalogEntry(SelectedData selectedData) {
        String[] selectedSources = new String[] { selectedData.getSourcesKey() };
        String[] selectedPlanes = new String[] { selectedData.getPlanesKey() };
        String[] selectedFields = new String[] { selectedData.getFieldsKey() };

        if (!getAvailableSources(selectedFields, selectedPlanes).contains(
                selectedData.getSourcesKey())) {
            return null;
        }

        if (!getAvailablePlanes(selectedSources, selectedFields).contains(
                selectedData.getPlanesKey())
                && !isPointLine(selectedData.getPlanesKey())) {
            return null;
        }

        if (!getAvailableFields(selectedSources, selectedPlanes).contains(
                selectedData.getFieldsKey())) {
            return null;
        }
        return new DataCatalogEntry(selectedData);
    }

    @Override
    public void getAvailableData(AvailableDataRequest request) {
        for (String source : getAvailableSources(request.getSelectedFields(),
                request.getSelectedPlanes())) {
            request.addAvailableSource(source);
        }
        for (String field : getAvailableFields(request.getSelectedSources(),
                request.getSelectedPlanes())) {
            request.addAvailableField(field);
        }
        for (String plane : getAvailablePlanes(request.getSelectedSources(),
                request.getSelectedFields())) {
            request.addAvailablePlane(plane);
        }
    }

    private Collection<String> getAvailableFields(String[] selectedSources,
            String[] selectedPlanes) {
        if (selectedSources.length == 0
                || Arrays.asList(selectedSources).contains("radar149")) {
            Set<String> fields = new HashSet<String>();
            List<String> planes = Arrays.asList(selectedPlanes);
            boolean hasPointLine = false;
            for (String plane : planes) {
                hasPointLine = hasPointLine || isPointLine(plane);
            }
            for (String fieldPlanePair : fieldPlanePairs) {
                String[] split = fieldPlanePair.split(":");
                if (planes.isEmpty() || planes.contains(split[0])) {
                    fields.add(split[1]);
                } else if (hasPointLine && split[0].equals(POINT_LINE_KEY)) {
                    fields.add(split[1]);
                }
            }
            return fields;
        }
        return Collections.emptyList();
    }

    private Collection<String> getAvailablePlanes(String[] selectedSources,
            String[] selectedFields) {
        if (selectedSources.length == 0
                || Arrays.asList(selectedSources).contains("radar149")) {
            Set<String> planes = new HashSet<String>();
            List<String> fields = Arrays.asList(selectedFields);
            for (String fieldPlanePair : fieldPlanePairs) {
                String[] split = fieldPlanePair.split(":");
                if (fields.contains(split[1]) || fields.isEmpty()) {
                    if (split[0].equals(POINT_LINE_KEY)) {
                        for (String plane : getPointLineKeys()) {
                            planes.add(plane);
                        }
                    } else {
                        planes.add(split[0]);
                    }
                }
            }
            return planes;
        }
        return Collections.emptyList();
    }

    private Collection<String> getAvailableSources(String[] selectedFields,
            String[] selectedPlanes) {
        List<String> fields = Arrays.asList(selectedFields);
        List<String> planes = Arrays.asList(selectedPlanes);
        boolean hasPointLine = false;
        for (String plane : planes) {
            hasPointLine = hasPointLine || isPointLine(plane);
        }
        for (String fieldPlanePair : fieldPlanePairs) {
            String[] split = fieldPlanePair.split(":");
            if (fields.contains(split[1]) || fields.isEmpty()) {
                if (planes.contains(split[0]) || planes.isEmpty()) {

                    return Arrays.asList("radar149");
                } else if (hasPointLine && split[0].equals(POINT_LINE_KEY)) {
                    return Arrays.asList("radar149");
                }
            }
        }
        return Collections.emptyList();
    }

}
