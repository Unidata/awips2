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
package com.raytheon.uf.common.grib;

import java.io.InputStream;
import java.util.ArrayList;
import java.util.Collections;
import java.util.HashSet;
import java.util.List;
import java.util.Set;

import com.raytheon.uf.common.gridcoverage.GridCoverage;
import com.raytheon.uf.common.localization.ILocalizationFile;
import com.raytheon.uf.common.localization.IPathManager;
import com.raytheon.uf.common.localization.LocalizationContext;
import com.raytheon.uf.common.localization.LocalizationContext.LocalizationType;
import com.raytheon.uf.common.serialization.SingleTypeJAXBManager;

/**
 * Loads the grid definition xml files for use by the grib decoder from
 * localization.
 * 
 * <pre>
 * 
 * SOFTWARE HISTORY
 * 
 * Date          Ticket#  Engineer  Description
 * ------------- -------- --------- -----------------
 * Apr 19, 2016  5572     bsteffen  Initial creation
 * 
 * </pre>
 * 
 * @author bsteffen
 */
public class GribCoverageStore implements GribCoverageNameLookup {

    public static final String GRIB_GRID_DIRECTORY = "grib"
            + IPathManager.SEPARATOR + "grids";

    private static final String[] EXTENSION_FILTER = new String[] { "xml" };

    private final List<GridCoverage> coverages;

    public GribCoverageStore(IPathManager pathManager) throws Exception {
        LocalizationContext[] contexts = pathManager
                .getLocalSearchHierarchy(LocalizationType.COMMON_STATIC);
        ILocalizationFile[] files = pathManager.listFiles(contexts,
                GRIB_GRID_DIRECTORY, EXTENSION_FILTER, true, true);
        SingleTypeJAXBManager<GridCoverage> gridCovJaxb = new SingleTypeJAXBManager<>(
                GridCoverage.class);

        coverages = new ArrayList<>(files.length);
        for (ILocalizationFile file : files) {
            try (InputStream is = file.openInputStream()) {
                GridCoverage coverage = gridCovJaxb
                        .unmarshalFromInputStream(is);
                coverage.initialize();
                coverages.add(coverage);
            }
        }
    }

    public List<GridCoverage> getAllCoverages() {
        return Collections.unmodifiableList(coverages);
    }

    public Set<String> getAllCoverageNames() {
        Set<String> names = new HashSet<>(coverages.size(), 1.0f);
        for (GridCoverage coverage : coverages) {
            names.add(coverage.getName());
        }
        return names;
    }

    public GridCoverage getCoverageByName(String name) {
        for (GridCoverage coverage : coverages) {
            if (name.equals(coverage.getName())) {
                return coverage;
            }
        }
        return null;
    }

    @Override
    public Set<String> getGribCoverageNames(GridCoverage coverage) {
        Set<String> result = new HashSet<>();
        for (GridCoverage existing : coverages) {
            if (coverage.spatialEquals(existing)) {
                result.add(existing.getName());
            }
        }
        return result;
    }


}
