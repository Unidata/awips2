/**
 * This software was developed and / or modified by Raytheon Company,
 * pursuant to Contract EA133W-17-CQ-0082 with the US Government.
 *
 * U.S. EXPORT CONTROLLED TECHNICAL DATA
 * This software product contains export-restricted data whose
 * export/transfer/disclosure is restricted by U.S. law. Dissemination
 * to non-U.S. persons whether in the United States or abroad requires
 * an export license or other authorization.
 *
 * Contractor Name:        Raytheon Company
 * Contractor Address:     2120 South 72nd Street, Suite 900
 *                         Omaha, NE 68124
 *                         402.291.0100
 *
 * See the AWIPS II Master Rights File ("Master Rights File.pdf") for
 * further licensing information.
 **/
package com.raytheon.uf.viz.d2d.xy.adapters.crosssection;

import static org.junit.jupiter.api.Assertions.assertEquals;

import java.util.HashMap;
import java.util.stream.Stream;

import org.junit.jupiter.params.ParameterizedTest;
import org.junit.jupiter.params.provider.Arguments;
import org.junit.jupiter.params.provider.MethodSource;

import com.raytheon.uf.common.dataplugin.grid.GridConstants;
import com.raytheon.uf.common.dataquery.requests.RequestConstraint;
import com.raytheon.uf.viz.xy.crosssection.rsc.CrossSectionResourceData;

/**
 *
 * Unit tests for {@link GridCSAdapter}.
 *
 * <pre>
 *
 * SOFTWARE HISTORY
 *
 * Date         Ticket#    Engineer    Description
 * ------------ ---------- ----------- --------------------------
 * Jul 3, 2024  2037476    bines       Initial creation
 *
 * </pre>
 *
 * @author bines
 */
class TestGridCSAdapter {

    static Stream<Arguments> provideParamsForGetCreatingEntity() {

        Arguments args1 = Arguments.of(null, "");

        Arguments args2 = Arguments.of(new RequestConstraint("radar-koax"),
                "radar-koax");

        Arguments args3 = Arguments.of(new RequestConstraint("HRRR"), "HRRR");

        return Stream.of(args1, args2, args3);
    }

    @ParameterizedTest
    @MethodSource("provideParamsForGetCreatingEntity")
    void testGetCreatingEntity(RequestConstraint creatingEntityConstraint,
            String expectedCreatingEntity) {
        GridCSAdapter adapter = new GridCSAdapter();
        CrossSectionResourceData rscData = new CrossSectionResourceData();
        HashMap<String, RequestConstraint> metadata = new HashMap<>();
        metadata.put(GridConstants.DATASET_ID, creatingEntityConstraint);
        rscData.setMetadataMap(metadata);
        adapter.setResourceData(rscData);

        String creatingEntity = adapter.getCreatingEntity();

        assertEquals(expectedCreatingEntity, creatingEntity);
    }
}
