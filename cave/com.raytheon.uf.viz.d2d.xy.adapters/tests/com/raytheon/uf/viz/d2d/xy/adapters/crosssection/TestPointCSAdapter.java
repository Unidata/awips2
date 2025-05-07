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

import com.raytheon.uf.common.dataquery.requests.RequestConstraint;
import com.raytheon.uf.common.dataquery.requests.RequestConstraint.ConstraintType;
import com.raytheon.uf.common.pointdata.PointDataConstants;
import com.raytheon.uf.viz.xy.crosssection.rsc.CrossSectionResourceData;

/**
 * Unit tests for {@link PointCSAdapter}.
 *
 * <pre>
 *
 * SOFTWARE HISTORY
 *
 * Date         Ticket#    Engineer    Description
 * ------------ ---------- ----------- --------------------------
 * Jun 20, 2024 2037565    mapeters    Initial creation
 *
 * </pre>
 *
 * @author mapeters
 */
class TestPointCSAdapter {

    static Stream<Arguments> provideParamsForGetExtraNameText() {
        // No station ID in metadata map -> empty text
        Arguments args1 = Arguments.of(null, "");
        // Single station ID in metadata map -> return that ID
        Arguments args2 = Arguments.of(new RequestConstraint("74389"), "74389");
        /*
         * Multiple station IDs in metadata map -> return those IDs, with commas
         * replaced by hyphens
         */
        Arguments args3 = Arguments.of(
                new RequestConstraint("74389,72501", ConstraintType.IN),
                "74389-72501");
        return Stream.of(args1, args2, args3);
    }

    @ParameterizedTest
    @MethodSource("provideParamsForGetExtraNameText")
    void testGetExtraNameText1(RequestConstraint stationIdConstraint,
            String expectedText) {
        PointCSAdapter adapter = new PointCSAdapter();
        CrossSectionResourceData rscData = new CrossSectionResourceData();
        HashMap<String, RequestConstraint> metadata = new HashMap<>();
        metadata.put(PointDataConstants.LOCATION_STATIONID,
                stationIdConstraint);
        rscData.setMetadataMap(metadata);
        adapter.setResourceData(rscData);

        String extraNameText = adapter.getExtraNameText();

        assertEquals(expectedText, extraNameText);
    }
}
