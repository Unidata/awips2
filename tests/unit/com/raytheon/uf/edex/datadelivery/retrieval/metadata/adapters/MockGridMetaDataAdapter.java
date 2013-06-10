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
package com.raytheon.uf.edex.datadelivery.retrieval.metadata.adapters;

import org.junit.Ignore;

import com.raytheon.uf.common.datadelivery.registry.GriddedCoverageFixture;
import com.raytheon.uf.common.datadelivery.retrieval.xml.RetrievalAttribute;
import com.raytheon.uf.common.dataplugin.level.Level;
import com.raytheon.uf.common.gridcoverage.GridCoverage;

/**
 * Overrides specific methods in {@link GridMetadataAdapter} that require
 * database interaction.
 * 
 * <pre>
 * 
 * SOFTWARE HISTORY
 * 
 * Date         Ticket#    Engineer    Description
 * ------------ ---------- ----------- --------------------------
 * Feb 06, 2013 1543       djohnson     Initial creation
 * 
 * </pre>
 * 
 * @author djohnson
 * @version 1.0
 */
@Ignore
public class MockGridMetaDataAdapter extends GridMetadataAdapter {

    /**
     * Constructor.
     * 
     * @param attXML
     * @throws InstantiationException
     */
    public MockGridMetaDataAdapter() {
    }

    /**
     * {@inheritDoc}
     */
    @Override
    GridCoverage getCoverageFromCache(GridCoverage coverage) {
        return GriddedCoverageFixture.INSTANCE.get().getGridCoverage();
    }

    /**
     * {@inheritDoc}
     */
    @Override
    Level[] getLevels(RetrievalAttribute attXML) {
        return new Level[] { new Level("0.0") };
    }
}
