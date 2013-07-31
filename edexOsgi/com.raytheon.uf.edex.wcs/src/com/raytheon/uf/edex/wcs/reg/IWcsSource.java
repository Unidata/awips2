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
 *
 * SOFTWARE HISTORY
 *
 * Date         Ticket#    Engineer    Description
 * ------------ ---------- ----------- --------------------------
 * May 5, 2011            bclement     Initial creation
 * Aug 18, 2013 #2097      dhladky     interface standards
 *
 */

package com.raytheon.uf.edex.wcs.reg;

import java.util.List;

import com.raytheon.uf.common.time.DataTime;
import com.raytheon.uf.edex.ogc.common.db.SimpleDimension;
import com.raytheon.uf.edex.ogc.common.db.SimpleLayer;
import com.raytheon.uf.edex.ogc.common.spatial.Composite3DBoundingBox;
import com.raytheon.uf.edex.wcs.WcsException;

public interface IWcsSource<D extends SimpleDimension, L extends SimpleLayer<D>> {

    /**
     * long name attribute for coverages
     */
    public static final String LONG_NAME_ATTR = "long_name";

    public List<CoverageDescription> listCoverages(boolean summary);

    public CoverageDescription describeCoverage(String identifier)
            throws WcsException;

    public CoverageTransform<D, L> getCoverageTransform();

    public Coverage getCoverage(String identifier, DataTime time,
            Composite3DBoundingBox bbox, List<RangeField> rangeFields)
            throws WcsException;

    public String getKey();

    public boolean hasCoverage(String id) throws WcsException;

    /**
     * @param c
     *            extension class
     * @return extension object for class, null if none registered
     */
    public <T> T getExtension(Class<T> c);

}
