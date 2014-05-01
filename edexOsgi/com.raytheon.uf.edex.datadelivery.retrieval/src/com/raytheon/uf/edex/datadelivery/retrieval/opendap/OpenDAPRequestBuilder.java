package com.raytheon.uf.edex.datadelivery.retrieval.opendap;

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

import com.google.common.annotations.VisibleForTesting;
import com.raytheon.uf.common.datadelivery.registry.Coverage;
import com.raytheon.uf.common.datadelivery.registry.Ensemble;
import com.raytheon.uf.common.datadelivery.registry.GriddedCoverage;
import com.raytheon.uf.common.datadelivery.registry.GriddedTime;
import com.raytheon.uf.common.datadelivery.registry.Levels;
import com.raytheon.uf.common.datadelivery.registry.Parameter;
import com.raytheon.uf.common.datadelivery.registry.Time;
import com.raytheon.uf.common.datadelivery.retrieval.xml.RetrievalAttribute;
import com.raytheon.uf.common.gridcoverage.GridCoverage;
import com.raytheon.uf.common.gridcoverage.subgrid.SubGrid;
import com.raytheon.uf.edex.datadelivery.retrieval.request.RequestBuilder;

/**
 * Request XML translation related utilities
 * 
 * <pre>
 * 
 * SOFTWARE HISTORY
 * Date         Ticket#    Engineer    Description
 * ------------ ---------- ----------- --------------------------
 * Jan 07, 2011             dhladky     Initial creation
 * Jul 25, 2012 955         djohnson    Make package-private.
 * Aug 14, 2012 1022        djohnson    Throw exception if invalid OpenDAP grid coordinates specified, 
 *                                      make immutable, use StringBuilder instead of StringBuffer.
 * Dec 10, 2012 1259        bsteffen    Switch Data Delivery from LatLon to referenced envelopes.
 * May 12, 2013 753         dhladky     address field
 * Sept 25, 2013 1797       dhladky     separated time from gridded time
 * 
 * </pre>
 * 
 * @author dhladky
 * @version 1.0
 */
class OpenDAPRequestBuilder<T extends Time, C extends Coverage> extends RequestBuilder<T, C> {

    private final String openDAPURL;

    /**
     * useful constructor
     * 
     * @param prXML
     */
    OpenDAPRequestBuilder(OpenDAPRetrievalAdapter adapter,
            RetrievalAttribute<T, C> ra) {
        super(ra);

        // Create URL
        // this works in this order
        // Ensemble TIME LEVELS Y X
        StringBuilder buffer = new StringBuilder();
        buffer.append(adapter.getProviderRetrievalXMl().getConnection()
                .getUrl());
        buffer.append("?");
        buffer.append(getRetrievalAttribute().getParameter().getProviderName());
        // process ensemble first
        buffer.append(processEnsemble());
        // process time second
        buffer.append(processTime(getRetrievalAttribute().getTime()));
        // process the coverage, w/levels
        buffer.append(processCoverage(ra.getCoverage()));

        this.openDAPURL = buffer.toString().trim();
    }

    /**
     * Gets a string vertical levels DAP from XML
     * 
     * @param prcXML
     * @return
     */
    private String processDAPLevels(Parameter parm) {

        StringBuilder buf = new StringBuilder();

        // multilevel request
        if (parm.getLevels() != null || parm.getLevels().size() > 1) {

            Levels levs = parm.getLevels();

            if (levs.size() == 1 && !levs.getLevel().contains(Double.NaN)) {
                buf.append("[0]");
            } else if (levs.size() > 1) {
                // one particular level selected
                if (levs.getRequestLevelStart() == levs.getRequestLevelEnd()) {
                    buf.append("[" + levs.getRequestLevelStart() + "]");
                }
                // range of levels selected
                else {
                    buf.append("[" + levs.getRequestLevelStart() + ":1:"
                            + levs.getRequestLevelEnd() + "]");
                }
            } else {
                buf.append("");
            }
        }

        return buf.toString();
    }

    @Override
    public String processTime(Time time) {

        StringBuilder buf = new StringBuilder();
        GriddedTime gtime = (GriddedTime)time;

        if (gtime.getNumTimes() == 1) {
            // only one time available
            buf.append("[0]");
        } else {
            // a particular time selected from the list
            if (gtime.getRequestStart().equals(gtime.getRequestEnd())) {
                buf.append("[" + (gtime.getRequestStartTimeAsInt()) + "]");
            }
            // a range of times selected from the list
            else {
                buf.append("[" + (gtime.getRequestStartTimeAsInt()) + ":1:"
                        + (gtime.getRequestEndTimeAsInt()) + "]");
            }
        }

        return buf.toString();
    }

    @Override
    public String processCoverage(C coverage) {
        
        StringBuilder sb = new StringBuilder();

        if (coverage instanceof GriddedCoverage) {
            // manage the vertical levels
            sb.append(processDAPLevels(getRetrievalAttribute()
                    .getParameter()));
            sb.append(getCoverageString((GriddedCoverage) coverage));
        } else {
            // TODO: MADIS POINT DATA, If available in OPenDap?
        }
           
        return sb.toString();
    }

    /**
     * @param coverage
     * @return
     */
    @VisibleForTesting
    static String getCoverageString(GriddedCoverage coverage) {
        StringBuilder sb = new StringBuilder();
        // Check for Sub-gridding!!!!!!!!!!!!!!
        // y
        SubGrid requestSubGrid = coverage.getRequestSubGrid();
        if (requestSubGrid != null) {
            GridCoverage fullGridCoverage = coverage.getGridCoverage();

            int startX = requestSubGrid.getUpperLeftX();
            int endX = startX + requestSubGrid.getNX() - 1;

            // Y is more complex because GridCoverages uses 0 at the top
            // increasing down and openDap uses 0 at the botton increasing up so
            // it is necessary to invert the y values.
            int startY = fullGridCoverage.getNy()
                    - requestSubGrid.getUpperLeftY() - requestSubGrid.getNY();
            int endY = fullGridCoverage.getNy()
                    - requestSubGrid.getUpperLeftY() - 1;

            sb.append("[" + startY + ":1:" + endY + "]");
            sb.append("[" + startX + ":1:" + endX + "]");

        }

        return sb.toString();
    }

    /**
     * Returns an {@link IllegalArgumentException} that denotes an invalid grid
     * coordinate combination.
     * 
     * @param gridCoordinate
     *            the grid coordinate
     * @param startValue
     *            the start value
     * @param endValue
     *            the end value
     * @return the exception
     */
    private static IllegalArgumentException getIllegalArgumentException(
            String gridCoordinate, int startValue, int endValue) {
        return new IllegalArgumentException(
                String.format(
                        "The start %s value [%d] is greater than the end %s value [%d]!",
                        gridCoordinate, startValue, gridCoordinate, endValue));
    }

    /**
     * Take care of ensembles
     * 
     * @return
     */
    public String processEnsemble() {

        if (getRetrievalAttribute().getEnsemble() != null) {
            Ensemble e = getRetrievalAttribute().getEnsemble();
            int[] range = e.getSelectedRange();
            if (range[0] == range[1]) {
                return "[" + range[0] + "]";
            } else {
                return "[" + range[0] + ":" + range[1] + "]";
            }
        } else {
            return "";
        }
    }

    @Override
    public String getRequest() {
        return openDAPURL;
    }

    @Override
    public RetrievalAttribute<T, C> getAttribute() {
        return getRetrievalAttribute();
    }

}
