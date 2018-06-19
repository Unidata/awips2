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
package com.raytheon.uf.edex.plugin.mpe.fieldgen;

/**
 * Identifies Gridded Radar Records that occur prior to and/or after a specific
 * date/time.
 * 
 * <pre>
 *
 * SOFTWARE HISTORY
 *
 * Date         Ticket#    Engineer    Description
 * ------------ ---------- ----------- --------------------------
 * Sep 27, 2016 5631       bkowal      Initial creation
 *
 * </pre>
 *
 * @author bkowal
 */

public class GriddedRadarMatchBounds extends GriddedRadarMatch {

    private long offsetPast = Long.MAX_VALUE;

    private Short meanFieldBiasFuture;

    private String gridFilenameFuture;

    private long offsetFuture = Long.MAX_VALUE;

    public GriddedRadarMatchBounds() {
    }

    public boolean isPastCloserToPresent(final long offset) {
        return (offset < offsetPast);
    }

    public boolean isFutureCloserToPresent(final long offset) {
        return (offset < offsetFuture);
    }

    public boolean isPastMatch() {
        return (offsetPast != Long.MAX_VALUE);
    }

    public boolean isFutureMatch() {
        return (offsetFuture != Long.MAX_VALUE);
    }

    public long getOffsetPast() {
        return offsetPast;
    }

    public void setOffsetPast(long offsetPast) {
        this.offsetPast = offsetPast;
    }

    public Short getMeanFieldBiasFuture() {
        return meanFieldBiasFuture;
    }

    public void setMeanFieldBiasFuture(Short meanFieldBiasFuture) {
        this.meanFieldBiasFuture = meanFieldBiasFuture;
    }

    public String getGridFilenameFuture() {
        return gridFilenameFuture;
    }

    public void setGridFilenameFuture(String gridFilenameFuture) {
        this.gridFilenameFuture = gridFilenameFuture;
    }

    public long getOffsetFuture() {
        return offsetFuture;
    }

    public void setOffsetFuture(long offsetFuture) {
        this.offsetFuture = offsetFuture;
    }
}