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
 * Identifies a Gridded Radar record that is associated exactly with a specified
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

public class GriddedRadarMatch {

    private Short meanFieldBias;

    private String gridFilename;

    public GriddedRadarMatch() {
    }

    public GriddedRadarMatch(final Short meanFieldBias,
            final String gridFilename) {
        this.meanFieldBias = meanFieldBias;
        this.gridFilename = gridFilename;
    }

    public Short getMeanFieldBias() {
        return meanFieldBias;
    }

    public void setMeanFieldBias(Short meanFieldBias) {
        this.meanFieldBias = meanFieldBias;
    }

    public String getGridFilename() {
        return gridFilename;
    }

    public void setGridFilename(String gridFilename) {
        this.gridFilename = gridFilename;
    }
}