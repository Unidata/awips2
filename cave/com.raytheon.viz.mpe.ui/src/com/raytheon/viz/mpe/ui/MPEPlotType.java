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
package com.raytheon.viz.mpe.ui;

/**
 * Available MPE plot types
 * 
 * <pre>
 * 
 * SOFTWARE HISTORY
 * 
 * Date         Ticket#    Engineer    Description
 * ------------ ---------- ----------- --------------------------
 * Dec 6, 2012            mschenke     Initial creation
 * 
 * </pre>
 * 
 * @author mschenke
 * @version 1.0
 */

public enum MPEPlotType {
    T_24hGRID_PRECIP("24hGRID_PRECIP", 24), T_24hMAREAPRECIP("24hMAREAPRECIP",
            24), T_6hGRID_PRECIP("6hGRID_PRECIP", 6), T_6hMAREA_PRECIP(
            "6hMAREA_PRECIP", 6), T_6hGRID_FREEZL("6hGRID_FREEZL", 6), T_6hMAREA_FREEZL(
            "6hMAREA_FREEZL", 6), T_sixhGRID_TEMP("sixhGRID_TEMP", 6), T_sixhMAREA_TEMP(
            "sixhMAREA_TEMP", 6), T_maxGRID_TEMP("maxGRID_TEMP", 24), T_minGRID_TEMP(
            "minGRID_TEMP", 24);

    private final String cv_use;

    private final int duration;

    private MPEPlotType(String cv_use, int duration) {
        this.cv_use = cv_use;
        this.duration = duration;
    }

    public String getCvUse() {
        return cv_use;
    }

    public int getDurationInHrs() {
        return duration;
    }
}
