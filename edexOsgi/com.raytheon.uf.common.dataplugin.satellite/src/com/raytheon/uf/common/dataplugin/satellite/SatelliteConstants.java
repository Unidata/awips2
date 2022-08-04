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
package com.raytheon.uf.common.dataplugin.satellite;

/**
 * Constants used in the Satellite Resource.
 *
 * <pre>
 *
 * SOFTWARE HISTORY
 *
 * Date          Ticket#  Engineer   Description
 * ------------- -------- ---------- -------------------------------------------
 * Feb 26, 2009           jsanchez   Initial Creation.
 * Mar 16, 2009  800      jsanchez   Updated legend.
 * Dec 16, 2014  15196    kshrestha  Converted Millimeters to Inches for Total
 *                                   Precip Water
 * Mar 02, 2015  14960    jgerth     Support custom legends to override the
 *                                   default constants
 * Mar 22, 2018  6846     bsteffen   Allow substitution of real units in legend
 *                                   strings.
 * Jun 06, 2018  7310     mapeters   Moved from com.raytheon.viz.satellite.SatelliteConstants,
 *                                   extracted out SatelliteLegendLookup
 *
 * </pre>
 *
 * @author jsanchez
 */
public class SatelliteConstants {

    public static final String DMSP = "DMSP";

    public static final String POES = "POES-NPOESS";

    public static final String MISC = "Miscellaneous";

    public static final String COMP = "Composite";

    public static final String PRECIP = "Sounder Based Derived Precipitable Water (PW)";

    public static final String RAIN = "Rain fall rate";
}
