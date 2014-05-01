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
package com.raytheon.uf.common.dataplugin.grid;

/**
 * 
 * Contains some useful constants for dealing with grid data, mostly just
 * hibernate names of attributes.
 * 
 * <pre>
 * 
 * SOFTWARE HISTORY
 * 
 * Date         Ticket#    Engineer    Description
 * ------------ ---------- ----------- --------------------------
 * Sep 11, 2012            bsteffen     Initial creation
 * 
 * </pre>
 * 
 * @author bsteffen
 * @version 1.0
 */
public class GridConstants {

    public static final String GRID = "grid";

    public static final String PLUGIN_NAME = "pluginName";

    public static final String INFO = "info";

    public static final String INFO_ID = INFO + "." + GridInfoConstants.ID;

    public static final String DATASET_ID = INFO + "."
            + GridInfoConstants.DATASET_ID;

    public static final String SECONDARY_ID = INFO + "."
            + GridInfoConstants.SECONDARY_ID;

    public static final String ENSEMBLE_ID = INFO + "."
            + GridInfoConstants.ENSEMBLE_ID;

    public static final String LOCATION = INFO + "."
            + GridInfoConstants.LOCATION;

    public static final String LOCATION_ID = INFO + "."
            + GridInfoConstants.LOCATION_ID;

    public static final String PARAMETER = INFO + "."
            + GridInfoConstants.PARAMETER;

    public static final String PARAMETER_ABBREVIATION = INFO + "."
            + GridInfoConstants.PARAMETER_ABBREVIATION;

    public static final String PARAMETER_NAME = INFO + "."
            + GridInfoConstants.PARAMETER_NAME;

    public static final String PARAMETER_UNIT = INFO + "."
            + GridInfoConstants.PARAMETER_UNIT;

    public static final String LEVEL = INFO + "." + GridInfoConstants.LEVEL;

    public static final String LEVEL_ID = INFO + "."
            + GridInfoConstants.LEVEL_ID;

    public static final String MASTER_LEVEL_NAME = INFO + "."
            + GridInfoConstants.MASTER_LEVEL_NAME;

    public static final String LEVEL_ONE = INFO + "."
            + GridInfoConstants.LEVEL_ONE;

    public static final String LEVEL_TWO = INFO + "."
            + GridInfoConstants.LEVEL_TWO;

}
