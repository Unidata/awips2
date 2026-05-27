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
package com.raytheon.viz.grid.inv;

import com.raytheon.uf.common.dataplugin.grid.GridConstants;

/**
 * @deprecated. Use {@link GridConstants} for access to constants. The actual
 *              inventory is now in VizGridInventory.
 * 
 *              <pre>
 * 
 * SOFTWARE HISTORY
 * 
 * Date          Ticket#  Engineer    Description
 * ------------- -------- ----------- ------------------------------------------
 * Mar 25, 2009  2149     brockwoo    Initial creation
 * Nov 20, 2009  3387     jelkins     Use derived script's variableId instead of
 *                                    filename
 * Nov 21, 2009  3576     rjpeter     Refactored use of DerivParamDesc.
 * Feb 26, 2013  1659     bsteffen    Add time agnostic caching to grid derived
 *                                    parameters.
 * Jan 30, 2014  2725     ekladstrup  updated exception handling during move of
 *                                    derived parameters to common
 * Sep 09, 2014  3356     njensen     Remove CommunicationException
 * Mar 03, 2016  5439     bsteffen    Split grid inventory into common and viz
 * 
 * </pre>
 * 
 * @author brockwoo
 */
@Deprecated
public class GridInventory {

    public static final String PLUGIN_NAME_QUERY = GridConstants.PLUGIN_NAME;

    public static final String MODEL_NAME_QUERY = GridConstants.DATASET_ID;

    public static final String PARAMETER_QUERY = GridConstants.PARAMETER_ABBREVIATION;

    public static final String LEVEL_ID_QUERY = GridConstants.LEVEL_ID;

    public static final String MASTER_LEVEL_QUERY = GridConstants.MASTER_LEVEL_NAME;

    public static final String LEVEL_ONE_QUERY = GridConstants.LEVEL_ONE;

    public static final String LEVEL_TWO_QUERY = GridConstants.LEVEL_TWO;

    public static final String ENSEMBLE_QUERY = GridConstants.ENSEMBLE_ID;

    public static final String CUBE_MASTER_LEVEL_NAME = "MB";

    public static final String PLUGIN_NAME = GridConstants.GRID;

}
