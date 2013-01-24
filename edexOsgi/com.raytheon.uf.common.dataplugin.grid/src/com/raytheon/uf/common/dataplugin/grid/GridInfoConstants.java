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
 * Contains some useful constants for dealing with grid info objects, mostly
 * just hibernate names of attributes.
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
public class GridInfoConstants {

    public static final String ID = "id";

    public static final String DATASET_ID = "datasetId";

    public static final String SECONDARY_ID = "secondaryId";

    public static final String ENSEMBLE_ID = "ensembleId";

    public static final String LOCATION = "location";

    public static final String LOCATION_ID = LOCATION + ".id";

    public static final String PARAMETER = "parameter";

    public static final String PARAMETER_ABBREVIATION = PARAMETER
            + ".abbreviation";

    public static final String PARAMETER_NAME = PARAMETER + ".name";

    public static final String PARAMETER_UNIT = PARAMETER + ".unitString";

    public static final String LEVEL = "level";

    public static final String LEVEL_ID = LEVEL + ".id";

    public static final String MASTER_LEVEL_NAME = LEVEL + ".masterLevel.name";

    public static final String LEVEL_ONE = LEVEL + ".levelonevalue";

    public static final String LEVEL_TWO = LEVEL + ".leveltwovalue";

}
