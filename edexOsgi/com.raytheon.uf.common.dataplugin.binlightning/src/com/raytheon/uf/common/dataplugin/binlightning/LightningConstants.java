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
package com.raytheon.uf.common.dataplugin.binlightning;

/**
 * Constants used for lightning data such as dataset names in HDF5
 * 
 * <pre>
 * 
 * SOFTWARE HISTORY
 * 
 * Date         Ticket#    Engineer    Description
 * ------------ ---------- ----------- --------------------------
 * May 30, 2014 3226       bclement     Initial creation
 * Jul 07, 2014 3333       bclement     added SOURCE
 * Jan 22, 2014 2949       nabowle      Add default and unknown source values.
 * Feb 27, 2015 4181       mapeters     Added AVAILABLE_PARAMETERS.
 * Apr 20, 2016 5551       tgurney      Move AVAILABLE_PARAMETERS to DAF plugin
 * 
 * </pre>
 * 
 * @author bclement
 */
public class LightningConstants {

    public static final String PULSE_HDF5_GROUP_SUFFIX = "pulse";

    // metadata column names

    public static final String SOURCE = "source";

    // HDF5 dataset names

    public static final String TIME_DATASET = "obsTime";

    public static final String LAT_DATASET = "latitude";

    public static final String LON_DATASET = "longitude";

    public static final String INTENSITY_DATASET = "intensity";

    public static final String MSG_TYPE_DATASET = "msgType";

    public static final String STRIKE_TYPE_DATASET = "strikeType";

    public static final String PULSE_COUNT_DATSET = "pulseCount";

    public static final String PULSE_INDEX_DATASET = "pulseIndex";

    public static final String PULSE_TYPE_DATASET = "pulseType";

    public static final String HEIGHT_DATASET = "height";

    public static final String SENSOR_COUNT_DATASET = "sensorCount";

    // Field values

    public static final String DEFAULT_SOURCE = "NLDN";

    public static final String UNKNOWN_SOURCE = "UNKN";

}
