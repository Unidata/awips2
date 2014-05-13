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
package com.raytheon.edex.plugin.sfcobs.decoder;

import java.util.Calendar;

import com.raytheon.edex.exception.DecoderException;
import com.raytheon.uf.common.dataplugin.PluginDataObject;
import com.raytheon.uf.common.wmo.WMOHeader;
import com.raytheon.uf.edex.decodertools.core.IDecoderConstants;

/**
 * Declare the methods common to the surface observation class data. In the
 * event that a null reference is pass to setReportData, no exceptions should be
 * thrown and the decode method should return a null reference in response to
 * that data. The isNILObs method allows for nil observations to be identified
 * early so they be discarded if desired.
 * 
 * <pre>
 * 
 * SOFTWARE HISTORY
 * 
 * Date         Ticket#    Engineer    Description
 * ------------ ---------- ----------- --------------------------
 * 20070928            391 jkorman     Initial Coding.
 * </pre>
 * 
 * @author jkorman
 * @version 1.0
 */
public interface ISfcObsDecoder {
    // The following values are redefined for implementing classes.
    public static final Integer VAL_ERROR = IDecoderConstants.VAL_ERROR;

    public static final Integer VAL_MISSING = IDecoderConstants.VAL_MISSING;

    /**
     * Get the text report data.
     * 
     * @return The text report data.
     */
    public String getReportData();

    /**
     * Set the text report data.
     * 
     * @param report
     *            The text report data.
     */
    public void setReportData(String report);

    /**
     * Get the WMO header data.
     * 
     * @return The WMO header.
     */
    public WMOHeader getHeader();

    /**
     * Set the WMO header data.
     * 
     * @param header
     *            The WMO header.
     */
    public void setHeader(WMOHeader header);

    /**
     * Perform the decode behavior for the data type.
     * 
     * @return The decoded data object.
     * @throws DecoderException
     */
    public PluginDataObject decode() throws DecoderException;

    /**
     * Does the current observation data match the pattern for a NIL report?
     * 
     * @return
     */
    public boolean isNILObs();

    /**
     * Get the observation time decoded from a text observation.
     * 
     * @return The observation time.
     */
    public Calendar getObsTime();

}
