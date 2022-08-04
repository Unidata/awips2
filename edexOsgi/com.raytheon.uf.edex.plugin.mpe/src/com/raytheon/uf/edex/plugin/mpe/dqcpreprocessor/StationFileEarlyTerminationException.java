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
package com.raytheon.uf.edex.plugin.mpe.dqcpreprocessor;

/**
 * Indicates that a Station File that was being read ended earlier than expected.
 * 
 * <pre>
 *
 * SOFTWARE HISTORY
 *
 * Date         Ticket#    Engineer    Description
 * ------------ ---------- ----------- --------------------------
 * Jan 29, 2018 7184       bkowal      Initial creation
 *
 * </pre>
 *
 * @author bkowal
 */

public class StationFileEarlyTerminationException extends Exception {

    private static final long serialVersionUID = -1199796039917494451L;

    private static final String MESSAGE_TEMPLATE_FMT = "Expected to read %d station(s); only read %d station(s).";

    public StationFileEarlyTerminationException(final int stationsExpected,
            final int stationsRead) {
        super(String.format(MESSAGE_TEMPLATE_FMT, stationsExpected,
                stationsRead));
    }
}