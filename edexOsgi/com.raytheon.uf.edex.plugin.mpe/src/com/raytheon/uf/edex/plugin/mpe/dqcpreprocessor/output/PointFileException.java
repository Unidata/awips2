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
package com.raytheon.uf.edex.plugin.mpe.dqcpreprocessor.output;

import java.nio.file.Path;

/**
 * Indicates that a problem has occurred during an attempt to write a Point
 * Precipitation file.
 * 
 * <pre>
 *
 * SOFTWARE HISTORY
 *
 * Date         Ticket#    Engineer    Description
 * ------------ ---------- ----------- --------------------------
 * Feb 12, 2018 7184       bkowal      Initial creation
 *
 * </pre>
 *
 * @author bkowal
 */

public class PointFileException extends Exception {

    private static final long serialVersionUID = -8443590529797911035L;

    private static final String MESSAGE_TEMPLATE_FMT = "Failed to write Point %s File: %s.";

    public PointFileException(final String dataType, final Path pointPrecipFilePath, Throwable e) {
        super(String.format(MESSAGE_TEMPLATE_FMT,
                dataType, pointPrecipFilePath.toString()), e);
    }
}