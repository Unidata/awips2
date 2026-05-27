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

import java.awt.Rectangle;
import java.nio.file.Files;
import java.nio.file.Path;
import java.text.SimpleDateFormat;
import java.util.Arrays;
import java.util.Calendar;

import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

import com.raytheon.uf.common.time.util.TimeUtil;

/**
 * Used to read xmrg prism data.
 * 
 * <pre>
 *
 * SOFTWARE HISTORY
 *
 * Date         Ticket#    Engineer    Description
 * ------------ ---------- ----------- --------------------------
 * Sep 20, 2016 5631       bkowal      Initial creation
 *
 * </pre>
 *
 * @author bkowal
 */

public class PrismXmrgReader {

    private static final Logger logger = LoggerFactory
            .getLogger(PrismXmrgReader.class);

    private static final String PRISM_NAME_DATE_FORMAT = "MM";

    private static final ThreadLocal<SimpleDateFormat> prismMonthDF = TimeUtil
            .buildThreadLocalSimpleDateFormat(PRISM_NAME_DATE_FORMAT,
                    TimeUtil.GMT_TIME_ZONE);

    private static final double PRISM_DEFAULT_VALUE = 1.0;

    private static final String PRISM_NAME_FMT = "PRISM_%s";

    protected PrismXmrgReader() {
    }

    public static double[][] readPrismData(final Path containingDirectory,
            final Calendar prismDateTime, final Rectangle geoGridData) {
        final String prismMonth = prismMonthDF.get()
                .format(prismDateTime.getTime());
        /*
         * Determine which prism file needs to be read.
         */
        final Path prismPath = containingDirectory
                .resolve(String.format(PRISM_NAME_FMT, prismMonth));

        final double[][] prismData = new double[geoGridData.height][geoGridData.width];

        /*
         * Verify that the prism file exists and is available.
         */
        if (!Files.exists(prismPath)) {
            logger.warn(
                    "Unable to find the expected Prism file: {}. Defaulting to an array of: {}.",
                    prismPath.toString(), PRISM_DEFAULT_VALUE);
            for (int i = 0; i < prismData.length; i++) {
                Arrays.fill(prismData[i], PRISM_DEFAULT_VALUE);
            }
            return prismData;
        }
        
        /*
         * TODO: finish
         */

        return prismData;
    }
}