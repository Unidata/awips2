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
package com.raytheon.edex.plugin.grib;

import java.io.File;
import java.io.IOException;
import java.util.ArrayList;
import java.util.List;

import ucar.grib.grib2.Grib2IndicatorSection;
import ucar.unidata.io.KMPMatch;
import ucar.unidata.io.RandomAccessFile;

import com.raytheon.edex.plugin.grib.exception.GribException;
import com.raytheon.uf.common.status.IUFStatusHandler;
import com.raytheon.uf.common.status.UFStatus;
import com.raytheon.uf.common.status.UFStatus.Priority;

/**
 * 
 * Split a single grib file into one or more {@link GribDecodeMessage}.
 * 
 * <pre>
 * 
 * SOFTWARE HISTORY
 * 
 * Date          Ticket#  Engineer    Description
 * ------------- -------- ----------- --------------------------
 * Oct 07, 2013  2402     bsteffen    Rewritten to output GribDecodeMessage.
 * 
 * </pre>
 * 
 * @author bsteffen
 * @version 2.0
 */
public class GribSplitter {

    private static final transient IUFStatusHandler statusHandler = UFStatus
            .getHandler(GribSplitter.class);

    private static final KMPMatch matcher = new KMPMatch("GRIB".getBytes());

    public List<GribDecodeMessage> split(File file) throws GribException {
        List<GribDecodeMessage> messages = new ArrayList<GribDecodeMessage>();
        RandomAccessFile raf = null;
        try {
            raf = new RandomAccessFile(file.getAbsolutePath(), "r", 1024);
            raf.order(RandomAccessFile.BIG_ENDIAN);
            while (raf.searchForward(matcher, Integer.MAX_VALUE)) {
                GribDecodeMessage message = new GribDecodeMessage();
                message.setFileName(file.getAbsolutePath());
                long startPosition = raf.getFilePointer();
                message.setStartPosition(startPosition);
                raf.skipBytes(4);
                Grib2IndicatorSection is = new Grib2IndicatorSection(raf);
                message.setGribEdition((byte) is.getGribEdition());
                long length = is.getGribLength();
                message.setMessageLength(length);
                messages.add(message);
                raf.seek(startPosition + length);
                /*
                 * A significant amount of files contain one grib record with
                 * several bytes of gibberish on the end. This prevents us from
                 * reading the gibberish if it is too small to be a grib record
                 * anyway.
                 */
                if (raf.length() - raf.getFilePointer() < 24) {
                    break;
                }
            }
        } catch (IOException e) {
            throw new GribException("Unable to split file: "
                    + file.getAbsolutePath(), e);
        } finally {
            try {
                raf.close();
            } catch (Throwable e) {
                statusHandler.handle(Priority.DEBUG, "Cannot close grib file: "
                        + file.getAbsolutePath(), e);
            }
        }
        return messages;
    }

}
