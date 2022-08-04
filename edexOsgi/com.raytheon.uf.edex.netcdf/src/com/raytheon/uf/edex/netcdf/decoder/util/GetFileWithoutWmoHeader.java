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
package com.raytheon.uf.edex.netcdf.decoder.util;

import java.io.BufferedInputStream;
import java.io.File;
import java.io.FileInputStream;
import java.io.IOException;
import java.io.InputStream;
import java.nio.charset.StandardCharsets;
import java.nio.file.Files;
import java.nio.file.Path;

import org.apache.camel.Exchange;
import org.apache.camel.Processor;
import org.apache.camel.spi.Synchronization;
import org.apache.camel.support.DefaultExchange;
import org.slf4j.LoggerFactory;

import com.raytheon.uf.common.wmo.util.WMOHeaderFinder;

/**
 *
 * A {@link Processor} which produces files without WMO headers. If the input
 * file does not have a WMO header or if the body is not a File then this
 * processor does nothing. If the file contains a wmo header then a temporary
 * copy of the file is created with the WMO Header removed. The temporary file
 * will automatically be deleted when the camel exchange is completed.
 *
 * <pre>
 *
 * SOFTWARE HISTORY
 *
 * Date         Ticket#  Engineer  Description
 * ------------ -------- --------- --------------------------
 * Sep 08, 2015  4709    bsteffen  Initial creation
 * Nov 30, 2016  5970    njensen   Moved to netcdf plugin
 * Mar  3, 2021  8326    tgurney   Camel 3 fixes
 *
 * </pre>
 *
 * @author bsteffen
 */
public class GetFileWithoutWmoHeader implements Processor {

    @Override
    public void process(Exchange exchange) throws IOException {
        Object body = exchange.getIn().getBody();
        if (!(body instanceof File)) {
            return;
        }
        File file = (File) body;

        String header = WMOHeaderFinder.find(file);
        if (header == null) {
            // No header found, nothing to do
            return;
        }
        byte[] headerBytes = header.getBytes(StandardCharsets.ISO_8859_1);

        try (InputStream is = new BufferedInputStream(
                new FileInputStream(file))) {
            int matchLen = 0;
            int index = 0;
            byte[] buffer = new byte[headerBytes.length];
            int read = is.read(buffer);
            while (matchLen != headerBytes.length) {
                if (index == read) {
                    if (matchLen > 0) {
                        System.arraycopy(buffer, buffer.length - matchLen,
                                buffer, 0, matchLen);
                    }
                    read = matchLen + is.read(buffer, matchLen,
                            buffer.length - matchLen);
                    index = matchLen;
                }
                if (headerBytes[matchLen] == buffer[index]) {
                    matchLen += 1;
                } else {
                    index -= matchLen;
                    matchLen = 0;
                }
                index += 1;
            }

            /*
             * The id is used in case multiple components are processing the
             * same file.
             */
            String uniqueId = exchange.getExchangeId();
            if (uniqueId.length() > 8) {
                int hash = uniqueId.hashCode();
                uniqueId = String.format("%08x", hash);
            }
            Path result = file.toPath().getParent()
                    .resolve(file.getName() + "-" + uniqueId + ".nowmo");
            Files.copy(is, result);
            file = result.toFile();
            if (exchange instanceof DefaultExchange) {
                DefaultExchange defaultExchange = (DefaultExchange) exchange;
                defaultExchange
                        .addOnCompletion(new DeleteFileOnCompletion(file));
            } else {
                exchange.getUnitOfWork()
                        .addSynchronization(new DeleteFileOnCompletion(file));
            }

            exchange.getIn().setBody(file);

        }
    }

    private static class DeleteFileOnCompletion implements Synchronization {

        private final File file;

        public DeleteFileOnCompletion(File file) {
            this.file = file;
        }

        @Override
        public void onComplete(Exchange exchange) {
            delete();
        }

        @Override
        public void onFailure(Exchange exchange) {
            delete();
        }

        private void delete() {
            if (file.exists()) {
                if (!file.delete()) {
                    LoggerFactory.getLogger(GetFileWithoutWmoHeader.class)
                            .error("Unable to delete file: {}", file);
                }
            }
        }

    }
}
