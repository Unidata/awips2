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
package com.raytheon.uf.edex.plugin.mpe.biasmesgen;

import java.io.FileInputStream;
import java.io.FileOutputStream;
import java.io.IOException;
import java.nio.ByteBuffer;
import java.nio.ByteOrder;
import java.nio.channels.FileChannel;
import java.nio.channels.FileChannel.MapMode;
import java.nio.file.Path;
import java.nio.file.Paths;
import java.util.ArrayList;
import java.util.List;

import com.raytheon.uf.edex.plugin.mpe.biasmesgen.BiasTableIOException.ACTION;

/**
 * POJO representation of the bias table file that provides a way to both read
 * and write bias table files. Bias tables are written as binary files.
 * Structure of the bias table is based on the fwrite statements in:
 * rary.ohd.pproc/src/biasmesgen/TEXT/create_biastable_mesg.pgc.
 * 
 * <pre>
 * 
 * SOFTWARE HISTORY
 * 
 * Date         Ticket#    Engineer    Description
 * ------------ ---------- ----------- --------------------------
 * May 12, 2016 5576       bkowal      Initial creation
 * Jun 13, 2016 5576       bkowal      Use {@link BiasTableWriteResult}.
 * 
 * </pre>
 * 
 * @author bkowal
 */

public class BiasTableFile {

    private static final String TMP_FILE_EXT = ".temp";

    private BiasTableHeader header;

    private BiasTableSubHeader subHeader;

    private BiasTableDate observationDate;

    private BiasTableDate generationDate;

    private List<BiasTableRow> biasTable = new ArrayList<>(1);

    public static BiasTableFile loadBiasTable(final Path biasTablePath)
            throws BiasTableIOException {
        BiasTableFile biasTable = null;
        try (FileInputStream fis = new FileInputStream(biasTablePath.toFile())) {
            FileChannel fc = fis.getChannel();
            ByteBuffer byteBuffer = fc.map(MapMode.READ_ONLY, 0,
                    fis.available());
            /*
             * File may be in little endian format.
             */
            if (byteBuffer.get(0) == 0x0f && byteBuffer.get(1) == 0x00) {
                byteBuffer.order(ByteOrder.LITTLE_ENDIAN);
            }
            biasTable = new BiasTableFile();
            biasTable.readHeader(byteBuffer);
            biasTable.readSubHeader(byteBuffer);
            biasTable.setObservationDate(biasTable.readDate(byteBuffer));
            biasTable.setGenerationDate(biasTable.readDate(byteBuffer));
            biasTable.readTable(byteBuffer);
        } catch (Exception e) {
            /*
             * Will not only handle general i/o errors; but, it will also handle
             * invalid value assignment. Ex: if the bias table file were to
             * contain a month field set to 64.
             */
            throw new BiasTableIOException(biasTablePath, ACTION.READ, e);
        }

        return biasTable;
    }

    public BiasTableWriteResult writeBiasTable(final Path destinationPath)
            throws BiasTableIOException {
        final Path tmpDestinationPath = Paths.get(destinationPath.getParent()
                .toString(), destinationPath.getFileName().toString()
                + TMP_FILE_EXT);
        /*
         * First verify that all required data exists.
         */
        if (header == null) {
            throw new BiasTableIOException(tmpDestinationPath, ACTION.WRITE,
                    new IllegalStateException(
                            "The bias table header has not been initialized."));
        }
        if (subHeader == null) {
            throw new BiasTableIOException(
                    tmpDestinationPath,
                    ACTION.WRITE,
                    new IllegalStateException(
                            "The bias table sub-header has not been initialized."));
        }
        if (observationDate == null) {
            throw new BiasTableIOException(
                    tmpDestinationPath,
                    ACTION.WRITE,
                    new IllegalStateException(
                            "The bias table observation date has not been initialized."));
        }
        if (generationDate == null) {
            throw new BiasTableIOException(
                    tmpDestinationPath,
                    ACTION.WRITE,
                    new IllegalStateException(
                            "The bias table generation date has not been initialized."));
        }
        ByteBuffer byteBuffer = ByteBuffer.allocate(computeSize());
        /*
         * for now, always write the file as little endian because that is what
         * the legacy code did.
         */
        byteBuffer.order(ByteOrder.LITTLE_ENDIAN);
        writeHeader(byteBuffer);
        writeSubHeader(byteBuffer);
        writeDate(byteBuffer, observationDate);
        writeDate(byteBuffer, generationDate);
        writeTable(byteBuffer);

        try (FileOutputStream fos = new FileOutputStream(
                tmpDestinationPath.toFile())) {
            FileChannel fc = fos.getChannel();

            byteBuffer.rewind();
            fc.write(byteBuffer);
        } catch (IOException e) {
            throw new BiasTableIOException(tmpDestinationPath, ACTION.WRITE, e);
        }

        return new BiasTableWriteResult(byteBuffer.array().length,
                tmpDestinationPath);
    }

    private int computeSize() {
        return 2 + (BiasTableRow.NUM_BYTES * biasTable.size())
                + BiasTableHeader.NUM_BYTES + BiasTableSubHeader.NUM_BYTES
                + (BiasTableDate.NUM_BYTES * 2);
    }

    private void readHeader(final ByteBuffer byteBuffer) {
        header = new BiasTableHeader();
        header.setMessageCode(byteBuffer.getShort());
        header.setMessageDate(byteBuffer.getShort());
        header.setMessageTime(byteBuffer.getInt());
        header.setMessageLength(byteBuffer.getInt());
        header.setSourceId(byteBuffer.getShort());
        header.setDestinationId(byteBuffer.getShort());
        header.setNumberBlocks(byteBuffer.getShort());
    }

    private void writeHeader(final ByteBuffer byteBuffer) {
        byteBuffer.putShort(header.getMessageCode());
        byteBuffer.putShort(header.getMessageDate());
        byteBuffer.putInt(header.getMessageTime());
        byteBuffer.putInt(header.getMessageLength());
        byteBuffer.putShort(header.getSourceId());
        byteBuffer.putShort(header.getDestinationId());
        byteBuffer.putShort(header.getNumberBlocks());
    }

    private void readSubHeader(final ByteBuffer byteBuffer) {
        subHeader = new BiasTableSubHeader();
        subHeader.setDivider(byteBuffer.getShort());
        subHeader.setBlockId(byteBuffer.getShort());
        subHeader.setVersion(byteBuffer.getShort());
        subHeader.setBlockLength(byteBuffer.getShort());
        byte[] strBytes = new byte[BiasTableSubHeader.STRING_LENGTH];
        byteBuffer.get(strBytes);
        subHeader.setBiasSource(new String(strBytes, 0, strBytes.length - 1));
        byteBuffer.get(strBytes);
        subHeader.setRadarId(new String(strBytes, 0, strBytes.length - 1));
    }

    private void writeSubHeader(final ByteBuffer byteBuffer) {
        byteBuffer.putShort(subHeader.getDivider());
        byteBuffer.putShort(subHeader.getBlockId());
        byteBuffer.putShort(subHeader.getVersion());
        byteBuffer.putShort(subHeader.getBlockLength());
        byte[] strBytes = new byte[BiasTableSubHeader.STRING_LENGTH * 2];
        System.arraycopy(subHeader.getBiasSource().getBytes(), 0, strBytes, 0,
                Math.min(subHeader.getBiasSource().length(),
                        BiasTableSubHeader.STRING_LENGTH));
        System.arraycopy(subHeader.getRadarId().getBytes(), 0, strBytes,
                BiasTableSubHeader.STRING_LENGTH, Math.min(subHeader
                        .getRadarId().length(),
                        BiasTableSubHeader.STRING_LENGTH));
        byteBuffer.put(strBytes);
    }

    private BiasTableDate readDate(final ByteBuffer byteBuffer) {
        short year = byteBuffer.getShort();
        short month = byteBuffer.getShort();
        short day = byteBuffer.getShort();
        short hour = byteBuffer.getShort();
        short minute = byteBuffer.getShort();
        short second = byteBuffer.getShort();

        return new BiasTableDate((int) year, (int) month, (int) day,
                (int) hour, (int) minute, (int) second);
    }

    private void writeDate(final ByteBuffer byteBuffer,
            final BiasTableDate biasTableDate) {
        byteBuffer.putShort(biasTableDate.getYear());
        byteBuffer.putShort(biasTableDate.getMonth());
        byteBuffer.putShort(biasTableDate.getDay());
        byteBuffer.putShort(biasTableDate.getHour());
        byteBuffer.putShort(biasTableDate.getMinute());
        byteBuffer.putShort(biasTableDate.getSecond());
    }

    private void readTable(final ByteBuffer byteBuffer) {
        final int nrows = (int) byteBuffer.getShort();
        if (nrows <= 0) {
            return;
        }
        biasTable = new ArrayList<>(nrows);
        for (int i = 0; i < nrows; i++) {
            int memorySpan = byteBuffer.getInt();
            int numPairs = byteBuffer.getInt();
            int sumGag = byteBuffer.getInt();
            int sumRad = byteBuffer.getInt();
            int bias = byteBuffer.getInt();

            biasTable.add(new BiasTableRow(memorySpan, numPairs, sumGag,
                    sumRad, bias));
        }
    }

    private void writeTable(final ByteBuffer byteBuffer) {
        /*
         * First write the number of rows in the table.
         */
        byteBuffer.putShort((short) getNumberOfRows());
        for (BiasTableRow biasTableRow : biasTable) {
            byteBuffer.putInt(biasTableRow.getMemorySpan());
            byteBuffer.putInt(biasTableRow.getNumPairs());
            byteBuffer.putInt(biasTableRow.getSumGag());
            byteBuffer.putInt(biasTableRow.getSumRad());
            byteBuffer.putInt(biasTableRow.getBias());
        }
    }

    public int getNumberOfRows() {
        return biasTable.size();
    }

    public void addRow(BiasTableRow biasTableRow) {
        biasTable.add(biasTableRow);
    }

    public BiasTableHeader getHeader() {
        return header;
    }

    public void setHeader(BiasTableHeader header) {
        this.header = header;
    }

    public BiasTableSubHeader getSubHeader() {
        return subHeader;
    }

    public void setSubHeader(BiasTableSubHeader subHeader) {
        this.subHeader = subHeader;
    }

    public BiasTableDate getObservationDate() {
        return observationDate;
    }

    public void setObservationDate(BiasTableDate observationDate) {
        this.observationDate = observationDate;
    }

    public BiasTableDate getGenerationDate() {
        return generationDate;
    }

    public void setGenerationDate(BiasTableDate generationDate) {
        this.generationDate = generationDate;
    }

    public List<BiasTableRow> getBiasTable() {
        return biasTable;
    }

    public void setBiasTable(List<BiasTableRow> biasTable) {
        this.biasTable = biasTable;
    }
}