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
package com.raytheon.uf.common.mpe.util;

import java.awt.Rectangle;
import java.io.File;
import java.io.FileInputStream;
import java.io.FileOutputStream;
import java.io.IOException;
import java.nio.ByteBuffer;
import java.nio.ByteOrder;
import java.nio.channels.FileChannel;
import java.nio.channels.FileChannel.MapMode;
import java.text.ParseException;
import java.text.SimpleDateFormat;
import java.util.Arrays;
import java.util.Date;
import java.util.TimeZone;

import javax.xml.bind.annotation.XmlAccessType;
import javax.xml.bind.annotation.XmlAccessorType;
import javax.xml.bind.annotation.XmlElement;

/**
 * This class supports reading and writing of XMRG formatted files
 * 
 * <pre>
 * SOFTWARE HISTORY
 * Date         Ticket#    Engineer    Description
 * ------------ ---------- ----------- --------------------------
 * Sep 30, 2008            randerso     Initial creation
 * May 20, 2014 2913       bsteffen     Remove main
 * 
 * </pre>
 * 
 * @author randerso
 * @version 1.0
 */
@XmlAccessorType(XmlAccessType.NONE)
public class XmrgFile {

    private Rectangle hrapExtent;

    private XmrgHeader header;

    private short[] data;

	@XmlElement
    private File file;

    private static SimpleDateFormat sdf;
    static {
        sdf = new SimpleDateFormat("yyyy-MM-dd HH:mm:ss");
        sdf.setTimeZone(TimeZone.getTimeZone("GMT"));
    }

	public XmrgFile() {

	}

    public XmrgFile(String fileName) {
        this(new File(fileName));
    }

    public XmrgFile(File file) {
        this.file = file;
    }

    public void load() throws IOException {
        FileInputStream fis = new FileInputStream(file);
        FileChannel fc = fis.getChannel();
        ByteBuffer byteBuf = fc.map(MapMode.READ_ONLY, 0, fis.available());

        readExtent(byteBuf);
        readHeader(byteBuf);
        readData(byteBuf);

        fis.close();
    }

    public void save(String fileName) throws IOException {
        save(new File(fileName));
    }

    public void save(File file) throws IOException {
        FileOutputStream fos = new FileOutputStream(file);
        FileChannel fc = fos.getChannel();

        int size = computeSize();
        ByteBuffer byteBuf = ByteBuffer.allocate(size);

        writeExtent(byteBuf);
        writeHeader(byteBuf);
        writeData(byteBuf);

        byteBuf.rewind();
        fc.write(byteBuf);
        fos.close();
    }

    private int computeSize() {
        return (16 + 8) + (66 + 8) + (2 * hrapExtent.width + 8)
                * hrapExtent.height;
    }

    public Rectangle getHrapExtent() {
        return hrapExtent;
    }

    public void setHrapExtent(Rectangle hrapExtent) {
        this.hrapExtent = hrapExtent;
    }

    public XmrgHeader getHeader() {
        return header;
    }

    public void setHeader(XmrgHeader header) {
        this.header = header;
    }

    public short[] getData() {
        return Arrays.copyOf(data, data.length);
    }

    /**
     * Retrieve a rectangular subset of the data as a two dimensional array.
     * 
     * @param rect
     *            Rectangle defining the requested data in HRAP coordinates
     * @return two dimensional array of short containing the desired data
     * @throws IllegalArgumentException
     *             if rect is not fully contained in the HRAP extent of this
     *             file
     */
    public short[][] getData(Rectangle rect) {

        if (hrapExtent.contains(rect)) {
            short[][] retVal = new short[rect.height][];

            int x = rect.x - hrapExtent.x;
            int y = hrapExtent.height - 1
                    - (rect.y + rect.height - 1 - hrapExtent.y);

            for (int i = 0; i < +rect.height; i++) {
                retVal[i] = new short[rect.width];
                System.arraycopy(data, (y + i) * hrapExtent.width + x,
                        retVal[i], 0, rect.width);
            }
            return retVal;
        }

        throw new IllegalArgumentException(
                "Requested rectangle must be completely contained in the hrapExtent");
    }

    public void setData(short[] data) {
        this.data = data;
    }

    private void readExtent(ByteBuffer byteBuf) throws IOException {
        byteBuf.order(ByteOrder.LITTLE_ENDIAN);
        int len = byteBuf.getInt();
        if (len != 16) {
            byteBuf.order(ByteOrder.BIG_ENDIAN);
            byteBuf.rewind();
            len = byteBuf.getInt();
        }

        if (len != 16) {
            throw new IOException(
                    "Invalid record length encountered in xmrg file");
        }

        int xOrig = byteBuf.getInt();
        int yOrig = byteBuf.getInt();
        int maxX = byteBuf.getInt();
        int maxY = byteBuf.getInt();

        len = byteBuf.getInt();
        if (len != 16) {
            throw new IOException(
                    "Invalid record length encountered in xmrg file");
        }

        hrapExtent = new Rectangle(xOrig, yOrig, maxX, maxY);
    }

    private void writeExtent(ByteBuffer byteBuf) throws IOException {
        byteBuf.order(ByteOrder.LITTLE_ENDIAN);
        byteBuf.putInt(16);

        byteBuf.putInt(hrapExtent.x);
        byteBuf.putInt(hrapExtent.y);
        byteBuf.putInt(hrapExtent.width);
        byteBuf.putInt(hrapExtent.height);

        byteBuf.putInt(16);
    }

    private void readHeader(ByteBuffer byteBuf) throws IOException {
        int len = byteBuf.getInt();

        byte[] bytes = new byte[58];
        byteBuf.get(bytes, 0, 58);
        int maxValue = byteBuf.getInt();
        float versionNumber = byteBuf.getFloat();

        if (byteBuf.getInt() != len) {
            throw new IOException(
                    "Invalid record length encountered in xmrg file");
        }

        header = new XmrgHeader();

        header.operatingSystem = new String(bytes, 0, 2);
        header.userId = new String(bytes, 2, 8);
        int pos = header.userId.indexOf(0);
        if (pos != -1) {
            header.userId = header.userId.substring(0, pos);
        }

        String date = new String(bytes, 10, 20);
        try {
            header.saveDate = sdf.parse(date);
        } catch (ParseException e) {
            header.saveDate = new Date(0);
        }

        header.processFlag = new String(bytes, 30, 8);
        pos = header.processFlag.indexOf(0);
        if (pos != -1) {
            header.processFlag = header.processFlag.substring(0, pos);
        }

        try {
            date = new String(bytes, 38, 20);
            header.validDate = sdf.parse(date);
        } catch (ParseException e) {
            header.validDate = new Date(0);
        }

        header.maxValue = maxValue;
        header.versionNumber = versionNumber;
    }

    private void writeHeader(ByteBuffer byteBuf) throws IOException {
        byte[] bytes = new byte[58];
        Arrays.fill(bytes, (byte) 0);

        System.arraycopy(header.operatingSystem.getBytes(), 0, bytes, 0, Math
                .max(header.operatingSystem.length(), 2));

        System.arraycopy(header.userId.getBytes(), 0, bytes, 2, Math.min(
                header.userId.length(), 8));

        String date = sdf.format(header.saveDate);
        System.arraycopy(date.getBytes(), 0, bytes, 10, Math.min(date.length(),
                20));

        System.arraycopy(header.processFlag.getBytes(), 0, bytes, 30, Math.min(
                header.processFlag.length(), 8));

        date = sdf.format(header.validDate);
        System.arraycopy(date.getBytes(), 0, bytes, 38, Math.min(date.length(),
                20));

        byteBuf.putInt(66);
        byteBuf.put(bytes);
        byteBuf.putInt(header.maxValue);
        byteBuf.putFloat(header.versionNumber);
        byteBuf.putInt(66);
    }

    private void readData(ByteBuffer byteBuf) throws IOException {
        data = new short[hrapExtent.width * hrapExtent.height];

        int expectedLen = hrapExtent.width * 2;

        for (int y = hrapExtent.height - 1; y >= 0; y--) {

            int len = byteBuf.getInt();
            if (len != expectedLen) {
                throw new IOException(
                        "Invalid record length encountered in xmrg file");
            }

            for (int x = 0; x < hrapExtent.width; x++) {
                data[y * hrapExtent.width + x] = byteBuf.getShort();
            }

            len = byteBuf.getInt();

            if (len != expectedLen) {
                throw new IOException(
                        "Invalid record length encountered in xmrg file");
            }
        }
    }

    private void writeData(ByteBuffer byteBuf) throws IOException {
        int len = hrapExtent.width * 2;

        for (int y = hrapExtent.height - 1; y >= 0; y--) {
            byteBuf.putInt(len);

            for (int x = 0; x < hrapExtent.width; x++) {
                byteBuf.putShort(data[y * hrapExtent.width + x]);
            }

            byteBuf.putInt(len);
        }
    }

    public static class XmrgHeader {
        private String operatingSystem;

        private String userId;

        private Date saveDate;

        private String processFlag;

        private Date validDate;

        private int maxValue;

        private float versionNumber;

        @Override
        public String toString() {
            StringBuffer s = new StringBuffer();

            s.append("XmrgFile[OS:" + operatingSystem);
            s.append(", User:" + userId);
            s.append(", Saved:" + sdf.format(saveDate));
            s.append(", Process:" + processFlag);
            s.append(", Valid:" + sdf.format(validDate));
            s.append(", Max:" + maxValue);
            s.append(", Version:" + versionNumber + "]");

            return s.toString();
        }

        public String getOperatingSystem() {
            return operatingSystem;
        }

        public String getUserId() {
            return userId;
        }

        public Date getSaveDate() {
            return saveDate;
        }

        public String getProcessFlag() {
            return processFlag;
        }

        public Date getValidDate() {
            return validDate;
        }

        public int getMaxValue() {
            return maxValue;
        }

        public float getVersionNumber() {
            return versionNumber;
        }

        /**
         * @param operatingSystem
         *            the operatingSystem to set
         */
        public void setOperatingSystem(String operatingSystem) {
            this.operatingSystem = operatingSystem;
        }

        /**
         * @param userId
         *            the userId to set
         */
        public void setUserId(String userId) {
            this.userId = userId;
        }

        /**
         * @param saveDate
         *            the saveDate to set
         */
        public void setSaveDate(Date saveDate) {
            this.saveDate = saveDate;
        }

        /**
         * @param processFlag
         *            the processFlag to set
         */
        public void setProcessFlag(String processFlag) {
            this.processFlag = processFlag;
        }

        /**
         * @param validDate
         *            the validDate to set
         */
        public void setValidDate(Date validDate) {
            this.validDate = validDate;
        }

        /**
         * @param maxValue
         *            the maxValue to set
         */
        public void setMaxValue(int maxValue) {
            this.maxValue = maxValue;
        }

        /**
         * @param versionNumber
         *            the versionNumber to set
         */
        public void setVersionNumber(float versionNumber) {
            this.versionNumber = versionNumber;
        }
    }

    /**
     * @return the file
     */
    public File getFile() {
        return file;
    }
}
