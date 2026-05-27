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
package com.raytheon.uf.common.nc.bufr.util;

import java.io.IOException;
import java.io.InputStream;
import java.util.ArrayList;
import java.util.List;

import org.jdom2.Element;
import org.jdom2.JDOMException;
import org.jdom2.input.SAXBuilder;

import ucar.nc2.iosp.bufr.tables.BufrTables;
import ucar.nc2.iosp.bufr.tables.TableD;
import ucar.nc2.iosp.bufr.tables.WmoXmlReader;
import ucar.nc2.wmo.Util;

/**
 * The {@link TableD}s that are created in {@link BufrTables} for wmo versions
 * 17 and 18 are missing descriptors. This class will reread those table files
 * and will handle more descriptors than the default implementation.
 * 
 * Specifically the problem with the tables is that there are some entries in
 * the xml definitions provided by the WMO that are missing the ElementName_en
 * element. The default implementation will completely skip the descriptors with
 * a missing name, when these groups are read the missing descriptors cause the
 * read to become misaligned from the descriptors and garbage comes out. The
 * solution in this class is to use the ExistingElementName_en as the name when
 * ElementName_en is missing. Most entries include both of these elements and
 * they are the same, so it seems reasonable that they are interchangeable. I
 * can't find any documentation from the WMO indicating why these two duplicate
 * elements exist, however starting at table version 19 the
 * ExistingElementName_en has been completely removed and the problems seems
 * resolved.
 * 
 * 
 * This class was written specifically for UCAR BUFR IOSP 4.6.10. Newer versions
 * of BUFR IOSP might make this unnecessary.
 * 
 * <pre>
 * 
 * SOFTWARE HISTORY
 * 
 * Date         Ticket#    Engineer    Description
 * ------------ ---------- ----------- --------------------------
 * Sep 11, 2017  6406      bsteffen     Initial creation
 * 
 * </pre>
 * 
 * @author bsteffen
 */
public class BufrTableFixer {

    private static final int[] VERSIONS_TO_FIX = { 17, 18 };

    /*
     * The tables are stored statically so they only need to be fixed once.
     * Track that here so that callers can just fix whenever is convenient.
     */
    private static boolean fixed = false;

    public static synchronized void fix() throws IOException {
        if (fixed) {
            return;
        }
        for (int version : VERSIONS_TO_FIX) {
            TableD table = BufrTables.getWmoTableD(version);
            String location = table.getLocation();
            InputStream ios = null;
            if (location.startsWith("resource:")) {
                location = location.substring(9);
                ios = BufrTables.class.getResourceAsStream(location);
            }
            if (ios != null) {
                readWmoXmlTableD(ios, table);
            }
        }
        fixed = true;
    }

    /**
     * This is exactly like
     * {@link WmoXmlReader#readWmoXmlTableD(InputStream, TableD)} except when
     * the ElementName_en is not present it will fall back to
     * ExistingElementName_en
     */
    static void readWmoXmlTableD(InputStream ios, TableD tableD)
            throws IOException {
        org.jdom2.Document doc;
        try {
            SAXBuilder builder = new SAXBuilder();
            doc = builder.build(ios);
        } catch (JDOMException e) {
            throw new IOException(e.getMessage(), e);
        }

        int currSeqno = -1;
        TableD.Descriptor currDesc = null;

        Element root = doc.getRootElement();

        List<Element> featList = root.getChildren();
        for (Element elem : featList) {
            String nameElement = "ElementName_en";
            Element ce = elem.getChild(nameElement);
            if (ce == null) {
                nameElement = "ExistingElementName_en";
                ce = elem.getChild(nameElement);
                if (ce == null) {
                    continue;
                }
            }

            String seqs = elem.getChildTextNormalize("FXY1");
            int seq = Integer.parseInt(seqs);

            if (currSeqno != seq) {
                int y = seq % 1000;
                int w = seq / 1000;
                int x = w % 100;
                String seqName = Util
                        .cleanName(elem.getChildTextNormalize(nameElement));
                currDesc = tableD.addDescriptor((short) x, (short) y, seqName,
                        new ArrayList<Short>());
                currSeqno = seq;
            }

            String fnos = elem.getChildTextNormalize("FXY2");
            int fno = Integer.parseInt(fnos);
            int y = fno % 1000;
            int w = fno / 1000;
            int x = w % 100;
            int f = w / 100;
            int fxy = (f << 14) + (x << 8) + y;
            currDesc.addFeature((short) fxy);
        }
        ios.close();
    }
}
