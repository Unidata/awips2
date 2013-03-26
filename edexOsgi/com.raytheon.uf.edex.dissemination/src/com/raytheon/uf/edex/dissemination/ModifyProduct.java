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
package com.raytheon.uf.edex.dissemination;

import java.text.SimpleDateFormat;
import java.util.Date;
import java.util.List;

import com.raytheon.edex.plugin.text.dao.AfosToAwipsDao;
import com.raytheon.uf.common.dataplugin.text.db.AfosToAwips;
import com.raytheon.uf.common.dissemination.OUPRequest;
import com.raytheon.uf.common.dissemination.OfficialUserProduct;
import com.raytheon.uf.edex.database.DataAccessLayerException;
import com.raytheon.uf.edex.dissemination.transmitted.TransProdHeader;
import com.raytheon.uf.edex.dissemination.transmitted.TransmittedProductList;

/**
 * Utilities for generating a wmo header or tracking headers
 * 
 * <pre>
 * 
 * SOFTWARE HISTORY
 * Date         Ticket#    Engineer    Description
 * ------------ ---------- ----------- --------------------------
 * Nov 13, 2009            njensen     Initial creation
 * 08/20/2012   DR 15340   D. Friedman Fix BBB problems
 * 03/08/2013	15564   mgamazaychikov Trimmed extra spaces in afosId
 * 
 * </pre>
 * 
 * @author njensen
 * @version 1.0
 */

public class ModifyProduct {

    private static final SimpleDateFormat DDHHMM = new SimpleDateFormat(
            "ddHHmm");

    public static TransProdHeader getProductHeader(OfficialUserProduct product)
            throws OUPHeaderException {
        TransProdHeader header = null;
        String[] splitLines = product.getProductText().split("\n");
        String[] firstLine = splitLines[0].split(" ");
        if (firstLine.length < 3) {
            throw new OUPHeaderException("Bad wmo header on product: "
                    + splitLines[0]);
        }
        String ttaaii = firstLine[0];
        String cccc = firstLine[1];
        String productTime = firstLine[2];
        AfosToAwipsDao dao = new AfosToAwipsDao();
        try {
            String productAwipsId = product.getAwipsWanPil().substring(4);
            List<AfosToAwips> list = dao.lookupAfosId(ttaaii, cccc).getIdList();
            String productId = null;
            for (AfosToAwips ata : list) {
                String afosId = ata.getAfosid().trim();
                String awipsId = afosId.substring(3);
                if (awipsId.equals(productAwipsId)) {
                    productId = afosId;
                    break;
                }
            }
            if (productId != null) {
                String wmoId = ttaaii + " " + cccc;
                String bbbid = product.getWmoType();
                header = new TransProdHeader(productId, wmoId, productTime,
                        bbbid);
            } else {
                throw new OUPHeaderException(
                        "Error determining afosID.  No matching afosID found for ttaaii "
                                + ttaaii + " cccc " + cccc + " afosID %"
                                + productAwipsId);
            }
        } catch (DataAccessLayerException e) {
            throw new OUPHeaderException("Error accessing afos_to_awips", e);
        }

        return header;
    }

    public static OUPRequest addWmoHeader(OUPRequest req)
            throws OUPHeaderException {
        String text = req.getProduct().getProductText();
        String awipsWanPil = req.getProduct().getAwipsWanPil();
        String cccc = awipsWanPil.substring(0, 4);
        String nnn = awipsWanPil.substring(4, 7);
        String xxx = null;
        if (awipsWanPil.length() >= 10) {
            xxx = awipsWanPil.substring(7, 10);
        } else {
            xxx = awipsWanPil.substring(7);
        }

        AfosToAwipsDao dao = new AfosToAwipsDao();
        List<AfosToAwips> list = dao.lookupAfosId(cccc, nnn, xxx.trim())
                .getIdList();
        if (list.size() == 1) {
            StringBuilder sb = new StringBuilder();
            sb.append(list.get(0).getWmottaaii());
            sb.append(" ");
            sb.append(list.get(0).getWmocccc());
            sb.append(" ");
            if (req.getProduct().getUserDateTimeStamp() != null) {
                sb.append(req.getProduct().getUserDateTimeStamp());
            } else {
                synchronized (DDHHMM) {
                    sb.append(DDHHMM.format(new Date()));
                }
            }
            if (req.getProduct().getWmoType() != null
                    && req.getProduct().getWmoType().length() > 0) {
                sb.append(" ");
                sb.append(req.getProduct().getWmoType());
            }
            sb.append("\n");
            sb.append(nnn);
            sb.append(xxx);
            sb.append("\n");
            sb.append(text);

            req.getProduct().setProductText(sb.toString());
            req.getProduct().setNeedsWmoHeader(false);

            return req;
        } else if (list.size() == 0) {
            throw new OUPHeaderException(
                    "Error building WMO header.  No matching ttaaii found for cccc "
                            + cccc + " nnn " + nnn + " xxx " + xxx);
        } else {
            throw new OUPHeaderException(
                    "Error building WMO header.  Too many matching ttaaii found for cccc "
                            + cccc + " nnn " + nnn + " xxx " + xxx);
        }
    }

    public static boolean checkBBBField(OfficialUserProduct product,
            TransProdHeader header) throws DataAccessLayerException {
        boolean changed = false;
        String productBBB = header.getBbb();
        String[] splitLines = product.getProductText().split("\n", 2);
        String bbbToUse = TransmittedProductList.getBBB(header.getProductId(),
                header.getWmoId(), header.getProductTime(), header.getBbb());

        if (!productBBB.equals(bbbToUse)) {
            productBBB = bbbToUse;
        }

        if (productBBB != null) {
            // if the BBB is already in the wmo header do not append
            if (!splitLines[0].endsWith(" " + productBBB)) {
                splitLines[0] += " " + productBBB;
                StringBuilder sb = new StringBuilder();
                boolean first = true;
                for (String line : splitLines) {
                    if (first)
                        first = false;
                    else
                        sb.append("\n");
                    sb.append(line);
                }
                product.setProductText(sb.toString());
                changed = true;
            }
            header.setBbb(productBBB);
        }
        product.setWmoType(productBBB);

        return changed;
    }

    public static String convertNewline2rrn(String textString) {
        StringBuffer newString = new StringBuffer();

        // Don't do any change if string doesn't contain any newline
        if (textString.contains("\n") == false) {
            return textString;
        }

        String[] lines = textString.split("\n");

        for (String line : lines) {
            int length = line.length();

            // The index of the first "\n" is bigger than 1
            if (length > 1) {

                // "...xx\n" case
                if (line.charAt(length - 1) != '\r') {
                    // replace with "...xx\r\r\n"
                    newString.append(line.substring(0, length));
                    newString.append("\r\r\n");

                    // "...x\r\n" and case
                } else if (line.charAt(length - 2) != '\r') {
                    // replace with ""...x\r\r\n"
                    newString.append(line.substring(0, length - 1));
                    newString.append("\r\r\n");

                    // "...\r\r\n" case
                } else {
                    // jusy copy "..."
                    newString.append(line);
                }

                // "\r\n" and "x\n" case
            } else if (length == 1) {
                char char0 = line.charAt(0);

                // copy the "x" if is the "x\n" case
                if (char0 != '\r') {
                    newString.append(char0);
                }

                newString.append("\r\r\n");

                // "\n" case
            } else {
                newString.append("\r\r\n");
            }

        }

        return newString.toString();
    }
}
