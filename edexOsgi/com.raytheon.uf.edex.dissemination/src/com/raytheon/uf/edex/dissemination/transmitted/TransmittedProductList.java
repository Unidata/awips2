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
package com.raytheon.uf.edex.dissemination.transmitted;

import java.util.ArrayList;
import java.util.List;

import com.raytheon.uf.common.status.IUFStatusHandler;
import com.raytheon.uf.common.status.UFStatus;
import com.raytheon.uf.common.status.UFStatus.Priority;
import com.raytheon.uf.edex.dissemination.StatusConstants;

/**
 * TODO Add Description
 * 
 * <pre>
 * 
 * SOFTWARE HISTORY
 * Date         Ticket#    Engineer    Description
 * ------------ ---------- ----------- --------------------------
 * Nov 10, 2009            njensen     Initial creation
 * 08/20/2012   DR 15340   D. Friedman Fix BBB problems
 * 
 * </pre>
 * 
 * @author njensen
 * @version 1.0
 */

public class TransmittedProductList {
    private static final transient IUFStatusHandler statusHandler = UFStatus.getHandler(TransmittedProductList.class);

    private static List<TransProdHeader> transmittedProdList = new ArrayList<TransProdHeader>();

    public static String getBBB(String productId, String wmoId,
            String productTime, String productBBB) {
        // If the user has assigned a value to the BBB field, just pass the
        // product
        // through without incrementing the BBB value.
        if (productBBB.length() == 3) {
            String left2 = productBBB.substring(0, 2);
            if (left2.equals("AA") || left2.equals("CC") || left2.equals("RR"))
                return productBBB;
        }

        // Search the list for a match with the specified product header.
        synchronized (transmittedProdList) {
            for (TransProdHeader tph : transmittedProdList) {
                if (tph.matches(productId, wmoId)
                        && productTime.equals(tph.getProductTime())) {
                    statusHandler.handle(Priority.VERBOSE,

                            "Product match found in Transmitted Product List");
                    // Assign the correct BBB.
                    String newBBB = assignBBB(productBBB, tph.getBbb());
                    return newBBB;
                }
            }
        }

        // If there's no entry in the list for this product, return null. This
        // will
        // be the first product issued, and should have an empty BBB field.
        statusHandler.handle(Priority.VERBOSE,
                "Product header not found in Transmitted Product list.");
        return null;
    }

    private static String assignBBB(String productBBB, String transmittedBBB) {
        if (transmittedBBB == null || transmittedBBB.length() == 0)
            return "RRA";

        String newBBB = null;
        char[] newX = new char[] { transmittedBBB.charAt(2) };
        if (newX[0] == 'X') {
            newX[0] = 'A';
        } else {
            newX[0]++;
        }
        newBBB = transmittedBBB.substring(0, 2) + new String(newX);

        return newBBB;
    }

    public static void addProduct(String productId, String wmoId,
            String productTime, String productBBB) {
        // Don't save products with CCX or AAX in the BBB field. These are not
        // currently being tracked.
        if (productBBB.length() == 3) {
            String left2 = productBBB.substring(0, 2);
            if (left2.equals("AA") || left2.equals("CC"))
                return;
        }

        // Create a TransProdHeader object to put in the list
        TransProdHeader prodHeader = new TransProdHeader(productId, wmoId,
                productTime, productBBB);

        // See if this product is already in the list.
        synchronized (transmittedProdList) {
            for (int i = 0; i < transmittedProdList.size(); i++) {
                if (transmittedProdList.get(i).matches(productId, wmoId)) {
                    statusHandler.handle(Priority.VERBOSE,
                            "Replacing product " + productId
                                    + " in Transmitted Product List");
                    transmittedProdList.remove(i);
                    transmittedProdList.add(prodHeader);
                    return;
                }
            }

            statusHandler.handle(Priority.VERBOSE,
                    "Adding new product " + productId
                            + " to Transmitted Product List");
            transmittedProdList.add(prodHeader);
        }
    }
}
