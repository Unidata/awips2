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
package com.raytheon.uf.edex.datadelivery.retrieval;

/**
 * Domain object for a specific LinkStore associated to a Provider.
 * 
 * <pre>
 * 
 * SOFTWARE HISTORY
 * 
 * Date         Ticket#    Engineer    Description
 * ------------ ---------- ----------- --------------------------
 * Jul 17, 2012 740        djohnson     Initial creation
 * 
 * </pre>
 * 
 * @author djohnson
 * @version 1.0
 */

public class ProviderCollectionLinkStore {
    private final LinkStore linkStore;

    private final String providerName;

    private final String collectionName;

    public ProviderCollectionLinkStore(String providerName,
            String collectionName, LinkStore linkStore) {
        this.providerName = providerName;
        this.collectionName = collectionName;
        this.linkStore = linkStore;
    }

    /**
     * @return the linkStore
     */
    public LinkStore getLinkStore() {
        return linkStore;
    }

    /**
     * @return the providerName
     */
    public String getProviderName() {
        return providerName;
    }

    /**
     * @return the collectionName
     */
    public String getCollectionName() {
        return collectionName;
    }
}
