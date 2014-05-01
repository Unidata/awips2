package com.raytheon.uf.viz.productbrowser.bookmarks;

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

import java.util.HashMap;

import javax.xml.bind.annotation.XmlAccessType;
import javax.xml.bind.annotation.XmlAccessorType;
import javax.xml.bind.annotation.XmlAttribute;
import javax.xml.bind.annotation.XmlElement;
import javax.xml.bind.annotation.XmlRootElement;
import javax.xml.bind.annotation.adapters.XmlJavaTypeAdapter;

import com.raytheon.uf.common.dataquery.requests.RequestConstraint;
import com.raytheon.uf.common.dataquery.requests.RequestableMetadataMarshaller;
import com.raytheon.uf.common.serialization.ISerializableObject;
import com.raytheon.uf.viz.productbrowser.AbstractRequestableProductBrowserDataDefinition;

/**
 * Implementation of the product browser bookmark, uses request constriants
 * 
 * <pre>
 * 
 * SOFTWARE HISTORY
 * Date         Ticket#    Engineer    Description
 * ------------ ---------- ----------- --------------------------
 * June 4, 2010            mnash     Initial creation
 * 
 * </pre>
 * 
 * @author mnash
 * @version 1.0
 */

@XmlRootElement(name = "bookmark")
@XmlAccessorType(XmlAccessType.NONE)
public class ProductBrowserBookmark<T extends AbstractRequestableProductBrowserDataDefinition<?>>
        implements ISerializableObject {

    @XmlAttribute
    private String bookmarkName;

    @XmlJavaTypeAdapter(value = RequestableMetadataMarshaller.class)
    private HashMap<String, RequestConstraint> requestConstraints;

    @XmlElement
    private T resourceClass;

    public ProductBrowserBookmark() {

    }

    public ProductBrowserBookmark(String bookmarkName,
            HashMap<String, RequestConstraint> requestConstraints,
            T resourceClass) {
        this.bookmarkName = bookmarkName;
        this.requestConstraints = requestConstraints;
        this.resourceClass = resourceClass;
    }

    public String getBookmarkName() {
        return bookmarkName;
    }

    public HashMap<String, RequestConstraint> getRequestConstraints() {
        return requestConstraints;
    }

    public T getResourceClass() {
        return resourceClass;
    }

    /*
     * (non-Javadoc)
     * 
     * @see java.lang.Object#toString()
     */
    @Override
    public String toString() {
        return getBookmarkName();
    }
}
