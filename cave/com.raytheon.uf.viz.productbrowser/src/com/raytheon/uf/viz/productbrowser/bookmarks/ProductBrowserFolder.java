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

import java.util.ArrayList;
import java.util.List;

import javax.xml.bind.annotation.XmlAccessType;
import javax.xml.bind.annotation.XmlAccessorType;
import javax.xml.bind.annotation.XmlAttribute;
import javax.xml.bind.annotation.XmlElement;
import javax.xml.bind.annotation.XmlRootElement;

import com.raytheon.uf.common.serialization.ISerializableObject;
import com.raytheon.uf.viz.productbrowser.AbstractRequestableProductBrowserDataDefinition;

/**
 * Implementation of the product browser folder, contains more folders and
 * bookmarks
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

@XmlRootElement(name = "folder")
@XmlAccessorType(XmlAccessType.NONE)
public class ProductBrowserFolder implements ISerializableObject {

    @XmlElement
    List<ProductBrowserFolder> folders = new ArrayList<ProductBrowserFolder>();

    @XmlElement
    List<ProductBrowserBookmark<AbstractRequestableProductBrowserDataDefinition<?>>> bookmarks = new ArrayList<ProductBrowserBookmark<AbstractRequestableProductBrowserDataDefinition<?>>>();

    @XmlAttribute
    private String folderName;

    public ProductBrowserFolder() {
    }

    public void addFolder(ProductBrowserFolder folder) {
        folders.add(folder);
    }

    public void removeBookmark(
            ProductBrowserBookmark<AbstractRequestableProductBrowserDataDefinition<?>> bookmark) {
        bookmarks.remove(bookmark);
    }

    public void addBookmark(
            ProductBrowserBookmark<AbstractRequestableProductBrowserDataDefinition<?>> bookmark) {
        bookmarks.add(bookmark);
    }

    public List<ProductBrowserBookmark<AbstractRequestableProductBrowserDataDefinition<?>>> getBookmarks() {
        return bookmarks;
    }

    public List<ProductBrowserFolder> getFolders() {
        return folders;
    }

    public String getFolderName() {
        return folderName;
    }

    /*
     * (non-Javadoc)
     * 
     * @see java.lang.Object#toString()
     */
    @Override
    public String toString() {
        return getFolderName();
    }

    @Override
    public int hashCode() {
        final int prime = 31;
        int result = 1;
        result = prime * result
                + ((bookmarks == null) ? 0 : bookmarks.hashCode());
        result = prime * result + ((folders == null) ? 0 : folders.hashCode());
        return result;
    }

    @Override
    public boolean equals(Object obj) {
        if (this == obj)
            return true;
        if (obj == null)
            return false;
        if (getClass() != obj.getClass())
            return false;
        ProductBrowserFolder other = (ProductBrowserFolder) obj;
        if (bookmarks == null) {
            if (other.bookmarks != null)
                return false;
        } else if (!bookmarks.equals(other.bookmarks))
            return false;
        if (folders == null) {
            if (other.folders != null)
                return false;
        } else if (!folders.equals(other.folders))
            return false;
        return true;
    }

}
