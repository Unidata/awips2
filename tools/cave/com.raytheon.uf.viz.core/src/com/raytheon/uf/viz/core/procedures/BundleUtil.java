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
package com.raytheon.uf.viz.core.procedures;

import java.util.HashSet;
import java.util.Map;
import java.util.Set;

import com.raytheon.uf.common.dataquery.requests.RequestConstraint;
import com.raytheon.uf.common.time.BinOffset;
import com.raytheon.uf.viz.core.drawables.AbstractRenderableDisplay;
import com.raytheon.uf.viz.core.drawables.IDescriptor;
import com.raytheon.uf.viz.core.drawables.ResourcePair;
import com.raytheon.uf.viz.core.rsc.AbstractRequestableResourceData;
import com.raytheon.uf.viz.core.rsc.AbstractResourceData;
import com.raytheon.uf.viz.core.rsc.IResourceGroup;
import com.raytheon.uf.viz.core.rsc.ResourceList;

/**
 * Utilities for manipulating bundles
 * 
 * <pre>
 * 
 * SOFTWARE HISTORY
 * Date         Ticket#    Engineer    Description
 * ------------ ---------- ----------- --------------------------
 * Sep 18, 2009            chammack     Initial creation
 * 
 * </pre>
 * 
 * @author chammack
 * @version 1.0
 */

public class BundleUtil {

    private BundleUtil() {

    }

    public static Set<BundleDataItem> extractMetadata(Bundle b) {
        Set<BundleDataItem> list = new HashSet<BundleDataItem>();
        if (b.getDisplays() != null) {
            for (AbstractRenderableDisplay ard : b.getDisplays()) {
                IDescriptor descriptor = ard.getDescriptor();
                if (descriptor != null) {
                    ResourceList rl = descriptor.getResourceList();
                    if (rl != null) {
                        recursiveResourceSearch(rl, list);
                    }
                }
            }
        }
        return list;
    }

    private static void recursiveResourceSearch(ResourceList rl,
            Set<BundleDataItem> list) {
        for (ResourcePair rp : rl) {
            AbstractResourceData rd = rp.getResourceData();
            if (rd != null) {
                if (rd instanceof AbstractRequestableResourceData) {
                    AbstractRequestableResourceData rrd = (AbstractRequestableResourceData) rd;
                    Map<String, RequestConstraint> rc = rrd.getMetadataMap();
                    if (rc != null) {
                        list.add(new BundleDataItem(rc, rrd.getBinOffset()));
                    }
                } else if (rd instanceof IResourceGroup) {
                    ResourceList rl2 = ((IResourceGroup) rd).getResourceList();
                    recursiveResourceSearch(rl2, list);
                }
            }
        }
    }

    public static class BundleDataItem {
        public final Map<String, RequestConstraint> metadata;

        public final BinOffset offset;

        public BundleDataItem(Map<String, RequestConstraint> metadata,
                BinOffset offset) {
            this.metadata = metadata;
            this.offset = offset;
        }

        @Override
        public int hashCode() {
            final int prime = 31;
            int result = 1;
            result = prime * result
                    + ((metadata == null) ? 0 : metadata.hashCode());
            result = prime * result
                    + ((offset == null) ? 0 : offset.hashCode());
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
            BundleDataItem other = (BundleDataItem) obj;
            if (metadata == null) {
                if (other.metadata != null)
                    return false;
            } else if (!metadata.equals(other.metadata))
                return false;
            if (offset == null) {
                if (other.offset != null)
                    return false;
            } else if (!offset.equals(other.offset))
                return false;
            return true;
        }

    }
}
