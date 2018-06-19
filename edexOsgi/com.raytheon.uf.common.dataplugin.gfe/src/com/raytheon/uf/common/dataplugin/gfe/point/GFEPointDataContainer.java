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
package com.raytheon.uf.common.dataplugin.gfe.point;

import java.util.ArrayList;
import java.util.HashSet;
import java.util.List;
import java.util.Set;

import com.raytheon.uf.common.pointdata.IPointDataContainerReader;
import com.raytheon.uf.common.pointdata.IPointDataViewReader;
import com.raytheon.uf.common.serialization.annotations.DynamicSerialize;
import com.raytheon.uf.common.serialization.annotations.DynamicSerializeElement;

/**
 * GFE Point Data Container
 * 
 * <pre>
 * 
 * SOFTWARE HISTORY
 * Date         Ticket#    Engineer    Description
 * ------------ ---------- ----------- --------------------------
 * Aug  4, 2009            njensen     Initial creation
 * Apr 23, 2014  #3006     randerso    Added toString to aid in debugging
 * 
 * </pre>
 * 
 * @author njensen
 * @version 1.0
 */

@DynamicSerialize
public class GFEPointDataContainer implements IPointDataContainerReader {

    @DynamicSerializeElement
    private List<GFEPointDataView> views = new ArrayList<GFEPointDataView>();

    public void append(GFEPointDataView view) {
        views.add(view);
    }

    /*
     * (non-Javadoc)
     * 
     * @see
     * com.raytheon.uf.common.pointdata.IPointDataContainerReader#getCurrentSz()
     */
    @Override
    public int getCurrentSz() {
        return views.size();
    }

    /*
     * (non-Javadoc)
     * 
     * @see
     * com.raytheon.uf.common.pointdata.IPointDataContainerReader#getParameters
     * ()
     */
    @Override
    public Set<String> getParameters() {
        if (views.size() > 0) {
            return views.get(0).getParameters();
        } else {
            return new HashSet<String>();
        }
    }

    /*
     * (non-Javadoc)
     * 
     * @see
     * com.raytheon.uf.common.pointdata.IPointDataContainerReader#readRandom
     * (int)
     */
    @Override
    public IPointDataViewReader readRandom(int idx) {
        return views.get(idx);
    }

    public List<GFEPointDataView> getViews() {
        return views;
    }

    public void setViews(List<GFEPointDataView> views) {
        this.views = views;
    }

    /*
     * (non-Javadoc)
     * 
     * @see java.lang.Object#toString()
     */
    @Override
    public String toString() {
        return views.toString();
    }

}
