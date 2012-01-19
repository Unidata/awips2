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
package com.raytheon.viz.grid.gridcache;

import java.util.ArrayList;
import java.util.List;

/**
 * Model node, top of the node tree.
 * 
 * <pre>
 *
 * SOFTWARE HISTORY
 *
 * Date         Ticket#    Engineer    Description
 * ------------ ---------- ----------- --------------------------
 * Nov 13, 2009 3579       mpduff      Initial creation
 *
 * </pre>
 *
 * @author mpduff
 * @version 1.0	
 */

public class ModelNode extends CacheNode {
    private List<ParameterNode> parameterNodeList = new ArrayList<ParameterNode>();
    
    
    /**
     * @return the parameterNodeList
     */
    public List<ParameterNode> getParameterNodeList() {
        return parameterNodeList;
    }


    public void addParameterNode(ParameterNode pnode) {
        parameterNodeList.add(pnode);
    }
}
