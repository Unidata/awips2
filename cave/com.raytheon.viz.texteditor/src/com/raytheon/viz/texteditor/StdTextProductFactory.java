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
package com.raytheon.viz.texteditor;

import com.raytheon.uf.common.dataplugin.text.db.OperationalStdTextProduct;
import com.raytheon.uf.common.dataplugin.text.db.PracticeStdTextProduct;
import com.raytheon.uf.common.dataplugin.text.db.StdTextProduct;
import com.raytheon.uf.common.dataplugin.text.db.StdTextProductId;
import com.raytheon.uf.common.wmo.AFOSProductId;
import com.raytheon.uf.common.wmo.WMOHeader;
import com.raytheon.viz.core.mode.CAVEMode;

/**
 * CAVEMode.
 * 
 * Holds the constants that define the CAVE mode.
 * 
 * <pre>
 * SOFTWARE HISTORY
 * Date         Ticket#     Engineer    Description
 * ----------   ----------  ----------- --------------------------
 * 10May2010    2187        cjeanbap    Initial Creation.
 * </pre>
 * 
 * @author cjeanbap
 * @version 1
 */
public class StdTextProductFactory {

    public static StdTextProduct getInstance(CAVEMode mode) {
        StdTextProduct textProduct = null;
        
        if (CAVEMode.OPERATIONAL.equals(mode) || CAVEMode.TEST.equals(mode)) {
            textProduct = new OperationalStdTextProduct();
        } 
        else {
            textProduct = new PracticeStdTextProduct();
        }
        
        return textProduct; 
    }
    
        
    /**
     * Returns the appropriate implementation of StdTextProduct. 
     * 
     * @return StdTextProduct abstract class.
     */
    public static StdTextProduct getStdTextProduct() {
        StdTextProduct textProduct = null;
                
        return textProduct;
    }
    
    /**
     * Returns the appropriate implementation of StdTextProduct. 
     * 
     * @return StdTextProduct abstract class.
     */
    public static StdTextProduct getStdTextProduct(WMOHeader wmoHeader, AFOSProductId afosId,
            String product) {
        StdTextProduct textProduct = null;
                
        
        return textProduct;
    }
    
    /**
     * Returns the appropriate implementation of StdTextProduct.
     *     
     * @param wmoid
     * @param site
     * @param cccid
     * @param nnnid
     * @param xxxid
     * @param hdrtime
     * @param bbbid
     * @param createtime
     * @param product
     * @return StdTextProduct abstract class.
     */
    public static StdTextProduct getStdTextProduct(String wmoid, String site, String cccid,
            String nnnid, String xxxid, String hdrtime, String bbbid,
            Long createtime, String product) {
        StdTextProduct textProduct = null;
        
        return textProduct;
    }
    
    /**
     * Returns the appropriate implementation of StdTextProduct.
     * 
     * @param operationaModeFlag
     * @param wmoid
     * @param site
     * @param cccid
     * @param nnnid
     * @param xxxid
     * @param hdrtime
     * @param bbbid
     * @param createtime
     * @param product
     * @return
     */
    public static StdTextProduct getStdTextProduct(boolean operationalMode, String wmoid, String site, String cccid,
            String nnnid, String xxxid, String hdrtime, String bbbid,
            Long createtime, String product) {
        StdTextProduct textProduct = null;
        
        return textProduct;
    }
    
   /**
    * Returns the appropriate implementation of StdTextProduct.
    *     
    * @param wmoid
    * @param site
    * @param cccid
    * @param nnnid
    * @param xxxid
    * @param hdrtime
    * @param bbbid
    * @param createtime
    * @param product
    * @param operationalFlag
    * @return StdTextProduct abstract class.
    */
    public static StdTextProduct getStdTextProduct(String wmoid, String site, String cccid,
            String nnnid, String xxxid, String hdrtime, String bbbid,
            Long createtime, String product, boolean operationalFlag) {
        StdTextProduct textProduct = null;
                
        
        return textProduct;
    }
    
    /**
     * Returns the appropriate implementation of StdTextProduct. 
     * 
     * @return StdTextProduct abstract class.
     */
    
    public static StdTextProduct getStdTextProduct(StdTextProductId prodIdToCopy, String bbbid,
            Long createtime, String product)  {
        StdTextProduct textProduct = null;
                        
        return textProduct;
    }
}
