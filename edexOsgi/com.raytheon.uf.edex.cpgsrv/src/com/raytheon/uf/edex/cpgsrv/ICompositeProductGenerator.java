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

package com.raytheon.uf.edex.cpgsrv;

/**
 * Interface ICompoisteProductGenerator
 * 
 * Interface for creating composite products that are derived from 
 * combinations of products that currently are plugged into EDEX.
 * It listens to the JMS message that notifies DataURI's to client CAVE
 * instances.  When the rules for creation of the product you are using 
 * this interface for are satisfied (i.e. all of your products are present and current), 
 * it kicks off your product generator. It ends successfully when your product 
 * is dropped into EDEX thus triggering message notifying CAVE 
 * instances that the product is ready.
 * 
 * <pre>
 * SOFTWARE HISTORY
 * Date         Ticket#    Engineer    Description
 * ------------ ---------- ----------- --------------------------
 * 06Feb2009    1981       dhladky     Creation.
 * 30Nov2012    1372       dhladky     Added latency timing stats
 *
 * @author dhladky
 * @version 1.0
 */

import com.raytheon.edex.urifilter.URIGenerateMessage;
import com.raytheon.uf.common.time.DataTime;

public interface ICompositeProductGenerator {

    public void generateProduct(URIGenerateMessage genMessage);

    public void setCompositeProductType(String dataType);

    public String getCompositeProductType();

    public void setProductTime(URIGenerateMessage genMessage);

    public DataTime getProductTime();

    public void log(URIGenerateMessage message);

}
