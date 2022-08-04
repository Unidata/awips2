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

package com.raytheon.viz.texteditor.msgs;

import com.raytheon.uf.common.dataplugin.text.db.StdTextProduct;

/**
 * The IWmoBrowserCallback interface specifies methods that return a selected
 * text product.
 * 
 * <pre>
 * 
 * SOFTWARE HISTORY
 * 
 * Date       	Ticket#		Engineer	Description
 * ------------	----------	-----------	--------------------------
 * 08/3/2009    2191        rjpeter     Initial creation.
 * 04/14/2010   4734        mhuang      Corrected StdTextProduct import 
 *                                       dependency
 * 09/11/2014   3580        mapeters    Removed getQueryTransport().
 * 
 * </pre>
 * 
 * @author rjpeter
 * 
 */

public interface IWmoBrowserCallback {

    public void setDisplayedProduct(StdTextProduct product);

    public void setCommandText(String commandText);

    public void setTTAAiiField(String ttaaii);

    public void setCCCCField(String cccc);
}
