package com.raytheon.uf.edex.datadelivery.retrieval;

import java.util.List;

import com.raytheon.uf.common.datadelivery.registry.SubscriptionBundle;

/**
 * Generate Retrieval Interface
 * 
 * <pre>
 * 
 * SOFTWARE HISTORY
 * Date         Ticket#    Engineer    Description
 * ------------ ---------- ----------- --------------------------
 * Feb 07, 2012            dhladky     Initial creation
 * 
 * </pre>
 * 
 * @author dhladky
 * @version 1.0
 */

public interface IGenerateRetrieval {

    public List<String> generateRetrieval(List<SubscriptionBundle> bundle);

}
