package com.raytheon.uf.edex.datadelivery.retrieval;

import java.util.Date;
import java.util.Map;

/**
 * Extract MetaData Interface
 * 
 * <pre>
 * 
 * SOFTWARE HISTORY
 * Date         Ticket#    Engineer    Description
 * ------------ ---------- ----------- --------------------------
 * Feb 20, 2011    218      dhladky     Initial creation
 * 
 * </pre>
 * 
 * @author dhladky
 * @version 1.0
 */

public interface IExtractMetaData {

    Map<String, Object> extractMetaData() throws Exception;

    boolean checkLastUpdate(Date lastUpdate);

    void setDataDate() throws Exception;

    Date getDataDate();

    void setUrl(String url);
}
