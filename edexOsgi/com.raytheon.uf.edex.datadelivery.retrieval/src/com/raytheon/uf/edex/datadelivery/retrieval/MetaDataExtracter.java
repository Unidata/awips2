package com.raytheon.uf.edex.datadelivery.retrieval;

import java.util.Date;

import com.raytheon.uf.common.datadelivery.registry.Connection;
import com.raytheon.uf.common.datadelivery.retrieval.xml.ServiceConfig;

/**
 * Extract MetaData over the web.
 * 
 * <pre>
 * 
 * SOFTWARE HISTORY
 * Date         Ticket#    Engineer    Description
 * ------------ ---------- ----------- --------------------------
 * Feb 20, 2011    218      dhladky     Initial creation
 * Aug 08, 2012   1022      djohnson    Add @NotThreadSafe.
 * 
 * </pre>
 * 
 * @author dhladky
 * @version 1.0
 */
public abstract class MetaDataExtracter implements IExtractMetaData {

    protected String url;

    protected Connection conn = null;

    protected Date dataDate = null;

    protected ServiceConfig serviceConfig = null;

    public MetaDataExtracter(Connection conn) {
        this.conn = conn;
    }

    public Connection getConn() {
        return conn;
    }

    @Override
    public Date getDataDate() {
        return dataDate;
    }

    public String getUrl() {
        return url;
    }

    public void setConn(Connection conn) {
        this.conn = conn;
    }

    public void setDataDate(Date dataDate) {
        this.dataDate = dataDate;
    }

    @Override
    public void setUrl(String url) {
        this.url = url;
    }
}
