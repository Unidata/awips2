package gov.noaa.nws.ncep.edex.purgeutil;

/**
 * 
 * gov.noaa.nws.ncep.edex.purgeutil.DbPurge
 * 
 * This java class provides AWIPS web plugin DB purging interface implementations.
 * This code has been developed by the SIB for use in the AWIPS2 system.
 * 
 * <pre>
 * SOFTWARE HISTORY
 * 
 * Date         Ticket#    	Engineer    Description
 * -------		------- 	-------- 	-----------
 * 12/02/2010	TBD			Chin Chen	Initial coding
 *
 * </pre>
 * 
 * @author Chin Chen
 * @version 1.0
 */

import java.io.ByteArrayOutputStream;
import java.io.IOException;
import java.io.InputStream;

import org.apache.http.HttpResponse;
import org.apache.http.client.methods.HttpPost;
import org.apache.http.entity.StringEntity;
import org.apache.http.impl.conn.tsccm.ThreadSafeClientConnManager;
import org.apache.http.util.EntityUtils;

import com.raytheon.uf.edex.core.EdexException;
import com.raytheon.uf.edex.database.plugin.PluginDao;
import com.raytheon.uf.edex.database.plugin.PluginFactory;

public class DbPurge {

    private static org.apache.http.impl.client.DefaultHttpClient client;

    /*
     * This is purge entry point called by JavaScript PurgeControls.purge
     * (purgeControls.js) from AWIPS Plugin Database Purge web interface.
     */
    public String purgeDbTable(String plugin) {
        // System.out.println("purging "+ plugin);
        if (plugin == null || plugin.length() == 0)
            return "Purge failed! <null plugin>";

        try {
            String response = requestHTTP(plugin);
            return response;
        } catch (EdexException e1) {
            // TODO Auto-generated catch block
            e1.printStackTrace();
            return "Purge " + plugin + " failed! <exception>";
        }

    }

    public String purgeAllTable(String plugin) {
        // System.out.println("purgeAllTable entered");
        PluginFactory pf = PluginFactory.getInstance();
        if (pf.isRegistered(plugin)) {
            PluginDao dao = null;
            try {
                dao = pf.getPluginDao(plugin);
                dao.purgeAllData();
            } catch (Throwable e) {
                System.out.println("purge all failed");
                e.printStackTrace();
                return "Purge " + plugin + " failed!";
            }

        } else
            return plugin + " is not registered as plugin!";

        return "Purge " + plugin + " is done!";
    }

    /*
     * Since web browser is running on its own JVM, a HTTP request to EDEX
     * server JVM is used. Http address
     * http://localhost:9581/services/purgePlugin, and Executing Java bean
     * (DbPurge) and method (purgeAllTable) are defined in Spring XML -
     * purgeutil-request.xml.
     */
    public static String requestHTTP(String message) throws EdexException {
        if (client == null)
            client = new org.apache.http.impl.client.DefaultHttpClient(
                    new ThreadSafeClientConnManager());

        try {

            HttpPost put = new HttpPost(
                    "http://localhost:9581/services/purgePlugin");
            put.setEntity(new StringEntity(message, "text/xml", "ISO-8859-1"));

            HttpResponse resp = client.execute(put);
            int code = resp.getStatusLine().getStatusCode();

            if (code != 200) {
                throw new EdexException(
                        "Error reading server response.  Got error message: "
                                + EntityUtils.toString(resp.getEntity()));
            }

            ByteArrayOutputStream baos = null;
            InputStream is = null;
            try {
                is = resp.getEntity().getContent();
                baos = new ByteArrayOutputStream();
                int read = 0;
                do {
                    byte[] tmp = new byte[1024];
                    read = is.read(tmp);

                    if (read > 0)
                        baos.write(tmp, 0, read);
                } while (read > 0);

                return new String(baos.toByteArray());
            } finally {
                // It seems we do not need to do this with 4.1 closing the
                // input stream from the entity ( 'is' at the time of
                // writing ) should allow the connection te be released

                // if (put != null) {
                // put.releaseConnection();
                // }

                try {
                    if (baos != null)
                        baos.close();
                } catch (IOException e1) {
                    // ignore
                }
                try {
                    if (is != null)
                        is.close();
                } catch (IOException e) {
                    // ignore
                }

                try {
                    if (resp != null && resp.getEntity() != null) {
                        EntityUtils.consume(resp.getEntity());
                    }
                } catch (IOException e) {
                    // if there was an error reading the input stream, assume it
                    // was closed
                    // TODO - log?
                }
            }
        } catch (Exception e) {
            throw new EdexException("Unable to connect to server", e);
        }

    }
}
