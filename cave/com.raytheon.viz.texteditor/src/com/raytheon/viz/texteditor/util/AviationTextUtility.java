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
package com.raytheon.viz.texteditor.util;

import java.text.SimpleDateFormat;
import java.util.Date;
import java.util.HashMap;
import java.util.List;
import java.util.TimeZone;

import javax.xml.bind.JAXBException;

import com.raytheon.uf.common.dataplugin.text.db.StdTextProduct;
import com.raytheon.uf.common.dataquery.requests.RequestConstraint;
import com.raytheon.uf.common.serialization.SerializationUtil;
import com.raytheon.uf.common.site.SiteMap;
import com.raytheon.uf.common.status.IUFStatusHandler;
import com.raytheon.uf.common.status.UFStatus;
import com.raytheon.uf.common.status.UFStatus.Priority;
import com.raytheon.uf.common.time.SimulatedTime;
import com.raytheon.uf.viz.core.catalog.LayerProperty;
import com.raytheon.uf.viz.core.catalog.ScriptCreator;
import com.raytheon.uf.viz.core.comm.Loader;
import com.raytheon.uf.viz.core.exception.VizException;
import com.raytheon.uf.viz.core.localization.LocalizationManager;
import com.raytheon.uf.viz.core.status.StatusConstants;
import com.raytheon.viz.core.mode.CAVEMode;
import com.raytheon.viz.texteditor.Activator;
import com.raytheon.viz.texteditor.StdTextProductFactory;
import com.raytheon.viz.texteditor.msgs.IAviationObserver;

/**
 * This class is a utility for the Aviation Plugin.
 * 
 * <pre>
 * SOFTWARE HISTORY
 * Date			Ticket#		Engineer	Description
 * ------------	----------	-----------	--------------------------
 * May 15, 2008	1119		grichard	Initial creation.
 * 07/28/2009   2610        rjpeter     Moved error handling to alert viz.
 * 04/14/2010   4734        mhuang      Corrected StdTextProduct import 
 *                                       dependency
 * 05/10/2010   2187        cjeanbap    Added StdTextProductFactory 
 *                                       functionality.
 * </pre>
 * 
 * @author grichard
 * @version 1.0
 */

public class AviationTextUtility implements IAviationObserver {
    private static final transient IUFStatusHandler statusHandler = UFStatus.getHandler(AviationTextUtility.class);

    /**
     * Method to save a temporary working version of a TAF bulletin to the text
     * database.
     * 
     * @param tmpStr
     *            -- the temporary working version of a TAF bulletin
     */
    @Override
    public void saveTafBulletin(String tmpStr) {
        // Convert the text to uppercase
        tmpStr = tmpStr.toUpperCase();
        String currentDate = getCurrentDate();

        // Set the node based on localization.
        String siteNode = LocalizationManager.getInstance().getCurrentSite();

        // Set the Site ID based on localization.
        String siteName = SiteMap.getInstance().getSite4LetterId(siteNode);
        if ((siteName == null) || (siteName.equals(""))) {
            siteName = "CCCC";
        }

        String siteWmoId = "FTUS43";
        String currentHeader = getHeaderTextField(siteWmoId, siteName,
                currentDate, "\n", "WRK" + "TAF");
        // System.out.println("Current header: " + currentHeader);

        // New up request constraint for table request and
        // also of the classname of the table for the request
        RequestConstraint rcTable = new RequestConstraint("table");
        String tableDatabaseName = "fxa";
        String tableClassName = StdTextProduct.class.getName();

        RequestConstraint rcDatabase = new RequestConstraint(tableDatabaseName);
        RequestConstraint rcClass = new RequestConstraint(tableClassName);
        // New up request constraint for table response using the
        // tmpStr editor content after marshalling this string to
        // XML format via the Util class.
        // New up a StdTextProduct, then set the product component
        // to the tmpStr that represents the new content.

        StdTextProduct tmpProd = StdTextProductFactory.getInstance(CAVEMode
                .getMode());
        tmpProd.setWmoid(siteWmoId);
        tmpProd.setSite(siteName);
        tmpProd.setCccid(siteNode);
        tmpProd.setNnnid("WRK");
        tmpProd.setXxxid("TAF");
        tmpProd.setHdrtime(currentDate);
        tmpProd.setBbbid("NOR");
        tmpProd.setRefTime(System.currentTimeMillis());
        tmpProd.setProduct(currentHeader + "\n" + tmpStr);

        RequestConstraint rcRow;
        try {
            rcRow = new RequestConstraint(
                    SerializationUtil.marshalToXml(tmpProd));
        } catch (JAXBException e) {
            statusHandler.handle(Priority.PROBLEM,
                    "Error serializing data", e);
            return;
        }
        // New up hash map and populate with entry query
        // parameters before newing up layer property
        HashMap<String, RequestConstraint> query = new HashMap<String, RequestConstraint>();
        query.put("pluginName", rcTable);
        query.put("databasename", rcDatabase);
        query.put("classname", rcClass);
        query.put("rowname", rcRow);
        // New up layer property and set then entry parameters
        LayerProperty lpParm = new LayerProperty();
        try {
            lpParm.setEntryQueryParameters(query, false);
            // Create Image <Any> Script for table request
            String tableScript = ScriptCreator.createUpdateScript(lpParm);
            // Capture the script to the console for now...
            // System.out.printf("The update script is: %n%s%n", tableScript);
            // Later call loadData method to run the script...
            // For now, mouse the script in to the AWIPS Test Driver
            // Interface through the Request/Response Message dialog.
            // List<IMarshallable> list;
            // list = Loader.loadData(tableScript, 10000);
            Loader.loadData(tableScript, 10000);
            // textEditor.insert(((StdTextProduct) (list.get(0)))
            // .getProduct());
        } catch (VizException e1) {
            statusHandler.handle(Priority.PROBLEM,
                    "Error retrieving metadata", e1);
        }
    }

    /**
     * Method to recover a temporary working version of a TAF bulletin from the
     * text database.
     * 
     * @return String -- recovered temporary working version of a TAF bulletin
     */
    @Override
    public List<Object> recoverTafBulletin() {
        // Set the node based on localization.
        String siteNode = LocalizationManager.getInstance().getCurrentSite();

        // Set the Site ID based on localization.
        String siteName = LocalizationManager.getInstance().getCurrentSite()
                .toUpperCase();
        if (siteName.length() == 3) {
            siteName = SiteMap.getInstance().getSite4LetterId(siteName);
        }
        if ((siteName == null) || (siteName.equals(""))) {
            siteName = "CCCC";
        }

        // New up request constraint for table request and
        // also of the class name of the table for the request
        RequestConstraint rcTable = new RequestConstraint("table");
        String tableDatabaseName = "fxa";
        String tableClassName = StdTextProduct.class.getName();

        RequestConstraint rcDatabase = new RequestConstraint(tableDatabaseName);
        RequestConstraint rcClass = new RequestConstraint(tableClassName);

        RequestConstraint rcNode = new RequestConstraint(siteNode);
        RequestConstraint rcCategory = new RequestConstraint("WRK");
        RequestConstraint rcDesignator = new RequestConstraint("TAF");

        // New up hash map and populate with entry query
        // parameters before newing up layer property
        HashMap<String, RequestConstraint> query = new HashMap<String, RequestConstraint>();
        query.put("pluginName", rcTable);
        query.put("databasename", rcDatabase);
        query.put("classname", rcClass);
        query.put("prodId.cccid", rcNode);
        query.put("prodId.nnnid", rcCategory);
        query.put("prodId.xxxid", rcDesignator);

        // New up layer property and set then entry parameters
        LayerProperty lpParm = new LayerProperty();
        try {
            lpParm.setEntryQueryParameters(query, false);
            // Create Image <Any> Script for table request
            String tableScript = ScriptCreator.createScript(lpParm);
            // Capture the script to the console for now...
            // System.out.printf("The query script is: %n%s%n",
            // tableScript);
            return Loader.loadData(tableScript, 10000);
        } catch (VizException e1) {
            statusHandler.handle(Priority.PROBLEM,
                    "Error retrieving metadata", e1);
        }
        return null;
    }

    private String getCurrentDate() {
        Date now = SimulatedTime.getSystemTime().getTime();
        SimpleDateFormat formatter = new SimpleDateFormat("ddHHmm");
        formatter.setTimeZone(TimeZone.getTimeZone("GMT"));
        return (formatter.format(now));
    }

    private String getHeaderTextField(String wmoId, String siteId,
            String dateId, String separator, String nnnxxx) {
        return wmoId + " " + siteId + " " + dateId + separator + nnnxxx;

    }

}
