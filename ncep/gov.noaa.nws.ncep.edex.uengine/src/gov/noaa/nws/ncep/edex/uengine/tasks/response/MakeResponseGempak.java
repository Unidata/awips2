/*
 * Response
 *
 * Date created 06 Feb 2009
 *
 * This code has been developed by the SIB for use in the AWIPS2 system.
 *
 */

package gov.noaa.nws.ncep.edex.uengine.tasks.response;

import static com.raytheon.uf.common.localization.LocalizationContext.LocalizationType.EDEX_STATIC;

import java.io.ByteArrayInputStream;
import java.io.ByteArrayOutputStream;
import java.io.File;
import java.util.ArrayList;
import java.util.List;

import javax.xml.bind.JAXBException;
import javax.xml.transform.Source;
import javax.xml.transform.Transformer;
import javax.xml.transform.TransformerException;
import javax.xml.transform.TransformerFactory;
import javax.xml.transform.stream.StreamResult;
import javax.xml.transform.stream.StreamSource;

import com.raytheon.uf.common.dataplugin.PluginDataObject;
import com.raytheon.uf.common.localization.IPathManager;
import com.raytheon.uf.common.localization.LocalizationContext;
import com.raytheon.uf.common.localization.LocalizationContext.LocalizationLevel;
import com.raytheon.uf.common.localization.PathManagerFactory;

/**
 * MakeResponseGempak
 * 
 * Performs conversion of input XML string, containing data described in
 * database terms, into a string, containing data described in GEMPAK SFPARM
 * terms.
 * 
 * <pre>
 * 
 * SOFTWARE HISTORY
 * 
 * Date                 Ticket#         Engineer                Description
 * ------------         ----------      -----------             --------------------------
 * 03/18/2009                           mgamazaychikov          Initial Creation
 * 06/02/2009			92				mgamazaychikov			Added error message for
 * 																unavailable data class
 * 
 * </pre>
 * 
 * @author mgamazaychikov
 * @version 1
 */

public class MakeResponseGempak {
    private PluginDataObject record;

    private String input;

    public MakeResponseGempak(PluginDataObject aRecord) throws JAXBException {
        record = aRecord;
        input = aRecord.toXML();
        System.out.println("input =" + input);
    }

    @SuppressWarnings("unchecked")
    public List<String> execute() throws Exception {

        final String NCEP_DIR = "ncep";
        final String xsltDir = "xslt";
        final String Db2GempakTableName = "db2gempak_metar.xlt";

        IPathManager manager = PathManagerFactory.getPathManager();

        LocalizationContext baseContext = null;

        File baseDir = null;

        baseContext = manager.getContext(EDEX_STATIC, LocalizationLevel.BASE);
        baseContext.setContextName(NCEP_DIR);

        baseDir = manager.getFile(baseContext, "");

        String xsltFilename = null;
        List<String> resultsBack = new ArrayList();
        /*
         * Create string representing the record class
         */
        String responseClass = record.getClass().getSimpleName();

        /*
         * Prepare the input file
         */
        if (responseClass.equals("MetarRecord")) {
            // xsltFilename = "baseDir/ncep/xslt/db2gempak_metar.xlt";
            xsltFilename = baseDir + File.separator + xsltDir + File.separator
                    + Db2GempakTableName;
        } else {
            resultsBack
                    .add("Response for this data class is currently unavailable.");
            return resultsBack;
        }
        System.out.println("xsltFilename=" + xsltFilename);
        File xsltFile = new File(xsltFilename);

        /*
         * Remove the start of heading and the end of text control characters
         * from the string before processing
         */
        String stControlC = "\\u0003";
        String stControlA = "\\u0001";
        input = input.replaceAll(stControlC, " ");
        input = input.replaceAll(stControlA, " ");

        /*
         * Convert XML string into xmlSourse
         */
        Source xmlSource = new StreamSource(new ByteArrayInputStream(
                input.getBytes()));

        /*
         * Construct xsltSource from xslfFile
         */
        Source xsltSource = new StreamSource(xsltFile);

        /*
         * Use the factory for XSLT transformer
         */
        TransformerFactory transFact = TransformerFactory.newInstance();
        Transformer trans = transFact.newTransformer(xsltSource);

        /*
         * Create object for the transformation product
         */
        ByteArrayOutputStream baos = new ByteArrayOutputStream();
        try {
            trans.transform(xmlSource, new StreamResult(baos));
        } catch (TransformerException e) {
            /*
             * Catch invalid control characters in the report
             */
            resultsBack.add("The report contains invalid control characters");
            return resultsBack;
        }

        /*
         * Convert transformation product to string
         */
        String res = new String(baos.toByteArray());

        /*
         * Create the resulting string
         */
        String result = responseClass + ":" + res;

        /*
         * Return the response
         */
        resultsBack.add(result);
        return resultsBack;
    }

}
